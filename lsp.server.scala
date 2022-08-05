//> using lib "tech.neander::langoustine-lsp::0.0.6"
//> using lib "tech.neander::jsonrpclib-fs2::0.0.2"
//> using lib "co.fs2::fs2-io::3.2.11"

import langoustine.lsp.*

import requests.*
import notifications as nt
import structures.*
import aliases.*
import enumerations.*
import json.*

import cats.effect.*
import jsonrpclib.fs2.*

import fs2.io.file.Files
import fs2.text
import fs2.io.file.Path
import cats.parse.Parser
import QuickmaffsCompiler.CompileError
import QuickmaffsCompiler.Index
import langoustine.lsp.RuntimeBase.DocumentUri
import langoustine.lsp.RuntimeBase.uinteger

import cats.syntax.all.*

object LSP extends IOApp.Simple:
  import QuickmaffsLSP.{server, State}
  def run =
    FS2Channel
      .lspCompliant[IO](
        byteStream = fs2.io.stdin(256),
        byteSink = fs2.io.stdout
      )
      .evalTap { channel =>
        IO.ref(Map.empty[DocumentUri, State])
          .flatMap { state =>
            server(state).bind(channel)
          }
      }
      .flatMap(_.openStream)
      .evalMap(_ => IO.never)
      .compile
      .drain
  end run

end LSP

object QuickmaffsLSP:
  enum State:
    case Empty
    case InvalidCode(err: QuickmaffsParser.ParsingError)
    case InvalidProgram(errors: Vector[CompileError])
    case RuntimeError(error: QuickmaffsEvaluator.EvaluationError)
    case Ok(
        idx: Index,
        interpreted: Map[String, Int],
        program: Program[WithSpan]
    )
  end State

  extension (s: cats.parse.Caret)
    def toPosition: Position =
      Position(line = uinteger(s.line), character = uinteger(s.col))

  extension (s: Span)
    def toRange: Range = Range(s.from.toPosition, s.to.toPosition)

  extension (s: Position)
    def toCaret = cats.parse.Caret(s.line.value, s.character.value, -1)

  def server(state: Ref[IO, Map[DocumentUri, State]]) =
    import QuickmaffsCompiler.*

    def process(s: String) =
      QuickmaffsParser.parse(s) match
        case Left(e) => State.InvalidCode(e)
        case Right(parsed) =>
          compile(parsed) match
            case Left(errs) => State.InvalidProgram(errs)
            case Right(idx) =>
              QuickmaffsEvaluator.evaluate(parsed) match
                case Left(err) => State.RuntimeError(err)
                case Right(ok) => State.Ok(idx, ok, parsed)
    end process

    def processFile(path: Path) =
      Files[IO].readAll(path).through(text.utf8.decode).compile.string.map {
        contents =>
          process(contents)
      }

    def processUri(uri: DocumentUri) =
      val path = uri.value.drop("file://".length)
      processFile(Path(path))

    def set(u: DocumentUri)(st: State) =
      state.update(_.updated(u, st)) <*
        cats.effect.std
          .Console[IO]
          .errorln(s"Setting $st for $u")

    def get(u: DocumentUri) =
      state.get.map(_.get(u))

    def recompile(uri: DocumentUri, back: Communicate[IO]) =
      def publish(vec: Vector[Diagnostic]) =
        back.notification(
          nt.textDocument.publishDiagnostics,
          PublishDiagnosticsParams(uri, diagnostics = vec)
        )

      processUri(uri)
        .flatTap(set(uri))
        .flatTap {
          case _: State.Ok | State.Empty => publish(Vector.empty)
          case State.InvalidProgram(errs) =>
            val diags = errs.map { case CompileError(span, msg) =>
              Diagnostic(
                range = span.toRange,
                message = msg,
                severity = Opt(DiagnosticSeverity.Error)
              )
            }

            publish(diags)

          case State.InvalidCode(parseError) =>
            publish(
              Vector(
                Diagnostic(
                  range = Span(parseError.caret, parseError.caret).toRange,
                  message = "Parsing failed",
                  severity = Opt(DiagnosticSeverity.Error)
                )
              )
            )
          case State.RuntimeError(err) =>
            val zero = uinteger(0)
            publish(
              Vector(
                Diagnostic(
                  range = Range(Position(zero, zero), Position(zero, zero)),
                  message = s"Runtime: ${err.message}",
                  severity = Opt(DiagnosticSeverity.Error)
                )
              )
            )
        }
        .void
    end recompile

    def variableUnderCursor(doc: DocumentUri, position: Position) =
      get(doc).map {
        case Some(State.Ok(idx, _, _)) =>
          idx.variables
            .find { case (name, vdf) =>
              vdf.references.exists(_.contains(position.toCaret))
            }
        case _ => None
      }

    ImmutableLSPBuilder
      .create[IO]
      .handleRequest(initialize) { (in, back) =>
        back.notification(
          nt.window.showMessage,
          ShowMessageParams(
            message = "Hello from Quickmaffs",
            `type` = enumerations.MessageType.Info
          )
        ) *>
          IO {
            InitializeResult(
              ServerCapabilities(
                hoverProvider = Opt(true),
                definitionProvider = Opt(true),
                documentSymbolProvider = Opt(true),
                renameProvider = Opt(true),
                textDocumentSync = Opt(
                  TextDocumentSyncOptions(
                    openClose = Opt(true),
                    save = Opt(true)
                  )
                )
              ),
              Opt(
                InitializeResult
                  .ServerInfo(name = "Quickmaffs LSP", version = Opt("0.0.1"))
              )
            )
          }
      }
      .handleNotification(nt.textDocument.didOpen) { (in, back) =>
        recompile(in.textDocument.uri, back)
      }
      .handleNotification(nt.textDocument.didSave) { (in, back) =>
        recompile(in.textDocument.uri, back)
      }
      .handleRequest(textDocument.definition) { (in, back) =>
        variableUnderCursor(in.textDocument.uri, in.position).map {
          foundMaybe =>
            foundMaybe
              .map(_._2)
              .map { vdf =>
                Definition(Location(in.textDocument.uri, vdf.definedAt.toRange))
              }
              .getOrElse(null)
        }
      }
      .handleRequest(textDocument.hover) { (in, back) =>
        get(in.textDocument.uri).map {
          case Some(State.Ok(idx, values, program)) =>
            idx.variables
              .find { case (name, vdf) =>
                vdf.references.exists(_.contains(in.position.toCaret))
              }
              .map { case (varName, vdf) =>
                val value = values(varName)
                val text = program.text.slice(
                  vdf.fullDefinition.from.offset,
                  vdf.fullDefinition.to.offset
                )

                Nullable {
                  Hover(
                    MarkupContent(
                      kind = MarkupKind.Markdown,
                      s"""
                        |`$varName`
                        |---
                        |
                        |**Value**: $value 
                        |
                        |**Formula**: $text
                        """.stripMargin.trim
                    )
                  )
                }
              }
              .getOrElse(Nullable.NULL)

          case _ => Nullable.NULL
        }
      }
      .handleRequest(textDocument.documentSymbol) { (in, back) =>
        get(in.textDocument.uri).map {
          case Some(State.Ok(idx, _, _)) =>
            idx.variables.toVector.sortBy(_._1).map { case (n, df) =>
              SymbolInformation(
                location = Location(in.textDocument.uri, df.definedAt.toRange),
                name = n,
                kind = enumerations.SymbolKind.Variable
              )
            }

          case _ => Vector.empty
        }
      }
      .handleRequest(textDocument.rename) { (in, back) =>
        variableUnderCursor(in.textDocument.uri, in.position).map {
          foundMaybe =>
            foundMaybe
              .map { case (oldName, vdf) =>
                val edits = (vdf.definedAt +: vdf.references).map { span =>
                  TextEdit(range = span.toRange, newText = in.newName)
                }

                Nullable {
                  WorkspaceEdit(
                    changes = Opt(
                      Map(
                        in.textDocument.uri -> edits
                      )
                    )
                  )
                }
              }
              .getOrElse(Nullable.NULL)
        }
      }
  end server
end QuickmaffsLSP
