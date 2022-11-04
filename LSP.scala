import langoustine.lsp.*

import cats.effect.*
import jsonrpclib.fs2.*

import fs2.io.file.Files
import fs2.text
import fs2.io.file.Path
import cats.parse.Parser
import QuickmaffsCompiler.CompileError
import QuickmaffsCompiler.Index
import langoustine.lsp.runtime.DocumentUri
import langoustine.lsp.runtime.Opt
import langoustine.lsp.runtime.uinteger

import cats.syntax.all.*
import io.chrisdavenport.crossplatformioapp.CrossPlatformIOApp

object LSP extends CrossPlatformIOApp.Simple:
  import QuickmaffsLSP.{server, State}

  def run =
    FS2Channel[IO](2048, None)
      .evalTap { channel =>
        IO.ref(Map.empty[DocumentUri, State])
          .flatMap { state =>
            server(state).bind(channel, shutdown = IO.unit)
          }
      }
      .flatMap(channel =>
        fs2.Stream
          .eval(IO.never) // running the server forever
          .concurrently(
            fs2.io
              .stdin[IO](512)
              .through(lsp.decodePayloads)
              .through(channel.input)
          )
          .concurrently(
            channel.output
              .through(lsp.encodePayloads)
              .through(fs2.io.stdout[IO])
          )
      )
      .compile
      .drain
      .guarantee(IO.consoleForIO.errorln("Terminating server"))
  end run

end LSP

object QuickmaffsLSP:
  import requests.*
  import aliases.*
  import enumerations.*
  import structures.*

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
      Position(line = s.line, character = s.col)

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
          textDocument.publishDiagnostics,
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
            val zero = 0
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

    LSPBuilder
      .create[IO]
      .handleRequest(initialize) { (in, back) =>
        back.notification(
          window.showMessage,
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
      .handleNotification(textDocument.didOpen) { (in, back) =>
        recompile(in.textDocument.uri, back)
      }
      .handleNotification(textDocument.didSave) { (in, back) =>
        recompile(in.textDocument.uri, back)
      }
      .handleRequest(textDocument.definition) { (in, back) =>
        variableUnderCursor(in.textDocument.uri, in.position).map {
          foundMaybe =>
            foundMaybe
              .map(_._2)
              .map { vdf =>
                Opt(
                  Definition(
                    Location(in.textDocument.uri, vdf.definedAt.toRange)
                  )
                )
              }
              .getOrElse(Opt.empty)
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

                Opt {
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
              .getOrElse(Opt.empty)

          case _ => Opt.empty
        }
      }
      .handleRequest(textDocument.documentSymbol) { (in, back) =>
        get(in.textDocument.uri).map {
          case Some(State.Ok(idx, _, _)) =>
            Opt {
              idx.variables.toVector.sortBy(_._1).map { case (n, df) =>
                SymbolInformation(
                  location =
                    Location(in.textDocument.uri, df.definedAt.toRange),
                  name = n,
                  kind = enumerations.SymbolKind.Variable
                )
              }
            }

          case _ => Opt(Vector.empty)
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

                Opt {
                  WorkspaceEdit(
                    changes = Opt(
                      Map(
                        in.textDocument.uri -> edits
                      )
                    )
                  )
                }
              }
              .getOrElse(Opt.empty)
        }
      }
  end server
end QuickmaffsLSP
