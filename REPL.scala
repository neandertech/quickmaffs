import cats.effect.*
import cats.effect.std.Console as CE_Console
import fs2.concurrent.SignallingRef

import cats.syntax.all.*
import cats.Show

object Colors:
  def red[A](s: A)(using show: Show[A] = Show.fromToString[A]): String =
    Console.RED + show.show(s) + Console.RESET

  def green[A](s: A)(using show: Show[A] = Show.fromToString[A]): String =
    Console.GREEN + show.show(s) + Console.RESET

  def yellow[A](s: A)(using show: Show[A] = Show.fromToString[A]): String =
    Console.YELLOW + show.show(s) + Console.RESET
end Colors

class REPL(linesIn: fs2.Stream[IO, String], out: String => IO[Unit]):
  def outLine[A](s: A)(using show: Show[A] = Show.fromToString) =
    out(show.show(s) + "\n")

  def run =
    import Colors.*
    val state = (
      Program[WithSpan](statements = Vector.empty, text = ""),
      Map.empty[String, Int]
    )
    fs2.Stream.eval(IO.ref(state)).flatMap { ref =>
      fs2.Stream.eval(IO.deferred[Either[Throwable, Unit]]).flatMap { halt =>
        val prompt = out(yellow("> "))
        val intro =
          outLine("Welcome to Quickmaffs REPL") *> prompt

        def process(line: String) =
          QuickmaffsParser.parseExpr(line) match
            case Left(err) =>
              QuickmaffsParser.parseStatement(line) match
                case Left(err) =>
                  outLine(red("Parsing error"))

                case Right(statement) =>
                  ref.get.flatMap { case (prog, state) =>
                    val newProg = Program(prog.statements :+ statement, "")

                    QuickmaffsCompiler.compile(newProg) match
                      case Left(errs) =>
                        errs.traverse(msg => outLine(red("! " + msg.message)))
                      case Right(idx) =>
                        QuickmaffsEvaluator.evaluate(newProg) match
                          case Right(interpreted) =>
                            ref.set(newProg -> interpreted)
                          case Left(err) =>
                            outLine(red("!! " + err.message))
                    end match

                  }

            case Right(expr) =>
              ref.get.map(_._2).flatMap { state =>
                QuickmaffsEvaluator.evaluate(expr, state) match
                  case Right(value) =>
                    outLine(green(value))
                  case Left(err) =>
                    outLine(red("! " + err.message))
              }

          end match
        end process

        fs2.Stream.eval(intro) ++
          linesIn
            .map(_.trim)
            .evalMap {
              case e if Set("quit", "exit", "stop").contains(e.toLowerCase()) =>
                halt.complete(Right(()))
              case other =>
                process(other) *> prompt
            }
            .interruptWhen(halt)
      }
    }

  end run
end REPL

object REPL extends IOApp.Simple:
  def run =
    REPL(
      fs2.io
        .stdin[IO](128)
        .through(fs2.text.utf8.decode)
        .through(fs2.text.lines),
      cats.effect.std.Console[IO].print
    ).run.compile.drain
  end run
end REPL
