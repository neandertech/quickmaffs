import cats.effect.*
import cats.effect.std.*
import fs2.io.file.*
import cats.parse.*

object INTERPRETER extends IOApp:
  def run(args: List[String]) =
    Files[IO]
      .readAll(Path(args.head))
      .through(fs2.text.utf8.decode)
      .compile
      .string
      .flatMap { content =>
        QuickmaffsParser.parse(content) match
          case Left(err) =>
            printErrors(content, List(err.caret -> "Parsing error"))
          case Right(prg) =>
            QuickmaffsCompiler.compile(prg) match
              case Left(errs) =>
                printErrors(
                  content,
                  errs.map(ce => ce.position.from -> ce.message)
                )
              case Right(idx) =>
                QuickmaffsEvaluator.evaluate(prg) match
                  case Left(errs) =>
                    printErrors(
                      content,
                      List(Caret(0, 0, 0) -> errs.message)
                    )
                  case Right(st) =>
                    printErrors(
                      content,
                      List.empty,
                      st.map { case (name, value) =>
                        idx.variables(name).definedAt.from.line -> value
                      }
                    )

      }
      .as(ExitCode.Success)
end INTERPRETER

def printErrors(
    program: String,
    errors: Seq[(Caret, String)],
    results: Map[Int, Int] = Map.empty
): IO[Unit] =
  val byLine = errors.groupBy(_._1.line)
  val sb     = StringBuilder()

  import Colors.*

  val size  = program.linesIterator.length
  val round = math.log10(size).toInt + 1

  program.linesIterator.zipWithIndex.foreach { case (l, i) =>
    val lineNumber = i.toString.reverse.padTo(round, '0').reverse
    val result     = results.get(i).map(v => green(s" // $v")).getOrElse("")
    val pref       = s"[$lineNumber]: "
    sb.append(yellow(pref) + l + s"$result\n")
    inline def shift(c: Caret) = " " * (pref.length + c.col)
    byLine.get(i).foreach { errs =>
      errors.headOption.foreach { (caret, msg) =>
        sb.append(shift(caret) + red("^") + "\n")
        sb.append(shift(caret) + red("|") + "\n")
        sb.append(shift(caret) + red(msg) + "\n")
      }
    }
  }

  IO.println(sb.result())
end printErrors
