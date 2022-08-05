//> using scala "3.1.2"
//> using lib "org.typelevel::cats-parse::0.3.8"
//> using plugin "org.polyvariant:::better-tostring:0.3.15"

import cats.parse.Parser as P
import cats.parse.Parser0 as P0
import cats.parse.Caret
import cats.parse.Rfc5234.{sp, alpha, digit, crlf, lf}

enum Expr[F[_]]:
  case Add(l: Expr[F], r: Expr[F])
  case Mul(l: Expr[F], r: Expr[F])
  case Name(value: F[String])
  case Lit(value: F[Int])

  def map[G[_]](f: [A] => F[A] => G[A]): Expr[G] =
    this match
      case Lit(num)    => Lit(f(num))
      case Name(num)   => Name(f(num))
      case Add(e1, e2) => Add(e1.map(f), e2.map(f))
      case Mul(e1, e2) => Mul(e1.map(f), e2.map(f))
end Expr

enum Statement[F[_]]:
  case Ass(name: Expr.Name[F], e: Expr[F])

  def map[G[_]](f: [A] => F[A] => G[A]): Statement[G] =
    this match
      case Ass(nm, e) =>
        // TODO: how to preserve this?
        Ass(nm.map(f).asInstanceOf[Expr.Name[G]], e.map(f))

case class Program[F[_]](statements: Vector[F[Statement[F]]], text: String)

case class Span(from: Caret, to: Caret):
  def contains(c: Caret) =
    val before =
      (from.line < c.line) || (from.line == c.line && from.col <= c.col)
    val after = (to.line > c.line) || (to.line == c.line && to.col >= c.col)

    before && after

case class WithSpan[A](span: Span, value: A)

object QuickmaffsParser:
  case class ParsingError(caret: Caret)

  def parse(s: String): Either[ParsingError, Program[WithSpan]] =
    parseWithPosition(statements.map(_.toVector).map(Program.apply(_, s)), s)

  def parseExpr(s: String): Either[ParsingError, Expr[WithSpan]] =
    parseWithPosition(expr, s)

  def parseStatement(
      s: String
  ): Either[ParsingError, WithSpan[Statement[WithSpan]]] =
    parseWithPosition(statement, s)

  private def withSpan[A](p: P[A]): P[WithSpan[A]] =
    (P.caret.with1 ~ p ~ P.caret).map { case ((start, a), end) =>
      WithSpan(Span(start, end), a)
    }

  private val name: P[Expr.Name[WithSpan]] =
    withSpan(
      (P.charWhere(_.isLetter) ~ P.charsWhile0(_.isLetter)).map(_.toString + _)
    ).map(Expr.Name.apply)

  val litNum =
    withSpan(P.charsWhile(_.isDigit).map(_.toInt)).map(Expr.Lit.apply)

  val LB        = P.char('(').surroundedBy(sp.rep0)
  val RB        = P.char(')').surroundedBy(sp.rep0)
  val PLUS      = P.char('+').surroundedBy(sp.rep0)
  val MUL       = P.char('*').surroundedBy(sp.rep0)
  val ASS       = P.char('=').surroundedBy(sp.rep0)
  val SEMICOLON = P.char(';')

  private val expr = P.recursive[Expr[WithSpan]] { recurse =>

    inline def pair[F[_]](p: P[Expr[F]], sym: P0[Unit]) =
      (p <* sym) ~ p

    val e = recurse.surroundedBy(sp.rep0)

    val add = pair(e, PLUS).between(LB, RB).map(Expr.Add.apply)

    val mul = pair(e, MUL).between(LB, RB).map(Expr.Mul.apply)

    P.oneOf(add.backtrack :: mul :: litNum :: name :: Nil)
  }

  private val assignment =
    ((name <* sp.? <* ASS) ~ expr)
      .map(Statement.Ass.apply)
      .surroundedBy(sp.rep0)

  private val sep = SEMICOLON orElse crlf orElse lf

  private val statement = withSpan(assignment)

  private val statements = statement
    .repSep0(sep.rep)
    .surroundedBy(sep.rep0)

  private def parseWithPosition[A](
      p: P0[A] | P[A],
      s: String
  ): Either[ParsingError, A] =
    p.parseAll(s).left.map { err =>
      val offset = err.failedAtOffset
      var line   = 0
      var col    = 0
      (0 to (offset min s.length)).foreach { i =>
        if (s(i) == '\n') then
          line += 1
          col = 0
        else col += 1

      }

      ParsingError(Caret(line, col, offset))
    }

end QuickmaffsParser
