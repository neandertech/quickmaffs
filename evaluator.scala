object QuickmaffsEvaluator:
  enum EvaluationError(val message: String):
    case NameNotFound(name: String)
        extends EvaluationError(s"Variable named $name not found")

  import scala.annotation.targetName
  @targetName("evalue_WithSpan")
  def evaluate(
      pg: Program[WithSpan]
  ): Either[EvaluationError, Map[String, Int]] =
    evaluate(unwrap(pg))

  type Result = Either[EvaluationError, Map[String, Int]]

  private def evaluate(
      pg: Program[Id]
  ): Result =
    pg.statements.foldLeft[Result](Right(Map.empty[String, Int])) {
      case (acc, Statement.Ass(name, expr)) =>
        acc.flatMap { st =>
          evaluate(expr, st).map { ev =>
            st.updated(name.value, ev)
          }
        }
    }
  type Id[A] = A

  import cats.syntax.all.*

  def evaluate(
      expr: Expr[Id],
      state: Map[String, Int]
  ): Either[EvaluationError, Int] =
    expr match
      case Expr.Lit(num) => Right(num)

      case Expr.Name(v) =>
        state.get(v).toRight(EvaluationError.NameNotFound(v))

      case Expr.Add(e1, e2, _) =>
        (evaluate(e1, state), evaluate(e2, state)).mapN(_ + _)

      case Expr.Mul(e1, e2, _) =>
        (evaluate(e1, state), evaluate(e2, state)).mapN(_ * _)

  @targetName("evaluate_WithSpan")
  def evaluate(
      expr: Expr[WithSpan],
      state: Map[String, Int]
  ): Either[EvaluationError, Int] =
    evaluate(expr.map([A] => (ws: WithSpan[A]) => ws.value: Id[A]), state)

  private def unwrap(pg: Program[WithSpan]): Program[Id] =
    pg.copy(statements =
      pg.statements.map(
        _.value.map([A] => (ws: WithSpan[A]) => ws.value: Id[A])
      )
    )
end QuickmaffsEvaluator
