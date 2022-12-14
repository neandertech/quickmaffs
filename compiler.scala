object QuickmaffsCompiler:
  type Id[A] = A
  case class CompileError(position: Span, message: String)

  case class VariableDefinition(
      definedAt: Span,
      fullDefinition: Span,
      references: Vector[Span]
  )

  case class Index(
      variables: Map[String, VariableDefinition]
  )
  object Index:
    val empty = Index(Map.empty)

  type Result = Either[Vector[CompileError], Index]

  def compile(p: Program[WithSpan]): Result =
    val errors = Vector.newBuilder[CompileError]

    def indexReferences(e: Expr[WithSpan]): Map[String, Vector[Span]] =
      inline def pair(e1: Expr[WithSpan], e2: Expr[WithSpan]) =
        val m1      = go(e1)
        val m2      = go(e2)
        val allKeys = m1.keySet ++ m2.keySet
        allKeys.map { k =>
          k -> (m1.getOrElse(k, Vector.empty) ++ m2.getOrElse(k, Vector.empty))
        }.toMap

      def go(expr: Expr[WithSpan]): Map[String, Vector[Span]] =
        import Expr.*
        expr match
          case _: Lit[?]                 => Map.empty
          case Name(WithSpan(pos, name)) => Map(name -> Vector(pos))
          case Add(e1, e2, _)            => pair(e1, e2)
          case Mul(e1, e2, _)            => pair(e1, e2)

      go(e)
    end indexReferences

    val idx = p.statements.foldLeft(Index.empty) {
      case (idx, WithSpan(span, Statement.Ass(name, e))) =>
        val varName = name.value.value
        val indexed = indexReferences(e)

        val recursiveRefs = indexed.getOrElse(varName, Vector.empty)

        if recursiveRefs.nonEmpty then
          errors += CompileError(
            recursiveRefs.head,
            s"Recrusive reference to variable ${varName} is not allowed"
          )
          idx
        else if indexed.keySet.exists(ref => !idx.variables.contains(ref)) then
          val undefinedVars =
            indexed.filter((nm, _) => !idx.variables.contains(nm))

          undefinedVars.foreach { case (undefined, refs) =>
            refs.foreach { ref =>
              errors += CompileError(
                ref,
                s"Reference to undefined variable ${undefined}"
              )
            }
          }

          idx
        else if !idx.variables.contains(varName) then
          idx.copy(variables =
            idx.variables
              .updated(
                varName,
                VariableDefinition(
                  name.value.span,
                  fullDefinition = span,
                  Vector.empty
                )
              )
              .map { case (name, vdf) =>
                name -> vdf.copy(references =
                  vdf.references ++ indexed.getOrElse(name, Vector.empty)
                )
              }
          )
        else
          errors += CompileError(
            span,
            s"Variable ${varName} has already been defined"
          )
          idx.copy(variables = idx.variables.filterNot(_._1 == varName))
        end if
    }

    if errors.result().nonEmpty then Left(errors.result())
    else Right(idx)
  end compile
end QuickmaffsCompiler
