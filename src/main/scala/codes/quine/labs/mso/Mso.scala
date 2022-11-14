package codes.quine.labs.mso

/** A monadic second-order logic formula over strings.
 * 
 * exists x. exists y. succ(x, y) /\ x in A(a) /\ y in A(a)
  *
  * Syntax:
  * ```
  * f, g ::= ⊤ | ⊥
  *         | x ∈ A(a)
  *         | x ∈ Y
  *         | succ(x, y)
  *         | ¬ f
  *         | f ∧ g
  *         | f ∨ g
  *         | f ⇒ g
  *         | ∀x. f
  *         | ∀X. f
  *         | ∃x. f
  *         | ∃X. f
  * ```
  *
  * @tparam X1
  *   first-order variable type
  * @tparam X2
  *   second-order variable type
  * @tparam A
  *   character type
  */
enum Mso[X1, X2, A]:
  case True()
  case False()
  case InA(x: X1, a: A)
  case In(x: X1, y: X2)
  case Succ(x: X1, y: X1)
  case Not(f: Mso[X1, X2, A])
  case And(f: Mso[X1, X2, A], g: Mso[X1, X2, A])
  case Or(f: Mso[X1, X2, A], g: Mso[X1, X2, A])
  case Imp(f: Mso[X1, X2, A], g: Mso[X1, X2, A])
  case ForAll1(x: X1, f: Mso[X1, X2, A])
  case ForAll2(x: X2, f: Mso[X1, X2, A])
  case Exists1(x: X1, f: Mso[X1, X2, A])
  case Exists2(x: X2, f: Mso[X1, X2, A])

  /** Converts this formula into the automaton. */
  def toMsoNfa: MsoNfa[Either[X1, X2], A] = this match
    case True()        => MsoNfa.True()
    case False()       => MsoNfa.False()
    case InA(x, a)     => MsoNfa.SubsetA(Left(x), a)
    case In(x, y)      => MsoNfa.Subset(Left(x), Right(y))
    case Succ(x, y)    => MsoNfa.Succ(Left(x), Left(y))
    case Not(f)        => MsoNfa.Not(f.toMsoNfa)
    case And(f, g)     => MsoNfa.And(f.toMsoNfa, g.toMsoNfa)
    case Or(f, g)      => MsoNfa.Or(f.toMsoNfa, g.toMsoNfa)
    case Imp(f, g)     => MsoNfa.Imp(f.toMsoNfa, g.toMsoNfa)
    case ForAll1(x, f) => MsoNfa.ForAll1(Left(x), f.toMsoNfa)
    case ForAll2(x, f) => MsoNfa.ForAll2(Right(x), f.toMsoNfa)
    case Exists1(x, f) => MsoNfa.Exists1(Left(x), f.toMsoNfa)
    case Exists2(x, f) => MsoNfa.Exists2(Right(x), f.toMsoNfa)
