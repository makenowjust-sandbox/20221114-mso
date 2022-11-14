package codes.quine.labs.mso

/** A monadic second-order logic formula over strings for conversion from MSO
  * formula to automaton.
  *
  * Syntax:
  *
  * ```
  * f, g ::= ⊤ | ⊥
  *         | X ⊆ A(a)
  *         | X ⊆ Y
  *         | succ(X, Y)
  *         | sing(X)
  *         | ¬ f
  *         | f ∨ g
  *         | ∃X. f
  * ```
  *
  * @tparam X
  *   second-order variable type
  * @tparam A
  *   character type
  */
sealed trait MsoNfa[X, A] extends Nfa[A]:
  def goto(q: State, l: Alphabet.Letter[A]): Set[State] =
    goto(q, MsoNfa.Input(l, Set.empty))

  /** Invokes a transition from the given state by the input. */
  def goto(q: State, input: MsoNfa.Input[X, A]): Set[State]

  /** Invokes all transitions from the given states by the input. */
  def gotoAll(qs: Set[State], input: MsoNfa.Input[X, A]): Set[State] =
    qs.flatMap(goto(_, input))

object MsoNfa:

  /** An input character for automaton corresponding to MSO formula. */
  final case class Input[X, A](value: Alphabet.Letter[A], set: Set[X]):
    def +(x: X): Input[X, A] = Input(value, set + x)
    def -(x: X): Input[X, A] = Input(value, set - x)

  /** A formula `⊤`. */
  final case class True[X, A]() extends MsoNfa[X, A]:
    type State = Unit
    def alphabet: Alphabet[A] = Alphabet.empty
    def initials: Set[State] = Set(())
    def goto(q: State, input: Input[X, A]): Set[State] = Set(())
    def accepts(q: State): Boolean = true

  /** A formula `⊥`. */
  final case class False[X, A]() extends MsoNfa[X, A]:
    type State = Nothing
    def alphabet: Alphabet[A] = Alphabet.empty
    def initials: Set[State] = Set.empty
    def goto(q: State, input: Input[X, A]): Set[State] = Set.empty
    def accepts(q: State): Boolean = false

  /** A formula `X ⊆ A(a)` that checks `X` is subset of a set `A(a)`.
    *
    * `A(a)` is a set contains all positions at which a character `a` appears.
    */
  final case class SubsetA[X, A](x: X, a: A) extends MsoNfa[X, A]:
    type State = Unit
    def alphabet: Alphabet[A] = Alphabet(a)
    def initials: Set[State] = Set(())
    def goto(q: State, input: Input[X, A]): Set[State] =
      (input.set.contains(x), input.value == Alphabet.Appeared(a)) match
        case (true, true) | (false, _) => Set(())
        case _                         => Set.empty
    def accepts(q: State): Boolean = true

  /** A formula `X ⊆ Y` that checks `X` is subset of a set `Y`. */
  final case class Subset[X, A](x: X, y: X) extends MsoNfa[X, A]:
    type State = Unit
    def alphabet: Alphabet[A] = Alphabet.empty
    def initials: Set[State] = Set(())
    def goto(q: State, input: Input[X, A]): Set[State] =
      (input.set.contains(x), input.set.contains(y)) match
        case (true, true) | (false, _) => Set(())
        case _                         => Set.empty
    def accepts(q: State): Boolean = true

  /** A formula `succ(X, Y)` that checks `X` and `Y` are singleton and `Y` is
    * successor to `X`.
    *
    * Let `X = {x}, Y = {y}`, `succ(X, Y)` means `x + 1 = y`.
    */
  final case class Succ[X, A](x: X, y: X) extends MsoNfa[X, A]:
    type State = Int
    def alphabet: Alphabet[A] = Alphabet.empty
    def initials: Set[State] = Set(0)
    def goto(q: State, input: Input[X, A]): Set[State] =
      (q, (input.set.contains(x), input.set.contains(y))) match
        case (0, (false, false)) => Set(0)
        case (0, (true, false))  => Set(1)
        case (1, (false, true))  => Set(2)
        case (2, (false, false)) => Set(2)
        case _                   => Set.empty
    def accepts(q: State): Boolean = q == 2

  /** A formula `sing(X)` that checks `X` is singleton. */
  final case class Sing[X, A](x: X) extends MsoNfa[X, A]:
    type State = Int
    def alphabet: Alphabet[A] = Alphabet.empty
    def initials: Set[State] = Set(0)
    def goto(q: State, input: Input[X, A]): Set[State] =
      (q, input.set.contains(x)) match
        case (0, false) => Set(0)
        case (0, true)  => Set(1)
        case (1, false) => Set(1)
        case _          => Set.empty
    def accepts(q: State): Boolean = q == 1

  /** A formula `¬ f`. */
  final case class Not[X, A](f: MsoNfa[X, A]) extends MsoNfa[X, A]:
    type State = Set[f.State]
    def alphabet: Alphabet[A] = f.alphabet
    def initials: Set[State] = Set(f.initials)
    def goto(q: State, input: Input[X, A]): Set[State] =
      Set(f.gotoAll(q, input))
    def accepts(q: State): Boolean = !q.exists(f.accepts)

  /** A formula `f ∨ g`. */
  final case class Or[X, A](f: MsoNfa[X, A], g: MsoNfa[X, A])
      extends MsoNfa[X, A]:
    type State = Either[f.State, g.State]
    def alphabet: Alphabet[A] = f.alphabet merge g.alphabet
    def initials: Set[State] =
      f.initials.map(Left(_)) ++ g.initials.map(Right(_))
    def goto(q: State, input: Input[X, A]): Set[State] =
      q.fold(f.goto(_, input).map(Left(_)), g.goto(_, input).map(Right(_)))
    def accepts(q: State): Boolean = q.fold(f.accepts, g.accepts)

  /** A formula `∃X. f` that is second-order existential quantifier. */
  final case class Exists2[X, A](x: X, f: MsoNfa[X, A]) extends MsoNfa[X, A]:
    type State = f.State
    def alphabet: Alphabet[A] = f.alphabet
    def initials: Set[State] = f.initials
    def goto(q: State, input: Input[X, A]): Set[State] =
      f.goto(q, input + x) ++ f.goto(q, input - x)
    def accepts(q: State): Boolean = f.accepts(q)

  /** A formula `f ∧ g`. */
  def And[X, A](f: MsoNfa[X, A], g: MsoNfa[X, A]): MsoNfa[X, A] =
    Not(Or(Not(f), Not(g)))

  /** A formula `f ⇒ g`. */
  def Imp[X, A](f: MsoNfa[X, A], g: MsoNfa[X, A]): MsoNfa[X, A] =
    Or(Not(f), g)

  /** A formula `∀x. f` that is first-order universal quantifier. */
  def ForAll1[X, A](x: X, f: MsoNfa[X, A]): MsoNfa[X, A] =
    Not(Exists1(x, Not(f)))

  /** A formula `∀x. f` that is second-order universal quantifier. */
  def ForAll2[X, A](x: X, f: MsoNfa[X, A]): MsoNfa[X, A] =
    Not(Exists2(x, Not(f)))

  /** A formula `∃x. f` that is first-order existential quantifier. */
  def Exists1[X, A](x: X, f: MsoNfa[X, A]): MsoNfa[X, A] =
    Exists2(x, And(Sing(x), f))
