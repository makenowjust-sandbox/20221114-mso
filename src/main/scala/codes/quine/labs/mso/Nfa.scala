package codes.quine.labs.mso

import scala.collection.mutable

/** A non-deterministic finite state automaton.
  *
  * @tparam A
  *   character type
  */
trait Nfa[A]:

  /** State type. */
  type State

  /** An alphabet for this formula. */
  def alphabet: Alphabet[A]

  /** Initial states. */
  def initials: Set[State]

  /** Invokes a transition from the given state by the given letter. */
  def goto(q: State, l: Alphabet.Letter[A]): Set[State]

  /** Checks the given state is acceptance. */
  def accepts(q: State): Boolean

  /** Invokes all transitions from the given states by the given letter. */
  def gotoAll(qs: Set[State], l: Alphabet.Letter[A]): Set[State] =
    qs.flatMap(goto(_, l))

  /** Runs this formula on the given string. */
  def run[X](s: Seq[A]): Boolean =
    s.map(alphabet.encode).foldLeft(initials)(gotoAll).exists(accepts)

  /** Converts this automaton to a concrete data. */
  def toConcreteNfa: ConcreteNfa[State, A] =
    val map = Map.newBuilder[(State, Alphabet.Letter[A]), Set[State]]
    val set = Set.newBuilder[State]
    val queue = mutable.Queue.from(initials)
    val queued = mutable.Set.empty[State]
    val alphabet = this.alphabet
    val ls = alphabet.enumerate

    while queue.nonEmpty do
      val q1 = queue.dequeue()
      if accepts(q1) then set.addOne(q1)
      for l <- ls do
        val qs = goto(q1, l)
        val notQueued = qs.filterNot(queued)
        queue.enqueueAll(notQueued)
        queued.addAll(notQueued)
        if qs.nonEmpty then map.addOne((q1, l) -> qs)

    ConcreteNfa(alphabet, initials, set.result(), map.result())
