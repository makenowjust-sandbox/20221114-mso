package codes.quine.labs.mso

import scala.collection.mutable

/** A non-deterministic finite state automaton implementation with concrete
  * data.
  *
  * @tparam A
  *   character type
  */
final case class ConcreteNfa[Q, A](
    alphabet: Alphabet[A],
    initials: Set[Q],
    accepts: Set[Q],
    goto: Map[(Q, Alphabet.Letter[A]), Set[Q]]
) extends Nfa[A]:
  type State = Q
  def goto(q: State, l: Alphabet.Letter[A]): Set[State] =
    goto.getOrElse((q, l), Set.empty)
  def accepts(q: State): Boolean = accepts.contains(q)

  /** Enumerates states in this automaton. */
  def states: Set[State] =
    initials ++ accepts ++ goto.flatMap { case ((q, _), qs) => Set(q) ++ qs }

  /** Returns a automaton renamed to simplified integers. */
  def renamed: ConcreteNfa[Int, A] =
    val rename = states.iterator.zipWithIndex.toMap
    ConcreteNfa(
      alphabet,
      initials.map(rename),
      accepts.map(rename),
      goto.map { case ((q, l), qs) => (rename(q), l) -> qs.map(rename) }
    )

  /** Returns a DOT format representation of this automaton. */
  def toDot: String =
    val dot = new mutable.StringBuilder

    dot.append("digraph {\n")

    dot.append("  initial [shape=none, label=\"\"];\n")
    for q <- initials do dot.append(s"  initial -> \"$q\";\n")

    dot.append("\n")
    dot.append("  accept [shape=none, label=\"\"];\n")
    for q <- accepts do dot.append(s"  \"$q\" -> accept;\n")

    for ((q1, l), qs) <- goto do
      dot.append("\n")
      for q2 <- qs do
        val c = l match
          case Alphabet.Appeared(value) => s"'$value'"
          case Alphabet.NonAppeared()   => "￮"
        dot.append(s"  \"$q1\" -> \"$q2\" [label=\"$c\"];\n")

    dot.append("}\n")
    dot.result()
