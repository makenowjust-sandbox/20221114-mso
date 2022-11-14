package codes.quine.labs.mso

// The number of ASCII characters is 128, and Unicode contains over 10000 characters.
// However, the number of characters appeared in formula or automaton is actually small.
// Therefore, by distinguishing between appeared characters and non-appeared,
// we can save a time- and memory-space for characters.

/** A character set wrapper a.k.a. alphabet.
  *
  * @tparam A
  *   character type
  */
final case class Alphabet[A](set: Set[A]):

  /** Encodes a character to compact representation. */
  def encode(a: A): Alphabet.Letter[A] =
    if set.contains(a) then Alphabet.Appeared(a) else Alphabet.NonAppeared()

  /** Enumerates all possible compact representations. */
  def enumerate: Set[Alphabet.Letter[A]] =
    set.map(Alphabet.Appeared(_)) ++ Set(Alphabet.NonAppeared())

  /** Merges two alphabet into one. */
  def merge(that: Alphabet[A]): Alphabet[A] = Alphabet(set ++ that.set)

object Alphabet:

  /** Creates a new alphabet from the given values. */
  def apply[A](as: A*): Alphabet[A] = Alphabet(as.toSet)

  /** Returns an empty alphabet. */
  def empty[A]: Alphabet[A] = Alphabet(Set.empty)

  /** A character wrapper for appeared and non-appeared characters in formula or
    * automaton.
    */
  enum Letter[A]:
    case Appeared(value: A)
    case NonAppeared()

  export Letter._
