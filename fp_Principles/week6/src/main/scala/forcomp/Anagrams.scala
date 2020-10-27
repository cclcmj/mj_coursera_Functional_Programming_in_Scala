package forcomp

import scala.annotation.tailrec

object Anagrams extends AnagramsInterface {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
    *  how often the character appears.
    *  This list is sorted alphabetically w.r.t. to the character in each pair.
    *  All characters in the occurrence list are lowercase.
    *
    *  Any list of pairs of lowercase characters and their frequency which is not sorted
    *  is **not** an occurrence list.
    *
    *  Note: If the frequency of some character is zero, then that character should not be
    *  in the list.
    */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
    *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
    */
  val dictionary: List[Word] = Dictionary.loadDictionary

  /** Converts the word into its character occurrence list.
    *
    *  Note: the uppercase and lowercase version of the character are treated as the
    *  same character, and are represented as a lowercase character in the occurrence list.
    *
    *  Note: you must use `groupBy` to implement this method!
    */
  def wordOccurrences(w: Word): Occurrences =
    w.toLowerCase.toList
      .groupBy(identity)
      .map { case (a, b) => (a, b.size) }
      .toList
      .sortBy(_._1)

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences =
    s.flatMap(w => wordOccurrences(w.toLowerCase()))
      .groupBy(_._1)
      .map { case (a, b) => (a, b.foldLeft(0)((a, b) => a + b._2)) }
      .toList
      .sortBy(_._1)

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
    *  the words that have that occurrence count.
    *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
    *
    *  For example, the word "eat" has the following character occurrence list:
    *
    *     `List(('a', 1), ('e', 1), ('t', 1))`
    *
    *  Incidentally, so do the words "ate" and "tea".
    *
    *  This means that the `dictionaryByOccurrences` map will contain an entry:
    *
    *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
    */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary.groupBy(w => wordOccurrences(w))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] =
    dictionaryByOccurrences(wordOccurrences(word))

  /** Returns the list of all subsets of the occurrence list.
    *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
    *  is a subset of `List(('k', 1), ('o', 1))`.
    *  It also include the empty subset `List()`.
    *
    *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
    *
    *    List(
    *      List(),
    *      List(('a', 1)),
    *      List(('a', 2)),
    *      List(('b', 1)),
    *      List(('a', 1), ('b', 1)),
    *      List(('a', 2), ('b', 1)),
    *      List(('b', 2)),
    *      List(('a', 1), ('b', 2)),
    *      List(('a', 2), ('b', 2))
    *    )
    *
    *  Note that the order of the occurrence list subsets does not matter -- the subsets
    *  in the example above could have been displayed in some other order.
    */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    val maxNumSubl: IndexedSeq[Occurrences] = for {
      i <- 0 to occurrences.length
      sub <- occurrences.combinations(i)
    } yield sub
    maxNumSubl.toList
      .flatMap { occ =>
        occ
          .map { case (c, i) => (1 to i).map((c, _)) }
          .foldLeft(List[List[(Char, Int)]](Nil))((acc, b) =>
            acc.flatMap(al => b.map(bl => al.::(bl)))
          )
      }
      .map(l => l.sortBy(_._1))
  }
  //非尾递归栈溢出
  def combinations2(occurrences: Occurrences): List[Occurrences] =
    occurrences match {
      case Nil => List(Nil)
      case (c, i) :: os =>
        (for { n <- 0 to i; oss <- combinations2(os) } yield ((
          c,
          n
        ) :: oss)).toList.map(_.filterNot(_._2 == 0))
    }

  /** Subtracts occurrence list `y` from occurrence list `x`.
    *
    *  The precondition is that the occurrence list `y` is a subset of
    *  the occurrence list `x` -- any character appearing in `y` must
    *  appear in `x`, and its frequency in `y` must be smaller or equal
    *  than its frequency in `x`.
    *
    *  Note: the resulting value is an occurrence - meaning it is sorted
    *  and has no zero-entries.
    */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    require(
      y.forall {
        case (c, i) => x.toMap.keySet.contains(c) && x.toMap.apply(c) >= i
      },
      "subtract y非x子集"
    )
    x.map {
      case (c, i) =>
        (c, i - y.toMap.getOrElse(c, 0))
    }.filterNot(_._2 == 0)
  }

  /** Returns a list of all anagram sentences of the given sentence.
    *
    *  An anagram of a sentence is formed by taking the occurrences of all the characters of
    *  all the words in the sentence, and producing all possible combinations of words with those characters,
    *  such that the words have to be from the dictionary.
    *
    *  The number of words in the sentence and its anagrams does not have to correspond.
    *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
    *
    *  Also, two sentences with the same words but in a different order are considered two different anagrams.
    *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
    *  `List("I", "love", "you")`.
    *
    *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
    *
    *    List(
    *      List(en, as, my),
    *      List(en, my, as),
    *      List(man, yes),
    *      List(men, say),
    *      List(as, en, my),
    *      List(as, my, en),
    *      List(sane, my),
    *      List(Sean, my),
    *      List(my, en, as),
    *      List(my, as, en),
    *      List(my, sane),
    *      List(my, Sean),
    *      List(say, men),
    *      List(yes, man)
    *    )
    *
    *  The different sentences do not have to be output in the order shown above - any order is fine as long as
    *  all the anagrams are there. Every returned word has to exist in the dictionary.
    *
    *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
    *  so it has to be returned in this list.
    *
    *  Note: There is only one anagram of an empty sentence.
    */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    val so = sentenceOccurrences(sentence)
    lazy val dicOcc = dictionaryByOccurrences
    @tailrec
    def loopAcc(acc: List[List[Word]]): List[List[Word]] = {
      val sheng = acc
        .map(lw => (lw, subtract(so, sentenceOccurrences(lw))))
        .map(elem => (elem._1, elem._2, combinations(elem._2)))
      if (
        sheng.forall {
          case (lw, sh, com) =>
            sh.size == 0 || dicOcc.keySet.intersect(com.toSet).size == 0
        }
      )
        sheng.filter(_._2.size == 0).map(_._1)
      else {
        val nacc = sheng.flatMap {
          case (lw, occ, com) if occ.size != 0 =>
            val int: Set[(Occurrences, List[Word])] = dicOcc.keySet
              .intersect(com.toSet)
              .map(wocc => (wocc, dicOcc(wocc)))
            int.toList.flatMap {
              case (_, lnw) => lnw.map(w => (lw :+ w))
            }
          case (lw, occ, com) if occ.size == 0 => lw :: Nil
        }
        loopAcc(nacc)
      }
    }
    loopAcc(List(Nil))
  }
  //非尾递归，栈溢出
  def sentenceAnagrams2(sentence: Sentence): List[Sentence] = {
    def aux(occ:Occurrences):List[Sentence] = {
      for {
        comb <- combinations(occ)
        word     <- dictionaryByOccurrences(comb)
         xs     <-  aux(subtract(occ,comb))
      }yield (word :: xs)
    }
    aux(sentenceOccurrences(sentence))
  }
}

object Dictionary {
  def loadDictionary: List[String] = {
    val wordstream = Option {
      getClass.getResourceAsStream(
        List("forcomp", "linuxwords.txt").mkString("/", "/", "")
      )
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      val s = scala.io.Source.fromInputStream(wordstream)
      s.getLines.toList
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      wordstream.close()
    }
  }
}
