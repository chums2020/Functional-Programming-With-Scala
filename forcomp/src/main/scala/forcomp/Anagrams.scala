package forcomp

import com.sun.xml.internal.bind.v2.TODO


object Anagrams {

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
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = {
   // w.map(letter => (letter, 1)).groupBy(_.1)
    {for (str <- w.toLowerCase) yield (str, 1)}.groupBy(_._1).mapValues(x => x.map(_._2).sum).toList.sortBy(_._1)
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    if (s == Nil) List()
    else wordOccurrences(s.reduce(_++_))
   /* def concatWord(sentence: Sentence): Word = {
      sentence match {
        case Nil => ""
        case w :: ws => w ++ concatWord(ws)
      }
    }
      wordOccurrences(concatWord(s))
  */
  }




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
   *
   */

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    // wordOccurrences
    // myMap.groupBy{ _._1 / (60)).map {case (sec, map) => sec -> map.values.sum}

    {for {w <- dictionary} yield (wordOccurrences(w), w)}.groupBy(_._1).map(elem => (elem._1 , elem._2.map(_._2)))

  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    dictionaryByOccurrences(wordOccurrences(word))

  }

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
  // given 2 occurrence, decide if the first is a subset of the second
  def isSubset(occ1: Occurrences, occ2: Occurrences) : Boolean  = {
    if (occ1 == List()) true
    else if (occ1 != List() && occ2 == List()) false
    // if all characters in occ1 is contained in occ2
    else if (occ1.map(_._1).forall( char => occ2.map(_._1).contains(char)))
      {
        for {
          (char1, int1) <- occ1
          (char2, int2) <- occ2
          if char1 == char2 && int1 <= int2
        } yield 1
      }.sum == occ1.size
    else false
  }

  def combinations(occurrences: Occurrences): List[Occurrences] = {
   for {(o, _) <- dictionaryByOccurrences if isSubset(o, occurrences)
   } yield o
//    dictionaryByOccurrences.filter{case (dict, _) => isSubset(dict, occurrences)}
  }.toList

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
    // precondition
    require(isSubset(y, x), "the occurrence list y must be a subset of the occurrence list x")
   // x.filter{
   //   case(x1, x2) => y.exists(case(y1, y2) => x1 == y1 && x2 == y2)
  //  }
  /* for {
      (x1, x2) <- x
      //(y1, y2) <- y
      for ((y1 , y2) <- y if x1 == y1 ) yield y2
      //if y.toList.filter(case(y1, y2) => x1 == y1 && x2 == y2)
    } yield (x1, x2)
    */
    x.map{ case(x1, x2) =>
        if (y.map(_._1).contains(x1)) (x1, x2 - y.toMap.get(x1).head) //  && x2 > y.toMap.get(x1).head
        //else if (y.map(_._1).contains(x1) && x2 == y.toMap.get(x1).head) Nil
        else (x1, x2) // if (!y.map(_._1).contains(x1))
    }.filter(_._2 >0)
  }

    /*
        for {
          (x1, x2) <- x
          if (y.map(_._1).contains(x1)
          if x1 == y1 && x2 > y2
        } yield (x1, x2-y2)
    */


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
 // combinations(occurrences: Occurrences): List[Occurrences]
 // subtract(x: Occurrences, y: Occurrences): Occurrences , Subtracts occurrence list `y` from occurrence list `x`.
 //   sentenceOccurrences(sentence) returns Occurrence of the sentence
 //   dictionaryByOccurrences(sentenceOccurrences(sentence)) returns a list of words with the
    /*
      if (sentence == Nil) List(List())
      else {
        val allOccurrence = sentenceOccurrences(sentence)
        for {
          occ <- combinations(allOccurrence)
          word <- dictionaryByOccurrences(occ)
          others <- sentenceAnagrams(subtract(allOccurrence, occ))
        } yield word :: others
         // yield  (dictionaryByOccurrences(occ), subtract(allOccurrence, occ))
      }
      //type Occurrences = List[(Char, Int)]
     */
          def sentenceAnagrams(occurrences: Occurrences): List[Sentence] =
          if (occurrences.isEmpty) List(List())
          else for {
            combinations <- combinations(occurrences)
            word <- dictionaryByOccurrences(combinations)
            others <- sentenceAnagrams(subtract(occurrences, combinations))
          } yield word :: others
           sentenceAnagrams(sentenceOccurrences(sentence))

  }
}
