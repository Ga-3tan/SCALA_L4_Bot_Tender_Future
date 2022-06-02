package Utils

import scala.annotation.tailrec
import scala.language.postfixOps

trait SpellCheckerService:
  /**
    * This dictionary is a Map object that contains valid words as keys and their normalized equivalents as values (e.g.
    * we want to normalize the words "veux" and "aimerais" in one unique term: "vouloir").
    */
  val dictionary: Map[String, String]

  /**
    * Calculate the Levenstein distance between two words.
    * @param s1 the first word
    * @param s2 the second word
    * @return an integer value, which indicates the Levenstein distance between "s1" and "s2"
    */
  def stringDistance(s1: String, s2: String): Int

  /**
    * Get the syntactically closest word in the dictionary from the given misspelled word, using the "stringDistance"
    * function. If the word is a number or a pseudonym, this function just returns it.
    * @param misspelledWord the misspelled word to correct
    * @return the closest normalized word from "misspelledWord"
    */
  def getClosestWordInDictionary(misspelledWord: String): String
end SpellCheckerService

class SpellCheckerImpl(val dictionary: Map[String, String]) extends SpellCheckerService:

  // Recursive version
  def stringDistance(s1: String, s2: String): Int =
    (s1, s2) match
      case (s1,s2) if (s1.length min s2.length) == 0 => s1.length max s2.length
      case (s1,s2) if s1.head == s2.head => stringDistance(s1.tail, s2.tail)
      case _ => 1 + (stringDistance(s1.tail, s2) min stringDistance(s1, s2.tail) min stringDistance(s1.tail, s2.tail))

  def getClosestWordInDictionary(misspelledWord: String): String =
    if misspelledWord.toDoubleOption.isDefined || misspelledWord.head == '_' then misspelledWord
    else
      val nearestWord = dictionary.foldLeft(("", Int.MaxValue))((acc, value) => {
        val dist = stringDistance(value._1, misspelledWord)
        if dist < acc._2 || (dist == acc._2 && value._1 < acc._1) then
          (value._1, dist)
        else
          acc
      })
      dictionary(nearestWord._1)

end SpellCheckerImpl
