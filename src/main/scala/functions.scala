object functions extends App {
  def randomWord(words : List[String]) : String = {
    words( scala.util.Random.nextInt(words.length) )
  }

  def wordSplit(word : String) : List[Char] = {
    word.toList
  }

  def wordJoin(wordlist : List[Char]) : String = {
    wordlist.mkString(" ")
  }
  def wordList(fname : String = "word_list.txt") : List[String] = {
    val source = io.Source.fromFile( fname )
    val words : List[String] = source.getLines.toList
    source.close()
    words
  }

  def alphaSet : Set[Char] = {
    ('A' to 'Z').toSet
  }

  def applyGuess(letter : Char, guesslist : List[Char], hanglist : List[Char]) : List[Char] = {
    guesslist.zip(hanglist).map({case(g,h) => if (letter == h) h else g})
  }
}
