object game extends App {
  println("Lets play Hangman game.")

  val figures =Map("""
     ------
     |    |
     |
     |
     |
     |
     |
     |
    ----------
"""->1, """
     ------
     |    |
     |    O
     |
     |
     |
     |
     |
    ----------
""" ->2) // no

  val fname = if (args.isEmpty) "src/resources/word_list.txt" else args(0)
  var words = functions.wordList(fname)
  val alphaset : Set[Char] = functions.alphaSet
  val fmtsummary = "%s  Wins : %2d  Losses : %2d"
  val fmtinput   = "\t%s  (Guesses left: %2d ) Enter letter : "
  // The game loop:
  var fexit = false
  var wins   = 0
  var losses = 0
  while (!fexit) {
    println("If you want to quit the game write: 'Exit' to leave the game, or write 'New' for a new game.")
    println("Good luck to you!\n")

    val hangword = functions.randomWord(words)
    words = words.filterNot(_ == hangword)
    val hanglist = functions.wordSplit(hangword.toUpperCase)
    var guesslist = functions.wordSplit("_" * hangword.length)
    var guesset : Set[Char] = Set()

    var guesses = hangword.length-1

    var fnewgame = false
    while (!fnewgame && !fexit) {

      def checkGuess() : Unit = {

        def printSummary(message : String) : Unit = {
          println("\t" + functions.wordJoin(hanglist) + "\n")
          println(fmtsummary.format(message, wins, losses))
        }

        if (hanglist == guesslist) {
          fnewgame = true
          wins += 1
          printSummary("Congratulations you win!")

        } else {
          guesses match {
            case 1 =>
              fnewgame = true
              losses += 1
              printSummary("You lose! Try again.")
            case _ => guesses -= 1
          }
        }
      }

      val line = scala.io.StdIn.readLine(fmtinput.format(functions.wordJoin(guesslist), guesses)).toUpperCase
      line match {
        case "NEW"  => guesses = 1 ; checkGuess()
        case "EXIT" => fexit = true
        case "" => Nil
        case _ => {
          val letter : List[Char] = line.toList
          if ((1 < letter.length) || !alphaset.contains(letter.head)) {
            println("Not a valid guess -> " + line)

          } else if (guesset.contains(letter.head)) {

          } else {
            if (hanglist.contains(letter.head)) {
              guesslist = functions.applyGuess(letter.head, guesslist, hanglist)
              guesses += 1
            }

            guesset = guesset + letter.head
            checkGuess()
          }
        }
      }
    }
  }
  println("Thank you for playing!!")
}
