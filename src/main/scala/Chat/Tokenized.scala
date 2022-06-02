package Chat

import Chat.Token.*
import Utils.SpellCheckerService

trait Tokenized:
  /**
    * Get the next token of the user input, or EOL if there is no more token.
    * @return a tuple that contains the string value of the current token, and the identifier of the token
    */
  def nextToken(): (String, Token)

class TokenizedImpl(val tokens: Array[(String, Token)]) extends Tokenized:
  var index = 0
  def nextToken(): (String, Token) =
    val output = if index < tokens.length then tokens(index) else ("EOL", Token.EOL)
    index += 1
    output

end TokenizedImpl
