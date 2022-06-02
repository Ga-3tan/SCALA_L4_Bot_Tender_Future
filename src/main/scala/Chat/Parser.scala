package Chat

import scala.annotation.tailrec

class UnexpectedTokenException(msg: String) extends Exception(msg) {}

// step 4
class Parser(tokenized: Tokenized):

  import ExprTree._
  import Chat.Token._

  // Start the process by reading the first token.
  var curTuple: (String, Token) = tokenized.nextToken()

  def curValue: String = curTuple._1
  def curToken: Token = curTuple._2

  /** Reads the next token and assigns it into the global variable curTuple */
  def readToken(): Unit =
    curTuple = tokenized.nextToken()

  /** "Eats" the expected token and returns it value, or terminates with an error. */
  private def eat(token: Token): String =
    if token == curToken then
      val tmp = curValue
      readToken()
      tmp
    else expected(token)

  /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type Token */
  private def expected(token: Token, more: Token*): Nothing =
    val expectedTokens = more.prepended(token).mkString(" or ")
    throw new UnexpectedTokenException(s"Expected: $expectedTokens, found: $curToken")

  /** the root method of the parser: parses an entry phrase */
  // Part 2 Step 4
  def parsePhrases() : ExprTree = {
    if curToken == BONJOUR then readToken()

    if curToken == JE then
      readToken()
      parseJE()
    else if curToken == QUEL then
      readToken()
      eat(ETRE)
      eat(LE)
      eat(PRIX)
      eat(DE)
      RequestPrice(handleProductRequest())
    else if curToken == COMBIEN then
      readToken()
      eat(COUTER)
      RequestPrice(handleProductRequest())
    else expected(JE, QUEL, COMBIEN)
  }

  def parseJE(): ExprTree = {
    if curToken == ETRE then
      readToken()
      parseETRE()
    else if curToken == VOULOIR then
      readToken()
      parseVOULOIR()
    else if curToken == ME then
      readToken()
      eat(APPELER)
      if curToken == PSEUDO then Identify(curValue)
      else expected(PSEUDO)
    else expected(ETRE, VOULOIR, ME)
  }

  def parseETRE(): ExprTree = {
    if curToken == ASSOIFFE then
      readToken()
      Thirsty()
    else if curToken == AFFAME then
      readToken()
      Hungry()
    else if curToken == PSEUDO then
      Identify(curValue)
    else expected(ASSOIFFE, AFFAME, PSEUDO)
  }

  def parseVOULOIR(): ExprTree = {
    if curToken == CONNAITRE then
      readToken()
      parseCONNAITRE()
    else if curToken == COMMANDER then
      readToken()
      RequestOrder(handleProductRequest())
    else if curToken == MON then
      readToken()
      eat(SOLDE)
      RequestBalance()
    else if curToken == NUM then // Pas de readToken après !! C'est intentionnel !
      RequestOrder(handleProductRequest())
    else expected(CONNAITRE, COMMANDER)
  }

  def parseCONNAITRE(): ExprTree = {
    if curToken == MON then
      readToken()
      eat(SOLDE)
      RequestBalance()
    else if curToken == QUEL then
      readToken()
      eat(ETRE)
      eat(LE)
      eat(PRIX)
      eat(DE)
      RequestPrice(handleProductRequest())
    else if curToken == LE then
      readToken()
      eat(PRIX)
      eat(DE)
      RequestPrice(handleProductRequest())
    else if curToken == COMBIEN then
      readToken()
      eat(COUTER)
      RequestPrice(handleProductRequest())
    else expected(QUEL, COMBIEN)
  }

  def parseProduct(): ExprTree = {
    val quantity = eat(NUM).toInt
    val product: String = if curToken == PRODUIT then eat(PRODUIT) else null
    val brand: String = if curToken == MARQUE then eat(MARQUE) else null

    if brand == null && product == null then expected(PRODUIT, MARQUE)

    Order(quantity, product, brand)
  }

  def handleProductRequest(): ExprTree = {
    @tailrec
    def loop(tLeft: ExprTree): ExprTree = {
      if curToken == ET  then
        readToken()
        loop(And(tLeft, parseProduct()))
      else if curToken == OU then
        readToken()
        loop(Or(tLeft, parseProduct()))
      else tLeft
    }

    loop(parseProduct())
  }
