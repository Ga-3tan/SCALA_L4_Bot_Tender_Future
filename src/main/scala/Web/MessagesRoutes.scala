package Web

import Chat.{AnalyzerService, Parser, TokenizerService, UnexpectedTokenException}
import Data.{AccountService, MessageService, Session, SessionService}

import scala.collection.mutable
import castor.Context.Simple.global

/**
  * Assembles the routes dealing with the message board:
  * - One route to display the home page
  * - One route to send the new messages as JSON
  * - One route to subscribe with websocket to new messages
  *
  * @param log
  */
class MessagesRoutes(tokenizerSvc: TokenizerService,
                     analyzerSvc: AnalyzerService,
                     msgSvc: MessageService,
                     accountSvc: AccountService,
                     sessionSvc: SessionService)(implicit val log: cask.Logger) extends cask.Routes :

    import Decorators.getSession

    /**
      * List of connected websockets
      */
    val websockets: mutable.ListBuffer[cask.WsChannelActor] = mutable.ListBuffer()

    /**
      * Display HomePage when a GET is made to `/`
      *
      * @param session session param
      * @return ScalaTag of the home page
      */
    @getSession(sessionSvc) // This decorator fills the `(session: Session)` part of the `index` method.
    @cask.get("/")
    def index()(session: Session): ScalaTag =
        val messages = msgSvc.getLatestMessages(20)
        Layouts.homePage(session.getCurrentUser.isDefined, messages)

    /**
      * Process the new messages sent as JSON object to `/send`. The JSON looks
      * like this: `{ "msg" : "The content of the message" }`.
      *
      * A JSON object is returned. If an error occurred, it looks like this:
      * `{ "success" : false, "err" : "An error message that will be displayed" }`.
      * Otherwise (no error), it looks like this:
      * `{ "success" : true, "err" : "" }`
      *
      * The following are treated as error:
      * - No user is logged in
      * - The message is empty
      * The exceptions raised by the `Parser` will be treated as an error too
      * If no error occurred, every other user is notified with the last 20 messages
      *
      * @param msg     json object from post
      * @param session session param
      * @return json response
      */
    @getSession(sessionSvc)
    @cask.postJson("/send")
    def processMsg(msg: ujson.Value)(session: Session): ujson.Obj =
        def jsonResponse(success: Boolean, err: String = ""): ujson.Obj =
            ujson.Obj("success" -> success, "err" -> err)

        if session.getCurrentUser.isEmpty then
            jsonResponse(false, "No user is logged in")
        else if msg.str == "" then
            jsonResponse(false, "The message is empty")
        else if msg.str.startsWith("@bot ") then
            val message = msg.str.stripPrefix("@bot ")

            try
                val tokenized = tokenizerSvc.tokenize(message.toLowerCase)
                val parser = new Parser(tokenized)
                val expr = parser.parsePhrases()

                val botReply = analyzerSvc.reply(session)(expr)

                // Process message of user
                val id = msgSvc.add(
                    sender = session.getCurrentUser.get,
                    msg = Layouts.getMessageSpan(message),
                    mention = Option("bot")
                )
                send20LastMessageToAll()

                // Process response from chatbot
                msgSvc.add(
                    sender = "Bot-tender",
                    msg = Layouts.getMessageSpan(botReply),
                    replyToId = Option(id)
                )
                send20LastMessageToAll()

                jsonResponse(true)
            catch
                case e: UnexpectedTokenException => jsonResponse(false, s"Invalid input. ${e.getMessage}")

        else
            msgSvc.add(
                sender = session.getCurrentUser.get,
                msg = Layouts.getMessageSpan(msg.str)
            )
            send20LastMessageToAll()

            jsonResponse(true)

    /**
      * Send the last 20 messages to all connected websockets
      */
    def send20LastMessageToAll(): Unit =
        websockets.foreach(
            ws => ws.send(cask.Ws.Text(
                Layouts.getBoardMessageContent(msgSvc.getLatestMessages(20)).foldLeft("")(_ + _))
            )
        )

    /**
      * Process and store the new websocket connection made to `/subscribe`
      *
      * @return websocket result
      */
    @cask.websocket("/subscribe")
    def subscribe(): cask.WebsocketResult =
        cask.WsHandler { channel =>
            websockets.addOne(channel)
            cask.WsActor {
                case cask.Ws.Close(_, _) => websockets -= channel // handle close, when closed : remove websocket
            }
        }

    /**
      * Delete the message history when a GET is made to `/clearHistory`
      */
    @cask.get("/clearHistory")
    def clearHistory(): Unit =
        msgSvc.deleteHistory()
        send20LastMessageToAll()

    initialize()
end MessagesRoutes
