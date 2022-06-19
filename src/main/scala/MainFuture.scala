import Chat.*
import Data.*
import Utils.*
import Web.{MessagesRoutes, StaticRoutes, UsersRoutes, WebsocketImpl}

object MainFuture extends cask.Main:
  val spellCheckerSvc = new SpellCheckerImpl(Dictionary.dictionary)
  val tokenizerSvc = new TokenizerService(spellCheckerSvc)
  val sessionSvc = new SessionImpl()
  val productSvc = new ProductImpl()
  val accountSvc: AccountService = new AccountImpl()
  val msgSvc: MessageService = new MessageConcurrentImpl(new MessageImpl())
  val websocketSvc = new WebsocketImpl()
  val analyzerSvc = new AnalyzerFutureService(productSvc, msgSvc, accountSvc, websocketSvc)

  val allRoutes = Seq(
      StaticRoutes(),
      UsersRoutes(accountSvc, sessionSvc),
      MessagesRoutes(tokenizerSvc, analyzerSvc, msgSvc, accountSvc, sessionSvc, websocketSvc),
  )

  override def port: Int = 8980
