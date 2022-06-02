import Chat.*
import Data.*
import Utils.*
import Web.{StaticRoutes, UsersRoutes}
import Web.{MessagesRoutes}

object MainFuture extends cask.Main:
  val spellCheckerSvc = new SpellCheckerImpl(Dictionary.dictionary)
  val tokenizerSvc = new TokenizerService(spellCheckerSvc)
  val sessionSvc = new SessionImpl()
  val productSvc = new ProductImpl()
  val accountSvc: AccountService = new AccountImpl()
  val analyzerSvc = new AnalyzerService(productSvc, accountSvc)
  val msgSvc: MessageService = new MessageConcurrentImpl(new MessageImpl())

  val allRoutes = Seq(
      StaticRoutes(),
      UsersRoutes(accountSvc, sessionSvc),
      MessagesRoutes(tokenizerSvc, analyzerSvc, msgSvc, accountSvc, sessionSvc),
  )

  override def port: Int = 8980
