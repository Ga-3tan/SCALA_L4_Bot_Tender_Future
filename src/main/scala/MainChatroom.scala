import Web.{UsersRoutes, MessagesRoutes, StaticRoutes}
import Chat._
import Data._
import Utils._

object MainChatroom extends cask.Main:
  val spellCheckerSvc: SpellCheckerService = new SpellCheckerImpl(Dictionary.dictionary)
  val tokenizerSvc: TokenizerService = new TokenizerService(spellCheckerSvc)
  val productSvc: ProductService = new ProductImpl()
  val sessionSvc: SessionService = new SessionImpl()
  val accountSvc: AccountService = new AccountImpl()
  val msgSvc: MessageService = new MessageImpl()
  val analyzerSvc: AnalyzerService = new AnalyzerService(productSvc, accountSvc)

  val allRoutes = Seq(
      StaticRoutes(),
      UsersRoutes(accountSvc, sessionSvc),
      MessagesRoutes(tokenizerSvc, analyzerSvc, msgSvc, accountSvc, sessionSvc))

  override def port: Int = 8980


