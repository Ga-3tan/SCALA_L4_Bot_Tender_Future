package Chat

import Data.{AccountService, MessageService, ProductService, Session}
import Chat.AnalyzerService
import Chat.ExprTree.{And, Or, Order, RequestOrder}
import Utils.FutureOps
import Web.{Layouts, WebsocketService}

import scala.util.{Failure, Success, Try}
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AnalyzerFutureService(productSvc: ProductService,
                            msgSvc: MessageService,
                            accountSvc: AccountService,
                            websocketSvc: WebsocketService) extends AnalyzerService(productSvc,accountSvc) :

  def getOrders(t: ExprTree): List[Order] =
    t match
      case RequestOrder(t) => getOrders(t)
      case And(tLeft, tRight) => getOrders(tLeft) ::: getOrders(tRight)
      case Or(tLeft, tRight) =>
        if computePrice(tLeft) <= computePrice(tRight)
        then getOrders(tLeft)
        else getOrders(tRight)
      case Order(quantity, product, brand) => List(Order(quantity, product, brand))
      case _ => throw new Error("Incorrect Type ExprTree in RequestOrder")

  def getTree(l: List[Order]): ExprTree =
    l match {
      case ::(head, next) =>
        if next.isEmpty then
          head
        else
          And(head, getTree(next))
      case Nil => null
    }

  override def processOrders(user: String, t: ExprTree): String =
    val orders = getOrders(t).map(o =>
      FutureOps.randomSchedule(productSvc.getMeanPrepTime(o.product),successRate = 0.5)
        .map(_ => Success(o)).recover { case x => Failure(x)}
    )

    val strRequest = reply(null)(t)
    val msgCommandOrigin: String = s"@$user la commande de $strRequest"
    val validOrders = Future.sequence(orders).map(_.collect{ case Success(v) => v})
    validOrders transform {
      case Success(l) =>
        val validTree = getTree(l)
        var msgContent = ""

        if l.isEmpty then
          msgContent = s"$msgCommandOrigin ne peut pas être délivré."
        else if l.length == orders.length then
          val msgReply = super.processOrders(user, validTree)
          msgContent = s"$msgCommandOrigin est prête. $msgReply"
        else
          val msgReply = super.processOrders(user, validTree)
          msgContent = s"$msgCommandOrigin est partiellement prête. $msgReply"

        msgSvc.add(
          sender = "Bot-tender",
          msg = Layouts.getMessageSpan(s"$msgContent")
        )

        // refresh page
        Success(websocketSvc.sendMessagesToAll(msgSvc.getLatestMessages(20)))
      case Failure(_) =>
        msgSvc.add(
          sender = "Bot-tender",
          msg = Layouts.getMessageSpan(s"$msgCommandOrigin n'a pas pu être délivrée.")
        )

        // refresh page
        Success(websocketSvc.sendMessagesToAll(msgSvc.getLatestMessages(20)))
    }

    s"Votre commande est en cours de préparation: $strRequest"

