package Chat

import Data.{AccountService, MessageService, ProductService, Session}

import scala.concurrent.*
import scala.concurrent.duration.Duration
import Utils.FutureOps
import Web.Layouts
import ujson.IndexedValue.False

import scala.util.{Failure, Success, Try}
import concurrent.ExecutionContext.Implicits.global

class AnalyzerService(productSvc: ProductService,
                      accountSvc: AccountService):

  import ExprTree._

  def msgIdentificationNeeded: String = "Veuillez d'abord vous identifier."
  def msgYourSold(balance: Double): String = s"votre solde est CHF $balance"
  def msgGivePrice(price: Double): String = s"Cela coûte CHF ${price}"
  def msgGiveOrder(orders: String): String = s"Voici donc $orders"
  def msgNotEnoughInBalance(price: Double, balance: Double): String = s"Vous n'avez pas assez d'argent dans votre solde. ${msgGivePrice(price)} et ${msgYourSold(balance)}."

  /**
    * Compute the price of the current node, then returns it. If the node is not a computational node, the method
    * returns 0.0.
    * For example if we had a "+" node, we would add the values of its two children, then return the result.
    *
    * @return the result of the computation
    */
  def computePrice(t: ExprTree): Double =
    t match
      case RequestOrder(t) => computePrice(t)
      case And(tLeft, tRight) => computePrice(tLeft) + computePrice(tRight)
      case Or(tLeft, tRight) => computePrice(tLeft) min computePrice(tRight)
      case Order(quantity, product, brand) => productSvc.getPrice(product, brand) * quantity
      case _ => 0.0

  def processOrders(user: String, t: ExprTree): String =
    val price = computePrice(t)
    var balance = accountSvc.getAccountBalance(user)
    if balance >= price then
      balance = accountSvc.purchase(user, price)
      s"${msgGiveOrder(reply(null)(t))} ! ${msgGivePrice(price)} et votre nouveau solde est de CHF $balance."
    else msgNotEnoughInBalance(price, balance)

  /**
    * Return the output text of the current node, in order to write it in console.
    *
    * @return the output text of the current node
    */
  def reply(session: Session)(t: ExprTree): String =
    // you can use this to avoid having to pass the session when doing recursion
    val inner: ExprTree => String = reply(session)

    t match
      /* User stat */
      case Thirsty() => "Eh bien, la chance est de votre côté, car nous offrons les meilleures bières de la région !"
      case Hungry() => "Pas de soucis, nous pouvons notamment vous offrir des croissants faits maisons !"

      /* User interaction  */
      case Identify(user) =>
        val pseudo = user.tail
        session.getCurrentUser match
          case Some(user) if user != pseudo => s"Euh... Non, vous êtes $user."
          case Some(user) => s"Je sais déjà qui vous êtes $user, vous êtes loggé."
          case _ => s"Bonjour, veuillez vous identifier via le log-in svp." // useless if the user can't send message when not logged.

      /** Requests */
      case RequestBalance() =>
        session.getCurrentUser
          .map(user => s"Le montant actuel de votre solde est de CHF ${accountSvc.getAccountBalance(user)}")
          .getOrElse(msgIdentificationNeeded)
      case RequestOrder(t) =>
        session.getCurrentUser.map(user => {
          processOrders(user,t)
        }).getOrElse(msgIdentificationNeeded)
      case RequestPrice(t) => msgGivePrice(computePrice(t))

      /* Logical operator */
      case And(tLeft, tRight) => s"${inner(tLeft)} et ${inner(tRight)}"
      case Or(tLeft, tRight) =>
        if computePrice(tLeft) < computePrice(tRight)
        then inner(tLeft)
        else inner(tRight)

      /** Request Types */
      case Order(quantity, product, brand) =>
        s"$quantity ${if brand == null then productSvc.getDefaultBrand(product) else brand}"

end AnalyzerService
