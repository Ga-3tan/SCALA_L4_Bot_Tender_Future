package Chat

import Data.{AccountService, ProductService, Session}

class AnalyzerService(productSvc: ProductService,
                      accountSvc: AccountService):

  import ExprTree._

  /**
    * Compute the price of the current node, then returns it. If the node is not a computational node, the method
    * returns 0.0.
    * For example if we had a "+" node, we would add the values of its two children, then return the result.
    *
    * @return the result of the computation
    */
  // Part 2 Step 3
  def computePrice(t: ExprTree): Double =
    t match
      case RequestOrder(t) => computePrice(t)
      case And(tLeft, tRight) => computePrice(tLeft) + computePrice(tRight)
      case Or(tLeft, tRight) => computePrice(tLeft) min computePrice(tRight)
      case Order(quantity, product, brand) => productSvc.getPrice(product, brand) * quantity
      case _ => 0.0

  /**
    * Return the output text of the current node, in order to write it in console.
    *
    * @return the output text of the current node
    */
  def reply(session: Session)(t: ExprTree): String =
    // you can use this to avoid having to pass the session when doing recursion
    val inner: ExprTree => String = reply(session)
    val msgIdentificationNeeded: String = "Veuillez d'abord vous identifier."

    t match
      // Part 2 Step 3
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
          val price = computePrice(t)
          var balance = accountSvc.getAccountBalance(user)
          if balance >= price then
            balance = accountSvc.purchase(user, price)
            s"Voici donc ${inner(t)} ! Cela coûte CHF $price et votre nouveau solde est de CHF $balance."
          else
            s"Vous n'avez pas assez d'argent dans votre solde. Cela coûte CHF $price. et votre solde est CHF $balance."
        }).getOrElse(msgIdentificationNeeded)
      case RequestPrice(t) => s"Cela nous coûte CHF ${computePrice(t)} au total"

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
