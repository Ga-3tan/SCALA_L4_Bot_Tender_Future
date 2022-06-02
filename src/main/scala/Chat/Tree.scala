package Chat

/**
  * This sealed trait represents a node of the tree.
  */
sealed trait ExprTree

/**
  * Declarations of the nodes' types.
  */
object ExprTree:
  // Part 2 Step 3
  case class Thirsty() extends ExprTree
  case class Hungry() extends ExprTree
  case class Identify(user: String) extends ExprTree // Rename Identification
  case class RequestOrder(t: ExprTree) extends ExprTree
  case class RequestBalance() extends ExprTree
  case class RequestPrice(t: ExprTree) extends ExprTree
  case class And(tLeft: ExprTree, tRight: ExprTree) extends ExprTree
  case class Or(tLeft: ExprTree, tRight: ExprTree) extends ExprTree
  case class Order(quantity: Int, product: String, brand: String) extends ExprTree
