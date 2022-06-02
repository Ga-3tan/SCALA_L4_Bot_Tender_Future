package Data

import scala.collection.mutable

trait   AccountService:
  /**
    * Retrieve the balance of a given account
    *
    * @param user the name of the user whose account will be retrieve
    * @return the current balance of the user
    */
  def getAccountBalance(user: String): Double

  /**
    * Add an account to the existing accounts
    *
    * @param user    the name of the user
    * @param balance the initial balance value
    */
  def addAccount(user: String, balance: Double): Unit

  /**
    * Indicate if an account exist
    *
    * @param user the name of the user whose account is checked to exist
    * @return whether the account exists or not
    */
  def isAccountExisting(user: String): Boolean

  /**
    * Update an account by decreasing its balance.
    *
    * @param user   the name of the user whose account will be updated
    * @param amount the amount to decrease
    * @return the new balance
    */
  def purchase(user: String, amount: Double): Double

class AccountImpl extends AccountService:
  // Part 2 Step 2
  var accounts: mutable.Map[String, Double] = mutable.Map().withDefaultValue(30.0)

  def getAccountBalance(user: String): Double = accounts(user)

  def addAccount(user: String, balance: Double): Unit = accounts.put(user, balance)

  def isAccountExisting(user: String): Boolean = accounts.contains(user)

  def purchase(user: String, amount: Double): Double =
    assert(isAccountExisting(user))
    accounts(user) -= amount
    accounts(user)

end AccountImpl
