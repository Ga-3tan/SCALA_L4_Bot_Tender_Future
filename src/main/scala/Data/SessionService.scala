package Data

import scala.collection.mutable
import scala.util.Random

/**
  * Allow the retrieval of information linked to a session.
  */
trait Session:
  val sid: String
  private var _currentUser: Option[String] = None

  def getCurrentUser: Option[String] = _currentUser
  def setCurrentUser(user: String): Unit = _currentUser = Some(user)
  def reset(): Unit = _currentUser = None

/**
  * Allow the creation of a session with an identifier.
  * The identifier allow its later retrieval
  */
trait SessionService:
  def create(): Session
  def exists(sid: String): Boolean
  def get(sid: String): Option[Session]

/**
  * In-memory local implementation of a SessionService.
  */
class SessionImpl extends SessionService:
  val existingSessions: mutable.Map[String, Session] = mutable.Map()

  private def nextSid(): String =
    val sid: String = Seq.fill(8)(Random.nextPrintableChar()).mkString
    if existingSessions.contains(sid) then nextSid() else sid

  override def create(): Session = new Session:
    override val sid: String = nextSid()
    existingSessions(sid) = this
  override def exists(sid: String): Boolean = existingSessions.contains(sid)
  override def get(sid: String): Option[Session] = existingSessions.get(sid)
end SessionImpl


