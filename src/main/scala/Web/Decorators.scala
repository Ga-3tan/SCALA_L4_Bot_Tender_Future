package Web

import Data.{Session, SessionService}
import cask.model.Response
import cask.router.Result


/**
  * Assembles the custom decorators.
  */
object Decorators:
    import cask.model.Response.Data
    /**
      * Wrap an endpoint. Read the request to search for the session id in
      * the cookies. Provide the endpoint the matching `Session` if it exists.
      * Create and provide a new `Session` otherwise.
      */
    class getSession(sessionSvc: SessionService) extends cask.RawDecorator:
        def wrapFunction(ctx: cask.Request, delegate: Delegate): Result[Response.Raw] =
            val SESSION_COOKIE = "sid"
            val existingSession = ctx.cookies.get(SESSION_COOKIE)
                .map(c => c.value)
                .flatMap(sessionSvc.get)
            val session: Session = existingSession
                .getOrElse(sessionSvc.create())

            val res = delegate(Map("session" -> session))

            if existingSession.isEmpty then
                // Add newly created session to the cookies
                res.map(r => r.copy(cookies = r.cookies.appended(cask.Cookie(SESSION_COOKIE, session.sid))))
            else
                res
