package Web

import Data.{AccountService, Session, SessionService}
import Web.Decorators.getSession

/**
  * Assembles the routes dealing with the users:
  * - One route to display the login form and register form page
  * - One route to process the login form and display the login success page
  * - One route to process the register form and display the register success page
  * - One route to logout and display the logout success page
  * 
  * The username of the current session user is stored inside a cookie called `username`.
  */
class UsersRoutes(accountSvc: AccountService,
                  sessionSvc: SessionService)(implicit val log: cask.Logger) extends cask.Routes:
    @getSession(sessionSvc)
    @cask.get("/login")
    def login()(session: Session): ScalaTag =
        Layouts.loginPage()

    @getSession(sessionSvc)
    @cask.get("/register")
    def register()(session: Session): ScalaTag =
        Layouts.registerPage()

    @getSession(sessionSvc)
    @cask.postForm("/login")
    def setLogin(text: cask.FormValue)(session: Session): ScalaTag =
        val username = text.value
        if accountSvc.isAccountExisting(username) then
            session.setCurrentUser(username)
            Layouts.loginStatusChangedPage(true,"You are logged in !")
        else
            Layouts.loginPage("The specified user does not exists")

    @getSession(sessionSvc)
    @cask.postForm("/register")
    def setRegister(text: cask.FormValue)(session: Session): ScalaTag =
        val username = text.value
        if accountSvc.isAccountExisting(username) then
            Layouts.registerPage("The specified user already exist.")
        else
            accountSvc.addAccount(username, 30)
            session.setCurrentUser(username)
            Layouts.loginStatusChangedPage(true,"Account created ! You are logged in !")

    @getSession(sessionSvc)
    @cask.get("/logout")
    def unsetLogin()(session: Session): ScalaTag =
        session.reset()
        Layouts.loginStatusChangedPage(false,"You have logged out successfully")

    initialize()
end UsersRoutes
