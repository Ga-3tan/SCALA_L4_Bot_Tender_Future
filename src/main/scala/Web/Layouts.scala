package Web

import Data.MessageService.{MsgContent, Username}
import scalatags.Text.all.*
import scalatags.Text.tags2

type ScalaTag = scalatags.Text.TypedTag[String]

/**
  * Assembles the method used to layout ScalaTags
  */
object Layouts:
  /** *******************
    * ----- Pages ----- *
    * ******************* */
  def bodyPage(navItems: Seq[ScalaTag], contents: Seq[ScalaTag]): ScalaTag =
    html(
      getHeader,
      body(
        getNav(navItems),
        div(`class` := "content")(contents)
      )
    )

  def message(msgContent: String): Frag =
    p(msgContent)

  def homePage(isLogged: Boolean = false, messages: Seq[(Username, MsgContent)] = null): ScalaTag =
    bodyPage(
      getConnectionNavItems(isLogged),
      Seq(
        div(id := "boardMessage")(getBoardMessageContent(messages)),
        getForm(
          text = "Your message:",
          placeholderText = "Write your message",
          submitAction = "submitMessageForm(); return false",
        )
      )
    )

  def loginPage(errorMsg: String = ""): ScalaTag =
    bodyPage(
      Seq(navItemHomePage),
      Seq(
        h1("Login"),
        getForm(
          text = "Username:",
          placeholderText = "Enter your pseudo",
          submitPath = "/login",
          errorMsg = errorMsg
        )
      )
    )

  def registerPage(errorMsg: String = ""): ScalaTag =
    bodyPage(
      Seq(navItemHomePage),
      Seq(
        h1("Register"),
        getForm(
          text = "Username:",
          placeholderText = "Enter your pseudo",
          submitPath = "/register",
          errorMsg = errorMsg
        )
      )
    )

  def loginStatusChangedPage(isLogin: Boolean, msg: String): ScalaTag =
    bodyPage(
      getConnectionNavItems(isLogin).+:(navItemHomePage),
      Seq(
        p(style := "text-align:center;")(msg)
      ),
    )

  /** **********************
    * ----- elements ----- *
    * ********************** */

  // head
  def getHeader: ScalaTag =
    head(
      meta(charset := "utf-8"),
      link(rel := "stylesheet", href := "/static/resources/css/main.css"),
      script(src := "/static/resources/js/main.js")
    )

  // nav
  def getNav(navItems: Seq[ScalaTag]): ScalaTag =
    tag("nav")(
      a(`class` := "nav-brand")("Bot-tender"),
      navItems
    )

  // Create navigation item. A link in the nav.
  def navItem(ref: String, text: String): ScalaTag =
    div(`class` := "nav-item")(a(href := ref)(text))

  // nav item to go to the homepage.
  def navItemHomePage: ScalaTag =
    navItem("/", "Go to the message board")

  // Get the navigation items based on the login status
  def getConnectionNavItems(isLogged: Boolean): Seq[ScalaTag] =
    if isLogged then
      Seq(navItem("/logout", "logout"))
    else
      Seq(
        navItem("/login", "Log-in"),
        navItem("/register", "Sign-up")
      )

  // chatroom display
  def getBoardMessageContent(messages: Seq[(Username, MsgContent)] = null): Seq[ScalaTag] =
    if messages == null || messages.isEmpty then
      Seq(p(style := "text-align:center;")("No messages have beeen sent yet"))
    else messages.map(m => getMessageDiv(m._1, m._2))

  // A line in the chatroom.
  def getMessageDiv(author: Username, msgContent: MsgContent): ScalaTag =
    div(`class` := "msg")(
      span(`class` := "author")(author),
      msgContent,
    )

  // Content of a message
  def getMessageSpan(msgContent: String): ScalaTag =
    span(`class` := "msg-content")(msgContent)

  // Form to submit message or login.
  def getForm(text: String,
              placeholderText: String,
              errorMsg: String = "",
              submitPath: String = null,
              submitAction: String = null): ScalaTag =
    val divError: Seq[ScalaTag] =
      Seq(div(id := "errorDiv", `class` := "errorMsg", errorMsg))

    val content: Seq[ScalaTag] =
      Seq(
        label(`for` := "messageInput")(text),
        input(name := "text", id := "messageInput", `type` := "text", placeholder := placeholderText),
        input(`type` := "submit"),
      )

    if submitPath != null then
      form(id := "msgForm", action := submitPath, method := "post")(
        divError ++ content
      )
    else
      form(id := "msgForm", onsubmit := submitAction)(
        divError ++ content
      )

end Layouts
