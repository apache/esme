package org.apache.esme.model
/**
 * Copyright 2008-2009 WorldWide Conferencing, LLC
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

import net.liftweb._
import mapper._
import util.Mailer.From
import scala.xml.{Node, Elem, NodeSeq, Text}

import sitemap._
import util._
import openid._
import http._
import js._
import JsCmds._
import Helpers._
import sitemap._
import Loc._

import org.openid4java.discovery.Identifier
import org.openid4java.consumer._
import org.openid4java.util._

import org.apache.esme._
import org.apache.esme.actor._
import org.apache.esme.view._
import java.net.URL
import java.util.logging._

object User extends User with KeyedMetaMapper[Long, User] {
  override def afterSave = profileChanged _ :: notifyActors _ :: super.afterSave

  private def notifyActors(in: User) {
    Distributor ! Distributor.UserUpdated(in.id)
  }

  private def profileChanged(in: User) {
    if (!in.needsChange_?)
    Message.create.author(in.id).
    when(Helpers.timeNow.getTime).
    source("profile").
    setTextAndTags("User " + in.nickname + " changed profile. Name: " + in.wholeName + ", Image: " + in.imageUrl, Nil, Empty).
    foreach{ msg =>
      if (msg.save) {
        Distributor ! Distributor.AddMessageToMailbox(in.id, msg, ProfileReason(in.id))
      }
    }
  }

  def findFromWeb(uid: String): Box[User] =
  User.find(By(User.nickname, uid)) or User.find(uid)

  override def dbTableName = "users" // define the DB table name

  def signupForm(xhtml: NodeSeq): NodeSeq = {
    val from = S.referer openOr "/"
    val user = User.create
    val auth = UserAuth.defaultAuthModule.createHolder()
    val snippetName = S.invokedAs

    def doSubmit() {
      S.mapSnippet(snippetName, genForm)
      user.validate ::: auth.validate match {
        case Nil =>
          user.save
          auth.save(user)
          User.logUserIn(user)
          S.notice(S.?("Welcome")+" "+user.niceName)
          S.redirectTo(from)

        case fe =>
          S.error(fe)
      }
    }

    def genForm(xhtml: NodeSeq): NodeSeq = {
      bind("signup", xhtml,
           "nickname" _id_> user.nickname.toForm,
           "firstname" _id_> user.firstName.toForm,
           "lastname" _id_> user.lastName.toForm,
           "image_url" _id_> user.imageUrl.toForm,
           "timezone" _id_> user.timezone.toForm,
           "locale" _id_> user.locale.toForm,
           "credentials" -> auth.toForm,
           "submit" _id_> SHtml.submit(S.?("Sign Up"), doSubmit))
    }


    genForm(xhtml)
  }

  /*
   def screenWrap = S.request.flatMap(_.location) match {
   case Full(l) if l.name == "Login" => Full(<lift:surround with="login" at="content">
   <lift:bind /></lift:surround>)
   case _ => Full(<lift:surround with="default" at="content">
   <lift:bind /></lift:surround>)
   }

   /**
    * The menu item for editing the user (make this "Empty" to disable)
    */
   def editUserMenuLoc: Box[Menu] =
   Full(Menu(Loc("EditUser", editPath, "Profile",
   Template(() => wrapIt(editFunc.map(_()) openOr edit)),
   testLogginIn)))



   def signupFields: List[BaseOwnedMappedField[User]] = nickname ::
   firstName :: lastName :: imageUrl :: timezone :: locale :: Nil

   override def fieldOrder: List[BaseOwnedMappedField[User]] = nickname ::
   firstName :: lastName :: imageUrl :: timezone :: locale :: Nil

   def loginXhtml = TemplateFinder.findAnyTemplate(List("user_template", "login")) openOr NodeSeq.Empty

   /*
    <form id="openid_submit" class="clear" method="POST" action={loginPath.mkString("/", "/", "")} >
    <div class="b-open-l">
    <p class="input"><label>Open ID</label><user:openid /></p>
    <p class="button">
    <img onclick="document.getElementById('openid_submit').submit()" src="/images/sign-on.png" alt="Sign On" />
    </p>
    </div>
    {
    if (openIdError.is)
    <div class="b-open-r">
    <h3>Oops!</h3>
    <p>We are not able validate your ID. Please try again.</p>
    </div>
    else Text("")
    }
    </form>
    */
   object openIdError extends RequestVar(false)

   def login = {
   if (S.post_?) {
   S.param("username").
   foreach(username =>
   ESMEOpenIDVendor.loginAndRedirect(username, logUserIn)
   )
   }

   def logUserIn(openid: Box[Identifier], fo: Box[VerificationResult], exp: Box[Exception]): LiftResponse = {
   (openid, exp) match {
   case (Full(id), _) =>
   val user = User.findOrCreate(id.getIdentifier)
   User.logUserIn(user)
   S.notice("Welcome "+user.niceName)

   Message.create.author(user.id).
   when(Helpers.timeNow.getTime).
   source("login").
   setTextAndTags("User " + user.nickname + " logged in.", Nil, Empty).
   foreach{ msg =>
   if (msg.save) {
   Distributor ! Distributor.AddMessageToMailbox(user.id, msg, LoginReason(user.id))
   }
   }

   RedirectResponse("/", S responseCookies :_*)

   case (_, Full(exp)) =>
   openIdError(true)
   S.error("Got an exception: "+exp.getMessage)
   RedirectResponse(S.uri, S responseCookies :_*)

   case _ =>
   openIdError(true)
   S.error("Unable to log you in: "+fo.map(_.getStatusMsg))
   RedirectResponse(S.uri, S responseCookies :_*)
   }


   }

   loginForm
   }

   def loginForm =
   bind("user", loginXhtml,
   "openid" -> (FocusOnLoad(<input type="text" name="username"/>)))

   def openIDVendor = ESMEOpenIDVendor

   def logout = {
   logoutCurrentUser
   S.redirectTo("/static/about")
   }
   */
  def followerIdsForUserId(userId: Long): List[Long] =
  Relationship.findAll(By(Relationship.target, userId)).map(_.owner.is)

  

  /**
   * Create and populate a new user
   */
  def createAndPopulate: User = {
    val user = this.create.
    nickname("change"+Helpers.randomInt(1000000000)).firstName("Unknown").
    lastName("Unknown").
    // email(Helpers.randomInt(100000000)+"unknown@unknown.com").
    saveMe
    // create a crypto record for the user
    UserCryptoSig.create.user(user).save
    user
  }
  

  /*
   // no need for these menu items with OpenID
   /**
    * The menu item for creating the user/sign up (make this "Empty" to disable)
    */
   def createUserMenuLoc: Box[Menu] = Empty

   /**
    * The menu item for lost password (make this "Empty" to disable)
    */
   def lostPasswordMenuLoc: Box[Menu] = Empty

   /**
    * The menu item for resetting the password (make this "Empty" to disable)
    */
   def resetPasswordMenuLoc: Box[Menu] = Empty

   /**
    * The menu item for changing password (make this "Empty" to disable)
    */
   def changePasswordMenuLoc: Box[Menu] = Empty

   /**
    * The menu item for validating a user (make this "Empty" to disable)
    */
   def validateUserMenuLoc: Box[Menu] = Empty

   */
  def findByNickname(str: String): List[User] =
  findAll(By(nickname, str))
  /*

   val basePath: List[String] = "user_mgt" :: Nil
   def signUpSuffix = "sign_up"
   lazy val signUpPath = thePath(signUpSuffix)
   def loginSuffix = "login"
   lazy val loginPath = thePath(loginSuffix)
   def lostPasswordSuffix = "lost_password"
   lazy val lostPasswordPath = thePath(lostPasswordSuffix)
   def passwordResetSuffix = "reset_password"
   lazy val passwordResetPath = thePath(passwordResetSuffix)
   def changePasswordSuffix = "change_password"
   lazy val changePasswordPath = thePath(changePasswordSuffix)
   def logoutSuffix = "logout"
   lazy val logoutPath = thePath(logoutSuffix)
   def editSuffix = "edit"
   lazy val editPath = thePath(editSuffix)
   def validateUserSuffix = "validate_user"
   lazy val validateUserPath = thePath(validateUserSuffix)

   def homePage = "/"



   case class MenuItem(name: String, path: List[String],
   loggedIn: Boolean) {
   lazy val endOfPath = path.last
   lazy val pathStr: String = path.mkString("/", "/", "")
   lazy val display = name match {
   case null | "" => false
   case _ => true
   }
   }

   def thePath(end: String): List[String] = basePath ::: List(end)

   /**
    * Return the URL of the "login" page
    */
   def loginPageURL = loginPath.mkString("/","/", "")

   def notLoggedIn_? = !loggedIn_?

   lazy val testLogginIn = If(loggedIn_? _, S.??("must.be.logged.in")) ;

   lazy val testSuperUser = If(superUser_? _, S.??("must.be.super.user"))

   def superUser_? : Boolean = currentUser.map(_.superUser.is) openOr false

   /**
    * The menu item for login (make this "Empty" to disable)
    */
   def loginMenuLoc: Box[Menu] = {
   Full(Menu(Loc("Login", loginPath, S.??("login"),
   If(notLoggedIn_? _, S.??("already.logged.in")),
   Template(() => wrapIt(login)))))
   }

   /**
    * The menu item for logout (make this "Empty" to disable)
    */
   def logoutMenuLoc: Box[Menu] =
   Full(Menu(Loc("Logout", logoutPath, S.??("logout"),
   Template(() => wrapIt(logout)),
   testLogginIn)))

   /*
    /**
     * The menu item for creating the user/sign up (make this "Empty" to disable)
     */
    def createUserMenuLoc: Box[Menu] =
    Full(Menu(Loc("CreateUser", signUpPath,
    S.??("sign.up"),
    Template(() => wrapIt(signupFunc.map(_()) openOr signup)),
    If(notLoggedIn_? _, S.??("logout.first")))))

    /**
     * The menu item for lost password (make this "Empty" to disable)
     */
    def lostPasswordMenuLoc: Box[Menu] =
    Full(Menu(Loc("LostPassword", lostPasswordPath,
    S.??("lost.password"),
    Template(() => wrapIt(lostPassword)),
    If(notLoggedIn_? _, S.??("logout.first"))))) // not logged in

    /**
     * The menu item for resetting the password (make this "Empty" to disable)
     */
    def resetPasswordMenuLoc: Box[Menu] =
    Full(Menu(Loc("ResetPassword", (passwordResetPath, true),
    S.??("reset.password"), Hidden,
    Template(() => wrapIt(passwordReset(snarfLastItem))),
    If(notLoggedIn_? _,
    S.??("logout.first"))))) //not Logged in

    /**
     * The menu item for editing the user (make this "Empty" to disable)
     */
    def editUserMenuLoc: Box[Menu] =
    Full(Menu(Loc("EditUser", editPath, S.??("edit.user"),
    Template(() => wrapIt(editFunc.map(_()) openOr edit)),
    testLogginIn)))
    */

   /*
    /**
     * The menu item for changing password (make this "Empty" to disable)
     */
    def changePasswordMenuLoc: Box[Menu] =
    Full(Menu(Loc("ChangePassword", changePasswordPath,
    S.??("change.password"),
    Template(() => wrapIt(changePassword)),
    testLogginIn)))
    */

   /*
    /**
     * The menu item for validating a user (make this "Empty" to disable)
     */
    def validateUserMenuLoc: Box[Menu] =
    Full(Menu(Loc("ValidateUser", (validateUserPath, true),
    S.??("validate.user"), Hidden,
    Template(() => wrapIt(validateUser(snarfLastItem))),
    If(notLoggedIn_? _, S.??("logout.first")))))
    */
   lazy val sitemap: List[Menu] =
   List(loginMenuLoc, logoutMenuLoc, createUserMenuLoc,
   lostPasswordMenuLoc, resetPasswordMenuLoc,
   editUserMenuLoc, changePasswordMenuLoc,
   validateUserMenuLoc).flatten(a => a)


   def skipEmailValidation = false

   def userMenu: List[Node] = {
   val li = loggedIn_?
   ItemList.
   filter(i => i.display && i.loggedIn == li).
   map(i => (<a href={i.pathStr}>{i.name}</a>))
   }

   protected def snarfLastItem: String =
   (for (r <- S.request) yield r.path.wholePath.last) openOr ""

   lazy val ItemList: List[MenuItem] =
   List(MenuItem(S.??("sign.up"), signUpPath, false),
   MenuItem(S.??("log.in"), loginPath, false),
   MenuItem(S.??("lost.password"), lostPasswordPath, false),
   MenuItem("", passwordResetPath, false),
   MenuItem(S.??("change.password"), changePasswordPath, true),
   MenuItem(S.??("log.out"), logoutPath, true),
   MenuItem(S.??("edit.profile"), editPath, true),
   MenuItem("", validateUserPath, false))

   // def requestLoans: List[LoanWrapper] = Nil // List(curUser)
   */
  var onLogIn: List[User => Unit] = List(ExtSession.userDidLogin _)

  var onLogOut: List[Box[User] => Unit] = List(ExtSession.userDidLogout _)

  def loggedIn_? : Boolean = currentUserId.isDefined

  def logUserIdIn(id: String) {
    curUser.remove()
    curUserId(Full(id))
  }
  def logUserIn(who: User) {
    curUser.remove()
    curUserId(Full(who.id.toString))
    onLogIn.foreach(_(who))
  }

  def logoutCurrentUser = logUserOut()

  def logUserOut() {
    onLogOut.foreach(_(curUser))
    curUserId.remove()
    curUser.remove()
    S.request.foreach(_.request.session.terminate)
  }

  private object curUserId extends SessionVar[Box[String]](Empty)

  def currentUserId: Box[String] = curUserId.is

  private object curUser extends RequestVar[Box[User]](currentUserId.flatMap(id => getSingleton.find(id)))


  def currentUser: Box[User] = curUser.is

  /*
   /*
    def signupXhtml(user: User) = {
    (<form method="post" action={S.uri}><table><tr><td
    colspan="2">Sign Up</td></tr>
    {localForm(user, false)}
    <tr><td>&nbsp;</td><td><user:submit/></td></tr>
    </table></form>)
    }


    def signupMailBody(user: User, validationLink: String) = {
    (<html>
    <head>
    <title>Sign Up Confirmation</title>
    </head>
    <body>
    <p>Dear {user.firstName},
    <br/>
    <br/>
    Click on this link to complete signup
    <br/><a href={validationLink}>{validationLink}</a>
    <br/>
    <br/>
    Thanks
    </p>
    </body>
    </html>)
    }

    def signupMailSubject = S.??("sign.up.confirmation")

    def sendValidationEmail(user: User) {
    val resetLink = S.hostAndPath+"/"+validateUserPath.mkString("/")+
    "/"+user.uniqueId

    val email: String = user.email

    val msgXml = signupMailBody(user, resetLink)

    import net.liftweb.util._
    import Mailer._

    Mailer.sendMail(From(emailFrom),Subject(signupMailSubject),
    (To(user.email) :: xmlToMailBodyType(msgXml) ::
    (bccEmail.toList.map(BCC(_)))) :_* )
    }

    protected object signupFunc extends RequestVar[Box[() => NodeSeq]](Empty)

    def signup = {
    val theUser: User = create
    val theName = signUpPath.mkString("")

    def testSignup() {
    theUser.validate match {
    case Nil =>
    theUser.validated(skipEmailValidation).uniqueId.reset()
    theUser.save
    if (!skipEmailValidation) {
    sendValidationEmail(theUser)
    S.notice(S.??("sign.up.message"))
    } else {
    S.notice(S.??("welcome"))
    logUserIn(theUser)
    }

    S.redirectTo(homePage)

    case xs => S.error(xs) ; signupFunc(Full(innerSignup _))
    }
    }

    def innerSignup = bind("user",
    signupXhtml(theUser),
    "submit" -> SHtml.submit(S.??("sign.up"), testSignup _))

    innerSignup
    }
    */

   def emailFrom = "noreply@"+S.hostName

   def bccEmail: Box[String] = Empty

   def testLoggedIn(page: String): Boolean =
   ItemList.filter(_.endOfPath == page) match {
   case x :: xs if x.loggedIn == loggedIn_? => true
   case _ => false
   }


   def validateUser(id: String): NodeSeq = getSingleton.find(By(uniqueId, id)) match {
   case Full(user) if !user.validated =>
   user.validated(true).uniqueId.reset().save
   S.notice(S.??("account.validated"))
   logUserIn(user)
   S.redirectTo(homePage)

   case _ => S.error(S.??("invalid.validation.link")); S.redirectTo(homePage)
   }

   /*
    def login = {
    if (S.post_?) {
    S.param("username").
    flatMap(username => getSingleton.find(By(email, username))) match {
    case Full(user) if user.validated &&
    user.password.match_?(S.param("password").openOr("*")) =>
    S.notice(S.??("logged.in"))
    logUserIn(user)
    S.redirectTo(homePage)

    case Full(user) if !user.validated =>
    S.error(S.??("account.validation.error"))

    case _ => S.error(S.??("invalid.credentials"))
    }
    }

    bind("user", loginXhtml,
    "email" -> (FocusOnLoad(<input type="text" name="username"/>)),
    "password" -> (<input type="password" name="password"/>),
    "submit" -> (<input type="submit" value={S.??("log.in")}/>))
    }
    */

   /*
    def lostPasswordXhtml = {
    (<form method="post" action={S.uri}>
    <table><tr><td
    colspan="2">{S.??("enter.email")}</td></tr>
    <tr><td>{S.??("email.address")}</td><td><user:email /></td></tr>
    <tr><td>&nbsp;</td><td><user:submit /></td></tr>
    </table>
    </form>)
    }

    def passwordResetMailBody(user: User, resetLink: String) = {
    (<html>
    <head>
    <title>{S.??("reset.password.confirmation")}</title>
    </head>
    <body>
    <p>{S.??("dear")} {user.firstName},
    <br/>
    <br/>
    {S.??("click.reset.link")}
    <br/><a href={resetLink}>{resetLink}</a>
    <br/>
    <br/>
    {S.??("thank.you")}
    </p>
    </body>
    </html>)
    }

    def passwordResetEmailSubject = S.??("reset.password.request")

    def sendPasswordReset(email: String) {
    getSingleton.find(By(this.email, email)) match {
    case Full(user) if user.validated =>
    user.uniqueId.reset().save
    val resetLink = S.hostAndPath+
    passwordResetPath.mkString("/", "/", "/")+user.uniqueId

    val email: String = user.email

    import Mailer._

    val msgXml = passwordResetMailBody(user, resetLink)
    Mailer.sendMail(From(emailFrom),Subject(passwordResetEmailSubject),
    (To(user.email) :: xmlToMailBodyType(msgXml) ::
    (bccEmail.toList.map(BCC(_)))) :_*)

    S.notice(S.??("password.reset.email.sent"))
    S.redirectTo(homePage)

    case Full(user) =>
    sendValidationEmail(user)
    S.notice(S.??("account.validation.resent"))
    S.redirectTo(homePage)

    case _ => S.error(S.??("email.address.not.found"))
    }
    }

    def lostPassword = {
    bind("user", lostPasswordXhtml,
    "email" -> SHtml.text("", sendPasswordReset _),
    "submit" -> <input type="Submit" value={S.??("send.it")} />)
    }

    def passwordResetXhtml = {
    (<form method="post" action={S.uri}>
    <table><tr><td colspan="2">{S.??("reset.your.password")}</td></tr>
    <tr><td>{S.??("enter.your.new.password")}</td><td><user:pwd/></td></tr>
    <tr><td>{S.??("repeat.your.new.password")}</td><td><user:pwd/></td></tr>
    <tr><td>&nbsp;</td><td><user:submit/></td></tr>
    </table>
    </form>)
    }

    def passwordReset(id: String) =
    getSingleton.find(By(uniqueId, id)) match {
    case Full(user) =>
    def finishSet() {
    user.validate match {
    case Nil => S.notice(S.??("password.changed"))
    user.save
    logUserIn(user); S.redirectTo(homePage)

    case xs => S.error(xs)
    }
    }
    user.uniqueId.reset().save

    bind("user", passwordResetXhtml,
    "pwd" -> SHtml.password_*("",S.LFuncHolder((p: List[String]) =>
    user.password.setList(p))),
    "submit" -> SHtml.submit(S.??("set.password"), finishSet _))
    case _ => S.error(S.??("pasword.link.invalid")); S.redirectTo(homePage)
    }

    def changePasswordXhtml = {
    (<form method="post" action={S.uri}>
    <table><tr><td colspan="2">{S.??("change.password")}</td></tr>
    <tr><td>{S.??("old.password")}</td><td><user:old_pwd /></td></tr>
    <tr><td>{S.??("new.password")}</td><td><user:new_pwd /></td></tr>
    <tr><td>{S.??("repeat.password")}</td><td><user:new_pwd /></td></tr>
    <tr><td>&nbsp;</td><td><user:submit /></td></tr>
    </table>
    </form>)
    }

    def changePassword = {
    val user = currentUser.open_! // we can do this because the logged in test has happened
    var oldPassword = ""
    var newPassword: List[String] = Nil

    def testAndSet() {
    if (!user.password.match_?(oldPassword)) S.error(S.??("wrong.old.password"))
    else {
    user.password.setFromAny(newPassword)
    user.validate match {
    case Nil => user.save; S.notice(S.??("pasword.changed")); S.redirectTo(homePage)
    case xs => S.error(xs)
    }
    }
    }

    bind("user", changePasswordXhtml,
    "old_pwd" -> SHtml.password("", oldPassword = _),
    "new_pwd" -> SHtml.password_*("", S.LFuncHolder(newPassword = _)),
    "submit" -> SHtml.submit(S.??("change"), testAndSet _))
    }
    */

   /*def editXhtml(user: User) = {
    (<form method="post" action={S.uri}>
    <table><tr><td colspan="2">{S.??("edit")}</td></tr>
    {localForm(user, true)}
    <tr><td>&nbsp;</td><td><user:submit/></td></tr>
    </table>
    </form>)
    }*/


   def editXhtml = <form method="post" action={S.uri}>{
   TemplateFinder.findAnyTemplate(List("user_template", "edit")) openOr NodeSeq.Empty
   }</form>

   object editFunc extends RequestVar[Box[() => NodeSeq]](Empty)

   def edit = {
   val theUser: User = currentUser.open_! // we know we're logged in
   val theName = editPath.mkString("")

   def testEdit() {
   theUser.validate match {
   case Nil =>
   theUser.save
   S.notice(S.??("profle.updated"))
   S.redirectTo(homePage)

   case xs => S.error(xs) ; editFunc(Full(innerEdit _))
   }
   }

   def innerEdit = bind("user", editXhtml,
   "user_info" -> localForm(theUser, true),
   "submit" -> SHtml.submit(S.??("edit"), testEdit _))

   innerEdit
   }

   private def localForm(user: User, ignorePassword: Boolean): NodeSeq = {
   signupFields.
   map(fi => getSingleton.getActualBaseField(user, fi)).
   filter(f => !ignorePassword || (f match {
   case f: MappedPassword[User] => false
   case _ => true
   })).
   flatMap(f =>
   f.toForm.toList.map(form =>
   (<tr><td>{f.displayName}</td><td>{form}</td></tr>) ) )
   }

   import scala.xml.transform.{RuleTransformer, RewriteRule}
   protected def wrapIt(in: NodeSeq): NodeSeq =
   screenWrap.map(new RuleTransformer(new RewriteRule {
   override def transform(n: Node) = n match {
   case e: Elem if "bind" == e.label && "lift" == e.prefix => in
   case _ => n
   }
   })) openOr in
   */

}

/*
 object ESMEOpenIDVendor extends OpenIdVendor {
 type UserType = User
 type ConsumerType = ESMEOpenIDConsumer

 def logUserOut(): Unit = User.logUserOut()

 def currentUser = User.currentUser

 def postLogin(id: Box[Identifier],res: VerificationResult): Unit = {
 id match {
 case Full(id) =>
 val user = User.findOrCreate(id.getIdentifier())
 User.logUserIn(user)
 S.notice("Welcome "+user.niceName)

 case _ =>
 logUserOut()
 S.error("Failed to authenticate")
 }
 }

 def displayUser(in: User): NodeSeq = Text("Welcome "+in.niceName)

 def createAConsumer = new ESMEOpenIDConsumer
 }

 class ESMEOpenIDConsumer extends OpenIdConsumer[User]
 {
 override val manager = {

 Log.info("Proxy settings: " + Props.get("http.proxyHost", "[no host]")
 + ":" + Props.get("http.proxyPort", "[no port]"))

 for (host <- Props.get("http.proxyHost")){
 val proxyProps = new ProxyProperties()
 proxyProps.setProxyHostName(host)
 proxyProps.setProxyPort(Props.getInt("http.proxyPort", 80))
 HttpClientFactory.setProxyProperties(proxyProps)
 }
 new ConsumerManager
 }
 }
 */

/**
 * An O-R mapped "User" class that includes first name, last name, password
 */
class User extends KeyedMapper[Long, User] with UserIdAsString {// OpenIDProtoUser[User] {
  import S._

  def getSingleton = User // what's the "meta" server

  object imageUrl extends MappedString(this, 256)

  def authTokens: List[AuthToken] =
  AuthToken.findAll(By(AuthToken. user, this),
                    OrderBy(AuthToken.description, Ascending))


  override lazy val toXml =
  <user id={id.toString} nickname={niceName} image={image} whole_name={wholeName} />

  def follow(who: User): Boolean = {
    if (who == this) false
    else
    Relationship.find(By(Relationship.owner, this),
                      By(Relationship.target, who)) match {
      case Full(x) => true
      case Empty => { if (Relationship.create.owner(this).target(who).save)
                     Message.create.author(who.id).
                     when(Helpers.timeNow.getTime).
                     source("followed").
                     setTextAndTags("User " + this.nickname + " followed " + who.nickname + ".", Nil, Empty).
                     foreach { msg =>
            if (msg.save) {
              Distributor ! Distributor.AddMessageToMailbox(who.id, msg, FollowedReason(this.id))
            }
          }
                     true
        }
      case _ => false
    }
  }

  def unfollow(who: User): Boolean = {
    Relationship.findAll(By(Relationship.owner, this),
                         By(Relationship.target, who)).foreach{ r =>
      if (r.delete_!) Message.create.author(who.id).
      when(Helpers.timeNow.getTime).
      source("unfollowed").
      setTextAndTags("User " + this.nickname + " unfollowed " + who.nickname + ".", Nil, Empty).
      foreach{ msg =>
        if (msg.save) {
          Distributor ! Distributor.AddMessageToMailbox(who.id, msg, UnfollowedReason(this.id))
        }
      }
    }
    true
  }

  def following_?(who: User): Boolean =
  Relationship.find(By(Relationship.owner, this),
                    By(Relationship.target, who)).isDefined

  def following(): List[User] =
  User.findAll(In.fk(Relationship.target, By(Relationship.owner, this)))

  def followers(): List[User] =
  User.findAll(In.fk(Relationship.owner, By(Relationship.target, this)))

  def wholeName: String = (firstName.is, lastName.is) match {
    case (f, l) if f.length > 1 && l.length > 1 => f+" "+l
    case (f, _) if f.length > 1 => f
    case (_, l) if l.length > 1 => l
    case (_, _) => niceName
  }

  def needsChange_? : Boolean = this.nickname.is.startsWith("chang") &&
  this.firstName.startsWith("Unkn") && this.lastName.startsWith("Unkn")

  def image: Option[NodeSeq] = tryo(Text((new URL(imageUrl)).toString)).toOption

  def tracking: List[Tracking] =
  Tracking.findAll(By(Tracking.user, this),
                   By(Tracking.disabled, false),
                   By(Tracking.removed, false),
                   OrderBy(Tracking.id, Ascending))

  def performing: List[Action] =
  Action.findAll(By(Action.user, this),
                 By(Action.disabled, false),
                 By(Action.removed, false),
                 OrderBy(Action.id, Ascending))


  override def primaryKeyField = id

  // the primary key for the database
  object id extends MappedLongIndex(this)

  def userIdAsString: String = id.is.toString

  // First Name
  object firstName extends MappedString(this, 32) {
    override def displayName = fieldOwner.firstNameDisplayName
    override val fieldId = Some(Text("txtFirstName"))
  }

  def firstNameDisplayName = ??("First Name")

  // Last Name
  object lastName extends MappedString(this, 32) {
    override def displayName = fieldOwner.lastNameDisplayName
    override val fieldId = Some(Text("txtLastName"))
  }

  def lastNameDisplayName = ??("Last Name")

  // Email
  /*
  object email extends MappedEmail(this, 48) {
    override def dbIndexed_? = true
    override def validations = valUnique(S.??("unique.email.address")) _ :: super.validations
    override def displayName = fieldOwner.emailDisplayName
    override val fieldId = Some(Text("txtEmail"))
  }*/

  // def emailDisplayName = ??("Email")
  // Password

  object superUser extends MappedBoolean(this) {
    override def defaultValue = false
  }

  def shortName: String = (firstName.is, lastName.is) match {
    case (f, l) if f.length > 1 && l.length > 1 => f+" "+l
    case (f, _) if f.length > 1 => f
    case (_, l) if l.length > 1 => l
    case _ => nickname // email.is
  }

  // def niceNameWEmailLink = <a href={"mailto:"+email.is}>{niceName}</a>

  object uniqueId extends MappedUniqueId(this, 32) {
    override def dbIndexed_? = true
    override def writePermission_?  = true
  }

  object validated extends MappedBoolean[User](this) {
    override def defaultValue = false
    override val fieldId = Some(Text("txtValidated"))
  }

  object locale extends MappedLocale[User](this) {
    override def displayName = fieldOwner.localeDisplayName
    override val fieldId = Some(Text("txtLocale"))
  }

  object timezone extends MappedTimeZone[User](this) {
    override def displayName = fieldOwner.timezoneDisplayName
    override val fieldId = Some(Text("txtTimeZone"))
  }


  object nickname extends MappedPoliteString(this, 64) {
    override def dbIndexed_? = true

    def deDupUnderscore(in: String): String = in.indexOf("__") match {
      case -1 => in
      case pos => deDupUnderscore(in.substring(0, pos)+in.substring(pos + 1))
    }

    override def setFilter = notNull _ :: toLower _ :: trim _ ::
    deDupUnderscore _ :: super.setFilter

    private def validateNickname(str: String): List[FieldError] = {
      val others = getSingleton.findByNickname(str).
      // getSingleton.findAll(By(getSingleton.nickname, str)).
      filter(_.id.is != fieldOwner.id.is)
      others.map(u => FieldError(this, <xml:group>Duplicate nickname: {str}</xml:group>))
    }

    private def validText(str: String): List[FieldError] =
    if (ValidNickName(str)) Nil
    else List(FieldError(this,
                         <xml:group>Invalid nickname.  Must start with
          a letter and contain only letters,
          numbers or "_"</xml:group>))

    override def validations = validText _ :: validateNickname _ :: super.validations
  }

  def niceName: String = nickname

  def timezoneDisplayName = ??("Time Zone")

  def localeDisplayName = ??("Locale")
}
