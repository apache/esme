import sbt._

class EsmeProject(info: ProjectInfo) extends DefaultWebProject(info) {
  val liftVersion = "2.2-RC4"
  val compassVersion = "2.1.1"
  val luceneVersion = "2.4.0"

  val mavenLocal = "Local Maven Repository" at "file://"+Path.userHome+"/.m2/repository"

  val scalatoolsSnapshot = ScalaToolsSnapshots
  val compassRepo = "Compass Repository" at "http://repo.compass-project.org"
  val twitterRepo = "Twitter Repository" at "http://maven.twttr.com"
  //val apacheRepo = "Apache repository for Derby" at "http://people.apache.org/repo/m1-ibiblio-rsync-repository" // legacy

  def extraResources = "LICENSE" +++ "NOTICE"
  override def mainResources = super.mainResources +++ extraResources

  override def ivyXML =
    <dependencies>
      <dependency org="net.lag" name="configgy" rev="2.0.1">
        <exclude org="org.scala-tools" module="vscaladoc"/>
      </dependency>
      <dependency org="com.twitter" name="ostrich" rev="2.3.2">
        <exclude org="org.scala-tools" module="vscaladoc"/>
      </dependency>
    </dependencies>

  override def libraryDependencies = Set(
    "net.liftweb" %% "lift-util" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-webkit" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-widgets" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-mapper" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-testkit" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-openid" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-actor" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-json" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-common" % liftVersion % "compile->default",
    "org.compass-project" % "compass" % compassVersion % "compile->default",
    "org.apache.lucene" % "lucene-core" % luceneVersion % "compile->default",
    "org.apache.lucene" % "lucene-snowball" % luceneVersion % "compile->default",
    "commons-httpclient" % "commons-httpclient" % "3.1" % "compile->default",
    "org.apache.derby" % "derby" % "10.5.3.0_1" % "compile->default",
    "org.mortbay.jetty" % "jetty" % "[6.1.6,)" % "test->default",
    "junit" % "junit" % "3.8.1" % "test->default",
    "junit" % "junit" % "4.4" % "test->default",
    "log4j" % "log4j" % "1.2.16" % "compile->default",
    "org.slf4j" % "slf4j-api" % "1.6.1" % "compile->default",
    "org.slf4j" % "slf4j-log4j12" % "1.6.1" % "compile->default",
    "org.scala-tools.testing" %% "specs" % "1.6.6" % "test->default"
  ) ++ super.libraryDependencies
}
