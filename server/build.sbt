import com.typesafe.startscript.StartScriptPlugin

seq(StartScriptPlugin.startScriptForClassesSettings: _*)

name := "Apache Enterprise Social Messaging Environment (ESME)"

version := "1.4"

organization := "Apache Software Foundation"

scalaVersion := "2.9.1"    

//scalacOptions ++= Seq("-unchecked", "-deprecation")  
scalacOptions ++= Seq("-deprecation") 

seq(webSettings :_*)

ivyXML :=
    <dependencies>
      <dependency org="com.twitter" name="ostrich" rev="4.7.3">
        <exclude org="org.scala-tools" module="vscaladoc"/>
      </dependency>
    </dependencies>

libraryDependencies ++= {
  val scalaVersion = "2.9.1"
  val liftVersion = "2.4"
  val compassVersion = "2.1.1"
  val luceneVersion = "2.4.0"
  val scalazVersion = "6.0.4"
  val eclipsejettyVersion = "7.3.1.v20110307"    
  val mortbayjettyVersion = "6.1.22"
  val slf4jVersion = "1.6.4" 
  val scalatestVersion = "1.6.9"   
  Seq(
    "net.liftweb" %% "lift-util" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-webkit" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-widgets" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-mapper" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-testkit" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-openid" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-actor" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-json" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-common" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-ldap" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-textile" % liftVersion % "compile->default",
    "org.scalaz" %% "scalaz-core" % scalazVersion % "compile->default",
    "javax.servlet" % "servlet-api" % "2.5" % "provided->default",
    "org.compass-project" % "compass" % compassVersion % "compile->default",
    "org.apache.lucene" % "lucene-core" % luceneVersion % "compile->default",
    "org.apache.lucene" % "lucene-snowball" % luceneVersion % "compile->default",
    "commons-httpclient" % "commons-httpclient" % "3.1" % "compile->default",
    "org.apache.derby" % "derby" % "10.5.3.0_1" % "compile->default",
    "org.mortbay.jetty" % "jetty" % mortbayjettyVersion % "container",    
    "org.eclipse.jetty" % "jetty-server" % eclipsejettyVersion % "compile->default",
    "org.eclipse.jetty" % "jetty-servlet" % eclipsejettyVersion % "compile->default",
    "junit" % "junit" % "3.8.1" % "test->default",
    "junit" % "junit" % "4.4" % "test->default",
    "log4j" % "log4j" % "1.2.16" % "compile->default",
    "org.slf4j" % "slf4j-api" % slf4jVersion % "compile->default",
    "org.slf4j" % "slf4j-log4j12" % slf4jVersion % "compile->default",
    "org.scala-tools.testing" %% "specs" % scalatestVersion % "test->default",
    "org.scala-lang" % "scala-compiler" % scalaVersion % "test->default",
    "org.mortbay.jetty" % "jetty" % mortbayjettyVersion % "test->default"
  )
}

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

resolvers += ScalaToolsSnapshots

resolvers += "Compass Repository" at "http://repo.compass-project.org"

resolvers += "Twitter Repository" at "http://maven.twttr.com"

resolvers += "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"   

resolvers += "Maven central" at "http://repo1.maven.org/maven2/"

// Execute tests in the current project serially.
// Tests from other projects may still run concurrently.
parallelExecution in Test := false 