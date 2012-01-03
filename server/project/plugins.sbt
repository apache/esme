resolvers  ++= Seq(
  "Web plugin repo" at "http://siasia.github.com/maven2",
  Resolver.url("Typesafe repository", new java.net.URL("http://typesafe.artifactoryonline.com/typesafe/ivy-releases/"))(Resolver.defaultIvyPatterns)
)                                                                     

libraryDependencies <+= sbtVersion(v => "com.github.siasia" %% "xsbt-web-plugin" % (v+"-0.2.10"))

resolvers += Classpaths.typesafeResolver

addSbtPlugin("com.typesafe.startscript" % "xsbt-start-script-plugin" % "0.5.0")