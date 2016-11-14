resolvers += Resolver.url(
  "bintray-sbt-plugin-releases",
    url("http://dl.bintray.com/content/sbt/sbt-plugin-releases"))(
        Resolver.ivyStylePatterns)

addSbtPlugin( "me.lessis" % "bintray-sbt" % "0.3.0" )

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.3")
