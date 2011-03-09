import sbt._

class Project(info: ProjectInfo) extends DefaultProject(info) {
  
  val liftVersion = "2.2"

  override def libraryDependencies = Set(
    "net.liftweb" %% "lift-ldap" % liftVersion % "compile->default" withSources(),
    "net.liftweb" %% "lift-proto" % liftVersion % "compile->default" withSources(),
    "net.liftweb" %% "lift-webkit" % liftVersion % "compile->default" withSources(),
    "net.liftweb" %% "lift-util" % liftVersion % "compile->default" withSources(),
    "net.liftweb" %% "lift-common" % liftVersion % "compile->default" withSources(),
    "net.liftweb" %% "lift-actor" % liftVersion % "compile->default" withSources()
  ) ++ super.libraryDependencies
}

