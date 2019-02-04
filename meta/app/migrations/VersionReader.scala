package migrations


trait VersionReader[A,B] {
  val defaultPattern: B
  def gt(a: A, b: A, pattern: B = defaultPattern): Boolean
  def lt(a: A, b: A, pattern: B = defaultPattern): Boolean
  def eq(a: A, b: A, pattern: B = defaultPattern): Boolean
  def versionNumber(in: A, pattern: B = defaultPattern): Option[A]
}


object DefaultVersionReader extends VersionReader[String, String] {
  
  val defaultPattern = """[a-z.]*V([0-9]+).*"""
  
  def versionNumber(s: String, pattern: String = defaultPattern): Option[String] = {
    Option(s).collect { case pattern.r(v) => v }
  }
      
  def gt(a: String, b: String, pattern: String = defaultPattern): Boolean = {
    val an = versionNumber(a, pattern) getOrElse invalidMigrationName(a)
    val bn = versionNumber(b, pattern) getOrElse invalidMigrationName(b)
    an > bn
  }
  
  def lt(a: String, b: String, pattern: String = defaultPattern): Boolean = {
    val an = versionNumber(a, pattern) getOrElse invalidMigrationName(a)
    val bn = versionNumber(b, pattern) getOrElse invalidMigrationName(b)
    an < bn
  }
  
  def eq(a: String, b: String, pattern: String = defaultPattern): Boolean = {
    val an = versionNumber(a, pattern) getOrElse invalidMigrationName(a)
    val bn = versionNumber(b, pattern) getOrElse invalidMigrationName(b)
    an == bn
  }
  
  protected def invalidMigrationName(n: String) = 
    throw new RuntimeException(s"No version number found in migration name '$n'")      
}
