package services.util

trait CommandExecutioner {
  def execute(command: Seq[String], env: Seq[(String,String)]): String
}

class DefaultCommandExecutioner extends CommandExecutioner {
  override def execute(command: Seq[String], env: Seq[(String, String)]): String = {
    scala.sys.process.Process(
      command = command,
      cwd = None,
      extraEnv = env:_*
    ).!!
  }
}
