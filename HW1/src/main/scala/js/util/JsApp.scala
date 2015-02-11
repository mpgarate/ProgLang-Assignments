package js.util

import java.io.File
import java.io.FileNotFoundException

abstract class JsApp extends App {
  def processFile(file: File)
  
  case class Config(debug: Boolean = false, files: List[File] = Nil)
  
  val usage = """JakartaScript interpreter 1.0
    Usage: run [options] [<file>...]
      
      -d | --debug
            Print debug messages
      -h | --help
            prints this usage text
      <file>...
            JakartaScript files to be interpreted
    """
  
  val config = ((Some(Config()): Option[Config]) /: args) {
    case (Some(c), "-d") => Some(c.copy(debug = true))
    case (Some(c), "--debug") => Some(c.copy(debug = true))
    case (Some(c), "-h") => None
    case (Some(c), "--help") => None
    case (Some(c), f) => Some(c.copy(files = c.files :+ new File(f)))
    case (None, _) => None
  } getOrElse {
    println(usage)
    System.exit(1)
    Config()
  }
  
   
  val debug = config.debug
  
  for (f: File <- config.files) {
    try {
      processFile(f)    
    } catch {
      case ex: FileNotFoundException =>
        println("Error: cannot find module '" + f.getCanonicalPath + "'"); System.exit(1)
      case ex: java.lang.RuntimeException =>
        println(ex.getMessage()); System.exit(1)
    }
  }
}