package js.hw1

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import ast._

class Parser extends JavaTokenParsers {
  private def stringToBop: String => Bop = {
    case "+" => Plus
    case "-" => Minus
    case "*" => Times
    case "/" => Div
  }
    
  private def stringToUop: String => Uop = {
    case "-" => UMinus
  }
  
  def expr: Parser[Expr] = add
  
  def addOp: Parser[Bop] = ("+" | "-") ^^ stringToBop
  
  def add: Parser[Expr] =
    mult~rep(addOp~mult) ^^ 
      { case e1~opes => 
        (e1 /: opes) { case (e1, op~e2) => BinOp(op, e1, e2).setPos(e1.pos)} 
      }
       
  def multOp: Parser[Bop] = ("*" | "/") ^^ stringToBop
  
  def mult: Parser[Expr] = 
    primary~rep(multOp~primary) ^^ 
      { case e1~opes => 
        (e1 /: opes) { case (e1, op~e2) => BinOp(op, e1, e2).setPos(e1.pos)} 
      }
  
  def primary: Parser[Expr] = 
    positioned(floatingPointNumber ^^ (d => Num(d.toDouble))) |
    positioned("-("~> expr <~ ")" ^^ (e => UnOp(UMinus, e))) |
    "("~> expr <~")"
}

object parse extends Parser {
  private def getExpr(optFile: Option[String], p: ParseResult[Expr]): Expr = 
    p match {
      case Success(e, _) => e
      case NoSuccess(msg, next) => 
        val file = optFile getOrElse "[eval]" + ":"
        val err = file + next.pos.line + ":" + next.pos.column + "\n" +
          next.pos.longString + "\n\n" +
          "SyntaxError: " + msg
        throw new java.lang.RuntimeException(err)
    }
  
  def apply(s: String) = getExpr(None, parseAll(expr, s))
  
  def apply(file: java.io.File) = {
    val reader = new java.io.FileReader(file)
    val result = parseAll(expr, StreamReader(reader))
    getExpr(Some(file.getName()), result)
  }
}