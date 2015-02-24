package js.hw2

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.token._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.input._
import ast._

class Parser extends JavaTokenParsers {
  
  val reserved: Set[String] =
    Set("const")
  
  def stmt: Parser[Expr] =
    basicStmt ~ rep(basicStmt) ^^
    { case st~(sts: List[Expr]) => 
        (st :: sts) reduceRight[Expr] {
          case (Undefined, st2) => st2
          case (st1, Undefined) => st1
          case (ConstDecl(v, e1, _), st2) => ConstDecl(v, e1, st2)
          case (st1, st2) => BinOp(Seq, st1, st2)
        }
    }
  
  def basicStmt: Parser[Expr] =
    ";" ^^^ Undefined | 
    "{" ~> stmt <~ "}" |
    constDecl <~ ";" |
    expr <~ ";"
  
  def constDecl: Parser[ConstDecl] =
    positioned(
        ("const" ~> ident <~ "=") ~ expr ^^ 
        { case s~e => ConstDecl(s, e, Undefined)})
    
  def expr: Parser[Expr] = commaExpr
  
  def commaExpr: Parser[Expr] =
    condExpr ~ rep("," ~> condExpr) ^^
      { case e~es => 
          (e :: es) reduceRight[Expr] { 
            case (e1, e2) => BinOp(Seq, e1, e2).setPos(e1.pos) 
          }
      }
  
  def condExpr: Parser[Expr] =
    (orExpr <~ "?") ~ (orExpr <~ ":") ~ orExpr ^^
    { case e1~e2~e3 => If(e1, e2, e3).setPos(e1.pos) } |
    orExpr
  
  def orExpr: Parser[Expr] =
    andExpr ~ rep("||" ~> andExpr) ^^ 
      { case e1~es => 
        (e1 /: es) { case (e1, e2) => BinOp(Or, e1, e2).setPos(e1.pos) } 
      }

  def andExpr: Parser[Expr] =
    relExpr ~ rep("&&" ~> relExpr) ^^ 
      { case e1~es => 
        (e1 /: es) { case (e1, e2) => BinOp(And, e1, e2).setPos(e1.pos) } 
      }
  
  def relOp: Parser[Bop] =
    "===" ^^^ Eq |
    "!==" ^^^ Ne |
    "<"   ^^^ Lt |
    "<="  ^^^ Le |
    ">"   ^^^ Gt |
    ">="  ^^^ Ge

  def relExpr: Parser[Expr] =
    additiveExpr ~ relOp ~ additiveExpr ^^ 
      { case e1~op~e2 => BinOp(op, e1, e2).setPos(e1.pos) } |
    additiveExpr
      
  def additiveOp: Parser[Bop] = 
    "+" ^^^ Plus | 
    "-" ^^^ Minus
      
  def additiveExpr: Parser[Expr] =
    multitiveExpr ~ rep(additiveOp ~ multitiveExpr) ^^ 
      { case e1~opes => 
        (e1 /: opes) { case (e1, op~e2) => BinOp(op, e1, e2).setPos(e1.pos) } 
      }
       
  def multitiveOp: Parser[Bop] = 
    "*" ^^^ Times |
    "/" ^^^ Div
  
  def multitiveExpr: Parser[Expr] = 
    unaryExpr ~ rep(multitiveOp ~ unaryExpr) ^^ 
      { case e1~opes => 
        (e1 /: opes) { case (e1, op~e2) => BinOp(op, e1, e2).setPos(e1.pos) } 
      }
  
  def unaryOp: Parser[Uop] =
    "-" ^^^ UMinus
    "!" ^^^ Not
     
  def unaryExpr: Parser[Expr] =
    positioned(unaryOp ~ primaryExpr ^^ { case uop~e => UnOp(uop, e) }) |
    primaryExpr
  
  def primaryExpr: Parser[Expr] = 
    literal |
    printExpr |
    positioned(ident ^^ (s => Var(s))) |
    "(" ~> expr <~ ")"
    
  def printExpr: Parser[Print] =
    positioned("console.log(" ~> expr <~ ")" ^^ (e => Print(e)))
  
    
  override def ident: Parser[String] =
    super.ident ^? ({
      case id if !reserved(id) => id
    }, { id => s"$id is reserved." }) | 
    "$" ~> super.ident ^^ (s => "$" + s)
    
  def literal: Parser[Expr] =
    positioned(floatingPointNumber ^^ (d => Num(d.toDouble))) |
    positioned("true" ^^^ Bool(true)) |
    positioned("false" ^^^ Bool(false)) |
    positioned("undefined" ^^^ Undefined) |
    positioned(stringLiteral ^^ (s => Str(s.substring(1, s.length() - 1))))
    
  override def stringLiteral: Parser[String] =
    ("\""+"""([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\[0-7]{3}|\\u[a-fA-F0-9]{2}|\\u[a-fA-F0-9]{4})*"""+"\"").r |
    ("\'"+"""([^'\p{Cntrl}\\]|\\[\\'"bfnrt]|\\[0-7]{3}|\\u[a-fA-F0-9]{2}|\\u[a-fA-F0-9]{4})*"""+"\'").r
    
  
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
  
  def apply(s: String) = getExpr(None, parseAll(stmt, s))
  
  def apply(file: java.io.File) = {
    val reader = new java.io.FileReader(file)
    val result = parseAll(stmt, StreamReader(reader))
    getExpr(Some(file.getName()), result)
  }
}