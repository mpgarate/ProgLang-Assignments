package js.hw4

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.token._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.input._
import ast._

object parse extends JavaTokenParsers {
  
  val reserved: Set[String] =
    Set("undefined", "const", "function", "return", 
        "bool", "string", "number", "Undefined")
  
  def stmt: Parser[Expr] =
    rep(basicStmt) ~ opt(lastBasicStmt) ^^
    { case (sts: List[Expr])~lst => 
        (sts :+ (lst getOrElse Undefined)) reduceRight[Expr] {
          case (Undefined, st2) => st2
          case (st1, Undefined) => st1
          case (f @ Function(Some(x), _, _, _), st2) =>
            ConstDecl(x, f, st2)
          case (ConstDecl(v, e1, _), st2) => ConstDecl(v, e1, st2)
          case (st1, st2) => BinOp(Seq, st1, st2)
        }
    }
   
  def stmtSep: Parser[String] = ";"
 
  def basicStmt: Parser[Expr] =
    stmtSep ^^^ Undefined | 
    "{" ~> stmt <~ "}" |
    constDecl <~ stmtSep |
    expr <~ stmtSep
  
  def lastBasicStmt: Parser[Expr] =
    stmtSep ^^^ Undefined |
    "{" ~> stmt <~ "}" |
    constDecl |
    expr
    
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
    eqExpr ~ rep("&&" ~> eqExpr) ^^ 
      { case e1~es => 
        (e1 /: es) { case (e1, e2) => BinOp(And, e1, e2).setPos(e1.pos) } 
      }
  
  def eqOp: Parser[Bop] =
    "===" ^^^ Eq |
    "!==" ^^^ Ne
    
  def eqExpr: Parser[Expr] =
    relExpr ~ rep(eqOp ~ relExpr) ^^ 
      { case e1~opes => 
          (e1 /: opes) { case (e1, op~e2) => BinOp(op, e1, e2).setPos(e1.pos) } 
      }
  
  def relOp: Parser[Bop] =
    "<="  ^^^ Le |
    "<"   ^^^ Lt |
    ">="  ^^^ Ge |
    ">"   ^^^ Gt
    
  def relExpr: Parser[Expr] =
    additiveExpr ~ rep(relOp ~ additiveExpr) ^^ 
      { case e1~opes => 
          (e1 /: opes) { case (e1, op~e2) => BinOp(op, e1, e2).setPos(e1.pos) } 
      } 
  
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
    callExpr
    
  def callExpr: Parser[Expr] =
    positioned("console.log(" ~> condExpr <~ ")" ^^ { Print(_) }) |  
    positioned(functionExpr ~ rep(callArgs) ^^ 
        { case e1~args => 
          (e1 /: args) { case (e1, e2) => Call(e1, e2).setPos(e1.pos) } })
    
  def callArgs: Parser[List[Expr]] =
    "(" ~> rep(condExpr <~ ",") ~ opt(condExpr) <~ ")" ^^
      { case es~eopt => es ++ eopt }
          
  def functionExpr: Parser[Expr] =
    positioned("function" ~> opt(ident) ~ functionArgs ~ opt(typAnn) ~ functionBody ^^
        { case f~params~tann~e => Function(f, params, tann, e) }) |
    memberExpr
         
  def functionArgs: Parser[List[(String, Typ)]] =
    "(" ~> typedIdentList(",") <~ ")"
         
  def functionBody: Parser[Expr] =
    positioned ("{" ~> opt(stmt) ~ ("return" ~> expr <~ opt(stmtSep)) <~ "}" ^^
        { case Some(s)~e => mkSeq(s, e)
          case None~e => e
        }
    )
    
  def memberExpr: Parser[Expr] =
    positioned (primaryExpr ~ ("." ~> ident) ^^ 
        { case e~f => GetField(e, f) } 
    ) | 
    primaryExpr
      
  def fieldDecl: Parser[(String, Expr)] =
    ident ~ (":" ~> condExpr) ^^ { case f~e => (f, e)}
    
  def primaryExpr: Parser[Expr] = 
    literal |
    positioned(ident ^^ { Var(_) }) |
    "(" ~> expr <~ ")"  
    
  override def ident: Parser[String] =
    super.ident ^? ({
      case id if !reserved(id) => id
    }, { id => s"$id is reserved." }) | 
    "$" ~> super.ident ^^ (s => "$" + s)
  
  def literal: Parser[Expr] =
    positioned("true" ^^^ Bool(true)) |
    positioned("false" ^^^ Bool(false)) |
    positioned("undefined" ^^^ Undefined) |
    positioned(floatingPointNumber ^^ { d => Num(d.toDouble) }) |
    positioned(stringLiteral ^^ (s => Str(s.substring(1, s.length() - 1)))) |
    objectLiteral
    
  override def stringLiteral: Parser[String] =
    ("\""+"""([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\[0-7]{3}|\\u[a-fA-F0-9]{2}|\\u[a-fA-F0-9]{4})*"""+"\"").r |
    ("\'"+"""([^'\p{Cntrl}\\]|\\[\\'"bfnrt]|\\[0-7]{3}|\\u[a-fA-F0-9]{2}|\\u[a-fA-F0-9]{4})*"""+"\'").r
    
  def objectLiteral: Parser[Expr] =
    positioned ("{" ~> rep(fieldDecl <~ ",") ~ opt(fieldDecl) <~ "}" ^^
        { case fs~fopt => Obj(Map(fs: _*) ++ fopt) }
    )
  
  /** Type expressions */    
  def typ: Parser[Typ] =
    objectTyp |
    functionTyp |
    baseTyp
    
  def baseTyp =
    "bool" ^^^ TBool |
    "string" ^^^ TString |
    "number" ^^^ TNumber |
    "Undefined" ^^^ TUndefined
    
  def objectTyp: Parser[TObj] =
    "{" ~> typedIdentList(";") <~ "}" ^^ 
      { case tfs => 
        val m = (Map[String, Typ]() /: tfs) { case (m, (f, typ)) => m + (f -> typ)}
        TObj(m) 
      }
    
  def functionTyp: Parser[TFunction] =
    ("(" ~> typedIdentList(",") <~ ")") ~ ("=>" ~> typ) ^^
      { case txs~typ => TFunction(txs, typ) }
    
    def typAnn: Parser[Typ] =
    ":" ~> typ
    
  def typedIdent: Parser[(String, Typ)] =
    ident ~ typAnn ^^ { case x~typ => (x, typ) }
    
  def typedIdentList(sep: String): Parser[List[(String, Typ)]] =
    rep(typedIdent <~ sep) ~ opt(typedIdent) ^^
    { case txs~txopt => txs ++ txopt }


  /** utility functions */
  private def getExpr(p: ParseResult[Expr]): Expr = 
    p match {
      case Success(e, _) => e
      case NoSuccess(msg, next) => 
        throw new js.util.JsException(msg, next.pos)
    }
  
  def fromString(s: String) = getExpr(parseAll(stmt, s))
  
  def fromFile(file: java.io.File) = {
    val reader = new java.io.FileReader(file)
    val result = parseAll(stmt, StreamReader(reader))
    getExpr(result)
  }
}