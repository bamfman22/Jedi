package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi2Parsers extends Jedi1Parsers {
  
  // params parser
  // a parameter list is zero or more comma-separated identifiers bracketed by parentheses:
  // params ::= "(" ~ (identifier ~ ("," ~ identifier)*)? ~ ")"
  def params: Parser[Identifier] = "(" ~ (identifier ~ rep("," ~ identifier)*)? ~ ")" ^^{
    case "("~i~","~ more ~ ")" => Identifier(i::more)
  }
  
  // lambda parser
  // lambda ::= "lambda" ~ params ~ expression
  def lambda: Parser[Lambda] = "lambda" ~ params ~ expression ^^ {
    case "lambda" ~ x ~ exp => Lambda(x, exp)
  }
  // block parser
  // a block is one or more semi-colon separated expressions bracketed by curly braces:
  // block ::= "{" ~ expression ~ (";" ~ expression)* ~ "}"
  def block: Parser[Block] = "{" ~ expression ~ rep(";" ~ expression)* ~ "}" ^^{
    case e ~ Nil => Block(e)
    case e ~ More => Block(e::More)
  }
    
    
  
  
  // override of term parser
  override def term: Parser[Expression]  = lambda | funCall | block | literal | "("~>expression<~")"