package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi3Parsers extends Jedi2Parsers {
  
  // assignment ::= identifier ~ "=" ~ expression
  def assignment: Parser[Assignment] = identifier ~ "=" ~ expression ^^{
    case vbl ~ "=" ~ update => Assignment(vbl,update)
  }
  // iteration ::= "while" ~ "(" ~ expression ~ ")" ~ expression
  def iteration: Parser[Iteration] = "while" ~ "(" ~ expression ~ ")" ~ expression ^^{
    case "while" ~ "(" ~ condition ~ ")" ~ body => Iteration(condition,body)
  }
  // dereference ::= "[" ~ expression ~ "]"
  def dereference: Parser[Variable] = "[" ~ expression ~ "]" ^^{
    case "[" ~ exp ~"]" => Variable(exp.execute(env))s
  }
  //SWITCH ::= "switch" ~ EXPRESSION ~ "{" ~ EXPRESSION~ rep(";" ~ EXPRESSION) ~ "}"
  def switch: Parser[List[Expression]] = "switch" ~ expression ~ "{" ~ (expression ~ rep(";" ~> expression))~ "}" ^^ {
     case "switch"~ exp ~ "{" ~ more ~ "}" =>  exp::more
     
  }
  
  
  
  override def expression: Parser[Expression] = declaration | conditional | iteration | disjunction | failure("Invalid expression")
  override def term: Parser[Expression]  = DO | lambda | funCall | block | assignment | dereference | literal | "("~>expression<~")"
}