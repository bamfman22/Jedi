package expression

import context._
import value._


case class Block(val expressions: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Value = {
    val length = expressions.length
    
 
    
    val tempEnv = new Environment(env)
    
    val assignment = for (x <- expressions) yield x.execute(tempEnv)
    
    return assignment(length -1) 
    
  }
}