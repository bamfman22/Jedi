package expression

import context._
import value._


case class Identifier(val name: String) extends Expression {
   override def toString = name
   def execute(env: Environment): Value = { 
  
     val result = env(this)
     
     if(result.isInstanceOf[Text])
       (result.asInstanceOf[Text]).body.execute(env)
     else if(result.isInstanceOf[Thunk])
       result.asInstanceOf[Thunk].apply()
     else 
       result
  }
     
}