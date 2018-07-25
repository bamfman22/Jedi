package value
import context._
import expression._

// I forgot to do this in sec 1
class Closure(val params: List[Identifier], val body: Expression, val defEnv: Environment) extends Value {
   def apply(args: List[Value],callEnv: Environment): Value =  {
      val localEnv =
       if (Flags.useStaticScopeRule) new Environment(defEnv)
       else new Environment(callEnv)
     localEnv.bulkPut(params, args)
     body.execute(localEnv)
     
      
   }
}