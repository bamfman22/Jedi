package expression
import context._
import expression._
import value._


case class Conditional(val condition: Expression,val consequent: Expression, val alternative: Expression ) extends SpecialForm {
  def execute(env: Environment) = {
    if(condition.execute(env) == null) Notification.UNSPECIFIED
    else if(condition.execute(env) == true) consequent.execute(env)
    else  alternative.execute(env)
  }
}