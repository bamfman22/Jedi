package expression
import context._
import expression._
import value._

case class Declaration(private val exp: Expression,private val id: Identifier) extends SpecialForm{
  def execute(env: Environment) = {
    env.put(id, exp.execute(env))
    Notification.OK
  
  }
}