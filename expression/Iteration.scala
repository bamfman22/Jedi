package expression
import context._
import value._


case class Iteration(val condition: Expression, val body: Expression) extends Expression {
    
  def execute(env: Environment) = {
    val c1 = condition.execute(env)
    if (!c1.isInstanceOf[Boole]) throw new TypeException("While condition must be Boole")
    var c2 = c1.asInstanceOf[Boole]
    while(c2.value) {
      body.execute(env)
      c2 = condition.execute(env).asInstanceOf[Boole]
    }
    Notification.DONE
  }
}