package expression

import context._
import value._

case class Assignment(private val vbl: Identifier, private val update: Expression) extends SpecialForm{
  def execute(env : Environment) = {
      val v1 = vbl.execute(env)
    if (!v1.isInstanceOf[Variable]) throw new TypeException("Only variables can be assigned new value")
    val v2 = v1.asInstanceOf[Variable]
    v2.dereference = update.execute(env)
    Notification.DONE
  }
}