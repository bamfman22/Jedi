package expression
import context._
import value._

case class Lambda(private val parameters: List[Identifier],private val body: Expression) extends SpecialForm {
  def execute(env: Environment): Closure = {
    val clos = new Closure(parameters,body,env)
    
    return clos
  }
}