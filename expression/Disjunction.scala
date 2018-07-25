package expression
import context._
import expression._
import value._

case class Disjunction(val operands: List[Expression]) extends SpecialForm {
  def execute(env: Environment) = {
    for(x <- operands) x.execute(env)
  }
}