package expression
import context._
import expression._
import value._

case class Conjunction(val exps:List[Expression]) extends Expression {

  def execute(env: Environment) = {
    var more = true
    var result = Boole(true)
    for(exp <- exps if more) {
      val arg = exp.execute(env)
      if (!arg.isInstanceOf[value.Boole])
        throw new TypeException("Conjunction inputs must be Booles")
      val b = arg.asInstanceOf[value.Boole]
      if (!b.value) {
        result = Boole(false)
        more = false
      }
    }
    result
  }

}