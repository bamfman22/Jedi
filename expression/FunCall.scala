package expression
import value._
import context._



case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression {


  def execute(env: Environment): Value = {

    try {
      val maybeClosure = operator.execute(env)
      if (maybeClosure.isInstanceOf[Closure]) {
        val closure = maybeClosure.asInstanceOf[Closure]
        val args = Flags.paramaterPassing match {
          case Flags.passByValue => operands.map(_.execute(env))
          case Flags.passByText => operands.map(new Text(_))
          case Flags.passByName => operands.map(new Thunk(_, env))
        }
        closure(args, env)
      } else {
        throw new TypeException("Only functions can be called")
      }
    } catch {
      case e: UndefinedException =>
        val args = operands.map(_.execute(env))
        alu.execute(operator, args)
    }
  }
}