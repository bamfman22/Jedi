package expression

import context._
import expression._
import value.Value

trait Literal extends Expression with Value{
  def execute(env: Environment) = this
}