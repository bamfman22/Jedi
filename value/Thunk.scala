package value

import expression._
import context._

class Thunk(body: Expression, defEnv: Environment) extends Closure(Nil, body, defEnv) {
  var cache: Value = null
  def apply() = {
    if (cache == null) cache = super.apply(Nil, null)
    cache
  }
}