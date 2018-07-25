package value

case class Variable( var content:  Value) extends Value {
  def dereference() = content
}