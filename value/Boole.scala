package value

import expression.Literal

case class Boole(val value: Boolean) extends Literal{
  def &&(other: Boole) = Boole(other.value && this.value)
  def ||(other: Boole) = Boole(other.value || this.value)
  def unary_! = Boole(!value)
  override def toString = value.toString
}