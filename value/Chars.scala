package value

import expression._

case class Chars(val value: String) extends Literal with Ordered[Chars]{
   def length: Integer = Integer(value.length)
   override def equals(other: Any): Boolean = 
    other match {
       case other: Chars => (other.isInstanceOf[Chars]) && (other.value == this.value)
       case _ => false
    }
  def substring(start: Integer, end: Integer) = Chars(value.substring(start.value.toInt,end.value.toInt))
  def +(other: Chars) = Chars(this.value + other.value)
  override def toString() = value
  override def hashCode = this.toString.##
  def compare(other: Chars): Int = this.value.compare(other.value)
  
}