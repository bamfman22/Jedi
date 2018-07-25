package value

class Notification extends Value{
  
  
}

object Notification{
  def apply() = new Notification
  var UNSPECIFIED: Value = Chars("Unspecified")
  var DONE: Value = Chars("done")
  var OK: Value = Chars("Ok")
}