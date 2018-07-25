package expression
import value._
import context._


case class Switch(val index: Expression, val block: Block) extends SpecialForm{
  def execute(env: Environment) = {
    val i = index.execute(env)
  if(!i.isInstanceOf[Integer])
  {
    throw new TypeException("index must an integer valued expression")
  }
  else
  {
    if(i.asInstanceOf[Integer].value > block.expressions.length)
    {
      Notification.UNSPECIFIED
    }
    else{
    val exp = block.expressions(i.asInstanceOf[Integer].value)
    
    exp.execute(env)
    }
  }
  
  
  
  
  
  }
}