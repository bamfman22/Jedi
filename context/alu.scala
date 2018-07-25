package context


  
import expression._
import value._



/*
 * Notes:
 * alu implements all low-level arithmetic, logic, and I/O functions
 * alu does lots of type checking
 * alu is a singleton
 */
object alu {
  // dispatcher
  def execute(opcode: Identifier, args: List[Value]): Value = {
    opcode.name match {
      case "add" => add(args)
      case "mul" => mul(args)
      case "sub" => sub(args)
      case "div" => div(args)
      case "less" => less(args) //binary
      case "more" => more(args) // binary
      case "equals" => equals(args) // note: equals(7, true) = false, not error
      case "unequals" => unequals(args) // binary, = not(equals(args))?
      case "not" => not(args) // unary
       case "dereference" => dereference(args)
      case "var" => makeVar(args)
      // primitive I/O ops:
      case "write" => write(args)
      case "prompt" => prompt(args)
      case "read" => read(args)
      // store ops
      case "store" => store(args)
      case "put" => put(args)
      case "rem" => rem(args)
      case "contains" => contains(args)
      case "map" => map(args)
      case "filter" => filter(args)
      case "get" => get(args)
      case "addLast" => addLast(args)
      case "size" => size(args)
      case "swap" => swap(args)
      case _ => throw new UndefinedException(opcode)
    }
  }
  
  private def swap(x: Variable, y: Variable) = {
    if(args.size != 2)
    {
      throw new TypeException("Arguments must have two swap values")
    }
    else if(!args(0).isInstanceOf[Variable] && !args(1).isInstanceOf[Variable])
    {
      throw new TypeException("Both Arguments must be variables")
    }
    else
    {
     val c1 = args(0).asInstanceOf[Variable].dereference
     val c2 = args(1).asInstanceOf[Variable].dereference
     
     args(0).asInstanceOf[Variable].content = c2
     args(1).asInstanceOf[Variable].content = c1
     
     Notification.DONE
        
    }
  }
  
  private def iter(args: List[e
  // variable ops
   
   // returns the content of args(0)
   private def dereference(args: List[Value]) = {args(0).asInstanceOf[Variable]}
   
   // creates a new variable cobtaining args(0)
   private def makeVar(args: List[Value]) = {new Variable(args(0))}
   
   // store ops
   
   // returns a new store containing args
   private def store(args: List[Value]) = {new Store(args)}
   
   // put(v: Value, p: Integer, s: Store) calls s.put(v, p)
   private def put(args: List[Value]) = {
     if (args.size != 3)
        throw new TypeException("expected signature: put(v: Value, p: Integer, s: Store)")
     if(!args(1).isInstanceOf[Integer] || !args(2).isInstanceOf[Store]) 
        throw new TypeException("expected signature: put(v: Value, p: Integer, s: Store)")
     args(2).asInstanceOf[Store].put(args(0), args(1).asInstanceOf[Integer])
     Notification.DONE
   } 
   
   // rem(p: Integer, s: Store) calls s.rem(p)
   private def rem(args: List[Value]) = {
     if (args.size != 2)
         throw new TypeException("expected signature: rem(p: Integer, s: Store)")
     if(!args(1).isInstanceOf[Integer] || !args(2).isInstanceOf[Store]) 
         throw new TypeException("expected signature: rem(p: Integer, s: Store)")
     args(1).asInstanceOf[Store].rem(args(0).asInstanceOf[Integer])
     Notification.DONE

   }
   
   // get(p: Integer, s: Store) calls s.get(p)
   private def get(args: List[Value]) = {
      if (args.size != 2)
         throw new TypeException("expected signature: get(p: Integer, s: Store)")
     if(!args(1).isInstanceOf[Integer] || !args(2).isInstanceOf[Store]) 
         throw new TypeException("expected signature: get(p: Integer, s: Store)")
     args(1).asInstanceOf[Store].get(args(0).asInstanceOf[Integer])
     Notification.DONE
   }
   
   // map(f: Closure, s: Store) calls s.map(f)
   private def map(args: List[Value]) = {
      if (args.size != 2)
         throw new TypeException("expected signature: map(f: Closure, s: Store)")
     if(!args(1).isInstanceOf[Closure] || !args(2).isInstanceOf[Store]) 
         throw new TypeException("expected signature: map(f: Closure, s: Store)")
     args(1).asInstanceOf[Store].map(args(0).asInstanceOf[Closure])
     Notification.DONE
   } 
   
   // filter(f: Closure, s: Store) calls s.filter(f)
   private def filter(args: List[Value]) = {
      if (args.size != 2)
         throw new TypeException("expected signature: filter(f: Closure, s: Store)")
     if(!args(1).isInstanceOf[Closure] || !args(2).isInstanceOf[Store]) 
         throw new TypeException("expected signature: filter(f: Closure, s: Store)")
     args(1).asInstanceOf[Store].filter(args(0).asInstanceOf[Closure])
     Notification.DONE
   } 
   
   // contains(v: Value, s: Store) calls s.contains(v)
   private def contains(args: List[Value]) = {
      if (args.size != 2)
         throw new TypeException("expected signature: contains(v: Value, s: Store)")
     if(!args(1).isInstanceOf[Value] || !args(2).isInstanceOf[Store]) 
         throw new TypeException("expected signature: contains(V: Value, s: Store)")
     args(1).asInstanceOf[Store].contains(args(0))
     Notification.DONE
   }
   
   // addLast(v: Value, s: Store) calls s.add(v)
   private def addLast(args: List[Value]) = {
       if (args.size != 2)
         throw new TypeException("expected signature: add(v: Value, s: Store)")
     if(!args(1).isInstanceOf[Value] || !args(2).isInstanceOf[Store]) 
         throw new TypeException("expected signature: add(V: Value, s: Store)")
     args(1).asInstanceOf[Store].add(args(0))
     Notification.DONE
   }
   
   // size(s: Store) calls s.size
   private def size(args: List[Value]) = {
     if(args.size >1)
       throw new TypeException("expected signature: size(s: Store)")
     if(!args(0).isInstanceOf[Store])
       throw new TypeException("expected signature: size(s: Store)")
     args(0).asInstanceOf[Store].size
     Notification.DONE
   }
   
  // etc.
  
  private def castAsReal(value: Value, opcode: String): Real = {
    value match {
      case n: Integer => Integer.intToReal(n)
      case n: Real => n
      case _ => throw new TypeException(opcode + " inputs must be numbers")
    }
  }
  
  private def castAsText(value: Value, opcode: String): Chars = {
    value match {
      case n: Chars => n
      case _ => throw new TypeException(opcode + " inputs must be texts")
    }
  }
  
  private def castAsIntegers(vals: List[Value], opcode: String): List[Integer] = {
    if (vals.isEmpty) throw new TypeException(opcode + " expected > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Integer])
    if (ok.length < vals.length) throw new TypeException(opcode + " inputs must be numbers")
    vals.map(_.asInstanceOf[Integer])
  }  
  
  private def castAsReals(vals: List[Value], opcode: String): List[Real] = {
    if (vals.isEmpty) throw new TypeException(opcode + " expected > 0 inputs")
    vals.map(castAsReal(_, opcode))
  }
  
  private def castAsTexts(vals: List[Value], opcode: String): List[Chars] = {
    if (vals.isEmpty) throw new TypeException(opcode + " expected > 0 inputs")
    vals.map(castAsText(_, opcode))
  }
  private def mul(vals: List[Value]): Value = {
    try{
        castAsIntegers(vals, "mul").reduce(_*_)
    }
    catch{
      case e: TypeException => castAsReals(vals,"mul").reduce(_*_)       
    }
  }
    
  private def sub(vals: List[Value]): Value = {
    try{
      castAsIntegers(vals,"sub").reduce(_-_)
    }catch{
      case e : TypeException => castAsReals(vals, "sub").reduce(_-_)
    }
  }
  
  private def div(vals: List[Value]): Value = {
    try{
      castAsIntegers(vals, "div").reduce(_/_)
    }catch{
      case e: TypeException => castAsReals(vals,"div").reduce(_/_)
    }
  }
  
  
  private def add(vals: List[Value]): Value = {
    try {
      castAsIntegers(vals, "add").reduce(_+_) 
    } catch {
       case e: TypeException => 
         try {
           castAsReals(vals, "add").reduce(_+_) 
        } catch {
          case e: TypeException => castAsTexts(vals, "concat").reduce(_+_)
      }
    }
  }
  def not(vals: List[Value]): Value = {
    if(vals.length != 1) throw new JediException("too many arguments")
    else{
      try{
        val int = castAsIntegers(vals,"not")
        Integer(int(0).unary_-)
      }
      catch{
        case e: TypeException =>{
          
            val real = castAsReals(vals,"not")
            Real(real(0).unary_-)
          
          
          
        }
      }
    }
  }
  def more(vals: List[Value]): Value = {
    if(vals.length != 2) throw new TypeException("more expects two inputs")
    try{
      val nums = castAsIntegers(vals, "more")
      Boole(nums(0) > nums(1))
    }catch{
      case e: TypeException => {
        try {
          val nums = castAsReals(vals, "more")
          Boole(nums(0) > nums(1))
        }catch{
          case e: TypeException => {
            val texts = castAsTexts(vals, "more")
            Boole(texts(0) > texts(1))
          }
        }
      }
    }
  }
  
  
  def less(vals: List[Value]): Value = {
    if (vals.length  != 2) throw new TypeException("less expects two inputs")
    try {
      val nums = castAsIntegers(vals, "less")
      Boole(nums(0) < nums(1))
    } catch {
      case e: TypeException => {
        try {
          val nums = castAsReals(vals, "less")
          Boole(nums(0) < nums(1))
        } catch {
          case e: TypeException => {
            val texts = castAsTexts(vals, "less")
            Boole(texts(0) < texts(1))
          }
        }
      }
    }
  }
  def unequals(vals: List[Value]): Value = {
    if(vals.length !=2) throw new TypeException("equals expects two inputs")
    try{
      val nums = castAsIntegers(vals, "unequals")
      Boole(nums(0) != nums(1))
    }catch{
      case e: TypeException => {
        try{
          val nums = castAsReals(vals, "unequals")
          Boole(nums(0) != nums(1))
        }catch {
          case e: TypeException => {
            try{
              val texts = castAsTexts(vals,"unequals")
              Boole(texts(0) != texts(1))
            }catch{
              case e: TypeException =>{
                Boole(true)
              }
            }
          }
        }
      }
    }
  }
  
  def equals(vals: List[Value]): Value = {
    if(vals.length !=2) throw new TypeException("equals expects two inputs")
    try{
      val nums = castAsIntegers(vals, "equals")
      Boole(nums(0) == nums(1))
    }catch{
      case e: TypeException => {
        try{
          val nums = castAsReals(vals, "equals")
          Boole(nums(0) == nums(1))
        }catch {
          case e: TypeException => {
            try{
              val texts = castAsTexts(vals,"equals")
              Boole(texts(0) == texts(1))
            }catch{
              case e: TypeException =>{
                Boole(false)
              }
            }
          }
        }
      }
    }
  }
 
   def write(vals: List[Value]): Value = { println(vals(0)); Notification.DONE }
   def read(vals: List[Value]): Value = { val result = io.StdIn.readDouble(); Real(result)}
   def prompt(vals: List[Value]): Value = { print("=> "); Notification.DONE }

  
  // etc.
}