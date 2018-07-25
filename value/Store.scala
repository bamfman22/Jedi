package value

import collection.mutable._
import context._

class Store(private var elems: ArrayBufffer[Value] = ArrayBuffer[Value]()) extends Value {
  // adds e to the end of store
  def add(e: Value) {elems += e}
  // inserts e at position pos in this
  def put(e: Value, pos: Integer) {elems.put(e, pos)}
  // removes element at position pos from this
  def rem(pos: Integer) {elems.rem(pos)}
  // returns element at position pos in this
  def get(pos: Integer): Value = elems.get(pos)
  // returns true ie this contains e
  def contains(e: Value): Boole = elems.contains(e)
  // returns the size of this
  def size: Integer = elems.size
  // returns "{e0 e1 e2 ...}"
  override def toString = {elems.toString()}
  // returns store containing the elements of this transformed by trans
  def map(trans: Closure): Store = {elems.map(trans)}
  // returns store containing the elements of this that passed test
  def filter(test: Closure): Store = {elems.filter(test)}
}