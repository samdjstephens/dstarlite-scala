package dstarlite

case class Key(a: Int, b: Int) extends Ordered[Key] {
  import scala.math.Ordered.orderingToOrdered
  def compare(that: Key): Int = (this.a, this.b) compare (that.a, that.b)
}
