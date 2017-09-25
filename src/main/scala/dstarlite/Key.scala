package dstarlite

case class Key(a: Float, b: Float) extends Ordered[Key] {
  import scala.math.Ordered.orderingToOrdered
  def compare(that: Key): Int = (this.a, this.b) compare (that.a, that.b)
}
