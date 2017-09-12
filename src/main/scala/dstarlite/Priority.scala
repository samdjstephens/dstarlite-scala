package dstarlite

case class Priority(a: Int, b: Int) extends Ordered[Priority] {
  import scala.math.Ordered.orderingToOrdered
  def compare(that: Priority): Int = (this.a, this.b) compare (that.a, that.b)
}
