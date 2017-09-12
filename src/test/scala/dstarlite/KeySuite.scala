package dstarlite
import org.scalatest._

class KeySuite extends FlatSpec {
  "Priority(1,2)" must "be less than Priority(2,2)" in {
    assert(Key(1, 2) < Key(2, 2))
  }

  it must "be less than Priority(2,1)" in {
    assert(Key(1, 2) < Key(2, 1))
  }

  it must "be less than Priority(1,3)" in {
    assert(Key(1, 2) < Key(1, 3))
  }

  it must "be greater than Priority(1,1)" in {
    assert(Key(1, 2) > Key(1, 1))
  }

  it must "be greater than Priority(0,1)" in {
    assert(Key(1, 2) > Key(0, 1))
  }

  it must "be less than or equal to Priority(2,1)" in {
    assert(Key(1, 2) <= Key(2, 1))
  }

  it must "be greater than or equal to Priority(0,1)" in {
    assert(Key(1, 2) >= Key(0, 1))
  }

  it must "be equal to itself" in {
    assert(Key(1, 2) == Key(1, 2))
  }

  it must "be less than or equal to itself" in {
    assert(Key(1, 2) <= Key(1, 2))
  }

  it must "be greater than or equal to itself" in {
    assert(Key(1, 2) >= Key(1, 2))
  }
}
