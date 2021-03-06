package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      k <- arbitrary[Int]
      m <- oneOf(const(empty), genHeap)
    } yield insert(k, m)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  //Hint1: If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("minIsTheSmallerWhenTwo") = forAll { (a: Int, b: Int) =>
    val h = insert(a, empty)
    val h2 = insert(b, h)
    val min = if (a <= b) a else b
    findMin(h2) == min
  }

  //Hint 2: If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("ifInsertAndDeleteThenEmpy") = forAll { a: Int =>
    val h = insert(a, empty)
    val minRemoved = deleteMin(h)
    isEmpty(minRemoved) == true
  }

  // Hint 3: Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  property("sortedseqWhenDeleting") = forAll { (h: H) =>
    def findAndDelete(h: H, list: List[A]): List[A] = h match {
      case empty => Nil
      case _ => findAndDelete(deleteMin(h), findMin(h) :: list)
    }

    findAndDelete(h, Nil) == findAndDelete(h, Nil).sorted
  }


  // Hint 4: Finding a minimum of the melding of any two heaps should return a minimum of one or the other.

  property("minimumOfOneEqualsMeldingsMinimum") = forAll { (h1: H, h2: H) =>
    (!isEmpty(h1) && !isEmpty(h2)) ==> {
      val min = findMin(h1).min(findMin(h2))
      findMin(meld(h1, h2)) == min
    }
  }


  // if the heap has two elements and the min is deleted, then the element that rests is both the maximum and the minimum

  property("iftwoElementsAndMinDeletedThenMinIsMax") = forAll { (a: Int, b: Int) =>
  val max= if (a>b) a else b

    val heap=insert(a,empty)
    val heap1=insert(b,heap)
    val minRemoved=deleteMin(heap1)

    findMin(minRemoved)==max
  }

  def removeAll(heap: H): List[Int] =
    if (isEmpty(heap)) Nil
    else {
      val min = findMin(heap)
      val h = deleteMin(heap)
      min :: removeAll(h)
    }

  def insertAll(xs: List[Int], heap: H): H = xs match {
    case Nil => empty
    case y :: ys =>
      insert(y, insertAll(ys, heap))
  }

  //
  property("InsertAndRemoveAllGivesSortedList") = forAll { (xs: List[Int]) =>
    val h = insertAll(xs, empty)
    val ys = removeAll(h)
    xs.sorted == ys
  }

}