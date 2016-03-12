// https://en.wikipedia.org/wiki/Heapsort

import scala.collection.mutable
import scala.math.Ordering.Implicits._
import scala.reflect.ClassTag

def heapSort[A: Ordering: ClassTag](array: Array[A]): Array[A] = {
  // helpful functions for working with heap indexes
  def parentIndex(index: Int): Int = math.floor((index - 1) / 2).toInt
  def leftChildIndex(index: Int): Int = 2 * index + 1
  def rightChildIndex(index: Int): Int = 2 * index + 2

  // local global heap
  var heap = mutable.MutableList[A]()
  def leftChild(index: Int): A = heap(leftChildIndex(index))
  def rightChild(index: Int): A = heap(rightChildIndex(index))
  def minChildIndex(index: Int): Option[Int] =
    if ((leftChildIndex(index) < heap.length) && (leftChild(index) < heap(index))) Some(leftChildIndex(index))
    else if ((rightChildIndex(index) < heap.length) && (rightChild(index) < heap(index))) Some(rightChildIndex(index))
    else None

  // insert elements to heap
  def initializeHeap(array: Array[A]): Unit = {
    for (element <- array) {
      insertIntoHeap(element)
    }
  }

  def insertIntoHeap(element: A): Unit = {
    // add element to the end of the heap
    heap += element
    // define index of recently added element
    var index = heap.length - 1

    // try bubble recently added element to the top of the heap
    while (index > 0 && heap(index) < heap(parentIndex(index))) {
      val temporary = heap(parentIndex(index))
      heap(parentIndex(index)) = heap(index)
      heap(index) = temporary

      index = parentIndex(index)
    }
  }

  def heapify(index: Int): Unit = {
    var childIndex: Option[Int] = new Some(index)
    while (childIndex.nonEmpty) {
      val temporary = heap(childIndex.get)
      heap(childIndex.get) = heap(index)
      heap(index) = temporary

      childIndex = minChildIndex(childIndex.get)
    }
  }

  def extractMinFromHeap(): A = {
      val min = heap.head
      heap(0) = heap.last
      heap = heap.dropRight(1)

      if (heap.nonEmpty) {
        heapify(0)
      }

      min
  }

  initializeHeap(array)
  Array[A]((for (i <- array.indices) yield extractMinFromHeap()): _*)
}

val array = Array(5, 4, 3, 2, 1)
val sorted = heapSort(array)

println(
  sorted .sameElements(Array(1, 2, 3, 4, 5))
)
