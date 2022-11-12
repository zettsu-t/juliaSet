package com.github.zettsut.juliaset
import java.io.File
import scala.collection.mutable.ArrayBuffer
import com.github.tototoshi.csv._

/** A point as a complex number in screens
  *
  *  @constructor Create a new point
  *  @param x the X coordinate
  *  @param y the Y coordinate
  */
case class Point(x: Float, y: Float) {

  /** Add a point
    *
    *  @param that another point
    *  @return a new added Point
    */
  def +(that: Point) = Point(this.x + that.x, this.y + that.y)

  /** Subtract a point
    *
    *  @param that another point
    *  @return a new subtracted Point
    */
  def -(that: Point) = Point(this.x - that.x, this.y - that.y)

  /** Multiply a point
    *
    *  @param that another point
    *  @return a new multiplied Point
    */
  def *(that: Point) = Point(this.x * that.x - this.y * that.y, this.x * that.y + this.y * that.x)

  /** Transform a point under the rule of Julia sets
    *
    *  @param offset an offset to be added
    *  @return a new transformed Point
    */
  def transform(offset: Point) = this * this + offset

  /** The squared length of this point from the origin
    *
    *  @return the squared length of this point
    */
  def lengthSquared = x * x + y * y

  /** Check if the squared distance of points is shorter than a limit
    *
    *  @param that another point
    *  @param limit the upper bound of the squared distance
    *  @return true if the squared distance of points is less than the limit, false otherwise
    */
  def isNear(that: Point, limit: Float) = (this - that).lengthSquared < limit
}

/** A point pair in a point sequence
  *
  *  @constructor Create a new point pair
  *  @param from a point
  *  @param to a transformed point
  */
case class PointPair(from: Point, to: Point) {

  /** The squared length between the from point and the origin
    */
  val length = to.lengthSquared

  /** The squared length between the from and to point
    */
  val diff = (to - from).lengthSquared
}

/** A point sequence to converge
  *
  *  @constructor Create a new point sequence
  *  @param init the initial point
  *  @param offset the offset to transform
  */
class PointSeq(init: Point, offset: Point) {

  /** Converge a point sequence
    *
    *  @param from a point
    *  @return the input point and the transformed point
    */
  private[this] def converge(from: Point): LazyList[PointPair] = {
    val to = from.transform(offset)
    LazyList.cons(PointPair(from, to), converge(to))
  }

  /** Count how many times a point is transformed until converged
    *
    *  @param maxIter the maximum number of iterations
    *  @param eps tolerance to check if transformations are converged
    *  @return how many times a point is transformed
    */
  def count(maxIter: Int, eps: Float): Int = {
    val pair =
      converge(init).zipWithIndex
        .takeWhile(x => (x._1.length <= 4.0f) && (x._1.diff > eps) && (x._2 < maxIter))
        .lastOption
    pair match {
      case Some(v) => v._2 + 1
      case _       => 0
    }
  }
}

object PointSeq {
  def apply(init: Point, offset: Point) = new PointSeq(init, offset)
}

/** A set of coordinates
  *
  *  @constructor Create a set of coordinates
  *  @param lower The lower bound of coordinates
  *  @param upper The upper bound of coordinates
  *  @param nPixels Numbers of pixels in x and y axes
  */
class CoordinateSet(lower: Float, upper: Float, nPixels: Int) {
  private val values: Vector[Float] = {
    nPixels match {
      case 1 => {
        Vector(0.0f)
      }
      case n if n < 1 => {
        Vector()
      }
      case _ => {
        val buf    = new ArrayBuffer[Float]
        val length = upper - lower
        val nSpans = nPixels.asInstanceOf[Float] - 1
        (0 until nPixels).foreach { i =>
          buf += lower + (i.asInstanceOf[Float] / nSpans) * length
        }
        buf(0) = lower;
        buf(nPixels - 1) = upper;
        buf.toVector
      }
    }
  }

  def apply() = values
}

object CoordinateSet {
  def apply(lower: Float, upper: Float, nPixels: Int) = new CoordinateSet(lower, upper, nPixels)
}

/** Julia set counts in a screen
  *
  *  @constructor Create a Julia set counts
  *  @param xs A x-coordinates view of points in a screen
  *  @param ys A y-coordinates view of points in a screen
  *  @param pointOffset An offset to be added to points
  *  @param maxIter The maximum number of iterations
  *  @param eps Tolerance to check if transformations are converged
  */
class CountSet(xs: CoordinateSet, ys: CoordinateSet, pointOffset: Point, maxIter: Int, eps: Float) {
  private val counts = {
    val matCounts = Array.ofDim[Int](ys().size, xs().size)
    ys().zipWithIndex.foreach {
      case (pointY, yIndex) =>
        xs().zipWithIndex.foreach {
          case (pointX, xIndex) =>
            val point    = Point(pointX, pointY)
            val pointSeq = PointSeq(point, pointOffset)
            val count    = pointSeq.count(maxIter, eps)
            matCounts(yIndex)(xIndex) = count
        }
    }
    matCounts
  }

  def apply() = counts

  /** Write Julia set counts to a CSV file
    *
    *  @param file a file to write
    */
  def writeCsv(file: File) {
    var writer: Option[CSVWriter] = None
    try {
      writer = Some(CSVWriter.open(file))
      val seq: Seq[Seq[Int]] = counts.map(_.toSeq)
      writer.get.writeAll(seq)
    } finally {
      writer match {
        case Some(wr) => wr.close()
        case _        => ()
      }
    }
  }
}

object CountSet {
  def apply(xs: CoordinateSet, ys: CoordinateSet, pointOffset: Point, maxIter: Int, eps: Float) =
    new CountSet(xs, ys, pointOffset, maxIter, eps)
}

/** Write a Julia set to a CSV file
  */
object Main extends App {
  val xs          = CoordinateSet(-1.5f, 1.5f, 512)
  val ys          = CoordinateSet(-1.5f, 1.5f, 512)
  val pointOffset = Point(0.382f, 0.382f)
  val countSet    = CountSet(xs, ys, pointOffset, 75, 1e-5f)
  countSet.writeCsv(new File("out.csv"))
}
