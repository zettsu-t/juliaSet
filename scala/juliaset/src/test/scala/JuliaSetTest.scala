import org.scalatest.funsuite.AnyFunSuite
import com.github.zettsut.juliaset._

class TestPoint extends AnyFunSuite {
  test("+") {
    val left = Point(2, -3)
    val right = Point(-5, 7)
    val expected = Point(-3, 4)
    assert((left + right) == expected)
    assert((right + left) == expected)
  }

  test("-") {
    val left = Point(2, -3)
    val right = Point(-5, 7)
    assert((left - right) == Point(7, -10))
    assert((right - left) == Point(-7, 10))
  }

  test("*") {
    val left = Point(2, -3)
    val right = Point(-5, 7)
    val expected = Point(11, 29)
    assert((left * right) == expected)
    assert((right * left) == expected)
  }

  test("transform") {
    val from = Point(2, 4)
    val offset = Point(3, 5)
    assert(from.transform(offset) == Point(-9, 21))
  }

  test("lengthSquared") {
    assert(Point(1, 2).lengthSquared == 5)
    assert(Point(2, -3).lengthSquared == 13)
  }

  test("IsNearOrigin") {
    val limitSqrt = 1e-3f
    val limit = limitSqrt * limitSqrt
    val eps = 1e-7f
    val from = Point(0, 0)
    val to = Point(0, limitSqrt)
    assert(from.isNear(to, limit + eps))
    assert(to.isNear(from, limit + eps))
    assert(!from.isNear(to, limit - eps))
    assert(!to.isNear(from, limit - eps))
  }

  test("IsNear") {
    val limitSqrt = 5f
    val limit = limitSqrt * limitSqrt
    val eps = 1e-5f
    val from = Point(-2.25f, -0.5f)
    val to = Point(0.75f, 3.5f)
    assert(from.isNear(to, limit + eps))
    assert(to.isNear(from, limit + eps))
    assert(!from.isNear(to, limit - eps))
    assert(!to.isNear(from, limit - eps))
  }
}

class TestPointPair extends AnyFunSuite {
  test("all") {
    val from = Point(1f, 2f)
    val to = Point(4f, 6f)
    val pair = PointPair(from, to)
    assert(pair.length == 52f)
    assert(pair.diff == 25f)

    val pairReversed = PointPair(to, from)
    assert(pairReversed.length == 5f)
    assert(pairReversed.diff == 25f)
  }
}

class TestPointSeq extends AnyFunSuite {
  test("all") {
    val eps = 1e-7f
    val zero = Point(0f, 0f)
    val seq1 = PointSeq(Point(23.0f, 0.0f), zero)
    assert(seq1.count(100, eps) == 0)

    val seq2 = PointSeq(Point(0.9f, 0.0f), zero)
    assert(seq2.count(100, 1e-3f) == 6)

    val pointC = Point(0.375f, 0.375f)
    val seq3 = PointSeq(pointC, pointC)
    assert(seq3.count(100, 0.01f) == 15)

    val seq4 = PointSeq(pointC, pointC)
    assert(seq4.count(11, 0.01f) == 11)

    val seq5 = PointSeq(Point(1.0f + eps, 0.0f), zero)
    assert(seq5.count(100, eps) == 0)

    val pointA = Point(0.5f, 0.375f)
    val seq6 = PointSeq(zero, pointA)
    assert(seq6.count(100, eps) == 4)

    val pointB = Point(0.375f, 0.5f)
    val seq7 = PointSeq(zero, pointB)
    assert(seq7.count(100, eps) == 8)

    val seq8 = PointSeq(pointA, pointB)
    assert(seq8.count(100, eps) == 3)

    val seq9 = PointSeq(pointB, pointA)
    assert(seq9.count(100, eps) == 7)
  }
}

class TestCoordinateSet extends AnyFunSuite {
  test("CoordinateSet") {
    val actual = CoordinateSet(-3.0f, 1.0f, 5)()
    val expected = Vector(-3.0f, -2.0f, -1.0f, 0.0f, 1.0f)
    assert(actual == expected)
  }
}

class TestCountSet extends AnyFunSuite {
  test("CountSet") {
    case class Coord(preset: Vector[Float]) extends CoordinateSet(0.0f, 0.0f, 0) {
      override def apply() = preset
    }

    val xs = Coord(Vector(-0.25f, -0.125f, -0.0625f));
    val ys = Coord(Vector(0.375f, 0.5f, 0.625f, 0.75f));
    val actual = CountSet(xs, ys, Point(-0.625f, 0.75f), 6, 1e-6f)
    val expected = Array(
      Array(6, 5, 4),
      Array(6, 5, 4),
      Array(6, 6, 5),
      Array(6, 6, 6))

    (0 until 3).foreach { y =>
      (0 until 3).foreach { x =>
        assert(actual()(y)(x) == expected(y)(x))
      }
    }
  }
}

class TestCommandLineArgs extends AnyFunSuite {
  test("Default") {
    val args: Array[String] = Array()
    val actual = CommandLineArgs(args)
    val expected = ParamSet(0.375f, 0.375f, 100, 256, "scala_juliaset.csv", "scala_juliaset.png")
    assert(actual == expected)
  }

  test("Explicit") {
    val args: Array[String] = Array("--x_offset", "0.25", "--y_offset", "0.5",
      "--max_iter", "16", "--size", "31", "--csv", "_test.csv", "--image", "_test.png")
    val actual = CommandLineArgs(args)
    val expected = ParamSet(0.25f, 0.5f, 16, 31, "_test.csv", "_test.png")
    assert(actual == expected)
  }
}
