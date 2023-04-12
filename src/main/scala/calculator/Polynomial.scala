package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    Signal {
      val bValue = b()
      (bValue * bValue) - (4 * a() * c())
    }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal {
      val bValue = b()
      val aValue = a()
      val deltaSqrt = Math.sqrt(computeDelta(a, b, c)())
      if (deltaSqrt >= 0) {
        val pos = (-1 * bValue + deltaSqrt) / (2 * aValue)
        val neg = (-1 * bValue - deltaSqrt) / (2 * aValue)
        Set(pos, neg)
      }
      else Set.empty
    }
