package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(
      a: Signal[Double],
      b: Signal[Double],
      c: Signal[Double]
  ): Signal[Double] = {
    Signal {
      val (va, vb, vc) = (a(), b(), c())
      math.pow(vb, 2) - 4 * va * vc
    }
  }

  def computeSolutions(
      a: Signal[Double],
      b: Signal[Double],
      c: Signal[Double],
      delta: Signal[Double]
  ): Signal[Set[Double]] = {
    Signal {
      val (va, vb, vc, vdelta) = (a(), b(), c(), delta())
      if (vdelta < 0) Set.empty[Double]
      else
        Set(
          (-vb + math.sqrt(vdelta)) / va / 2,
          (-vb - math.sqrt(vdelta)) / va / 2
        )
    }
  }
}
