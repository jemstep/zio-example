import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

class ScalaCheckUnitSpec extends Specification with ScalaCheck {

  "Numbers" should {

    "double correctly" in prop { x: Int =>
      2 * x must_== x * 2
    }

    "multiply correctly" in prop { (x: Int, y: Int) =>
      x * y must_== y * x
    }

  }
}
