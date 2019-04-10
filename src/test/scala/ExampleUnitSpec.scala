import org.specs2.mutable.Specification

class ExampleUnitSpec extends Specification {

  "Lists" should {

    "sum correctly" in {
      ExampleCode.emptyListOfNumbers.sum must_== 0
    }

  }
}
