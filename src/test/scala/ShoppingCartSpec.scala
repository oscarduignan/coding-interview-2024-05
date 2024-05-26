import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

case class Price(value: BigDecimal)

case class ShoppingCart() {
  def subtotal: Price = Price(BigDecimal(0))
}

class ShoppingCartSpec extends AnyFreeSpec with Matchers {

  "ShoppingCart" - {
    "subtotal" - {
      "with no items, should be 0" in {
        ShoppingCart().subtotal mustBe Price(BigDecimal(0))
      }
    }
  }

}
