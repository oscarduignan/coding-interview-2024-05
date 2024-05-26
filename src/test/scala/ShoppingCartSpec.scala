import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

case class Price(value: BigDecimal)
object Price {
  def apply(value: String): Price = Price(BigDecimal(value))
}

case class Item(name: String)

case class PricedItem(item: Item, price: Price)

case class Quantity(value: BigDecimal)

case class ShoppingCart(items: Map[PricedItem, Quantity]) {
  require(items.values.forall(_.value > 0), "negative quantities aren't allowed")
  def subtotal: Price = Price(items.map {
    case (item, quantity) => item.price.value * quantity.value
  }.sum)
}
object ShoppingCart {
  def apply(): ShoppingCart = ShoppingCart(Map.empty[PricedItem, Quantity])
  def apply(items: (PricedItem, Quantity)*): ShoppingCart = ShoppingCart(Map(items: _*))
}

class ShoppingCartSpec extends AnyFreeSpec with Matchers {

  "ShoppingCart" - {
    "not allow negative quantities" - {
      "should throw if instantiated with a negative quantity" in {
        an[IllegalArgumentException] must be thrownBy {
          ShoppingCart(
            PricedItem(Item("cornflakes"), Price("5.00")) -> Quantity(-1)
          )
        }
      }
    }
    "subtotal" - {
      "with no items, should be 0" in {
        ShoppingCart().subtotal mustBe Price(0)
      }
      "with one item, should be equal to it's price multiplied by it's quantity" in {
        ShoppingCart(
          PricedItem(Item("cornflakes"), Price("5.00")) -> Quantity(5)
        ).subtotal mustBe Price("25.00")
      }
      "with multiple items, should be equal to their subtotals summed together" in {
        ShoppingCart(
          PricedItem(Item("cornflakes"), Price("5.00")) -> Quantity(5),
          PricedItem(Item("frosties"), Price("2.50")) -> Quantity(1),
          PricedItem(Item("weetabix"), Price("2.50")) -> Quantity(3),
        ).subtotal mustBe Price("35.00")
      }
    }
  }

}
