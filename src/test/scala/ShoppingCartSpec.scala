import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

case class Price private (value: BigDecimal) {
    def taxPayable(implicit taxRate: TaxRate): Price = Price(value * taxRate.multiplier)
}
object Price {
  def apply(value: String): Price = Price(BigDecimal(value))
  def apply(value: BigDecimal): Price = new Price(value.setScale(2, BigDecimal.RoundingMode.UP))
}

case class TaxRate(multiplier: BigDecimal)

case class Item(name: String)

case class PricedItem(item: Item, price: Price)

case class Quantity(value: BigDecimal)

case class ShoppingCart(items: Map[PricedItem, Quantity]) {
  require(items.values.forall(_.value > 0), "negative quantities aren't allowed")
  def subtotal: Price = Price(items.map {
    case (item, quantity) => item.price.value * quantity.value
  }.sum)
  def taxPayable(implicit taxRate: TaxRate): Price = subtotal.taxPayable
  def totalPayable(implicit taxRate: TaxRate): Price = Price(subtotal.value + subtotal.taxPayable.value)
  def add(item: PricedItem, quantity: Quantity): ShoppingCart =
    this.copy(items =
      items.updated(
        item,
        Quantity(items.get(item).map(_.value).getOrElse(BigDecimal(0)) + quantity.value)
      ))
}
object ShoppingCart {
  def apply(): ShoppingCart = ShoppingCart(Map.empty[PricedItem, Quantity])
  def apply(items: (PricedItem, Quantity)*): ShoppingCart = ShoppingCart(Map(items: _*))
}

class ShoppingCartSpec extends AnyFreeSpec with Matchers {
  implicit val twentyPercent: TaxRate = TaxRate(BigDecimal("0.2"))

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
    "taxPayable" - {
      "with no items, should be 0" in {
        ShoppingCart().taxPayable mustBe Price(0)
      }
      "with multiple items, should be equal to their subtotal multiplied by the tax rate" in {
        ShoppingCart(
          PricedItem(Item("cornflakes"), Price("5.00")) -> Quantity(5),
          PricedItem(Item("frosties"), Price("2.50")) -> Quantity(1),
          PricedItem(Item("weetabix"), Price("2.50")) -> Quantity(3),
        ).taxPayable mustBe Price("7.00")
      }
    }
    "totalPayable" - {
      "with no items, should be 0" in {
        ShoppingCart().totalPayable mustBe Price(0)
      }
      "with multiple items, should be equal to their subtotal multiplied by the tax rate" in {
        ShoppingCart(
          PricedItem(Item("cornflakes"), Price("5.00")) -> Quantity(5),
          PricedItem(Item("frosties"), Price("2.50")) -> Quantity(1),
          PricedItem(Item("weetabix"), Price("2.50")) -> Quantity(3),
        ).totalPayable mustBe Price("42.00")
      }
    }
    "add" - {
      "when the same priced item exists, should increase the quantity" in {
        ShoppingCart(
          PricedItem(Item("cornflakes"), Price("2.52")) -> Quantity(1)
        )
          .add(PricedItem(Item("cornflakes"), Price("2.52")), Quantity(1))
          .subtotal mustBe Price("5.04")
      }
      "when it doesn't already exist, should add it to the item list" in {
        val twelveAndAHalfPercentTaxRate: TaxRate = TaxRate(0.125)

        val exampleFromReadme = ShoppingCart(
          PricedItem(Item("cornflakes"), Price("2.52")) -> Quantity(2)
        ).add(PricedItem(Item("weetabix"), Price("9.98")), Quantity(1))

        exampleFromReadme.subtotal mustBe Price("15.02")
        exampleFromReadme.taxPayable(twelveAndAHalfPercentTaxRate) mustBe Price("1.88")
        exampleFromReadme.totalPayable(twelveAndAHalfPercentTaxRate) mustBe Price("16.90")
      }
    }
  }

}
