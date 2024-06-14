import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

case class Price private (value: BigDecimal) {
  def taxPayable(implicit taxRate: TaxRate): Price = Price(value * taxRate.multiplier)
}
object Price {
  def apply(value: String): Price     = Price(BigDecimal(value))
  def apply(value: BigDecimal): Price = new Price(value.setScale(2, BigDecimal.RoundingMode.UP))
}

case class TaxRate(multiplier: BigDecimal)

case class Item(name: String)

case class PricedItem(item: Item, price: Price)

case class Quantity(value: BigDecimal)

case class ShoppingCart private (items: Map[PricedItem, Quantity]) {
  require(items.values.forall(_.value > 0), "negative quantities aren't allowed")
  def subtotal: Price = Price(items.map {
    case (item, quantity) => item.price.value * quantity.value
  }.sum)
  def taxPayable(implicit taxRate: TaxRate): Price   = subtotal.taxPayable
  def totalPayable(implicit taxRate: TaxRate): Price = Price(subtotal.value + subtotal.taxPayable.value)
  def add(item: PricedItem, quantity: Quantity): ShoppingCart = {
    this.copy(items =
      items.get(item).map(existing => Quantity(existing.value + quantity.value)).getOrElse(quantity) match {
        case newQuantity if newQuantity.value > 0 => items.updated(item, newQuantity)
        case _                                    => items.removed(item)
      }
    )
  }
  def remove(item: Item, quantity: Quantity): ShoppingCart = {
    /* can work it through, but on the spot being observed :oof: */
    val pricedItemsToUpdate = items.toList.filter(_._1.item == item).sortBy(_._1.price.value).reverse

    this.copy(
      items = pricedItemsToUpdate
        .foldLeft((items, quantity.value)) {
          case ((_items, leftToRemove), (pricedItem, itemQuantity)) => {
            val remainingItems = itemQuantity.value - leftToRemove
            if (remainingItems > 0)
              (_items.updated(pricedItem, Quantity(remainingItems)), 0)
            else
              (_items.removed(pricedItem), remainingItems.abs)
          }
        }
        ._1
    )
  }

  /* me psuedocoding it out...

    pricedItemsToUpdate =
      items to list
      filter matching our item
      sortBy item price with most expensive first
    this.copy(
      items = (pricedItemsToUpdate.foldLeft((items, quantity.value)) {
        case ((updatedItems, leftToRemove), (pricedItem, itemQuantity))) => {
          val remainingItems = itemQuantity.value - leftToRemove.value
          if (remainingItems > 0)
            (items.updated(pricedItem, Quantity(remainingItems)), 0)
          else
            (items.removed(pricedItem), remainingItems.abs)
        }
      })._1
    )
   */
}
object ShoppingCart {
  def apply(): ShoppingCart                                 = ShoppingCart(Map.empty[PricedItem, Quantity])
  def apply(items: Map[PricedItem, Quantity]): ShoppingCart = new ShoppingCart(items.filter(_._2.value > 0))
}

class ShoppingCartSpec extends AnyFreeSpec with Matchers {
  implicit val twentyPercent: TaxRate = TaxRate(BigDecimal("0.2"))

  "ShoppingCart" - {
    "not allow negative quantities" - {
      "should remove item from cart if it has negative quantity following after an add" in {
        ShoppingCart(
          Map(
            PricedItem(Item("frosties"), Price("2.50")) -> Quantity(1)
          )
        ).add(
          PricedItem(Item("frosties"), Price("2.50")),
          Quantity(-2)
        ).items mustBe empty
      }
      "should drop items with negative quantities" in {
        ShoppingCart(
          Map(
            PricedItem(Item("frosties"), Price("2.50"))   -> Quantity(1),
            PricedItem(Item("cornflakes"), Price("5.00")) -> Quantity(-1)
          )
        ).items mustBe Map(
          PricedItem(Item("frosties"), Price("2.50")) -> Quantity(1)
        )
      }
    }
    "subtotal" - {
      "with no items, should be 0" in {
        ShoppingCart().subtotal mustBe Price(0)
      }
      "with one item, should be equal to it's price multiplied by it's quantity" in {
        ShoppingCart(
          Map(
            PricedItem(Item("cornflakes"), Price("5.00")) -> Quantity(5)
          )
        ).subtotal mustBe Price("25.00")
      }
      "with multiple items, should be equal to their subtotals summed together" in {
        ShoppingCart(
          Map(
            PricedItem(Item("cornflakes"), Price("5.00")) -> Quantity(5),
            PricedItem(Item("frosties"), Price("2.50"))   -> Quantity(1),
            PricedItem(Item("weetabix"), Price("2.50"))   -> Quantity(3),
          )
        ).subtotal mustBe Price("35.00")
      }
    }
    "taxPayable" - {
      "with no items, should be 0" in {
        ShoppingCart().taxPayable mustBe Price(0)
      }
      "with multiple items, should be equal to their subtotal multiplied by the tax rate" in {
        ShoppingCart(
          Map(
            PricedItem(Item("cornflakes"), Price("5.00")) -> Quantity(5),
            PricedItem(Item("frosties"), Price("2.50"))   -> Quantity(1),
            PricedItem(Item("weetabix"), Price("2.50"))   -> Quantity(3),
          )
        ).taxPayable mustBe Price("7.00")
      }
    }
    "totalPayable" - {
      "with no items, should be 0" in {
        ShoppingCart().totalPayable mustBe Price(0)
      }
      "with multiple items, should be equal to their subtotal multiplied by the tax rate" in {
        ShoppingCart(
          Map(
            PricedItem(Item("cornflakes"), Price("5.00")) -> Quantity(5),
            PricedItem(Item("frosties"), Price("2.50"))   -> Quantity(1),
            PricedItem(Item("weetabix"), Price("2.50"))   -> Quantity(3),
          )
        ).totalPayable mustBe Price("42.00")
      }
    }
    "add" - {
      "when the same priced item exists, should increase the quantity" in {
        ShoppingCart(
          Map(
            PricedItem(Item("cornflakes"), Price("2.52")) -> Quantity(1)
          )
        )
          .add(PricedItem(Item("cornflakes"), Price("2.52")), Quantity(1))
          .subtotal mustBe Price("5.04")
      }
      "when it doesn't already exist, should add it to the item list" in {
        val twelveAndAHalfPercentTaxRate: TaxRate = TaxRate(0.125)

        val exampleFromReadme = ShoppingCart(
          Map(
            PricedItem(Item("cornflakes"), Price("2.52")) -> Quantity(2)
          )
        ).add(PricedItem(Item("weetabix"), Price("9.98")), Quantity(1))

        exampleFromReadme.subtotal mustBe Price("15.02")
        exampleFromReadme.taxPayable(twelveAndAHalfPercentTaxRate) mustBe Price("1.88")
        exampleFromReadme.totalPayable(twelveAndAHalfPercentTaxRate) mustBe Price("16.90")
      }
      "should reduce the number of items if adding with a negative quantity" in {
        val cart = ShoppingCart()
          .add(PricedItem(Item("frosties"), Price("2.50")), Quantity(1))
          .add(PricedItem(Item("frosties"), Price("2.50")), Quantity(1))
          .add(PricedItem(Item("frosties"), Price("2.50")), Quantity(-1))

        cart.items mustBe Map(
          PricedItem(Item("frosties"), Price("2.50")) -> Quantity(1)
        )
      }
    }
    "remove" - {
      "when item exists, should be removed" in {
        ShoppingCart(
          Map(
            PricedItem(Item("cornflakes"), Price("5.00")) -> Quantity(5)
          )
        ).remove(Item("cornflakes"), Quantity(2)).items mustBe Map(
          PricedItem(Item("cornflakes"), Price("5.00")) -> Quantity(3)
        )
      }
      "when more than one of the same item exists with different prices, remove most expensive first" in {
        ShoppingCart(
          Map(
            PricedItem(Item("cornflakes"), Price("5.00")) -> Quantity(5),
            PricedItem(Item("cornflakes"), Price("2.00")) -> Quantity(5)
          )
        ).remove(Item("cornflakes"), Quantity(7)).items mustBe Map(
          PricedItem(Item("cornflakes"), Price("2.00")) -> Quantity(3)
        )
      }
    }
  }

}
