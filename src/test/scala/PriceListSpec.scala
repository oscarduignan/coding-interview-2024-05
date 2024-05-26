import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.client.WireMock
import com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.{BeforeAndAfterAll, EitherValues}
import com.github.tomakehurst.wiremock.client.WireMock._

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

sealed trait PricingError extends Exception
case class PriceNotFound(item: Item) extends PricingError

trait PriceList {
  def priceFor(item: Item): Either[PricingError, PricedItem]
}
object PriceList {
  def apply(prices: Map[Item, Price]): PriceList =
    (item: Item) =>
      prices.get(item).map(price => PricedItem(item, price)).toRight(PriceNotFound(item))
}

class PricesFromEqualExperts(baseUrl: String) extends PriceList {
  override def priceFor(item: Item): Either[PricingError, PricedItem] = {
    val safeName = URLEncoder.encode(item.name, StandardCharsets.UTF_8.toString)
    val response = requests.get(s"$baseUrl/backend-take-home-test-data/$safeName.json", check=false)
    if(response.statusCode == 200) {
      Right(PricedItem(item, Price(ujson.read(response)("price").num)))
    } else {
      Left(PriceNotFound(item))
    }
  }
}

class PriceListSpec extends AnyFreeSpec with Matchers with BeforeAndAfterAll with EitherValues {

  val wireMockServer = new WireMockServer(wireMockConfig().dynamicPort())
  override def beforeAll(): Unit = {
    wireMockServer.start()
    WireMock.configureFor("localhost", wireMockServer.port())
  }
  override def afterAll(): Unit  = wireMockServer.stop()

  "PriceList" - {
    "price" - {
      "should return a PricedItem if it exists in the price list" in {
        PriceList(Map(
          Item("cornflakes") -> Price("2.52"),
          Item("weetabix") -> Price("9.98")
        )).priceFor(Item("weetabix")) mustBe Right(PricedItem(Item("weetabix"), Price("9.98")))
      }
      "should return a not found error if it is missing" in {
        PriceList(Map(
          Item("cornflakes") -> Price("2.52"),
          Item("weetabix") -> Price("9.98")
        )).priceFor(Item("apple")) mustBe Left(PriceNotFound(Item("apple")))
      }
    }
  }

  "PricesFromEqualExperts" - {
    "price" - {
      "should return a PricedItem if it exists in the equal experts api" in {
        stubFor(
          get(urlEqualTo("/backend-take-home-test-data/weetabix.json"))
            .willReturn(okJson("""
              |{
              |  "title": "Weetabix",
              |  "price": 9.98
              |}
              |""".stripMargin)))

        val apiUrl = s"http://localhost:${wireMockServer.port()}"
        new PricesFromEqualExperts(apiUrl)
          .priceFor(Item("weetabix")) mustBe Right(PricedItem(Item("weetabix"), Price("9.98")))
      }
      "should return a PriceNotFound error if a price can't be found" in {
        val apiUrl = s"http://localhost:${wireMockServer.port()}"
        new PricesFromEqualExperts(apiUrl)
          .priceFor(Item("apple")) mustBe Left(PriceNotFound(Item("apple")))
      }
      "should work with real data" in {
        val equalExperts = new PricesFromEqualExperts("https://equalexperts.github.io/")
        implicit val taxRate: TaxRate = TaxRate(0.125)
        val cart = for {
          cornflakes <- equalExperts.priceFor(Item("cornflakes"))
          weetabix   <- equalExperts.priceFor(Item("weetabix"))
        } yield ShoppingCart()
          .add(cornflakes, Quantity(1))
          .add(cornflakes, Quantity(1))
          .add(weetabix, Quantity(1))
        cart.value.subtotal mustBe Price("15.02")
        cart.value.taxPayable mustBe Price("1.88")
        cart.value.totalPayable mustBe Price("16.90")
      }
    }
  }

}
