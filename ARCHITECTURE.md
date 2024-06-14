# Architecture

I did this thinking of it as a Kata, but got feedback that wasn't what was expected - so I wrote up some notes to
explain how what I did create could fit into a service context to share at follow up interview
Where I think I was working pretty much just in the domain bit, as if I was doing a kata or something like that for
practice rather than thinking in terms of services

This was me thinking about the structure I kind of had in mind:
```
shoppingcart
  operations
    queries
      GetCartSummary(cartId): Future[CartSummary]
    commands
      AddToCart(cartId, item, quantity): Future[Boolean]
  connectors
    pricelist
      PriceList
        price(Item): Future[PricedItem]
      PriceListFromEqualExperts
  persistence
    ShoppingCartRepo
      getOrCreateById(CartId): Future[ShoppingCart]
      save(ShoppingCart): Future[Boolean]
  domain
    ShoppingCart
    CartSummary
    Item
    Price
    PricedItem
    TaxRate
    ... etc
  ShoppingCartService(PriceList, ShoppingCartRepo)
    getCartSummary(CartId)
      shoppingCartRepo.getOrCreateById(CartId).map(_.summary)
    addToCart(CartId, Item, Quantity) for {
      cart        <- shoppingCartRepo.getOrCreateCartById(cartId)
      pricedItem  <- priceList.price(item)
      updatedCart  = cart.add(pricedItem, quantity)
      _           <- shoppingCartRepo.saveCart(cart)
    } yield true
  ShoppingCartService
    fromConfig(config): ShoppingCartService =
      new ShoppingCartService(
        new PriceListFromEqualExperts(),
        new InMemoryShoppingCartRepo()
      )
```

This was me thinking about how acceptance tests might look for that:
```
ShoppingCartService(
    PriceList(
      Item -> PricedItem(Item, Price)
    ),
    ShoppingCartRepo(
      CartId(1) -> ShoppingCart()
    )
)

ShoppingCartService
    - if adding an item that exists in price list
        - then should be added to cart
    - if adding an item that is not in price list
        - then operation should fail
    - if item was already in cart
        - then quantity of item should be updated
    - if other items were already in the cart
        - then cart should contain all items added
    - if other carts exist
        - then only the matching cart should be updated 
    - if querying a cart that doesn't exist
        - then should get an empty cart
```
