package Data

import Utils.Dictionary

trait ProductService:
  type BrandName = String
  type ProductName = String

  /**
    * Return the price of the brand name. If product not found then return NaN.
    * @param product product name of the brand
    * @param brand brand name to get the price
    * @return price of the brand
    */
  def getPrice(product: ProductName, brand: BrandName): Double

  /**
    * Return the default brand of the product. If not found then return empty string
    * @param product product name
    * @return get the default brand from the product name
    */
  def getDefaultBrand(product: ProductName): BrandName



class ProductImpl extends ProductService:

  // Part 2 Step 2
  val productMap: Map[ProductName, Map[BrandName, Double]] = Map(
    "biere"->
      Map(
        "farmer"-> 1.0,
        "boxer" -> 1.0,
        "wittekop" -> 2.0,
        "punkipa" -> 3.0,
        "jackhammer" -> 3.0,
        "tenebreuse" -> 4.0,
      ),
    "croissant" ->
      Map(
        "maison" -> 2.0,
        "cailler" -> 2.0,
      ),
  )

  def getPrice(product: ProductName, brand: BrandName): Double = {
    productMap.get(product) match {
      case Some(brandMap) =>
        brandMap.getOrElse(brand,
          brandMap.getOrElse(getDefaultBrand(product), Double.NaN)
        )
      case None =>
        // Cherche avec seulement le brand.
        val result =
          for
            (key, brandMap) <- productMap
            (k, v) <- brandMap
            if k == brand
          yield v

        if result.isEmpty then Double.NaN else result.head
    }
  }

  def getDefaultBrand(product: ProductName): BrandName = {
    product match {
      case "biere" => "boxer"
      case "croissant" => "maison"
      case _ => ""
    }
  }
end ProductImpl
