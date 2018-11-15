/*
TST Question 1
Stephen Pardue
*/

case class Rate(rateCode: String, rateGroup: String)
case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)
case class BestGroupPrice(cabinCode: String, rateCode: String, price: BigDecimal, rateGroup: String)

/*
Get best group prices by cabin
*/
def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {
    
    val cabins = prices.map(_.cabinCode).distinct

    val rateGroup = rates.foldLeft(Map[String,String]()) { (acc, rate) =>
        acc + (rate.rateCode -> rate.rateGroup) 
    } // Map the rate codes to their rateGroup. This O(1) vs O(n) for the given data structure  


    cabins.flatMap { cabin =>

        val cabinPrices = prices.filter(_.cabinCode == cabin) //only filter on prices for the cabin

        // For each rate, find the lowest price for a cabin
        rates.map ( rate => { 
            // We only care about the lowest price for a rate group, hence the rateGroup lookup
            val min = cabinPrices.filter(price => rateGroup(price.rateCode) == rateGroup(rate.rateCode)) 
            .minBy(_.price)
            BestGroupPrice(min.cabinCode, min.rateCode, min.price, rate.rateGroup)  
        }).distinct
      
    }

}


val rates = List(
    Rate("M1", "Military"),
    Rate("M2", "Military"),
    Rate("S1", "Senior"),
    Rate("S2", "Senior"))

val prices = List(
    CabinPrice("CA", "M1", 200.00),
    CabinPrice("CA", "M2", 250.00),
    CabinPrice("CA", "S1", 225.00),
    CabinPrice("CA", "S2", 260.00),
    CabinPrice("CB", "M1", 230.00),
    CabinPrice("CB", "M2", 260.00),
    CabinPrice("CB", "S1", 245.00),
    CabinPrice("CB", "S2", 270.00))


val bgp = getBestGroupPrices(rates, prices)
println(bgp)