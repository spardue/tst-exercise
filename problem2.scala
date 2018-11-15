/*
TST Question 2
Stephen Pardue
*/

case class Promotion(code: String, notCombinableWith: Seq[String])
case class PromotionCombo(promotionCodes: Seq[String])




/*
Generates a map of promotions to combinable promptions.
Example output: 
Map(
    P3 -> List(P2, P4, P5), 
    P5 -> List(P1, P3, P4), 
    P2 -> List(P1, P3), 
    P1 -> List(P2, P4, P5), 
    P4 -> List(P1, P3, P5))
This is useful because the input relationship is in the other direction. Promotions to non combinable promotions.
*/
def getComboMap(allPromotions: Seq[Promotion]): Map[String, Seq[String]] = {
    allPromotions.foldLeft(Map[String, Seq[String]]()) { (acc, p) => 
        val combinableCodes = allPromotions.filter { op => op != p && ! op.notCombinableWith.contains(p.code)}.map(_.code)
        acc + (p.code -> combinableCodes)
    }
}

/* 
Returns the longest combinable promotions for a promotion. 
*/
def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] =  {

    val comboMap = getComboMap(allPromotions) 

    val seed = promotionCode :: comboMap(promotionCode).toList //Seed combination is the longest possible combination 

    val combos = (2 to seed.length).flatMap(seed.combinations(_)) // Generate all the possible sub combinations from the seed combination
    

    val validCombos = combos.filter { combo => 
        ! combo.flatMap { promotion =>
            combo.filter(_ != promotion).map ( oPromotion => comboMap(promotion).contains(oPromotion) )
        }.contains(false) //validate the combos by making sure every promotion in a combo list is compatiable 
    }

    val subCombos = validCombos.flatMap { combo => 
        (2 to combo.length).flatMap(combo.combinations(_)).filter(_ != combo).toList
    } //generate all the subcombinations of the valid combinations

    val longestCombos = validCombos.filter { combo => ! subCombos.contains(combo) } //filter out the valid combinations that are subcombinations

    longestCombos.map(combo => PromotionCombo(combo.sorted))

}

/* 
Get the longest combinable promotions for each promotion
*/
def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] =  {

    val promotionCodes = allPromotions.map(_.code).toSet
    
    promotionCodes.flatMap( promo => combinablePromotions(promo, allPromotions)).toSeq

}







val promotions = Seq(
    Promotion("P1", Seq("P3")),
    Promotion("P2", Seq("P4", "P5")),
    Promotion("P3", Seq("P1")),
    Promotion("P4", Seq("P2")),
    Promotion("P5", Seq("P2"))
)

println("All:"+allCombinablePromotions(promotions))


println("P1:"+combinablePromotions("P1", promotions))
println("P3:"+combinablePromotions("P3", promotions))
