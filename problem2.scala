/*
TST Question 2
Stephen Pardue
*/

case class Promotion(code: String, notCombinableWith: Seq[String])
case class PromotionCombo(promotionCodes: Seq[String])



def getComboMap(allPromotions: Seq[Promotion]): Map[String, Seq[String]] = {
    allPromotions.foldLeft(Map[String, Seq[String]]()) { (acc, p) => 
        val combinableCodes = allPromotions.filter { op => op != p && ! op.notCombinableWith.contains(p.code)}.map(_.code)
        acc + (p.code -> combinableCodes)
    }
}

def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] =  {

    val comboMap = getComboMap(allPromotions)

    val codes = comboMap.keys
    

    val seed = promotionCode :: comboMap(promotionCode).toList

    val combos = (2 to seed.length).flatMap(seed.combinations(_))
    

    val validCombos = combos.filter { combo => 
        ! combo.flatMap { promotion =>
            combo.filter(_ != promotion).map ( oPromotion => comboMap(promotion).contains(oPromotion) )
        }.contains(false)
    }

    val subCombos = validCombos.flatMap { combo => 
        (2 to combo.length).flatMap(combo.combinations(_)).filter(_ != combo).toList
    }

    val longestCombos = validCombos.filter { combo => ! subCombos.contains(combo) }

    longestCombos.map(combo => PromotionCombo(combo.sorted))



}

def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] =  {

    getComboMap(allPromotions)
    .keys
    .flatMap( promo => combinablePromotions(promo, allPromotions))
    .toSeq


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
