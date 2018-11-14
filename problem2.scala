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
    
    //combos.foreach(combo => println(combo))


    val x = combos.filter { combo => 
        println(combo)
        combo.map { promotion =>
            val j = combo.filter(_ != promotion).map ( oPromotion => comboMap(promotion).contains(oPromotion) )
            println(combo, j)
            j
        }.contains(false) 
    }

    println(x)
    /*
    codes.map( code => {
        comboMap(code).reduce(List[String]()) { (acc, p) =>


            acc + (p.code, )
        }
        

    })
    */



    null


}

def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] =  {

    




    /*

    val promotions = comboMap.keys

    println(promotions)

    */


    /*
    comboMap.map { case (p, combs) => 
        

        combs.filter(c => comboMap(c))

    }
    */


    

    null
}







val promotions = Seq(
    Promotion("P1", Seq("P3")),
    Promotion("P2", Seq("P4", "P5")),
    Promotion("P3", Seq("P1")),
    Promotion("P4", Seq("P2")),
    Promotion("P5", Seq("P2"))
)

//val result = allCombinablePromotions(promotions)
//println(result)

val result = combinablePromotions("P1", promotions)
println(result)