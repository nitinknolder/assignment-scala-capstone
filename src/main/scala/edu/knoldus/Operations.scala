package edu.knoldus

import edu.knoldus.database.MyDatabase
import search.Customer
import org.apache.log4j.Logger


object Operations extends App {
  val log = Logger.getLogger (this.getClass)
  val iType = "Shoes"
  val iName = "Sports Shoes"
  val info = "Branded"
  val price = 1000
  val vendInfo = "Adidas"
  val objShoes = MyDatabase (iType, iName, info, price, vendInfo)
  val objClothes = MyDatabase ("Clothes", "Shirt", "Cotton 100%", 2999, "Levis")
  val objTv = MyDatabase ("Television", "LedTv", "40 inch", 35000, "Sony")
  val objMobiles = MyDatabase ("Mobiles", "SmartPhone", "5 inch", 12000, "Asus")
  val map = Map (1 -> objShoes, 2 -> objClothes, 3 -> objTv, 4 -> objMobiles)
  val inventObj = new InventoryOperation (map)
  inventObj.inventoryMenu ()
    val value: Option[MyDatabase] = map.get (1)
    val list = map.values.toList
    val list1 = map.keySet.toList
    log.debug(list1.max)

  val db = new Customer(map)
  val returnObj = db.returnMap
  log.debug ("Return Map: " + returnObj)
}


