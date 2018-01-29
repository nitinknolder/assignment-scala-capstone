package edu.knoldus

import edu.knoldus.database.MyDatabase
import org.apache.log4j.Logger

import scala.collection.immutable.ListMap
import scala.io.StdIn


class InventoryOperation (product: Map[Int, MyDatabase] = Map ()) {

  val log = Logger.getLogger (getClass)

  def inventoryMenu (): Unit = {

    log.debug ("Select Your Choice From Menu: \n")
    log.debug ("1: Add Item \n")
    log.debug ("2: Delete Item \n")
    log.debug ("3: Update Item \n")
    log.debug ("4: View Item \n")
    log.debug ("5: Search Item \n")
    log.debug ("6: Exit \n")
    log.debug ("Enter your Choice: ")
    val select = StdIn.readInt ()

    select match {
      case 1 => addItem ()
      case 2 => deleteItem ()
      case 3 => update ()
      case 4 => viewItem ()
      case 5 => searchMenu ()
      case 6 => exit ()
    }
  }

  // ***********Add Items****************
  def addItem (): Unit = {

    log.debug ("Add Your Items \n")
    log.debug ("Enter Type Of Item")
    val iType = StdIn.readLine ()

    log.debug ("Enter Item Name: \n ")
    val iName = StdIn.readLine ()

    log.debug ("Enter Product Description: \n")
    val info = StdIn.readLine ()

    log.debug ("Enter Price: \n")
    val price = StdIn.readDouble ()

    log.debug ("Enter Vendor Info: \n")
    val vendInfo = StdIn.readLine ()
    log.debug (s"$iName $info $price $vendInfo Added" + "\n")

    val list = product.keySet.toList
    val maximum = list.max
    val reInventoryInfo = MyDatabase (iType: String, iName: String, info: String, price: Double, vendInfo: String)
    val newMap = Map (maximum + 1 -> reInventoryInfo)
    new InventoryOperation (product ++ newMap).inventoryMenu ()
  }

  //****************View Items***********
  def viewItem (): Unit = {

    val viewData = product.toList
    for (product <- viewData)
      log.debug (product + "\n")

    new InventoryOperation (product).inventoryMenu ()
  }

  //***************Delete Items**********
  def deleteItem (): Unit = {
    log.debug ("Enter Item to Be deleted")

    val itemToBeDeleted = StdIn.readInt ()
    val prod = product.filterKeys (_ != itemToBeDeleted)
    new InventoryOperation (prod).inventoryMenu ()
  }

  //***************Update Items *********
  def update (): Unit = {

    log.debug ("Product Details are as Follows: \n ")
    log.debug (product + "\n")

    log.debug ("Enter the key value for updation: \n ")
    val updateItemDetails = StdIn.readInt () //key to update
    log.debug ("Enter Type Of Item: \n")
    val iType = StdIn.readLine ()
    log.debug ("Enter Item Name: \n ")
    val iName = StdIn.readLine ()
    log.debug ("Enter Product Description: \n")
    val info = StdIn.readLine ()
    log.debug ("Enter Price: \n")
    val price = StdIn.readDouble ()
    log.debug ("Enter Vendor Info: \n")
    val vendInfo = StdIn.readLine ()
    val obj = MyDatabase (iType, iName, info, price, vendInfo)
    val map = Map (updateItemDetails -> obj)
    log.debug (map)
    new InventoryOperation (product ++ map).inventoryMenu ()
  }

  //***************Exit******************
  def exit (): Unit = {
    log.debug ("bye bye")
  }

  def lowToHigh (): Unit = {

    log.debug (ListMap (product.toSeq.sortWith (_._2.price < _._2.price): _*))
    log.debug ("\n")
    searchMenu ()
  }

  def highToLow (): Unit = {

    log.debug (ListMap (product.toSeq.sortWith (_._2.price > _._2.price): _*))
    log.debug ("\n")
    searchMenu ()
  }

  def searchMenu (): Unit = {

    log.debug ("Selection the Type Of searching \n")
    log.debug ("1: View All Items \n")
    log.debug ("2: Search By Filtering Price \n")
    log.debug ("3: View price of Item \n")
    log.debug ("4: Checkout --> \n")
    log.debug ("5: Main Menu \n")
    log.debug ("Enter Your Choice: \n")
    val select = StdIn.readInt ()
    select match {
      case 1 => viewItem ()
      case 2 => log.debug ("A: Filter Price: Low To High \n")
        log.debug ("B: Filter Price High To Low \n")
        log.debug ("Enter your choice:")
        val filterPrice = StdIn.readLine ().toLowerCase ()

        filterPrice match {
          case "a" => lowToHigh ()
          case "b" => highToLow ()
          case _ => searchMenu ()
        }
      case 3 => searchItemPrice ()
      case 4 => checkOut ()
      case 5 => inventoryMenu ()
    }
  }

  def searchItemPrice (): Unit = {

    val typeOfItem = StdIn.readLine ("Enter the Type Of Item: \n")
    val searching = product.filter ((t) => t._2.itemType == typeOfItem)
    log.debug (searching)
    log.debug ("\n")
    new InventoryOperation (product ++ searching).inventoryMenu ()
  }

  def returnMethod (): InventoryOperation = {
    new InventoryOperation (product)
  }

  def returnMap (): Map[Int, MyDatabase] = product

  def checkOut (): Unit = {

    log.debug (product)
    val selectProduct = StdIn.readLine ("\nDo You Want to Buy This Item --> \n")
    if (selectProduct.toLowerCase == "y") {
      log.debug ("Enter itemId for product you want to Buy \n")
      val itemId = StdIn.readInt ()
      val value: Option[MyDatabase] = product.get (itemId)
      val newMap = Map (itemId -> value)
      new InventoryOperation (product).searchMenu ()
      log.debug (newMap)
    }
    else {
      inventoryMenu ()
    }
  }
}













