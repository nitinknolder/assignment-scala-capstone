package edu.knoldus

import edu.knoldus.database.MyDatabase
import search.Customer
import org.apache.log4j.Logger

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
      case 5 => new Customer (product).searchMenu (product)
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
    log.debug ("<-----Thanks For Using KnolKart<---- \n")
  }
}















