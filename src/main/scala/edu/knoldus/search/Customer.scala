package edu.knoldus.search

import edu.knoldus.InventoryOperation
import edu.knoldus.edu.knoldus.database.MyDatabase
import org.apache.log4j.Logger

import scala.collection.immutable.ListMap
import scala.io.StdIn

class Customer (product: Map[Int, MyDatabase] = Map ()) {
  val log = Logger.getLogger (this.getClass)

  def lowToHigh (): Unit = {

    val increasePrice = ListMap (product.toSeq.sortWith (_._2.price < _._2.price): _*)
    for (low2high <- increasePrice)
      log.debug (low2high + "\n")
    searchMenu (product)
  }

  def highToLow (): Unit = {

    val decreasePrice = ListMap (product.toSeq.sortWith (_._2.price > _._2.price): _*)
    for (high2low <- decreasePrice)
      log.debug (high2low + "\n")
    searchMenu (product)
  }

  def searchMenu (product: Map[Int, MyDatabase]): Unit = {

    log.debug ("Selection the Type Of searching \n")
    log.debug ("1: View All Items \n")
    log.debug ("2: Search By Filtering Price \n")
    log.debug ("3: View price of Item \n")
    log.debug ("4: Checkout --> \n")
    log.debug ("5: Main Menu \n")
    log.debug ("Enter Your Choice: \n")
    val select = StdIn.readInt ()
    select match {
      case 1 => new InventoryOperation (product).viewItem ()
      case 2 => log.debug ("A: Filter Price: Low To High \n")
        log.debug ("B: Filter Price High To Low \n")
        log.debug ("Enter your choice:")
        val filterPrice = StdIn.readLine ().toLowerCase ()

        filterPrice match {
          case "a" => lowToHigh ()
          case "b" => highToLow ()
          case _ => searchMenu (product)
        }
      case 3 => searchItemPrice ()
      case 4 => checkOut ()
      case 5 => new InventoryOperation ().inventoryMenu ()
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

  def returnMap () {

    val mapReturn = product.toList
    for (product <- mapReturn)
      log.debug (product + "\n")
  }

  def checkOut (): Unit = {

    log.debug (product)
    val selectProduct = StdIn.readLine ("\nDo You Want to Buy This Item --> \n")
    log.debug ("Press y for yes||Press Anything for No")
    if (selectProduct.toLowerCase == "y") {
      log.debug ("Enter ItemId for product you want to Buy: \n")
      val itemId = StdIn.readInt ()
      val value: Option[MyDatabase] = product.get (itemId)
      val newMap = Map (itemId -> value)
      new Customer (product).searchMenu (product)
      log.debug (newMap)
    }
    else {
      new InventoryOperation ().inventoryMenu ()
    }
  }
}
