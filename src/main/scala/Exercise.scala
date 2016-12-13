import java.lang.Math._
import java.security.MessageDigest

import scala.io.Source

/**
  * Created by bcourbe on 05/12/2016.
  */
object Exercise {

  def main(args: Array[String]) {
    exercise31()
    exercise32()
    exercise41()
    exercise42()
    exercise61()
    exercise62()
    exercise71()
    exercise72()
  }

  def exercise11()={
    val res = List("R2", "L5", "L4", "L5", "R4", "R1", "L4", "R5", "R3", "R1", "L1", "L1", "R4", "L4", "L1", "R4", "L4", "R4", "L3", "R5", "R4", "R1", "R3", "L1", "L1", "R1", "L2", "R5", "L4", "L3", "R1", "L2", "L2", "R192", "L3", "R5", "R48", "R5", "L2", "R76", "R4", "R2", "R1", "L1", "L5", "L1", "R185", "L5", "L1", "R5", "L4", "R1", "R3", "L4", "L3", "R1", "L5", "R4", "L4", "R4", "R5", "L3", "L1", "L2", "L4", "L3", "L4", "R2", "R2", "L3", "L5", "R2", "R5", "L1", "R1", "L3", "L5", "L3", "R4", "L4", "R3", "L1", "R5", "L3", "R2", "R4", "R2", "L1", "R3", "L1", "L3", "L5", "R4", "R5", "R2", "R2", "L5", "L3", "L1", "L1", "L5", "L2", "L3", "R3", "R3", "L3", "L4", "L5", "R2", "L1", "R1", "R3", "R4", "L2", "R1", "L1", "R3", "R3", "L4", "L2", "R5", "R5", "L1", "R4", "L5", "L5", "R1", "L5", "R4", "R2", "L1", "L4", "R1", "L1", "L1", "L5", "R3", "R4", "L2", "R1", "R2", "R1", "R1", "R3", "L5", "R1", "R4")
      .foldLeft((0,0,"N"))(moving)
    val distance = abs(res._1) + abs(res._2)
    println(s"The distance is $distance")
  }

  def exercise12()={
    val res = List("R2", "L5", "L4", "L5", "R4", "R1", "L4", "R5", "R3", "R1", "L1", "L1", "R4", "L4", "L1", "R4", "L4", "R4", "L3", "R5", "R4", "R1", "R3", "L1", "L1", "R1", "L2", "R5", "L4", "L3", "R1", "L2", "L2", "R192", "L3", "R5", "R48", "R5", "L2", "R76", "R4", "R2", "R1", "L1", "L5", "L1", "R185", "L5", "L1", "R5", "L4", "R1", "R3", "L4", "L3", "R1", "L5", "R4", "L4", "R4", "R5", "L3", "L1", "L2", "L4", "L3", "L4", "R2", "R2", "L3", "L5", "R2", "R5", "L1", "R1", "L3", "L5", "L3", "R4", "L4", "R3", "L1", "R5", "L3", "R2", "R4", "R2", "L1", "R3", "L1", "L3", "L5", "R4", "R5", "R2", "R2", "L5", "L3", "L1", "L1", "L5", "L2", "L3", "R3", "R3", "L3", "L4", "L5", "R2", "L1", "R1", "R3", "R4", "L2", "R1", "L1", "R3", "R3", "L4", "L2", "R5", "R5", "L1", "R4", "L5", "L5", "R1", "L5", "R4", "R2", "L1", "L4", "R1", "L1", "L1", "L5", "R3", "R4", "L2", "R1", "R2", "R1", "R1", "R3", "L5", "R1", "R4")
      .foldLeft(List[String]())((liste : List[String], s : String)=> liste++oneByOne(s))
      .scanLeft((0,0,"N"))(moving)
    val pos = res.map(x=>(x._1,x._2)).find{ e =>
      res.map(x=>(x._1,x._2)).count{cur => cur == e} >= 2
    }
    val distance = abs(pos.get._1) + abs(pos.get._2)
    println(s"The distance is $distance")
  }

  def exercise21()={
    val in = Source.fromURL(getClass.getResource(s"/exercise2.txt")).getLines().toList
    val res = in.map(_.toList).scanLeft((2,2))((pos: (Int, Int), l : List[Char]) => findCode(l, pos))
    res.map(println)
  }

  def exercise22()={
    val in = Source.fromURL(getClass.getResource(s"/exercise2.txt")).getLines().toList
    val res = in.map(_.toList).scanLeft((1,3))((pos: (Int, Int), l : List[Char]) => findCode2(l, pos))
    res.map(println)
  }

  def exercise31()={
    val in = Source.fromURL(getClass.getResource(s"/exercise3.txt")).getLines()
    val parsed = in.toArray.map(_.trim).map(_.replaceAll("\\s+", " ").split(" "))
      .map(_.map(_.toInt))
    println(s"Number of triangles : ${countTriangle(parsed)}")
  }

  def exercise32()={
    val in = Source.fromURL(getClass.getResource(s"/exercise3.txt")).getLines()
    val parsed = in.toArray.map(_.trim).map(_.replaceAll("\\s+", " ").split(" "))
      .map(_.map(_.toInt))
    val col1 = parsed.map(_(0)).grouped(3).toArray
    val col2 = parsed.map(_(1)).grouped(3).toArray
    val col3 = parsed.map(_(2)).grouped(3).toArray
    println(s"Number of triangles : ${countTriangle(col1) + countTriangle(col2) + countTriangle(col3)}")
  }

  def exercise41()={
    val in = Source.fromURL(getClass.getResource(s"/exercise4.txt")).getLines()
    val parsed = in.map(_.split("\\[")).map(a => (a(0).split("-"), a(1).replace("]","")))
    println(s"Sum of sector ID : ${sumRealRooms(parsed)}")
  }

  def exercise42()={
    val in = Source.fromURL(getClass.getResource(s"/exercise4.txt")).getLines()
    val parsed = in.map(_.split("\\[")).map(a => (a(0).split("-"), a(1).replace("]","")))
    val decoded = parsed.map(_._1).map(decodeSentence)
    val res = decoded.filter(e => e._1.equals("northpole object storage"))
    println(s"The sector ID : ${res.toArray.head._2}")
  }

  def exercise61()={
    val in = Source.fromURL(getClass.getResource(s"/exercise6.txt")).getLines().toList
    println(s"The message is : ${message(in)}")
  }

  def exercise62()={
    val in = Source.fromURL(getClass.getResource(s"/exercise6.txt")).getLines().toList
    println(s"The message is : ${message2(in)}")
  }

  def exercise71()={
    val in = Source.fromURL(getClass.getResource(s"/exercise7.txt")).getLines()
    val parsed = in.map(parser)
    val containsAlmostPalindrome = parsed.map{
      e=>
        (e._1.map(containsAnAlmostPalindrome).reduce(_ || _),
        e._2.map(containsAnAlmostPalindrome).reduce(_ || _))
    }
    val res = containsAlmostPalindrome.count(t => t._1 && !t._2)
    println(s"There are $res TLS addresses")
  }

  def exercise72()={
    val in = Source.fromURL(getClass.getResource(s"/exercise7.txt")).getLines()
    val parsed = in.map(parser)
    val containsAlmostPalindrome = parsed.map{
      e=>
        (e._1.map(getListAlmostPalindrome).reduce(_++_),
          e._2.map(getListAlmostPalindrome).reduce(_++_))
    }
    println(s"There are ${containsAlmostPalindrome.count(isSLL)} SSL addresses")
  }

  // UTILS FOR EXERCISE 1
  def moving(tuple: (Int, Int, String), move: String): (Int, Int, String) = {
    val sens  = move(0)
    val nb = move.tail.toInt

    (tuple._3, sens) match {
      case ("N", 'L') => (tuple._1 - nb, tuple._2, "O")
      case ("N", 'R') => (tuple._1 + nb, tuple._2, "E")
      case ("S", 'L') => (tuple._1 + nb, tuple._2, "E")
      case ("S", 'R') => (tuple._1 - nb, tuple._2, "O")
      case ("E", 'L') => (tuple._1, tuple._2 + nb, "N")
      case ("E", 'R') => (tuple._1, tuple._2 - nb, "S")
      case ("O", 'L') => (tuple._1, tuple._2 - nb, "S")
      case ("O", 'R') => (tuple._1, tuple._2 + nb, "N")
      case ("N", 'T') => (tuple._1, tuple._2 + nb, "N")
      case ("S", 'T') => (tuple._1, tuple._2 - nb, "S")
      case ("O", 'T') => (tuple._1 - nb, tuple._2, "O")
      case ("E", 'T') => (tuple._1 + nb, tuple._2, "E")
    }
  }

  def oneByOne(s: String): List[String] = {
    s(0)+"1" :: List.fill(s.tail.toInt-1)("T1")
  }


  // UTILS FOR EXERCISE 2
  def findCode(in: List[Char], start:(Int, Int)): (Int, Int) ={
    in.foldLeft(start)(moveMyFinger)
  }

  def moveMyFinger(pos: (Int, Int), mov: Char): (Int, Int) ={
    val x = pos._1
    val y = pos._2
    mov match {
      case 'U' =>
        if (y!=1)
          (x, y-1)
        else
          (x, y)
      case 'D' =>
        if (y!=3)
          (x, y+1)
        else
          (x, y)
      case 'L' =>
        if (x!=1)
          (x-1, y)
        else
          (x, y)
      case 'R' =>
        if (x!=3)
          (x+1, y)
        else
          (x, y)
    }
  }

  val noUp = List((1,3), (2,2), (3,1), (4,2), (5,3))
  val noDown = List((1,3), (2,4), (3,5), (4,4), (5,3))
  val noLeft = noUp.map(e => (e._2, e._1))
  val noRight = noDown.map(e => (e._2, e._1))

  def moveMyFinger2(pos: (Int, Int), mov: Char): (Int, Int) ={
    val x = pos._1
    val y = pos._2
    mov match {
      case 'U' =>
        if (noUp.contains(pos))
          (x, y)
        else
          (x, y-1)
      case 'D' =>
        if (noDown.contains(pos))
          (x, y)
        else
          (x, y+1)
      case 'L' =>
        if (noLeft.contains(pos))
          (x, y)
        else
          (x-1, y)
      case 'R' =>
        if (noRight.contains(pos))
          (x, y)
        else
          (x+1, y)
    }
  }

  def findCode2(in: List[Char], start:(Int, Int)): (Int, Int) ={
    in.foldLeft(start)(moveMyFinger2)
  }

  // UTILS FOR EXERCISE 3
  def countTriangle(laListe : Array[Array[Int]]): Int ={
    laListe.map(_.sorted).count(checkTriangle)
  }

  def checkTriangle(sides: Array[Int]): Boolean={
    sides.take(2).sum > sides.last
  }

  // UTILS FOR EXERCISE 4
  def sumRealRooms(toCheck: Iterator[(Array[String], String)]): Int ={
    toCheck.filter(isARoom).map(e=>getSectorID(e._1)).sum
  }

  def isARoom(arg: (Array[String], String)): Boolean={
    val name = arg._1
    val checksum = arg._2
    val realChecksum =name
      .take(name.length-1)
      .mkString("").toList
      .groupBy(identity)
      .mapValues(_.size)
      .groupBy(_._2)
      .map(e => (e._1, e._2.keys.toArray.sorted)).toArray
      .sortWith(_._1 > _._1)
      .map(_._2.mkString(""))
      .mkString("")
      .take(5)

    realChecksum.equals(checksum)
  }

  def getSectorID(name: Array[String]): Int ={
    name.last.toInt
  }

  def decodeSentence(in : Array[String]): (String, Int)={
    val decal = in.last.toInt
    val res = in.take(in.length-1).map(_.toList)
      .map(_.map(c=>decode(c,decal)).mkString(""))
      .mkString(" ")
    (res, decal)
  }

  def decode(c: Char, decal: Int): Char={
    ((c.toInt-96 + decal%26)%26+96).toChar
  }

  // UTILS FOR EXERCISE 5
  def md5(s: String) = {
    MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02x".format(_)).mkString
    //revoir à partir du map
  }

  def findNextCode(s: String, i: Int, l: List[Char]): List[Char]={
    val hash = md5(s+i)
    if (hash.take(5).equals("00000") && l.size<8)
      findNextCode(s,i+1,l:+hash(5))
    else if (l.size==8)
      l
    else
      findNextCode(s,i+1,l)
  }

//  def fillArray(hash: String, a: Array[Char]): Array[Char]={
  //    val char = hash(5)
  //    if (char.isDigit)
  //      if (char.getNumericValue >= 8){
  //        val codeChar = hash(5+char.getNumericValue)
  //      }
  //  }

  // UTILS FOR EXERCISE 6
  def message(l: List[String]): String={
    val trans = l.map(_.toList).transpose
    val res = trans.map(_.groupBy(identity).mapValues(_.size))
      .map(_.toArray.sortWith(_._2 > _._2).head._1)
      .mkString("")
    res
  }

  def message2(l: List[String]): String={
    val trans = l.map(_.toList).transpose
    val res = trans.map(_.groupBy(identity).mapValues(_.size))
      .map(_.toArray.sortBy(_._2).head._1)
      .mkString("")
    res
  }

  // UTILS FOR EXERCISE 7
  def containsAnAlmostPalindrome(s: String): Boolean={
    val l = s.sliding(4, 1)
    l.map(isAlmostPalindrome).reduce(_ || _)
  }

  def isAlmostPalindrome(s: String): Boolean={
    s(0).equals(s(3)) && s(1).equals(s(2)) && (s(0) != s(1))
  }

  def isSLL(ls: (List[Option[String]], List[Option[String]])): Boolean={
    val l1 = ls._1
    val l2 = ls._2
    l1.flatten.map(_.take(2)).intersect(l2.flatten.map(_.take(2).reverse)).nonEmpty
  }

  def getListAlmostPalindrome(s: String): List[Option[String]] ={
    val l = s.sliding(3,1)
    l.map(isAlmostPalindrome2).toList
  }

  def isAlmostPalindrome2(s:String): Option[String]={
    if(s(0).equals(s(2)) && (s(0) != s(1)))
      Some(s)
    else
      None
  }

  val regexIn = """(\[[a-z]+\])""".r
  val regexOut = """([a-z]+\[)|(\][a-z]+\[)|(\][a-z]+)""".r

  def parser(s: String): (List[String], List[String])={
    val outBra = regexOut.findAllIn(s).toList.map(cleanField)
    val inBra = regexIn.findAllIn(s).toList.map(cleanField)
    (outBra, inBra)
  }

  def cleanField(s: String): String={
    s.replaceAll("""\[|\]""","")
  }
}
