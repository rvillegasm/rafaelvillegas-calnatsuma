package co.s4n.calnat

import scala.annotation.tailrec
import scala.io.StdIn

object Main extends App {

  def leerInt(prompt: String): Int = {
    val s = StdIn.readLine(prompt)
    s.toInt
  }

  def esCero(nat: Nat) = nat match {
    case Cero() => true
    case Suc(_) => false
  }

  def conIntANat(i: Int): Nat = {
    @tailrec
    def iConIntANat(i: Int, accum: Nat): Nat = i match {
      case 0 => accum
      case _ => iConIntANat(i - 1, Suc(accum))
    }
    iConIntANat(i, Cero())
  }

  def imprimirNat(nat: Nat): String = {
    if (esCero(nat)) {
      "Cero"
    }
    else {
      "Suc(" + imprimirNat(extractSuc(nat)) + ")"
    }
  }

  @tailrec
  def sumaNat(nat1: Nat, nat2: Nat): Nat = (nat1, nat2) match {
    case (Cero(), nat2) => nat2
    case (nat1, Cero()) => nat1
    case (nat1, nat2) => sumaNat(addSuc(nat1), extractSuc(nat2))
  }

  def extractSuc(nat: Nat): Nat = nat match {
    case Suc(n) => n
    case Cero() => Cero()
  }

  def addSuc(nat: Nat): Nat = Suc(nat)

  // Main
  val nat1 = conIntANat(leerInt("Leer primer entero "))
  val nat2 = conIntANat(leerInt("Leer segundo entero "))

  val sumNat = sumaNat(nat1, nat2)
  println(imprimirNat(sumNat))
}