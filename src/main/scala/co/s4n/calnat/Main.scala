package co.s4n.calnat

import scala.io.StdIn

object Main extends App {

  def leerInt(prompt: String): Int = {
    val s = StdIn.readLine(prompt)
    s.toInt
  }

  def esCero(nat: Nat) = nat match {
    case Cero() => true
    case Suc(nat) => false
  }

  def conIntANat(i: Int): Nat = i match {
    case 0 => Cero()
    case _ => Suc(conIntANat(i - 1))
  }

  def imprimirNat(nat: Nat): String = {
    if (esCero(nat)) {
      "Cero"
    }
    else {
      "Suc(" + imprimirNat(extractSuc(nat)) + ")"
    }
  }

  def sumaNat(nat1: Nat, nat2: Nat): Nat = (nat1, nat2) match {
    case (Cero(), nat2) => nat2
    case (nat1, Cero()) => nat1
    case (nat1, nat2) => sumaNat(addSuc(nat1), extractSuc(nat2))
  }

  def extractSuc(nat: Nat): Nat = nat match {
    case Suc(nat) => nat
  }

  def addSuc(nat: Nat): Nat = Suc(nat)

  // Main
  val nat1 = conIntANat(leerInt("Leer primer entero "))
  val nat2 = conIntANat(leerInt("Leer segundo entero "))

  val sumNat = sumaNat(nat1, nat2)
  println(imprimirNat(sumNat))
}