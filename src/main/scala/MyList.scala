package com.nik.lab

import MyList.*
import scala.annotation.tailrec

enum MyList[+A]:
  case MyNil
  case MyCons(h: A, t: MyList[A])

  override def toString: String =
    @scala.annotation.tailrec
    def go(sb: StringBuilder, as: MyList[A]): String = {
      as match {
        case MyNil => sb.append(']').result
        case MyCons(h, MyNil) => sb.append(h).append(']').result
        case MyCons(h, t) => go(sb.append(h).append(", "), t)
      }
    }
    go(new StringBuilder("["), this)

object MyList:
  def empty[A]: MyList[A] = MyNil
  def apply[A](xs: A*): MyList[A] = of(xs*)
  def of[A](xs: A*): MyList[A] =
    xs.foldRight(MyNil: MyList[A]) { case (x, acc) => MyCons(x, acc) }

def flatMap[A, B](xs: MyList[A], f: A => MyList[B]): MyList[B] =
  def concat[A](xs: MyList[A], ys: MyList[A]): MyList[A] =
    xs match
      case MyNil => ys
      case MyCons(h, t) => MyCons(h, concat(t, ys))
  xs match
    case MyNil => MyNil
    case MyCons(h, MyNil) => f(h)
    case MyCons(h, t) => concat(f(h), flatMap(t, f))

@tailrec
def any[A](xs: MyList[A], pred: A => Boolean): Boolean =
  xs match
    case MyNil => false
    case MyCons(h, t) =>
      if (pred(h))
        true
      else
        any(t, pred)

@tailrec
def all[A](xs: MyList[A], pred: A => Boolean): Boolean =
  xs match
    case MyNil => true
    case MyCons(h, t) =>
      if (pred(h))
        all(t, pred)
      else
        false

def sumBy[A](xs: MyList[A], f: A => Int): Int =
  @tailrec
  def go(xs: MyList[A], sum: Int): Int =
    xs match
      case MyNil => sum
      case MyCons(h, t) => go(t, sum + f(h))
  go(xs, 0)

@main def run(): Unit =
  println("Hello")
