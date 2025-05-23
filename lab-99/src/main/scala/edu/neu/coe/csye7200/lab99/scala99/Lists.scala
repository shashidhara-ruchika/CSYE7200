/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.lab99.scala99

import scala.annotation.tailrec

object P00 {
  def flatten[X](xss: List[List[X]]): List[X] = {
    @scala.annotation.tailrec
    def inner(r: List[X], wss: List[List[X]]): List[X] = wss match {
      case Nil => r
      case h :: t => inner(r ++ h, t)
    }

    inner(Nil, xss)
  }

  def fill[X](n: Int)(x: X): List[X] = {
    @scala.annotation.tailrec
    def inner(r: List[X], l: Int): List[X] = if (l <= 0) r else inner(r :+ x, l - 1)

    inner(Nil, n)
  }
}

object P01 {
  def last[X](xs: List[X]): X = {
    @scala.annotation.tailrec
    def inner(xs: List[X]): X = xs match {
      case x :: Nil => x
      case _ :: t => inner(t)
    }

    if (xs.isEmpty) throw new NoSuchElementException()
    inner(xs)
  }

}

object P02 {

  def penultimate[X](xs: List[X]): X = {
    @scala.annotation.tailrec
    def inner(xs: List[X]): X = xs match {
      case x :: _ :: Nil => x
      case _ :: t => inner(t)
    }

    if (xs.isEmpty || xs.size == 1) throw new NoSuchElementException()
    inner(xs)
  }

}

object P03 {
  def kth[X](k: Int, xs: List[X]): X = {
    @scala.annotation.tailrec
    def inner(xs: List[X], k: Int): X = (xs, k) match {
      case (x :: _, 0) => x
      case (_ :: t, k) => inner(t, k - 1)
    }

    if (xs.isEmpty || k > xs.size - 1 || k <= 0) throw new NoSuchElementException()
    inner(xs, k)
  }

}

object P04 {

  def length[X](xs: List[X]): Int = {
    @scala.annotation.tailrec
    def inner(xs: List[X], n: Int): Int = xs match {
      case Nil => n
      case _ :: t => inner(t, n + 1)
    }

    inner(xs, 0)
  }

}

object P05 {

  def reverse[X](xs: List[X]): List[X] = {
    @scala.annotation.tailrec
    def inner(xs: List[X], reversed_xs: List[X]): List[X] = xs match {
      case Nil => reversed_xs
      case h :: t => inner(t, h :: reversed_xs)
    }

    inner(xs, Nil)
  }
}

object P06 {

  def isPalindrome[X](ys: List[X]): Boolean = {
    @tailrec
    def inner(xs: List[X]): Boolean = xs match {
      case Nil | _ :: Nil => true
      case h :: t =>
        if (h == t.last) inner(t.init)
        else false
    }

    inner(ys)

    //    @tailrec
    //    def inner(r: Boolean, xs: List[X]): Boolean = xs match {
    //        case Nil | _ :: Nil => r
    //        case h :: t => inner(h == t.last, t.init)
    //        case _ => false
    //    }
    //
    //    inner(r = true, ys)
  }

}

object P07 {

  type ListAny = List[Any]

  def flatten(xs: ListAny): ListAny = {
    //        @scala.annotation.tailrec
    //        def inner(xs: ListAny, r: List[Any]): ListAny = xs match {
    //            case Nil => r
    //            case (h: List[_]) :: t => inner(h ++ t, r)
    //            case h :: t => inner(t, r :+ h) // :+ i.e. appending to end of list is O(n)
    //        }

    @scala.annotation.tailrec
    def inner(xs: ListAny, r: List[Any]): ListAny = xs match {
      case Nil => r.reverse // reverse is O(n)
      case (h: List[_]) :: t => inner(h ++ t, r)
      case h :: t => inner(t, h :: r) // :: prepending to list is O(1)
    }

    inner(xs, Nil)
  }
}

object P08 {

  def compress[X](xs: List[X]): List[X] = {
    @scala.annotation.tailrec
    def inner(xs: List[X], r: List[X]): List[X] = xs match {
      case Nil => r.reverse
      case h :: t =>
        if (r.isEmpty || h != r.head) inner(t, h :: r)
        else inner(t, r)
    }

    inner(xs, Nil)
  }
}

object P09 {

  def pack[X](xs: List[X]): List[List[X]] = {
    @scala.annotation.tailrec
    def inner(xs: List[X], r: List[List[X]]): List[List[X]] = xs match {
      case Nil => r.reverse
      case xh :: xt => r match {
        case Nil => inner(xt, List(List(xh)))
        case (rh :: rt) :: rs =>
          if (xh == rh) inner(xt, (xh :: rh :: rt) :: rs)
          else inner(xt, List(xh) :: r)
      }
    }

    inner(xs, Nil)
  }
}

object P10 {

  def encode[X](xs: List[X]): List[(Int, X)] = {
    @scala.annotation.tailrec
    def inner(xs: List[X], r: List[(Int, X)]): List[(Int, X)] = xs match {
      case Nil => r.reverse
      case h :: t => r match {
        case Nil => inner(t, List((1, h)))
        case (freq, item) :: rs =>
          if (item == h) inner(t, (freq + 1, item) :: rs)
          else inner(t, (1, h) :: (freq, item) :: rs)
      }
    }

    inner(xs, Nil)
  }

}

object P11 {

  def encodeModified[X](xs: List[X]): List[Any] = {
    @scala.annotation.tailrec
    def inner(xs: List[X], r: List[Any]): List[Any] = xs match {
      case Nil => r.reverse
      case h :: t => r match {
        case Nil => inner(t, List(h))
        case (freq: Int, item) :: rs =>
          if (item == h) inner(t, (freq + 1, item) :: rs)
          else inner(t, h :: (freq, item) :: rs)
        case item :: rs =>
          if (item == h) inner(t, (2, item) :: rs)
          else inner(t, h :: item :: rs)
      }
    }

    inner(xs, Nil)
  }

}

object P12 {

  def decode[X](xIs: List[(Int, X)]): List[X] = {
    @scala.annotation.tailrec
    def inner(xIs: List[(Int, X)], r: List[X]): List[X] = xIs match {
      case Nil => r.reverse
      case (1, x) :: t => inner(t, x :: r)
      case (freq, x) :: t => inner((freq - 1, x) :: t, x :: r)
    }

    inner(xIs, Nil)
  }

}

object P13 {

  def encodeDirect[X](xs: List[X]): List[(Int, X)] = {
    P10.encode(xs)
  }
}

object P14 {

  def duplicate[X](xs: List[X]): List[X] = {
    @scala.annotation.tailrec
    def inner(xs: List[X], r: List[X]): List[X] = xs match {
      case Nil => r.reverse
      case h :: t => inner(t, h :: h :: r)
    }

    inner(xs, Nil)
  }
}

object P15 {

  def duplicateN[X](n: Int, xs: List[X]): List[X] = {
    @scala.annotation.tailrec
    def inner(xs: List[X], count: Int, r: List[X]): List[X] = xs match {
      case Nil => r.reverse
      case h :: t =>
        if (count < n) inner(xs, count + 1, h :: r)
        else inner(t, 0, r)
    }

    inner(xs, 0, Nil)

  }
}