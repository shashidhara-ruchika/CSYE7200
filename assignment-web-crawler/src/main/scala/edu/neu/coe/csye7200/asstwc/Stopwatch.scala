package edu.neu.coe.csye7200.asstwc

import scala.language.implicitConversions

/**
 * Class to time processes
 */
class Stopwatch extends AutoCloseable {

  /**
   * Return the elapased time of this lap (in milliseconds)
   *
   * NOTE: lap is NOT a pure function. It has the side effect of updating lapStart
   *
   * @return a Long value of milliseconds
   */
  def lap: Int = {
    if (lapStart == 0) throw StopwatchException("Stopwatch has stopped")
    val current = System.nanoTime()
    val result = current - lapStart
    lapStart = current
    result // implicitly converts to milliseconds
  }

  def stop: (Int,Int) = {
    val lapTime: Int = lap // NOTE side-effect
    val totalTime = lapStart - start
    lapStart = 0L
    (lapTime, totalTime) // implicitly converts to milliseconds
  }

  def close(): Unit = {
    if (lapStart != 0) throw StopwatchException("Stopwatch.close(): still running")
  }

  private val start: Long = System.nanoTime()
  private var lapStart: Long = start // NOTE this is a var
  private val MILLION = 1_000_000

  private implicit def convertToMilliseconds(nanos: Long): Int = (nanos / MILLION).toInt
}

case class StopwatchException(msg: String) extends Exception(msg)
