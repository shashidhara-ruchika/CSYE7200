package edu.neu.coe.csye7200.asstwc

import scala.language.implicitConversions

/**
 * Simple stopwatch class to time processes.
 * Not suitable for dealing with pre- or post-operations, warmup runs, multiple repetitions, etc. (see Benchmark).
 *
 * NOTE Not suitable for timing longer than 68 years.
 */
class Stopwatch extends AutoCloseable {

  /**
   * Return the elapased time of this lap (in milliseconds)
   *
   * NOTE: lap is NOT a pure function. It has the side effect of updating lapStart
   *
   * @return an Int value in milliseconds of the time since the previous lap ended.
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

  // NOTE: the maximum elapsed time that can be handled is 2,147,483.647 seconds (35,791,394 minutes, 596,523 hours, 24,855 days, 68 years)
  private implicit def convertToMilliseconds(nanos: Long): Int = (nanos / MILLION).toInt
}

case class StopwatchException(msg: String) extends Exception(msg)
