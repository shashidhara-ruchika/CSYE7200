package edu.neu.coe.csye7200.asstwc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.Using

class StopwatchSpec extends AnyFlatSpec with should.Matchers {

  behavior of "lap"

  it should "work for lap 1" in {
    Using(new Stopwatch) { sw =>
      Thread.sleep(100)
      sw.lap shouldBe 100 +- 10
    }
  }

  it should "work for lap 2" in {
    Using(new Stopwatch) { sw =>
      Thread.sleep(100)
      sw.lap shouldBe 100 +- 10
      Thread.sleep(200)
      sw.lap shouldBe 200 +- 10
    }
  }

  it should "throw exception when already stopped" in {
    Using(new Stopwatch) { sw =>
      sw.stop
      a[StopwatchException] should be thrownBy sw.lap
    }
  }

  behavior of "stop"

  it should "work for stop 1" in {
    Using(new Stopwatch) { sw =>
      Thread.sleep(100)
      val (lap, total) = sw.stop
      lap shouldBe 100 +- 10
      total shouldBe 100 +- 10
    }
  }

  it should "work for stop 2" in {
    Using(new Stopwatch) { sw =>
      Thread.sleep(100)
      sw.lap
      Thread.sleep(200)
      val (lap, total) = sw.stop
      lap shouldBe 100 +- 10
      total shouldBe 300 +- 10
    }
  }

  it should "throw exception when already stopped" in {
    Using(new Stopwatch) { sw =>
      sw.stop
      a[StopwatchException] should be thrownBy sw.stop
    }
  }
}
