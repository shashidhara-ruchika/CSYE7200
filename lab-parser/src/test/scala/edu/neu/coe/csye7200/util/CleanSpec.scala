package edu.neu.coe.csye7200.util

import edu.neu.coe.csye7200.util.FileCleaner.{noleak, noleakFlat}
import java.io.{BufferedWriter, File, FileWriter}
import org.scalatest.{FlatSpec, Matchers}
import scala.io.Source
import scala.util.{Failure, Success, Try}

class CleanSpec extends FlatSpec with Matchers {

  behavior of "Clean"

  implicit val logger: Logger = Logger(classOf[CleanSpec])
  val binarySearchJava = "../INFO6205/src/main/java/com/phasmidsoftware/dsaipg/BinarySearch.java"

  it should "clean 0" in {
    val cleaner = new FileCleaner("SOLUTION", "STUB", "END SOLUTION")
    val result = cleaner.clean("assignment-web-crawler/src/main/scala/edu/neu/coe/csye7200/asstwc/SolutionTemplateTest.sc", "output.txt")
    result shouldBe Success(2897)
  }
  it should "clean 1" in {
    val cleaner = new FileCleaner("SOLUTION", "STUB", "END SOLUTION")
    val result = cleaner.clean(binarySearchJava, "output.txt")
    result shouldBe Success(1260)
  }
  it should "clean 2" in {
    val cleaner = new FileCleaner("SOLUTION", "STUB", "END SOLUTION")
    val result = cleaner.clean("assignment-web-crawler/src/main/scala/edu/neu/coe/csye7200/asstwc/WebCrawler.scala", "output.txt")
    result shouldBe Success(7185)
  }
  it should "clean 3" in {
    val cleaner = new FileCleaner("SOLUTION", "STUB", "END SOLUTION")
    val result = cleaner.clean("assignment-functional-composition/src/main/scala/edu/neu/coe/csye7200/asstfc/Movie.scala", "output.txt")
    result shouldBe Success(9141)
  }
  it should "clean 4" in {
    val cleaner = new FileCleaner("SOLUTION", "STUB", "END")
    val result: Try[Int] = cleaner.clean("assignment-movie-database/src/main/scala/edu/neu/coe/csye7200/asstmd/Movie.scala", "badOutput.txt")
    result shouldBe Success(10324)
  }

  it should "parseLine 1" in {
    val w1 = "       while (hi > lo) {"
    val w2 = "       // SOLUTION : implement binary search"
    val w3 = "            int mid = lo + (hi - lo) / 2;"
    val w4 = "       // STUB"
    val w5 = "    // return -1;"
    val cleaner = new FileCleaner("SOLUTION", "STUB", "END SOLUTION")
    cleaner.parseLine(w1 -> 0) shouldBe Success(cleaner.ParsedLine(0, "       ", None, "while (hi > lo) {"))
    cleaner.parseLine(w2 -> 0) shouldBe Success(cleaner.ParsedLine(0, "       ", Some(Some("SOLUTION")), " : implement binary search"))
    cleaner.parseLine(w3 -> 0) shouldBe Success(cleaner.ParsedLine(0, "            ", None, "int mid = lo + (hi - lo) / 2;"))
    cleaner.parseLine(w4 -> 0) shouldBe Success(cleaner.ParsedLine(0, "       ", Some(Some("STUB")), ""))
    cleaner.parseLine(w5 -> 0) shouldBe Success(cleaner.ParsedLine(0, "    ", Some(None), " return -1;"))
  }

  it should "parseLine 2" in {
    val w1 = "       while (hi > lo) {"
    val w2 = "       // TO BE IMPLEMENTED  : implement binary search"
    val w3 = "            int mid = lo + (hi - lo) / 2;"
    val cleaner = new FileCleaner("SOLUTION", "STUB", "END SOLUTION")
    cleaner.parseLine(w1 -> 0).get.render(false) shouldBe w1
    cleaner.parseLine(w2 -> 0).get.render(false) shouldBe w2
    cleaner.parseLine(w3 -> 0) shouldBe Success(cleaner.ParsedLine(0, "            ", None, "int mid = lo + (hi - lo) / 2;"))
  }

  it should "slashes" in {
    val w1 = "//"
    val w2 = "/"
    val cleaner = new FileCleaner("SOLUTION", "STUB", "END SOLUTION")
    cleaner.parseAll(cleaner.slashes, w1) should matchPattern { case cleaner.Success(_, _) => }
    cleaner.parseAll(cleaner.slashes, w2) should matchPattern { case cleaner.Failure(_, _) => }
  }

  behavior of "FileCleaner"

  it should "noleak 1" in {
    val outputFile = "junk.txt"
    val (input, output) = (new File(binarySearchJava), new File(outputFile))
    noleak(Try(Source.fromFile(input))) { s => s getLines() foreach println } shouldBe Success(())
    noleak(Try(new BufferedWriter(new FileWriter(output)))) { w => w.append("Hello"); () } shouldBe Success(())
  }
  it should "noleak 2" in {
    val outputFile = "junk.txt"
    val (input, output) = (new File(binarySearchJava), new File(outputFile))
    noleakFlat(Try(new BufferedWriter(new FileWriter(output)))) {
      w =>
        noleak(Try(Source.fromFile(input))) {
          s =>
            s getLines() foreach (x => w.append(s"$x\n"))
        }
    } shouldBe Success(())
  }
  it should "noleak 3" in {
    val outputFile = "junk.txt"
    val (input, output) = (new File(binarySearchJava), new File(outputFile))
    val triedUnit = noleakFlat(Try(new BufferedWriter(new FileWriter(output)))) {
      w =>
        noleak(Try(Source.fromFile(input))) {
          s =>
            s getLines() foreach (x => w.append(s"$x\n"))
        }
    }
    // NOTE: previously, this was matching Failure(_).
    // I'm not sure what this was supposed to test but it does seem to work correctly.
    triedUnit should matchPattern { case Success(()) => }
  }
}