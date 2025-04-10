package movies

import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.types.{DoubleType, StructField, StructType}
import org.apache.spark.sql.Row
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MovieDatabaseAnalyzerTest extends AnyFlatSpec with Matchers {

    implicit val spark: SparkSession = SparkSession
        .builder()
        .appName("MovieDatabaseAnalyzerTest")
        .master("local[*]")
        .getOrCreate()
    spark.sparkContext.setLogLevel("ERROR")

    import spark.implicits._

    it should ("testGetInputPath") in {
        val path = "src/main/resources/movies.csv"
        val args = Array(path)
        MovieDatabaseAnalyzer.getInputPath(args) should be(path)

        val invalidArgs = Array.empty[String]
        intercept[IllegalArgumentException] {
            MovieDatabaseAnalyzer.getInputPath(invalidArgs)
        }
    }

    it should ("testPreprocess") in {
        val schema = StructType(Seq(
            StructField("imdb_score", DoubleType, nullable = true)
        ))
        val data = Seq(
            Row(null),
            Row(7.5),
            Row(8.0)
        )
        val df = spark.createDataFrame(spark.sparkContext.parallelize(data), schema)

        val processedDf = MovieDatabaseAnalyzer.preprocess(df)
        processedDf.schema("imdb_score").dataType should be(DoubleType)
        processedDf.filter($"imdb_score".isNull).count() should be(1)
    }

    it should ("testPrintRatingStats") in {
        val data = Seq(
            (7.5),
            (8.0),
            (9.0)
        ).toDF("imdb_score")

        val output = new java.io.ByteArrayOutputStream()
        Console.withOut(output) {
            MovieDatabaseAnalyzer.printRatingStats(data)
        }
        val outputString = output.toString
        outputString should include("Total number of rows: 3")
        outputString should include("Mean IMDB rating: 8.1666")
        outputString should include("Standard deviation of IMDB rating: 0.6236")
    }
}