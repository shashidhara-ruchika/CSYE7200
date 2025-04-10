package movies

import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions.{col, sum, pow, lit}
import org.apache.spark.sql.types.DoubleType

object MovieDatabaseAnalyzer {

    def preprocess(df: org.apache.spark.sql.DataFrame): org.apache.spark.sql.DataFrame = {
        val naCount = df.filter(col("imdb_score").isNull).count()
        println(s"Number of rows with null imdb_score: $naCount")

        df.withColumn("imdb_score", col("imdb_score").cast(DoubleType))
    }

    def getInputPath(args: Array[String]): String = {
        if (args.length < 1) {
            throw new IllegalArgumentException("Usage: MovieDatabaseAnalyzer <file_path>")
        }
        args(0)
    }

    def printRatingStats(df: org.apache.spark.sql.DataFrame): Unit = {
        val totalRows = df.count()
        println(s"Total number of rows: $totalRows")

        val totalRatingSum = df.agg(sum("imdb_score")).first().getDouble(0)
        val mean = totalRatingSum / totalRows
        println(s"Mean IMDB rating: $mean")

        val varianceSum = df.agg(sum(pow(col("imdb_score") - lit(mean), 2))).first().getDouble(0)
        val stddev = Math.sqrt(varianceSum / totalRows)
        println(s"Standard deviation of IMDB rating: $stddev")
    }

    def main(args: Array[String]): Unit = {
        val path = getInputPath(args)

        implicit val spark: SparkSession = SparkSession
            .builder()
            .appName("MovieDatabaseAnalyzer")
            .master("local[*]")
            .getOrCreate()
        spark.sparkContext.setLogLevel("ERROR")

        val raw_df = spark.read
            .option("header", "true")
            .option("inferSchema", "true")
            .csv(path)

        val df = preprocess(raw_df)

        println(s"Repository CSV - Input path: $path")
        printRatingStats(df)

        df.describe("imdb_score").show()
    }

}
