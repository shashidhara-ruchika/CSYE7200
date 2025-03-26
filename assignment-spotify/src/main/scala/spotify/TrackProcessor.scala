package spotify

import common.Config
import org.apache.spark.sql.SparkSession
import org.apache.spark.streaming.{Seconds, StreamingContext}
import spotify.dao.Track

import scala.collection.mutable

object TrackProcessor {

    def getTop10LongestPlayListTracks(playListId: String): Option[List[Track]] = {
        val spark = SparkSession.builder()
            .appName("Spotify Top 10 Longest PlayList Tracks")
            .master("local[*]")
            .getOrCreate()

        spark.sparkContext.setLogLevel("ERROR") // Ignore all the INFO and WARN messages

        val ssc = new StreamingContext(spark.sparkContext, Seconds(Config.trackProcessorBatchDuration))
        val rddQueue = new mutable.Queue[org.apache.spark.rdd.RDD[Track]]()

        val inputStream = ssc.queueStream(rddQueue)

        implicit val trackOrdering: Ordering[Track] = Ordering.by(_.duration_ms)
        val top10LongestTracks = mutable.PriorityQueue.empty[Track]

        inputStream.foreachRDD { rdd =>
            val batchTracks = rdd.collect()
            top10LongestTracks ++= batchTracks
            while (top10LongestTracks.size > 10) {
                top10LongestTracks.dequeue()
            }
        }

        ssc.start()

        var offset = 0
        var totalTracks = 0
        val batchSize = Config.trackProcessorBatchSize

        do {
            SpotifyClient.getPlayListTracks(playListId, offset * batchSize, batchSize) match {
                case Some(playlistTracksPart) =>
                    totalTracks = playlistTracksPart.total
                    rddQueue += ssc.sparkContext.parallelize(playlistTracksPart.tracks)
                    offset += 1
                case None => return None
            }
        } while (offset * batchSize < totalTracks)

        ssc.awaitTerminationOrTimeout(30000) // 30 seconds

        spark.stop()

        Some(top10LongestTracks.dequeueAll.toList)
    }

}
