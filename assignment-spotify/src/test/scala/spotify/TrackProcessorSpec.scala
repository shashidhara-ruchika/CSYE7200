package spotify

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spotify.dao.Track

class TrackProcessorSpec extends AnyFlatSpec with Matchers {

    "TrackProcessor" should "return top 10 longest tracks" in {
        val playListId = "5Rrf7mqN8uus2AaQQQNdc1"

        // Invoke the TrackProcessor
        val result = TrackProcessor.getTop10LongestPlayListTracks(playListId)

        // Verify the result
        result shouldBe defined
        result.get should have size 10
        println(result.get)
    }
}