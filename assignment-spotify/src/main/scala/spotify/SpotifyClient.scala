package spotify

import common.ApiClient
import spotify.dao.Track
import spotify.dao.Artist
import upickle.default.read

import scala.util.{Failure, Success}

object SpotifyClient {

    def getPlayListTracks(playListId: String, offset: Int = 0, limit: Int = 25): Option[List[Track]] = {
        val playListUrl = s"https://api.spotify.com/v1/playlists/$playListId/tracks?offset=$offset&limit=$limit"
        val response = ApiClient.get(
            playListUrl,
            headers = Map("Authorization" -> s"Bearer ${AuthService.getToken}")
        )

        response match {
            case Success(body) =>
                val json = ujson.read(body)
                val tracks = json("items").arr.map {
                    item =>
                        val trackJson = item("track")
                        val artistIds = trackJson("artists").arr.map(artist => artist("id").str).toList
                        Track(
                            trackJson("id").str,
                            trackJson("name").str,
                            trackJson("duration_ms").num.toInt,
                            artistIds
                        )
                }.toList
                Some(tracks)
            case Failure(_) => None
        }
    }

    def getArtist(artistId: String): Option[List[Artist]] = {
        val artistUrl = s"https://api.spotify.com/v1/artists/$artistId"
        val response = ApiClient.get(
            artistUrl,
            headers = Map("Authorization" -> s"Bearer ${AuthService.getToken}")
        )

        response match {
            case Success(body) =>
                val json = ujson.read(body)
                val artist = Artist(
                    json("id").str,
                    json("name").str,
                    json("followers")("total").num.toInt
                )
                Some(List(artist))
            case Failure(_) => None
        }
    }
}
