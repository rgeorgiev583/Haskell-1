type Title  = String
type Album  = String
type Genre  = String
type Duration = Int

type Name        = String
type BirthYear   = Int
type RecordLabel = String

data Artist = Artist
    {
        name        :: Name,
        birthYear   :: BirthYear,
        recordLabel :: RecordLabel
    }
    deriving (Show)

data Song = Song
    {
        title    :: Title,
        artist   :: Artist,
        album    :: Album,
        genre    :: Genre,
        duration :: Duration
    }
    deriving (Show)

type MusicLibrary = [Song]

compareArtists :: Artist -> Artist -> Bool
compareArtists
        (Artist {name = firstName, birthYear = firstBirthYear, recordLabel = firstRecordLabel})
        (Artist {name = secondName, birthYear = secondBirthYear, recordLabel = secondRecordLabel}) =
        firstName == secondName && firstBirthYear == secondBirthYear && firstRecordLabel == secondRecordLabel

compareSongs :: Song -> Song -> Bool
compareSongs
        (Song {title =  firstTitle, artist =  firstArtist, album =  firstAlbum,
               genre =  firstGenre, duration =  firstDuration})
        (Song {title = secondTitle, artist = secondArtist, album = secondAlbum,
               genre = secondGenre, duration = secondDuration}) =
                        firstTitle  == secondTitle  &&
        compareArtists  firstArtist    secondArtist &&
                        firstAlbum  == secondAlbum  &&
                        firstGenre  == secondGenre  &&
                        firstDuration == secondDuration

addSong :: Song -> MusicLibrary -> MusicLibrary
addSong song lib = lib ++ [song]

removeSong :: Song -> MusicLibrary -> MusicLibrary
removeSong song lib = filter (compareSongs song) lib

removeSongsByTitle :: Title -> MusicLibrary -> MusicLibrary
removeSongsByTitle title lib = filter (\(Song {title = x}) -> x /= title) lib

removeSongsByArtist :: Artist -> MusicLibrary -> MusicLibrary
removeSongsByArtist artist lib = filter (\(Song {artist = x}) -> not (compareArtists x artist)) lib

removeSongsByAlbum :: Album -> MusicLibrary -> MusicLibrary
removeSongsByAlbum album lib = filter (\(Song {album = x}) -> x /= album) lib

removeSongsByGenre :: Genre -> MusicLibrary -> MusicLibrary
removeSongsByGenre genre lib = filter (\(Song {genre = x}) -> x /= genre) lib

removeSongsByDuration :: Duration -> MusicLibrary -> MusicLibrary
removeSongsByDuration duration lib = filter (\(Song {duration = x}) -> x /= duration) lib

getSongsByTitle :: Title -> MusicLibrary -> MusicLibrary
getSongsByTitle title lib = filter (\(Song {title = x}) -> x == title) lib

getSongsByArtist :: Artist -> MusicLibrary -> MusicLibrary
getSongsByArtist artist lib = filter (\(Song {artist = x}) -> compareArtists x artist) lib

getSongsByAlbum :: Album -> MusicLibrary -> MusicLibrary
getSongsByAlbum album lib = filter (\(Song {album = x}) -> x == album) lib

getSongsByGenre :: Genre -> MusicLibrary -> MusicLibrary
getSongsByGenre genre lib = filter (\(Song {genre = x}) -> x == genre) lib

getSongsByDuration :: Duration -> MusicLibrary -> MusicLibrary
getSongsByDuration duration lib = filter (\(Song {duration = x}) -> x == duration) lib

getSongTitle :: Song -> Title
getSongTitle (Song {title = x}) = x

getSongArtist :: Song -> Artist
getSongArtist (Song {artist = x}) = x

getSongAlbum :: Song -> Album
getSongAlbum (Song {album = x}) = x

getSongGenre :: Song -> Genre
getSongGenre (Song {genre = x}) = x

getSongDuration :: Song -> Duration
getSongDuration (Song {duration = x}) = x

getArtistName :: Artist -> Name
getArtistName (Artist {name = x}) = x

getArtistBirthYear :: Artist -> BirthYear
getArtistBirthYear (Artist {birthYear = x}) = x

getArtistRecordLabel :: Artist -> RecordLabel
getArtistRecordLabel (Artist {recordLabel = x}) = x
