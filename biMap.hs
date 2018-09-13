import qualified Data.List as List
import qualified Data.Map as Map

artisteToGenre = Map.fromList [("fraise",["rock"]),("beatles", ["rock","blues"]),("metallica", ["rock"]),("bach",["classique"])]


genreToArtiste = Map.fromList $ zip uniqueGenres (map (getKeysWithValue artisteToGenre) uniqueGenres)

uniqueGenres = List.nub $ concat $ Map.elems artisteToGenre

getKeysWithValue m v = Map.keys $ Map.filterWithKey (\_ val -> elem v val) m
