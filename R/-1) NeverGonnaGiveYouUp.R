library(rhdf5)
mainmenu <- h5ls("/Users/binKarim/Desktop/FYP Datasets/MSD/TRAXLZU12903D05F94.h5")

analysis.songs <- h5read("/Users/binKarim/Desktop/FYP Datasets/MSD/TRAXLZU12903D05F94.h5","/analysis/songs")
analysis.timbre <- h5read("/Users/binKarim/Desktop/FYP Datasets/MSD/TRAXLZU12903D05F94.h5","/analysis/segments_timbre")
analysis.pitches <- h5read("/Users/binKarim/Desktop/FYP Datasets/MSD/TRAXLZU12903D05F94.h5","/analysis/segments_pitches")

metadata.artist_terms <- h5read("/Users/binKarim/Desktop/FYP Datasets/MSD/TRAXLZU12903D05F94.h5","/metadata/artist_terms")
metadata.similar_artists <- h5read("/Users/binKarim/Desktop/FYP Datasets/MSD/TRAXLZU12903D05F94.h5","/metadata/similar_artists")
metadata.songs <- h5read("/Users/binKarim/Desktop/FYP Datasets/MSD/TRAXLZU12903D05F94.h5","/metadata/songs")

musicbrainz.artist_mbtags <- h5read("/Users/binKarim/Desktop/FYP Datasets/MSD/TRAXLZU12903D05F94.h5","/musicbrainz/artist_mbtags")
musicbrainz.songs <- h5read("/Users/binKarim/Desktop/FYP Datasets/MSD/TRAXLZU12903D05F94.h5","/musicbrainz/songs")

#################################################

mainmenu

analysis.songs
analysis.timbre
analysis.pitches

metadata.artist_terms ###
metadata.similar_artists
metadata.songs
metadata.songs$release
metadata.songs$release_7digitalid

musicbrainz.artist_mbtags
musicbrainz.songs


ooops <- matrix(0,nrow=4,ncol=6)
ooops[2,1:length(musicbrainz.artist_mbtags)] <- musicbrainz.artist_mbtags




sh5read("/Users/binKarim/Desktop/FYP Datasets/MSD/TRAXLZU12903D05F94.h5","/analysis/bars_confidence")
