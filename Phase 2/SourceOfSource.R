#....... Packages ......

library( twitteR ) ; library( RCurl ) ; library( RJSONIO ) ; library( stringr ) ; library( ggplot2 )

library( ggthemes ) ; library( ggmap ) ; library( plotly ) ; library( tm ) ; library( wordcloud ) ; library( RColorBrewer )

library( rvest ) ; library( plyr ) ; library( dplyr ) ; library( stringr )  ; library( plotly ) ; library( leaflet )

library( shinydashboard ) ; library( DT ) ; library( Rfacebook ) ; library( qdapRegex ) ; library( maps )


#....... Loading lexicon of positive and negative words (from Neal Caren) .......

set.seed( 123 )

lexicon = read.csv( 'lexicon.csv' , stringsAsFactors = F )

lexicon_polarity = read.csv( "subjectivity.csv", header=FALSE )

lexicon_emotion = read.csv("emotions.csv",header=FALSE)

twitter_insights_original = read.csv( 'twitter_insights_original.csv', stringsAsFactors = F )

twitter_insights_original_0 = mutate( twitter_insights_original, source = 'twitter' )

combined_df = bind_rows( twitter_insights_original_0 )

#....... Scripts defined by us .......

source( 'Twitter_Authentication.R' )       #....... twitter authentication

source( 'logic_Sentiment_Analysis.R' )     #....... url related functions


