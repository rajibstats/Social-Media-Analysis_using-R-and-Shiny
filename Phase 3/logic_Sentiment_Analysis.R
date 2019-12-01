#...... Logic of Sentiment Analysis tab .........

#...... Function to check if there are any invalid multibyte string in a character vector ......

has.invalid.multibyte.string  <- function(x,return.elements=F)
{
  # determine if "invalid multibyte string" error will be triggered
  # if return.elements=T, then output is logical along x, otherwise single logical
  if (is.null(x))
    return(F)
  if (return.elements)
  {
    n <- length(x)
    out <- rep(F,n)
    for (i in 1:n)
      out[i] <- is.error(try(toupper(x[i]),silent = T))
  }
  else
    out <- is.error(try(toupper(x),silent = T))
  return(out)
}

is.error <- function(x)
{
  # test output of try()
  return(class(x)[1]=="try-error")
}

#........ Function to clean text .........

clean.text <- function(some_txt)
{  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)

# Remove the text which start from "@"
some_txt = gsub("@\\w+", "", some_txt)

# Remove punctuations
some_txt = gsub("[[:punct:]]", "", some_txt)

#Remove Digits
some_txt = gsub("[[:digit:]]", "", some_txt)

#Remove links
some_txt = gsub("http\\w+", "", some_txt)

# remove extra white spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)


# Remove non-english characters
some_txt = gsub("[^\x20-\x7E]", "", some_txt)

# define "tolower error handling" function
try.tolower = function(x)
{  y = NA
try_error = tryCatch(tolower(x), error=function(e) e)
if (!inherits(try_error, "error"))
  y = tolower(x)
return(y)
}

some_txt = sapply(some_txt, try.tolower)
some_txt = some_txt[some_txt != ""]
names(some_txt) = NULL
return(some_txt)}

f_clean_location <- function (clean_tweets) {
  
  # remove retweet entities
  clean_tweets = gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', clean_tweets)
  # remove at people
  clean_tweets = gsub('@\\w+', '', clean_tweets)
  # remove punctuation
  clean_tweets = gsub('[[:punct:]]', '', clean_tweets)
  # remove numbers
  clean_tweets = gsub('[[:digit:]]', '', clean_tweets)
  # remove html links
  clean_tweets = gsub('http\\w+', '', clean_tweets)
  # remove unnecessary spaces
  clean_tweets = gsub('[ \t]{2,}', '', clean_tweets)
  clean_tweets = gsub('^\\s+|\\s+$', '', clean_tweets)
  # remove emojis or special characters
  clean_tweets = gsub('<.*>', '', enc2native(clean_tweets))
  
  clean_tweets = tolower(clean_tweets)
  
  clean_tweets
}

#.......... Function for finding sentiment score ........

score.sentiment = function(sentences, pos.words, neg.words,negation.words, .progress='none'){
  
  nscores = laply(sentences, function(sentence, pos.words, neg.words, negation.words) {
    
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # compare our words with list of negation words
    negation=match(words,negation.words)
    
    
    # match() returns the position of the matched term or NA
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    negation=!is.na(negation)
    
    # calculation of score
    score= sum(pos.matches)-sum(neg.matches)
    
    # calculation for negation handling
    score1=score
    if(score<0)
      nscore=score1+sum(negation)
    else 
      nscore=score1-sum(negation)
    return(nscore)
  }, pos.words, neg.words,negation.words, .progress=.progress )
  
  scores.df = data.frame(score=nscores, text=sentences)
  return(scores.df)
}

#...... Calculation .........

Sentiment_Output_Twitter = function( twitter_query, twitter_no_of_tweets_to_fetch, twitter_from_date, twitter_to_date ){
 
  tweets_list = searchTwitter( twitter_query, n = twitter_no_of_tweets_to_fetch, lang = 'en', since = twitter_from_date, until = twitter_to_date )
  
  if( length( tweets_list ) > 0 ){    #.... ensuring at least one tweet is found
    
    tweets_df = twListToDF( tweets_list )
    
    
    #....... Row indexes which has invalid multibyte stings .......
    
    invalid_row_indexes = which( has.invalid.multibyte.string( tweets_df$text, return.elements = T ) )
    
    if( length( invalid_row_indexes ) > 0 ){
      
      final_tweets_df = tweets_df[ -invalid_row_indexes, ]
      
    } else{ final_tweets_df = tweets_df }
    
    positive_words = lexicon$word[ lexicon$polarity == 'positive' ]
    
    negative_words = lexicon$word[ lexicon$polarity == 'negative' ]
    
    negation_words = NULL
    
    sentiment_score_df_0 = score.sentiment( final_tweets_df$text, positive_words, negative_words, negation_words )
    
    sentiment_score_df_1 = inner_join( sentiment_score_df_0, final_tweets_df, by = 'text' )
    
    sentiment_score_df_2 = sentiment_score_df_1[ c( 'created', 'screenName', 'text', 'favorited', 'favoriteCount', 'retweetCount', 'isRetweet', 'retweeted', 'statusSource', 'score' ) ]
    
    sentiment_score_df_3 = sentiment_score_df_2[ !duplicated( sentiment_score_df_2$text ), ]
    
    sentiment_score_df_4 = sentiment_score_df_3 %>% 
      
      mutate( source_device = case_when( grepl( 'android', tolower( sentiment_score_df_3$statusSource ) ) ~ 'Android',
                                         
                                         grepl( 'iphone', tolower( sentiment_score_df_3$statusSource ) ) ~ 'Iphone',
                                         
                                         grepl( 'ipad', tolower( sentiment_score_df_3$statusSource ) ) ~ 'IPad',
                                         
                                         grepl( 'ios', tolower( sentiment_score_df_3$statusSource ) ) ~ 'iOS',
                                         
                                         grepl( 'web', tolower( sentiment_score_df_3$statusSource ) ) ~ 'Web',
                                         
                                         TRUE ~ 'Other' ) ) %>% select( - 'statusSource' )
    
    sentiment_score_df = sentiment_score_df_4 %>%
      
      mutate( polarity = case_when( score > 0  ~ 'Positive',
                                    
                                    score < 0  ~ 'Negative',
                                    
                                    TRUE ~ 'Neutral' ) )
    
    total_rows = nrow( sentiment_score_df )
    
    positive_score_percent = round( 100*length( which( sentiment_score_df$score > 0 ) )/total_rows, 2 )
    
    neutral_score_percent = round( 100*length( which( sentiment_score_df$score == 0 ) )/total_rows, 2 )
    
    negative_score_percent = round( 100*length( which( sentiment_score_df$score < 0 ) )/total_rows, 2 )
    
    sentiment_distribution = data.frame( Sentiment = c( 'Positive', 'Neutral', 'Negative' ),
                                         
                                         Percentage = c( positive_score_percent, neutral_score_percent, negative_score_percent ) )
    
    reach_count = sum( tweets_df$retweetCount ) + nrow( tweets_df )
    
    domain_names_stats = table( paste0( na.omit( unlist( ex_between( tweets_df$statusSource, '<a href=\"', '.com' ) ) ), '.com' ) ) %>%
      
      as.data.frame() %>% `colnames<-` ( c( 'Domains', 'Count' ) ) %>% arrange( desc( Count ) )
    
    top_users = table( tweets_df$screenName ) %>% as.data.frame() %>% `colnames<-` ( c( 'Users', 'Count' ) ) %>% arrange( desc( Count ) ) %>% head( 5 )
    
    source_device_stats = round( 100*prop.table( table( sentiment_score_df$source_device ) ) , 2 ) %>% as.data.frame() %>% `colnames<-` ( c( 'Source_Device', 'Percentage' ) )
    
    share_of_posts_stats = round( 100*prop.table( table( sentiment_score_df$isRetweet ) ) , 2 ) %>% as.data.frame() %>% `colnames<-` ( c( 'Post_Type', 'Percentage' ) )
    
    share_of_posts_stats$Post_Type = ifelse( as.logical( share_of_posts_stats$Post_Type ), 'Retweet', 'Original Post' )
    
    top_posts_df_0 = tweets_df %>% arrange( desc( retweetCount ) ) %>% select( text, retweetCount, screenName ) %>% `colnames<-` ( c( 'Tweets', 'retweetCount', 'screenName' ) )
      
    top_posts_df = top_posts_df_0[ !duplicated( top_posts_df_0[ c( 'Tweets', 'retweetCount' ) ] ), ] %>% head(5)
    
    unique_users = length( unique( tweets_df$screenName ) )
    
    ### Calculation for location df #############
    
    userInfo <- lookupUsers(tweets_df$screenName)  # Batch lookup of user info
    
    userFrame <- twListToDF(userInfo)  # Convert to a nice dF
    
    if( F ){
    
    ### Location latitude longitude dataframe ###
    
    data_loc=data.frame(userFrame$location) 
    
    names(data_loc)=c("loc")
    
    data_loc[data_loc==""] <- NA
    
    data1_loc <-as.data.frame(na.omit(data_loc))
    
    ## Remove duplicate locations 
    
    data2_loc=data1_loc %>% distinct(loc)
    
    data3_loc=as.data.frame(data1_loc[!duplicated(data1_loc), ])
    
    names(data3_loc)=c("loc")
   
    register_google(key = "AIzaSyDfYOhXSS2C2LvZxssDndcc6B0fcKSTpek")
    
    lonlat= geocode(as.character(data2_loc$loc))
    
    cities1=cbind(data2_loc, lonlat)
    
    cities1 = cities1[complete.cases(cities1), ]
    
    cities1$lon = as.numeric(cities1$lon); cities1$lat = as.numeric(cities1$lat)
    
    }
    
    #locatedUsers <- !is.na(userFrame$location)  # Keep only users with location info
    
    if( length( which(userFrame$location == "" ) ) != 0 ){
    
    userFrame = userFrame[ -which(userFrame$location == "" ), ]
    
    } else{ userFrame = userFrame }
    
    userFrame$location = f_clean_location( userFrame$location )
    
    if( length( which(userFrame$location == "" ) ) != 0 ){
      
      userFrame = userFrame[ -which(userFrame$location == "" ), ]
      
    } else{ userFrame = userFrame }
    
    row.names(userFrame) = NULL
    
    userFrame = userFrame[ , c( "screenName", "name", "location" ) ]
    
    return( list( 'sentiment_score_df' = sentiment_score_df, 'sentiment_distribution' = sentiment_distribution, 'domain_names_stats' = domain_names_stats, 'unique_users' = unique_users,
                  
                  'source_device_stats' = source_device_stats, 'share_of_posts_stats' = share_of_posts_stats, 'reach_count' = reach_count, 'top_users' = top_users, 'top_posts_df' = top_posts_df, 
                  
                   'location_df' = userFrame, 'raw_data' = tweets_df #, 'location_lat_lon_df' = cities1 
                  
                  ) )
    
  } else {    #... when no tweets is found
    
    return( NULL )
    
  }
  
}

#...... Tweets and Sentiment DF for output .........

Fetched_Tweets_Sentiment_DF = function( sentiment_score_df ){
  
  tweet_sentiment = ifelse( sentiment_score_df$score > 0, 'Positive', ifelse( sentiment_score_df$score == 0, 'Neutral', 'Negative' ) )
  
  sentiment_df = data.frame( 'screenName' = sentiment_score_df$screenName, 'created' = sentiment_score_df$created, 
                             
                             'text' = sentiment_score_df$text, 'polarity' = tweet_sentiment )
  
  return( sentiment_df )
  
}


#...... Pie chart ........

#....... Function for donut and pie together ........

#' x      numeric vector for each slice
#' group  vector identifying the group for each slice
#' labels vector of labels for individual slices
#' col    colors for each group
#' radius radius for inner and outer pie (usually in [0,1])
#' plot_title title over plot

donuts <- function(x, group = 1, labels = NA, col = NULL, radius = c( 1, 0.5 ), title = NA ) {
  group <- rep_len(group, length(x))
  ug  <- unique(group)
  tbl <- table(group)[order(ug)]
  
  col <- if (is.null(col))
    seq_along(ug) else rep_len(col, length(ug))
  col.main <- Map(rep, col[seq_along(tbl)], tbl)
  col.sub  <- lapply(col.main, function(x) {
    al <- head(seq(0, 1, length.out = length(x) + 2L)[-1L], -1L)
    Vectorize(adjustcolor)(x, alpha.f = al)
  })
  
  plot.new()
  
  par(new = TRUE)
  pie(x, border = NA, radius = radius[2L], col = unlist(col.sub), labels = labels, main = title )
  
  par(new = TRUE)
  pie(x, border = NA, radius = radius[1L], col = unlist(col.main), labels = NA)
}

Tweet_Sentiment_Pie_Chart = function( twitter_query, values, group, pie_colors ){
  
  # plot_title = paste0( 'Pie Chart of Sentiment Analysis for ', twitter_query )
  
  return(
    
    donuts( values, group = group, labels = group, radius = c( 0.7, 1 ), col = pie_colors )
    
  )
  
}


#....... Bar Plot ........

Twitter_Sentiment_Bar_Plot = function( input_df, twitter_query, bar_colors, horizontal = F ){
  
  # plot_title = paste0( 'Bar Diagram of Sentiment Analysis for ', twitter_query )
  
  bar_plot_output = ggplot( input_df, aes( x = Type, y = Values, fill = Type ) ) +
    
    geom_bar( stat = 'identity', width = 0.6 ) + scale_fill_manual( values = bar_colors ) + guides( fill = F ) +
    
    xlab( 'Type' ) + ylab( 'Percentage' ) +
    
    theme( plot.title = element_text( size = 12, colour = "firebrick", face = 'bold' ), axis.text = element_text( colour = "firebrick", face = 'bold' ), 
           
           axis.title.y = element_text( size = 12, face = 'bold' ), axis.title.x = element_text(size = 12, face = 'bold') ) + theme_economist()
  
  if( horizontal ){ bar_plot_output = bar_plot_output + coord_flip() }
  
  return( bar_plot_output )
  
}


#....... Word Cloud generation ......

Word_Cloud_Logic = function( sentiment_score_df ){
  
  sentiment_output = sentiment_score_df
  
  text = Corpus( VectorSource( sentiment_output$text ) )
  
  text_data = tm_map( text, removeWords, stopwords( "english" ) )
  
  tdm_text_data = TermDocumentMatrix( text_data )    #..... creates a TDM
  
  TDM1 = as.matrix( tdm_text_data )    #..... convert this into a matrix format
  
  v = sort( rowSums( TDM1 ), decreasing = T )     #.... gives frequencies of every word
  
  d = data.frame( word = names(v), freq = v )
  
  effective_data = head( d, n = 100 )
  
  return( list( 'effective_data' = effective_data ) )
  
}

#...... user_informtion #..........
user_table=function(twitter_query, number_of_tweets_to_fetch, from_date, to_date){
  
  tweetFrame = combined_tweets_df
  
  userInfo <- lookupUsers(tweetFrame$screenName)  # Batch lookup of user info
  userFrame <- twListToDF(userInfo)  # Convert to a nice dF
  
  return(userFrame)
  
}

Sentiment_Output_Consumer_Insights <- function( all_reviews_data ) {
  
  #....... Row indexes which has invalid multibyte stings .......
  
  invalid_row_indexes = which( has.invalid.multibyte.string( all_reviews_data$review, return.elements = T ) )
  
  if( length( invalid_row_indexes ) > 0 ){
    
    final_reviews_df = all_reviews_data[ -invalid_row_indexes, ]
    
  } else{ final_reviews_df = all_reviews_data }
  
  positive_words = lexicon$word[ lexicon$polarity == 'positive' ]
  
  negative_words = lexicon$word[ lexicon$polarity == 'negative' ]
  
  negation_words = NULL
  
  sentiment_score_df_0 = score.sentiment( final_reviews_df$review, positive_words, negative_words, negation_words )
  
  names( final_reviews_df )[ which( names( final_reviews_df ) == 'review' ) ] = c( 'text' )
  
  names( final_reviews_df )[ which( names( final_reviews_df ) == 'date_created' ) ] = c( 'created_time' )
  
  sentiment_score_df_1 = inner_join( sentiment_score_df_0, final_reviews_df, by = 'text' )
  
  sentiment_score_df_2 = sentiment_score_df_1[ c( 'reviewer_details', 'text', 'created_time', 'score' ) ]
  
  sentiment_score_df = sentiment_score_df_2[ !duplicated( sentiment_score_df_2$text ), ]
  
  sentiment_score_df = sentiment_score_df %>%
    
    mutate( polarity = case_when( score > 0  ~ 'Positive',
                                  
                                  score < 0  ~ 'Negative',
                                  
                                  TRUE ~ 'Neutral' ) )
  
  # sentiment_score_df$created_time = as.Date( sentiment_score_df$created_time )
  
  total_rows = nrow( sentiment_score_df )
  
  positive_score_percent = round( 100*length( which( sentiment_score_df$score > 0 ) )/total_rows, 2 )
  
  neutral_score_percent = round( 100*length( which( sentiment_score_df$score == 0 ) )/total_rows, 2 )
  
  negative_score_percent = round( 100*length( which( sentiment_score_df$score < 0 ) )/total_rows, 2 )
  
  sentiment_distribution = data.frame( Sentiment = c( 'Positive', 'Neutral', 'Negative' ),
                                       
                                       Percentage = c( positive_score_percent, neutral_score_percent, negative_score_percent ) )
  
  return( list( 'sentiment_score_df' = sentiment_score_df, 'sentiment_distribution' = sentiment_distribution ) )
  
}

Sentiment_Output_fb = function( comments_data, posts_data ){
  
  #....... Row indexes which has invalid multibyte stings .......
  
  invalid_row_indexes = which( has.invalid.multibyte.string( comments_data$message, return.elements = T ) )
  
  if( length( invalid_row_indexes ) > 0 ){
    
    final_comments_df = comments_data[ -invalid_row_indexes, ]
    
  } else{ final_comments_df = comments_data }
  
  positive_words = lexicon$word[ lexicon$polarity == 'positive' ]
  
  negative_words = lexicon$word[ lexicon$polarity == 'negative' ]
  
  negation_words = NULL
  
  sentiment_score_df_0 = score.sentiment( final_comments_df$message, positive_words, negative_words, negation_words )
  
  names( final_comments_df )[ which( names( final_comments_df ) == 'message' ) ] = c( 'text' )
  
  sentiment_score_df_1 = inner_join( sentiment_score_df_0, final_comments_df, by = 'text' )
  
  sentiment_score_df_2 = sentiment_score_df_1[ c( 'Post', 'text', 'created_time', 'id', 'likes_count', 'score' ) ]
  
  sentiment_score_df = sentiment_score_df_2[ !duplicated( sentiment_score_df_2$text ), ]
  
  sentiment_score_df = sentiment_score_df %>%
    
    mutate( polarity = case_when( score > 0  ~ 'Positive',
                                  
                                  score < 0  ~ 'Negative',
                                  
                                  TRUE ~ 'Neutral' ) )
  
  sentiment_score_df$created_time = as.Date( sentiment_score_df$created_time )
  
  total_rows = nrow( sentiment_score_df )
  
  positive_score_percent = round( 100*length( which( sentiment_score_df$score > 0 ) )/total_rows, 2 )
  
  neutral_score_percent = round( 100*length( which( sentiment_score_df$score == 0 ) )/total_rows, 2 )
  
  negative_score_percent = round( 100*length( which( sentiment_score_df$score < 0 ) )/total_rows, 2 )
  
  sentiment_distribution = data.frame( Sentiment = c( 'Positive', 'Neutral', 'Negative' ),
                                       
                                       Percentage = c( positive_score_percent, neutral_score_percent, negative_score_percent ) )
  
  top_posts_df = posts_data %>% mutate( interactions = likes_count + comments_count + shares_count + love_count + haha_count + wow_count + sad_count + angry_count ) %>%
    
    arrange( desc( interactions ) ) %>% head( 5 ) %>% select( message, interactions ) %>% `colnames<-` ( c( 'Posts', 'Reactions' ) )
  
  fb_posts_type = round( 100*prop.table( table( posts_data$type ) ) , 2 ) %>% as.data.frame() %>% `colnames<-` ( c( 'Post Type', 'Percentage' ) )
  
  return( list( 'sentiment_score_df' = sentiment_score_df, 'sentiment_distribution' = sentiment_distribution, 'top_posts_df' = top_posts_df,

                'fb_posts_type' = fb_posts_type ) )
  
}

#### Functions polarity analysis of twitter

create_matrix <- function(textColumns, language="english", minDocFreq=1, minWordLength=3, removeNumbers=TRUE, removePunctuation=TRUE, removeSparseTerms=0, removeStopwords=TRUE, stemWords=FALSE, stripWhitespace=TRUE, toLower=TRUE, weighting=weightTf) {
  
  stem_words <- function(x) {
    split <- strsplit(x," ")
    return(wordStem(split[[1]],language=language))
  }
  
  control <- list(language=language,tolower=toLower,removeNumbers=removeNumbers,removePunctuation=removePunctuation,stripWhitespace=stripWhitespace,minWordLength=minWordLength,stopwords=removeStopwords,minDocFreq=minDocFreq,weighting=weighting)
  
  if (stemWords == TRUE) control <- append(control,list(stemming=stem_words),after=6)
  
  trainingColumn <- apply(as.matrix(textColumns),1,paste,collapse=" ")
  trainingColumn <- sapply(as.vector(trainingColumn,mode="character"),iconv,to="UTF8",sub="byte")
  
  corpus <- Corpus(VectorSource(trainingColumn),readerControl=list(language=language))
  matrix <- DocumentTermMatrix(corpus,control=control);
  if (removeSparseTerms > 0) matrix <- removeSparseTerms(matrix,removeSparseTerms)
  
  gc()
  return(matrix)
}

classify_polarity <- function(textColumns,algorithm="bayes",pstrong=0.5,pweak=1.0,prior=1.0,verbose=FALSE,...) {
  matrix <- create_matrix(textColumns,...)
  lexicon <- lexicon_polarity
  
  counts <- list(positive=length(which(lexicon[,3]=="positive")),negative=length(which(lexicon[,3]=="negative")),total=nrow(lexicon))
  documents <- c()
  
  for (i in 1:nrow(matrix)) {
    if (verbose) print(paste("DOCUMENT",i))
    scores <- list(positive=0,negative=0)
    doc <- matrix[i,]
    words <- findFreqTerms(doc,lowfreq=1)
    
    for (word in words) {
      index <- pmatch(word,lexicon[,1],nomatch=0)
      if (index > 0) {
        entry <- lexicon[index,]
        
        polarity <- as.character(entry[[2]])
        category <- as.character(entry[[3]])
        count <- counts[[category]]
        
        score <- pweak
        if (polarity == "strongsubj") score <- pstrong
        if (algorithm=="bayes") score <- abs(log(score*prior/count))
        
        if (verbose) {
          print(paste("WORD:",word,"CAT:",category,"POL:",polarity,"SCORE:",score))
        }
        
        scores[[category]] <- scores[[category]]+score
      }		
    }
    
    if (algorithm=="bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]]+score
      }
    } else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]]+0.000001
      }
    }
    
    best_fit <- names(scores)[which.max(unlist(scores))]
    ratio <- as.integer(abs(scores$positive/scores$negative))
    if (ratio==1) best_fit <- "neutral"
    documents <- rbind(documents,c(scores$positive,scores$negative,abs(scores$positive/scores$negative),best_fit))
    if (verbose) {
      print(paste("POS:",scores$positive,"NEG:",scores$negative,"RATIO:",abs(scores$positive/scores$negative)))
      cat("\n")
    }
  }
  
  colnames(documents) <- c("POS","NEG","POS/NEG","BEST_FIT")
  return(documents)
}

classify_emotion <- function(textColumns,algorithm="bayes",prior=1.0,verbose=FALSE,...) {
  matrix <- create_matrix(textColumns,...)
  lexicon <- lexicon_emotion
  
  counts <- list(anger=length(which(lexicon[,2]=="anger")),disgust=length(which(lexicon[,2]=="disgust")),fear=length(which(lexicon[,2]=="fear")),joy=length(which(lexicon[,2]=="joy")),sadness=length(which(lexicon[,2]=="sadness")),surprise=length(which(lexicon[,2]=="surprise")),total=nrow(lexicon))
  documents <- c()
  
  for (i in 1:nrow(matrix)) {
    if (verbose) print(paste("DOCUMENT",i))
    scores <- list(anger=0,disgust=0,fear=0,joy=0,sadness=0,surprise=0)
    doc <- matrix[i,]
    words <- findFreqTerms(doc,lowfreq=1)
    
    for (word in words) {
      for (key in names(scores)) {
        emotions <- lexicon[which(lexicon[,2]==key),]
        index <- pmatch(word,emotions[,1],nomatch=0)
        if (index > 0) {
          entry <- emotions[index,]
          
          category <- as.character(entry[[2]])
          count <- counts[[category]]
          
          score <- 1.0
          if (algorithm=="bayes") score <- abs(log(score*prior/count))
          
          if (verbose) {
            print(paste("WORD:",word,"CAT:",category,"SCORE:",score))
          }
          
          scores[[category]] <- scores[[category]]+score
        }
      }
    }
    
    if (algorithm=="bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]]+score
      }
    } else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]]+0.000001
      }
    }
    
    best_fit <- names(scores)[which.max(unlist(scores))]
    if (best_fit == "disgust" && as.numeric(unlist(scores[2]))-3.09234 < .01) best_fit <- NA
    documents <- rbind(documents,c(scores$anger,scores$disgust,scores$fear,scores$joy,scores$sadness,scores$surprise,best_fit))
  }
  
  colnames(documents) <- c("ANGER","DISGUST","FEAR","JOY","SADNESS","SURPRISE","BEST_FIT")
  return(documents)
}

f_clean_tweets <- function (tweets) {
  
  clean_tweets = sapply(tweets, function(x) x$getText())
  # remove retweet entities
  clean_tweets = gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', clean_tweets)
  # remove at people
  clean_tweets = gsub('@\\w+', '', clean_tweets)
  # remove punctuation
  clean_tweets = gsub('[[:punct:]]', '', clean_tweets)
  # remove numbers
  clean_tweets = gsub('[[:digit:]]', '', clean_tweets)
  # remove html links
  clean_tweets = gsub('http\\w+', '', clean_tweets)
  # remove unnecessary spaces
  clean_tweets = gsub('[ \t]{2,}', '', clean_tweets)
  clean_tweets = gsub('^\\s+|\\s+$', '', clean_tweets)
  # remove emojis or special characters
  clean_tweets = gsub('<.*>', '', enc2native(clean_tweets))
  
  clean_tweets = tolower(clean_tweets)
  
  clean_tweets
}

myModal <- function() {
  
  div(id = "test",
      
      modalDialog(downloadButton("download1","Download Data as csv"),
                  
                  easyClose = TRUE, title = "Download Table")
      
  )
  
}

myModal_raw_data <- function() {
  
  div(id = "test_raw_data",
      
      modalDialog(downloadButton("download_raw_data","Download Data as csv"),
                  
                  easyClose = TRUE, title = "Download Table")
      
  )
  
}

FB_Sentiment_Pie_Chart = function( sentiment_distribution, twitter_query ){
  
  plot_title = paste0( 'Pie Chart of Sentiment Analysis for ', twitter_query )
  
  return(
    
    with( sentiment_distribution, donuts( values, group = sentiment, labels = sentiment, radius = c( 0.7, 1 ),
                                          
                                          col = c( 'green', 'orange', 'red' ), title = plot_title ) )
    
  )
  
}

#....... Bar Plot ........

FB_Sentiment_Bar_Plot = function( sentiment_distribution, twitter_query ){
  
  plot_title = paste0( 'Bar Diagram of Sentiment Analysis for ', twitter_query )
  
  bar_plot_output = ggplot( sentiment_distribution, aes( x = sentiment, y = values, fill = sentiment ) ) +
    
    geom_bar( stat = 'identity', width = 0.6 ) + scale_fill_manual( values = c( "firebrick1", "orange", "green" ) ) + guides( fill = F ) +
    
    xlab( 'Sentiment' ) + ylab( 'Percentage' ) + ggtitle( plot_title ) +
    
    theme( plot.title = element_text( size = 12, colour = "Black", face = 'bold' ), axis.text = element_text( colour = "Black", face = 'bold' ), 
           
           axis.title.y = element_text( size = 12, face = 'bold' ), axis.title.x = element_text(size = 12, face = 'bold') ) # +
  
  #  theme_solarized_2( light = F ) + scale_colour_solarized( "black" )
  
  return( bar_plot_output )
  
}


#..... word_cloud ..........


Sentiment_Output_wc_fb = function( comments_data ){
  
  
  
  #....... Row indexes which has invalid multibyte stings .......
  invalid_row_indexes = which( has.invalid.multibyte.string( comments_data$message, return.elements = T ) )
  
  if( length( invalid_row_indexes ) > 0 ){
    
    final_comments_df = comments_data[ -invalid_row_indexes, ]
    
  } else{ final_comments_df = comments_data }
  
  
  
  positive_words = lexicon$word[ lexicon$polarity == 'positive' ]
  
  negative_words = lexicon$word[ lexicon$polarity == 'negative' ]
  
  negation_words = NULL
  
  fb_cleaned = clean.text( comments_data$message )
  
  fb_cleaned_duplicates_removed = fb_cleaned[ !duplicated( fb_cleaned ) ]
  
  
  sentiment_score_df = as.data.frame(score.sentiment( fb_cleaned_duplicates_removed, positive_words, negative_words, negation_words ))
  
  return(sentiment_score_df)
  #return( data.frame( 'sentiment_score_df' = sentiment_score_df, 'sentiment_distribution' = sentiment_distribution ) )
  
}


