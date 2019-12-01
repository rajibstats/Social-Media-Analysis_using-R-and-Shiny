#..... Server logic ....

#..... Twitter .....

observeEvent( input$twitter_submit_button, {
  
  showNotification( strong( 'Fetching Twitter Data ...' ), type = 'message' )
  
})

observeEvent( input$keyword_submit_button, {
  
  showNotification( strong( 'Fetching Twitter Data ...' ), type = 'message' )
  
})

twitter_data = reactiveValues(sentiment_info = c())

# Sentiment Analysis for Hashtag input

observeEvent( input$twitter_submit_button, {
  
  twitter_query = input$twitter_query ; twitter_no_of_tweets_to_fetch = input$twitter_no_of_tweets_to_fetch
  
  twitter_from_date = as.character( input$twitter_from_date ) ; twitter_to_date = as.character( input$twitter_to_date )
  
  #... contains sentiment_score_df, sentiment_distribution, source_device_stats, share_of_posts_stats, reach_count, domain_names_stats, top_users, top_posts_df, unique_users
  
  sentiment_output = Sentiment_Output_Twitter( twitter_query, twitter_no_of_tweets_to_fetch, twitter_from_date, twitter_to_date )
  
  if( !is.null( sentiment_output ) ){
    
    word_cloud_effective_data = Word_Cloud_Logic( sentiment_output$sentiment_score_df )   #.... contains effective data
    
    #..... Sentiment Analysis Emotions
    
    some_tweets = searchTwitter( twitter_query, n = twitter_no_of_tweets_to_fetch, lang = 'en', since = twitter_from_date, until = twitter_to_date )
    
    clean_tweets <- f_clean_tweets(some_tweets)
    
    # removing duplicates due to retweets
    clean_tweets <- clean_tweets[!duplicated(clean_tweets)]
    
    # using sentiment package to classify emotions
    emotions <- classify_emotion(clean_tweets, algorithm='bayes')
    
    # using sentiment package to classify polarities
    polarities = classify_polarity(clean_tweets, algorithm='bayes')
    
    emotion_df = data.frame(text=clean_tweets, emotion=emotions[,'BEST_FIT'],
                    polarity=polarities[,'BEST_FIT'], stringsAsFactors=FALSE)
    #df[is.na(df)] <- "N.A."
    
    emotion_df = emotion_df[complete.cases(emotion_df), ]
    
    count_emotion_df = as.data.frame(table( emotion_df$emotion ) )
    
    names( count_emotion_df ) = c( "Emotions", "Count" )
    
    emotion_df_list = list( 'emotion_df' = emotion_df, 'count_emotion_df' = count_emotion_df )
    
    twitter_data$sentiment_info = c( sentiment_output, word_cloud_effective_data, emotion_df_list,list( 'twitter_query' = twitter_query ) )
    
    
  } else{ twitter_data$sentiment_info = NULL }
  
  return( twitter_data$sentiment_info )
  
})

# Sentiment Analysis for Keyword input

observeEvent( input$keyword_submit_button, {
  
  twitter_query = input$twitter_keyword_query ; twitter_no_of_tweets_to_fetch = input$no_of_tweets_to_fetch_keyword
  
  twitter_from_date = as.character( input$keyword_from_date ) ; twitter_to_date = as.character( input$keyword_to_date )
  
  #... contains sentiment_score_df, sentiment_distribution, source_device_stats, share_of_posts_stats, reach_count, domain_names_stats, top_users, top_posts_df, unique_users
  
  sentiment_output = Sentiment_Output_Twitter( twitter_query, twitter_no_of_tweets_to_fetch, twitter_from_date, twitter_to_date )
  
  if( !is.null( sentiment_output ) ){
    
    word_cloud_effective_data = Word_Cloud_Logic( sentiment_output$sentiment_score_df )   #.... contains effective data
    
    #..... Sentiment Analysis Emotions
    
    some_tweets = searchTwitter( twitter_query, n = twitter_no_of_tweets_to_fetch, lang = 'en', since = twitter_from_date, until = twitter_to_date )
    
    clean_tweets <- f_clean_tweets(some_tweets)
    
    # removing duplicates due to retweets
    clean_tweets <- clean_tweets[!duplicated(clean_tweets)]
    
    # using sentiment package to classify emotions
    emotions <- classify_emotion(clean_tweets, algorithm='bayes')
    
    # using sentiment package to classify polarities
    polarities = classify_polarity(clean_tweets, algorithm='bayes')
    
    emotion_df = data.frame(text=clean_tweets, emotion=emotions[,'BEST_FIT'],
                            polarity=polarities[,'BEST_FIT'], stringsAsFactors=FALSE)
    #df[is.na(df)] <- "N.A."
    
    emotion_df = emotion_df[complete.cases(emotion_df), ]
    
    count_emotion_df = as.data.frame(table( emotion_df$emotion ) )
    
    names( count_emotion_df ) = c( "Emotions", "Count" )
    
    emotion_df_list = list( 'emotion_df' = emotion_df, 'count_emotion_df' = count_emotion_df )
    
    twitter_data$sentiment_info = c( sentiment_output, word_cloud_effective_data, emotion_df_list,list( 'twitter_query' = twitter_query ) )
    
    
  } else{ twitter_data$sentiment_info = NULL }
  
  return( twitter_data$sentiment_info )
  
})

output$twitter_reach_infobox = renderInfoBox({
  
  if( !is.null( twitter_data$sentiment_info ) ){
    
    reach_count = as.character( twitter_data$sentiment_info$reach_count )
    
  } else{ reach_count = 'Oops! No data found' }
  
  infoBox( 'Reach', reach_count, icon = icon( 'users' ), color = 'green' )
  
})

output$twitter_tweets_infobox = renderInfoBox({
  
  if( !is.null( twitter_data$sentiment_info ) ){
    
    share_of_posts_stats_df = twitter_data$sentiment_info$share_of_posts_stats
    
    tweets_percent = as.character( share_of_posts_stats_df$Percentage[1] )
    
  } else{ tweets_percent = 'Oops! No data found' }
  
  infoBox( 'Tweets %' , tweets_percent, icon = icon( 'twitter' ), color = 'yellow' )
  
})

output$twitter_retweets_infobox = renderInfoBox({
  
  if( !is.null( twitter_data$sentiment_info ) ){
    
    share_of_posts_stats_df = twitter_data$sentiment_info$share_of_posts_stats
    
    retweets_percent = as.character( share_of_posts_stats_df$Percentage[2] )
    
  } else{ retweets_percent = 'Oops! No data found' }
  
  infoBox( 'Retweets %', retweets_percent, icon = icon( 'retweet' ) )
  
})

output$twitter_unique_users_infobox = renderInfoBox({
  
  if( !is.null( twitter_data$sentiment_info ) ){
    
    unique_users = as.character( twitter_data$sentiment_info$unique_users )
    
  } else{ unique_users = 'Oops! No data found' }
  
  infoBox( 'Unique Users', unique_users, icon = icon( 'user' ), color = 'fuchsia' )
  
})

#...... Twitter Sentiment Pie Chart .......

output$twitter_sentiment_pie_chart = renderPlotly({
  
  if( !is.null( twitter_data$sentiment_info ) ){
    
    #sentiment_distribution = twitter_data$sentiment_info$sentiment_distribution
    
    #twitter_query = twitter_data$sentiment_info$twitter_query
    
    #Tweet_Sentiment_Pie_Chart( twitter_query, sentiment_distribution$Percentage, sentiment_distribution$Sentiment, c( 'black', 'red', 'orange' ) )
    
    plot_ly(twitter_data$sentiment_info$sentiment_distribution, labels = ~Sentiment, values = ~Percentage, type = 'pie') %>%
           layout(title = '',
                               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  } else{
    
    p = ggplot( data.frame() ) + geom_point() + xlim( 0, 10 ) + ylim( 0, 100 )
    
    ggplotly( p )
    
  }

})

#...... Twitter Sentiment Bar Chart .......

output$twitter_sentiment_bar_chart = renderPlotly({
  
  if( !is.null( twitter_data$sentiment_info ) ){
    
    #sentiment_distribution_bar = twitter_data$sentiment_info$sentiment_distribution ; names( sentiment_distribution_bar ) = c( 'Type', 'Values' )
    
    #twitter_query = twitter_data$sentiment_info$twitter_query
    
    #Twitter_Sentiment_Bar_Plot( sentiment_distribution_bar, twitter_query, c( "firebrick1", "orange", "green" ), horizontal = F )
    
    plot_ly( twitter_data$sentiment_info$sentiment_distribution, x = ~Sentiment, y = ~Percentage, type = 'bar', 
             
             text = twitter_data$sentiment_info$sentiment_distribution$Percentage, textposition = 'auto',
             
             marker = list(color = c("green", "orange", "red"))) 
    
  } else{
    
    p = ggplot( data.frame() ) + geom_point() + xlim( 0, 10 ) + ylim( 0, 100 )
    
    ggplotly( p )
    
  }
  
})

#...... Twitter Sentiment Dataframe .......

output$twitter_sentiment_distribution_df = DT::renderDataTable({
  
  if( !is.null( twitter_data$sentiment_info ) ){
    
    sentiment_distribution = twitter_data$sentiment_info$sentiment_distribution
    
    DT::datatable( sentiment_distribution, options = list ( pageLength = 10, dom = 'tip' ), rownames = F, editable = F ) %>%
      
      formatStyle( names( sentiment_distribution ),  color = 'black', backgroundColor = 'lightblue', fontWeight = 'bold' )
    
  } else{
    
    DT::datatable( data.frame( 'Message' = 'Sorry! No data found' ), options = list ( pageLength = 10, dom = 'tip' ), rownames = F, editable = F )
    
  }
  
})

#...... Twitter Top Sources Pie Chart .......

output$twitter_top_sources_pie_chart = renderPlotly({
  
  if( !is.null( twitter_data$sentiment_info ) ){
    
    #top_sources_df = twitter_data$sentiment_info$source_device_stats
    
    #twitter_query = twitter_data$sentiment_info$twitter_query
    
    #Tweet_Sentiment_Pie_Chart( twitter_query, top_sources_df$Percentage, top_sources_df[["Source_Device"]], c( 'black', 'red', 'lightblue', 'green', 'orange' ) )
    
    plot_ly(twitter_data$sentiment_info$source_device_stats, labels = ~Source_Device, values = ~Percentage, type = 'pie') %>%
          layout(title = '',
                             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  } else{
    
    p = ggplot( data.frame() ) + geom_point() + xlim( 0, 10 ) + ylim( 0, 100 )
    
    ggplotly( p )
    
  }

})

#...... Twitter Top Sources Bar Chart .......

output$twitter_top_sources_bar_chart = renderPlotly({
  
  if( !is.null( twitter_data$sentiment_info ) ){
    
    #top_sources_df_bar = twitter_data$sentiment_info$source_device_stats ; names( top_sources_df_bar ) = c( 'Type', 'Values' )
    
    #twitter_query = twitter_data$sentiment_info$twitter_query
    
    #Twitter_Sentiment_Bar_Plot( top_sources_df_bar, twitter_query, c( 'firebrick1', 'orange', 'green', 'lightblue', 'black' ), horizontal = F )
    
    plot_ly( twitter_data$sentiment_info$source_device_stats, x = ~Source_Device, y = ~Percentage, type = 'bar', 
             
             text = twitter_data$sentiment_info$source_device_stats$Percentage, textposition = 'auto' )
    
  } else{
    
    p = ggplot( data.frame() ) + geom_point() + xlim( 0, 10 ) + ylim( 0, 100 )
    
    ggplotly( p )
    
  }
  
})

#...... Twitter Top Sources Dataframe .......

output$twitter_top_sources_distribution_df = DT::renderDataTable({
  
  if( !is.null( twitter_data$sentiment_info ) ){
  
  top_sources_df = twitter_data$sentiment_info$source_device_stats
  
  DT::datatable( top_sources_df, options = list ( pageLength = 10, dom = 'tip' ), rownames = F, editable = F ) %>%
    
    formatStyle( names( top_sources_df ),  color = 'black', backgroundColor = 'lightblue', fontWeight = 'bold' )
  
  } else{
    
    DT::datatable( data.frame( 'Message' = 'Sorry! No data found' ), options = list ( pageLength = 10, dom = 'tip' ), rownames = F, editable = F )
    
  }
  
})

#...... Twitter Top Sites Bar Chart .......

output$twitter_top_sites_bar = renderPlotly({
  
  if( !is.null( twitter_data$sentiment_info ) ){
    
    #top_sites_df_bar = twitter_data$sentiment_info$domain_names_stats ; names( top_sites_df_bar ) = c( 'Type', 'Values' )
    
    #twitter_query = twitter_data$sentiment_info$twitter_query
    
    #Twitter_Sentiment_Bar_Plot( top_sites_df_bar, twitter_query, rep( 'orange', nrow( top_sites_df_bar ) ), horizontal = T )
    
    plot_ly( twitter_data$sentiment_info$domain_names_stats, x = ~Count, y = ~gsub(".com", "",gsub("https://", "",gsub("http://", "",as.character(twitter_data$sentiment_info$domain_names_stats$Domains) ) ) ), type = 'bar', marker = list(color = 'rgba(50, 171, 96, 0.6)'),
                      
                        text = twitter_data$sentiment_info$domain_names_stats$Count, textposition = 'auto', orientation = 'h' )%>%
          layout(
                  xaxis = list(title = "Count"),
                   yaxis = list(title =""))
    
  } else{
    
    p = ggplot( data.frame() ) + geom_point() + xlim( 0, 10 ) + ylim( 0, 100 )
    
    ggplotly( p )
    
  }
  
})

#...... Twitter Top Sites Dataframe .......

output$twitter_top_sites_df = DT::renderDataTable({
  
  if( !is.null( twitter_data$sentiment_info ) ){
    
    twitter_top_sites_df_0 = twitter_data$sentiment_info$domain_names_stats
    
    DT::datatable( twitter_top_sites_df_0, options = list ( pageLength = 10, dom = 'tip' ), rownames = F, editable = F ) %>%
      
      formatStyle( names( twitter_top_sites_df_0 ),  color = 'black', backgroundColor = 'lightblue', fontWeight = 'bold' )
    
  } else{
    
    DT::datatable( data.frame( 'Message' = 'Sorry! No data found' ), options = list ( pageLength = 10, dom = 'tip' ), rownames = F, editable = F )
    
  }
  
})

#...... Twitter Share of Posts Pie Chart .......

output$twitter_share_of_posts_pie = renderPlotly({
  
  if( !is.null( twitter_data$sentiment_info ) ){
    
    #twitter_posts_share_df = twitter_data$sentiment_info$share_of_posts_stats
    
    #twitter_query = twitter_data$sentiment_info$twitter_query
    
    #Tweet_Sentiment_Pie_Chart( twitter_query, twitter_posts_share_df$Percentage, twitter_posts_share_df$Post_Type, c( 'orange', 'green' ) )
    
    plot_ly(twitter_data$sentiment_info$share_of_posts_stats, labels = ~Post_Type, values = ~Percentage, type = 'pie') %>%
           layout(title = '',
                               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  } else{
    
    p = ggplot( data.frame() ) + geom_point() + xlim( 0, 10 ) + ylim( 0, 100 )
    
    ggplotly( p )
    
  }

})

#...... Twitter Share of Posts Dataframe .......

output$twitter_share_of_posts_df = DT::renderDataTable({
  
  if( !is.null( twitter_data$sentiment_info ) ){
    
    twitter_posts_share_df = twitter_data$sentiment_info$share_of_posts_stats
    
    DT::datatable( twitter_posts_share_df, options = list ( pageLength = 10, dom = 'tip' ), rownames = F, editable = F ) %>%
      
      formatStyle( names( twitter_posts_share_df ),  color = 'black', backgroundColor = 'lightblue', fontWeight = 'bold' )
    
  } else{
    
    DT::datatable( data.frame( 'Message' = 'Sorry! No data found' ), options = list ( pageLength = 10, dom = 'tip' ), rownames = F, editable = F )
    
  }
  
})

#...... Twitter Top Posts Dataframe .......

output$twitter_top_posts_df = DT::renderDataTable({
  
  if( !is.null( twitter_data$sentiment_info ) ){
    
    twitter_top_posts_df = twitter_data$sentiment_info$top_posts_df
    
    DT::datatable( twitter_top_posts_df, options = list ( pageLength = 3, dom = 'tip' ), rownames = F, editable = F ) %>%
      
      formatStyle( names( twitter_top_posts_df ),  color = 'black', backgroundColor = 'lightblue', fontWeight = 'bold' )
    
  } else{
    
    DT::datatable( data.frame( 'Message' = 'Sorry! No data found' ), options = list ( pageLength = 10, dom = 'tip' ), rownames = F, editable = F )
    
  }
  
})

#..... Buzz words download ....

output$twitter_influential_posts_download_data = downloadHandler(
  
  filename = function() {
    
    paste( input$twitter_query, '_influential_posts_', '.csv' )
    
  },
  
  content = function( file ){
    
    write.csv( twitter_data$sentiment_info$top_posts_df, file, row.names = F )
    
  }
  
)

#...... Twitter Top Users Dataframe .......

output$twitter_top_users_df = DT::renderDataTable({
  
  if( !is.null( twitter_data$sentiment_info ) ){
    
    twitter_top_users_df = twitter_data$sentiment_info$top_users
    
    DT::datatable( twitter_top_users_df, options = list ( pageLength = 5, dom = 'tip' ), rownames = F, editable = F ) %>%
      
      formatStyle( names( twitter_top_users_df ),  color = 'black', backgroundColor = 'lightblue', fontWeight = 'bold' )
    
  } else{
    
    DT::datatable( data.frame( 'Message' = 'Sorry! No data found' ), options = list ( pageLength = 10, dom = 'tip' ), rownames = F, editable = F )
    
  }
  
})

#...... Twitter Word Cloud .......

output$twitter_word_cloud = renderPlot({
  
  if( !is.null( twitter_data$sentiment_info ) ){
    
    twitter_word_cloud_effective_data = twitter_data$sentiment_info$effective_data
    
    set.seed( 123 )
    
    wordcloud( words = twitter_word_cloud_effective_data$word, freq = twitter_word_cloud_effective_data$freq, min.freq = 1, random.order = F, rot.per = 0.35,
               
               colors = brewer.pal( 8, 'Dark2' ) )
    
    } else{
      
      ggplot( data.frame() ) + geom_point() + xlim( 0, 10 ) + ylim( 0, 100 )
      
    }
  
})

#..... Buzz words download ....

output$twitter_buzz_words_download_data = downloadHandler(
  
  filename = function() {
    
    paste( input$twitter_query, '_buzz_words_', '.csv' )
    
  },
  
  content = function( file ){
    
    write.csv( twitter_data$sentiment_info$effective_data, file, row.names = F )
    
  }
  
)

#.... Twitter Time series plot .....

output$twitter_time_series_plot = renderPlotly({
  
  if( !is.null( twitter_data$sentiment_info ) ){
    
    daily_df = twitter_data$sentiment_info$sentiment_score_df
    
    daily_df_distribution = daily_df %>%
      
      group_by( created, polarity ) %>%
      
      dplyr::summarise( FREQ = n() )
    
    time_series_plot = daily_df_distribution %>%
      
      group_by( polarity ) %>%
      
      plot_ly( x = ~ created ) %>%
      
      add_lines( y = ~ FREQ , 
                 color = ~ factor( polarity )
      ) %>%
      
      layout( title = 'Time Series Distribution of Sentiment', xaxis = list( title = 'Date' ), yaxis = list( title = 'Number of Posts' ) )
    
    time_series_plot
    
  } else{
    
    ggplot( data.frame() ) + geom_point() + xlim( 0, 10 ) + ylim( 0, 100 )
    
  }
  
  
})

#.... Twitter extracted dataframe ....

output$twitter_time_series_plot_data = DT::renderDataTable({
  
  daily_df = twitter_data$sentiment_info$sentiment_score_df
  
  DT::datatable( daily_df, options = list (pageLength = 5, dom = 'tip' ), rownames = F ) %>%
    
    formatStyle( names( daily_df ),  color = 'black', backgroundColor = 'lightblue', fontWeight = 'bold' )
  
})

#.... Twitter Bar plot of Emotions .....

output$twitter_bar_plot_emotions = renderPlotly({
  
  if( !is.null( twitter_data$sentiment_info ) ){
    
    plot_ly( twitter_data$sentiment_info$count_emotion_df, x = ~Emotions, y = ~Count, type = 'bar', 
                      
                        text = twitter_data$sentiment_info$count_emotion_df$Count, textposition = 'auto',
                      
                        marker = list(color = c('grey', 'red',
                                                   'orange', 'navy',
                                                    'yellow'))
                      
                   )
    
    
  } else{
    
    ggplot( data.frame() ) + geom_point() + xlim( 0, 10 ) + ylim( 0, 100 )
    
  }
  
  
})

#.... Twitter emotion dataframe ....

output$twitter_emotion_data = DT::renderDataTable({
  
  DT::datatable( twitter_data$sentiment_info$emotion_df, options = list (pageLength = 5, dom = 'tip' ), rownames = F ) %>%
    
    formatStyle( names( twitter_data$sentiment_info$emotion_df ),  color = 'black', backgroundColor = 'lightblue', fontWeight = 'bold' )
  
})

#.... Twitter location dataframe ....

output$twitter_location_data = DT::renderDataTable({
  
  DT::datatable( twitter_data$sentiment_info$location_df, options = list (pageLength = 5, dom = 'tip' ), rownames = F ) %>%
    
    formatStyle( names( twitter_data$sentiment_info$location_df ),  color = 'black', backgroundColor = 'lightblue', fontWeight = 'bold' )
  
})



