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
    
    #daily_df = twitter_data$sentiment_info$sentiment_score_df
    
    #daily_df_distribution = daily_df %>%
      
    #  group_by( created, polarity ) %>%
      
    #  dplyr::summarise( FREQ = n() )
    
    tweets_time = as.character(twitter_data$sentiment_info$raw_data$created)
    
    date_hr_min_sec = unlist( strsplit(tweets_time, " ") )
    
    exact_date = date_hr_min_sec[seq(1,length(date_hr_min_sec),2)]
    
    date_tweets_df = as.data.frame(table(exact_date))
    
    names(date_tweets_df) = c( "Date", "Total_Tweets" )
    
    plot_ly(date_tweets_df, x = ~Date, y = ~Total_Tweets, name = '', type = 'scatter', mode = 'lines+markers') %>%
      layout( title = '', xaxis = list( title = 'Date' ), yaxis = list( title = 'Number of Tweets' ) )
    
  } else{
    
    ggplot( data.frame() ) + geom_point() + xlim( 0, 10 ) + ylim( 0, 100 )
    
  }
  
  
})

#.... Twitter extracted dataframe ....

observeEvent(input$test, {
  
  showModal(myModal())
  
})

output$download1 <- downloadHandler(
  
  filename = function() {
    
    paste("data-", Sys.Date(), ".csv", sep="")
    
  },
  
  content = function(file) {
    
    write.csv( twitter_data$sentiment_info$sentiment_score_df, file, row.names = F )
    
  }
  
)

observeEvent(input$test_raw_data, {
  
  showModal(myModal_raw_data())
  
})

output$download_raw_data <- downloadHandler(
  
  filename = function() {
    
    paste("raw_data-", Sys.Date(), ".csv", sep="")
    
  },
  
  content = function(file) {
    
    write.csv( twitter_data$sentiment_info$raw_data, file, row.names = F )
    
  }
  
)


############ Raw Data Table ##############

output$Raw_Data_Table = DT::renderDataTable({
  
  daily_df = data.frame( Tweets = twitter_data$sentiment_info$raw_data[ , 1 ] )
  
  DT::datatable( daily_df, extensions = 'Buttons', options = list (
    
    dom = 'Bfrtip',
    
    buttons = list(
      
      # "copy",
      
      list(
        
        extend = "collection",
        
        text = 'Download Data',
        
        action = DT::JS("function ( e, dt, node, config ) {
                                  
                                  Shiny.setInputValue('test_raw_data', true, {priority: 'event'});
                                  
                                  }")
      )
    )
  ), rownames = F ) %>%
    
    formatStyle( names( daily_df ),  color = 'black', backgroundColor = 'lightblue', fontWeight = 'bold' )
  
})

output$twitter_time_series_plot_data = DT::renderDataTable({
  
  daily_df = twitter_data$sentiment_info$sentiment_score_df
  
  DT::datatable( daily_df, extensions = 'Buttons', options = list (
    
    dom = 'Bfrtip',
    
    buttons = list(
      
      # "copy",
      
      list(
        
        extend = "collection",
        
        text = 'Download Data',
        
        action = DT::JS("function ( e, dt, node, config ) {
                                  
                                  Shiny.setInputValue('test', true, {priority: 'event'});
                                  
                                  }")
      )
    )
  ), rownames = F ) %>%
    
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

location_dataframe = eventReactive( input$map_submit_button, {
  
  data_loc=data.frame( twitter_data$sentiment_info$location_df$location ) 
  
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
  
  return( cities1 )
  
})

output$Location_Plot = renderLeaflet( { 
  
  tryCatch({
    
    leaflet( location_dataframe() ) %>%
      addTiles() %>%
      addMarkers(lat = ~lat, lng = ~lon)
    
  }, error = function( error ){ } )
  
})


###########################################################################################################
###################### Profile Analysis ###################################################################
###########################################################################################################

dataInput_sensor_presence_info = eventReactive( input$pa_submit_button, {
  
  profileInfo <- lookupUsers(input$profile_name)  # Batch lookup of user info
  
  profileFrame <- twListToDF(profileInfo)  # Convert to a nice dF
  
  profileFrame
  
})

output$profile_name_infobox = renderInfoBox({
  
  if( !is.null( dataInput_sensor_presence_info() ) ){
    
    name = as.character( dataInput_sensor_presence_info()$name )
    
  } else{ name = 'Oops! No data found' }
  
  infoBox( 'Name', name, icon = icon( 'users' ), color = 'green' )
  
})

output$created_on_infobox = renderInfoBox({
  
  if( !is.null( dataInput_sensor_presence_info() ) ){
    
    created_datetime = as.character( dataInput_sensor_presence_info()$created )
    
  } else{ created_datetime = 'Oops! No data found' }
  
  infoBox( 'Created on' , created_datetime, icon = icon( 'twitter' ), color = 'yellow' )
  
})

output$profile_id_infobox = renderInfoBox({
  
  if( !is.null( dataInput_sensor_presence_info() ) ){
    
    profile_id = as.character( dataInput_sensor_presence_info()$id )
    
  } else{ profile_id = 'Oops! No data found' }
  
  infoBox( 'Id' , profile_id, icon = icon( 'twitter' ), color = 'yellow' )
  
})

output$profile_description_infobox = renderInfoBox({
  
  if( !is.null( dataInput_sensor_presence_info() ) ){
    
    description = as.character( dataInput_sensor_presence_info()$description )
    
  } else{ description = 'Oops! No data found' }
  
  infoBox( 'Description', description, icon = icon( 'retweet' ) )
  
})

output$profile_statusesCount_infobox = renderInfoBox({
  
  if( !is.null( dataInput_sensor_presence_info() ) ){
    
    statusescount = as.character( dataInput_sensor_presence_info()$statusesCount )
    
  } else{ statusescount = 'Oops! No data found' }
  
  infoBox( 'Statuses', statusescount, icon = icon( 'user' ), color = 'fuchsia' )
  
})

output$profile_followers_infobox = renderInfoBox({
  
  if( !is.null( dataInput_sensor_presence_info() ) ){
    
    followers = as.character( dataInput_sensor_presence_info()$followersCount )
    
  } else{ followers = 'Oops! No data found' }
  
  infoBox( 'Followers', followers, icon = icon( 'users' ), color = 'green' )
  
})

output$profile_favorites_infobox = renderInfoBox({
  
  if( !is.null( dataInput_sensor_presence_info() ) ){
    
    favorites = as.character( dataInput_sensor_presence_info()$favoritesCount )
    
  } else{ favorites = 'Oops! No data found' }
  
  infoBox( 'Favorites' , favorites, icon = icon( 'twitter' ), color = 'yellow' )
  
})

output$profile_friends_infobox = renderInfoBox({
  
  if( !is.null( dataInput_sensor_presence_info() ) ){
    
    friends = as.character( dataInput_sensor_presence_info()$friendsCount )
    
  } else{ friends = 'Oops! No data found' }
  
  infoBox( 'Friends' , friends, icon = icon( 'twitter' ), color = 'yellow' )
  
})

output$profile_listedcount_infobox = renderInfoBox({
  
  if( !is.null( dataInput_sensor_presence_info() ) ){
    
    listed = as.character( dataInput_sensor_presence_info()$listedCount )
    
  } else{ listed = 'Oops! No data found' }
  
  infoBox( 'Listed' , listed, icon = icon( 'twitter' ), color = 'yellow' )
  
})

output$profile_location_infobox = renderInfoBox({
  
  if( !is.null( dataInput_sensor_presence_info() ) ){
    
    location = as.character( dataInput_sensor_presence_info()$location )
    
  } else{ location = 'Oops! No data found' }
  
  infoBox( 'Location', location, icon = icon( 'retweet' ) )
  
})

output$profile_analysis_barplot = renderPlotly({
  
  if( !is.null( dataInput_sensor_presence_info() ) ){
    
    df = data.frame( Sections = c( "StatusesCount", "FollowersCount", "FavoritesCount", "FriendsCount", "ListedCount" ),
                     
                     Values = c( dataInput_sensor_presence_info()$statusesCount, dataInput_sensor_presence_info()$followersCount, dataInput_sensor_presence_info()$favoritesCount, dataInput_sensor_presence_info()$friendsCount, dataInput_sensor_presence_info()$listedCount ) )

    plot_ly( df, x = ~Sections, y = ~Values, type = 'bar', 
             
             text = df$Values, textposition = 'auto',
             
             marker = list(color = c('grey', 'red',
                                     'orange', 'navy',
                                     'yellow'))
             
    )%>%
    layout( title = '', xaxis = list( title = '' ), yaxis = list( title = 'Counts' ) )
    
    
    
  } else{
    
    ggplot( data.frame() ) + geom_point() + xlim( 0, 10 ) + ylim( 0, 100 )
    
  }
  
  
})

