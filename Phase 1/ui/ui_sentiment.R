#.... OCR app UI .....

sentiment_header = dashboardHeader( title = 'Social Media Analysis' )
  
sentiment_sidebar =  dashboardSidebar( collapsed = T,
  
  sidebarMenu(
  
  menuItem( strong( '  Twitter' ), tabName = 'twitter', icon = icon( 'twitter', 'fa-1.5x' ) )
  
  )
  
)
  
sentiment_body =  dashboardBody(
  
  tabItems(
    
    #####.... twitter ....####
    
    tabItem( tabName = 'twitter',
      
             fluidRow(
               
               box( title = 'Inputs For Hashtag', status = 'success', collapsible = T, width = 12, solidHeader = T,
                    
                    column( 4, textInput( 'twitter_query', label = 'Hashtags', value = "#Afganistan" ) ),
                    
                    column( 2, numericInput( 'twitter_no_of_tweets_to_fetch', label = 'Max. no of tweets', value = 140 ) ),
                    
                    column( 2, dateInput( 'twitter_from_date', label = 'From Date', value = Sys.Date() - 7 ) ),
                    
                    column( 2, dateInput( 'twitter_to_date', label = 'To Date', value = Sys.Date() ) ), br(),
                    
                    column( 2, align = 'left',
                            
                            actionButton( 'twitter_submit_button', 'SUBMIT' ),
                            
                            tags$style( "#twitter_submit_button { vertical-align: middle; height: 30px; width: 70%; font-size: 15px;color: white;background-color:#1B618D;border-color: #374645 }" )
                            
                    )
                    
                  )
               
             ),
             
             fluidRow(
               
               box( title = 'Inputs For Keywords', status = 'success', collapsible = T, width = 12, solidHeader = T,
                    
                    column( 4, textInput( 'twitter_keyword_query', label = 'Keyword', value = "Afganistan" ) ),
                    
                    column( 2, numericInput( 'no_of_tweets_to_fetch_keyword', label = 'Max. no of tweets', value = 140 ) ),
                    
                    column( 2, dateInput( 'keyword_from_date', label = 'From Date', value = Sys.Date() - 7 ) ),
                    
                    column( 2, dateInput( 'keyword_to_date', label = 'To Date', value = Sys.Date() ) ), br(),
                    
                    column( 2, align = 'left',
                            
                            actionButton( 'keyword_submit_button', 'SUBMIT' ),
                            
                            tags$style( "#keyword_submit_button { vertical-align: middle; height: 30px; width: 70%; font-size: 15px;color: white;background-color:#1B618D;border-color: #374645 }" )
                            
                    )
                    
               )
               
             ),
             
             fluidRow(
               
               infoBoxOutput( 'twitter_reach_infobox', width = 3 ),
               
               infoBoxOutput( 'twitter_tweets_infobox', width = 3 ),
               
               infoBoxOutput( 'twitter_retweets_infobox', width = 3 ),
               
               infoBoxOutput( 'twitter_unique_users_infobox', width = 3 )
               
             ),
             
             #fluidRow(
               
             #  column( 3, downloadButton( 'twitter_influential_posts_download_data', strong( 'Influential Posts' ), width = 100 ), offset = 4 ),
               
             #  column( 3, downloadButton( 'twitter_buzz_words_download_data', strong( 'Buzz Words' ), width = 100 ), offset = 1 )
               
            # ), br(),
             
             fluidRow(
               
               tabBox( title = strong( 'Sentiment Analysis: Polarity' ), selected = strong( 'Bar Chart' ), width = 6,
                       
                       tabPanel( strong( 'Bar Chart' ), plotlyOutput( 'twitter_sentiment_bar_chart' ) ),
                 
                       tabPanel( strong( 'Pie Chart' ), plotlyOutput( 'twitter_sentiment_pie_chart' ) ),
                       
                       tabPanel( strong( 'Table' ), DT::dataTableOutput( 'twitter_sentiment_distribution_df' ) )
                 
               ),
               
               tabBox( title = strong( 'Sentiment Analysis: Emotions' ), selected = strong( 'Bar Chart' ), width = 6,
                       
                       tabPanel( strong( 'Bar Chart' ), plotlyOutput( 'twitter_bar_plot_emotions' ) ),
                       
                       tabPanel( strong( 'Table' ), DT::dataTableOutput( 'twitter_emotion_data' ) )
                       
               )
               
             ),
             
             fluidRow(
               
               tabBox( title = strong( 'Share of Posts' ), selected = strong( 'Pie Chart' ), width = 4,
                       
                       tabPanel( strong( 'Pie Chart' ), plotlyOutput( 'twitter_share_of_posts_pie' ) ),
                       
                       tabPanel( strong( 'Origin' ), DT::dataTableOutput( 'twitter_share_of_posts_df' ) )
                       
               ),
               
               tabBox( title = strong( 'Top Sources' ), selected = strong( 'Bar Chart' ), width = 4,
                       
                       tabPanel( strong( 'Bar Chart' ), plotlyOutput( 'twitter_top_sources_bar_chart' ) ),
                       
                       tabPanel( strong( 'Pie Chart' ), plotlyOutput( 'twitter_top_sources_pie_chart' ) ),
                       
                       tabPanel( strong( 'Values' ), DT::dataTableOutput( 'twitter_top_sources_distribution_df' ) )
                       
               ),
               
               tabBox( title = strong( 'Top Sites' ), selected = strong( 'Bar Chart' ), width = 4,
                       
                       tabPanel( strong( 'Bar Chart' ), plotlyOutput( 'twitter_top_sites_bar' ) ),
                       
                       tabPanel( strong( 'Domain' ), DT::dataTableOutput( 'twitter_top_sites_df' ) )
                       
               )
               
             ),
             
             fluidRow(
               
               tabBox( title = strong( 'Sentiment over time' ), selected = strong( 'Graph' ), width = 8,
                 
                 tabPanel( strong( 'Graph' ), plotlyOutput( 'twitter_time_series_plot' ) )
                 
               ),
               
               tabBox( title = strong( 'Buzz Words' ), width = 4,
                       
                       tabPanel( strong( 'Word Cloud' ), plotOutput( 'twitter_word_cloud' ) ) )
               
             ),
             
             fluidRow(
               
               tabBox( title = strong( 'Influential' ), selected = strong( 'Top Posts' ), width = 12,
                       
                       tabPanel( strong( 'Top Posts' ), DT::dataTableOutput( 'twitter_top_posts_df' ) ),
                       
                       tabPanel( strong( 'Top Users' ), DT::dataTableOutput( 'twitter_top_users_df' ) ),
                       
                       tabPanel( strong( 'Location Table' ), DT::dataTableOutput( 'twitter_location_data' ) ),
                       
                       tabPanel( strong( 'Santiment Table' ), DT::dataTableOutput( 'twitter_time_series_plot_data' ) )
                       
               )
               
             )     
             
    )
    
  )
  
)

dashboardPage( sentiment_header, sentiment_sidebar, sentiment_body, skin = 'yellow' )


