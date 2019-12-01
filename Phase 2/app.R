source( 'SourceOfSource.R' )

ui = source( file.path( "ui", "ui_sentiment.R" ), local = T )$value   #..... ui for ocr
  


server = function( input, output, session ){
  
  #..... Include server logic for each tab .....
  
  source( file.path( "server", "server_sentiment.R" ), local = T )$value    #..... server logic for ocrs
  
}

shinyApp( ui = ui, server = server )

