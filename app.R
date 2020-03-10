library(tidytext)
library(textdata)
library(lubridate)
library(httr)
library(plotly)
library(tidyverse)
library(jsonlite)
library(shiny)
library(ggplot2)
library(jpeg)

get_info <- function(artist, title){
  fromJSON((
    str_glue("https://api.vagalume.com.br/search.php?apikey=114e5edf0076c875c605607fa7b82eec&art={artist}&mus={title}&extra=relart,artpic" , artist = str_replace_all(artist , c(" " = "%20")),
             title = str_replace_all(title, c(" " = "%20"))
    ))
  )
}
# sentiment analysis


ui <- fluidPage(
  #suppress warings while wating for inputs
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  # Application title
  titlePanel("Music Lyrics App"),
  
  
  sidebarLayout(
    sidebarPanel(
      textInput("artist","artist", width = NULL,
                placeholder = "e.g Justin Bieber", value="Justin Bieber"
      ),
      textInput("song","song", width = NULL,
                placeholder = "e.g Baby", value="Baby"
      ),
      checkboxInput("checkboxes", "Sentiment Analysis Chart", value=TRUE),
      checkboxInput("top", "Top 10 words used", value = TRUE),
      checkboxInput("pic", "Artist Image", value = TRUE),
      actionButton("goButton", "Go!")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(column(10, verbatimTextOutput("lyric"))),
      plotlyOutput("pie"),
      plotOutput("bar"),
      plotOutput("image")
    )
  )
  
)


server <- function(input, output) {
  
  options(warn = -1) 
  
  output$lyric <- renderText({ 
    input$goButton
    lyric <- get_info(input$artist, input$song)$mus$text
    
  })
  
  
  img_url = reactive({
    id = get_info(input$artist, input$song)$art$id
    img_url = fromJSON(paste("https://api.vagalume.com.br/image.php?bandID=",toString(id),"&apikey=114e5edf0076c875c605607fa7b82eec",simplifyVector = FALSE))$images$url[1]
  })
  
  
  output$image = renderPlot({
    input$goButton
    if (input$pic) {
      download.file(img_url(),'y.jpg', mode = 'wb')
      jj=readJPEG("y.jpg",native=FALSE)
      plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
      rasterImage(jj,0,0,1,1)
    }
  })
  
  sentiment_analysis <-  reactive({
    input$goButton
    lines<- get_info(input$artist, input$song)$mus$text %>%
      str_split("\n") %>% 
      unlist() 
    lyric_df<- tibble(line = 1:length(lines), lines)
    
    token <-  lyric_df %>% unnest_tokens(word, lines) %>%
      anti_join(stop_words)
    
    
    sentiment <- token %>% left_join(get_sentiments("bing")) %>% 
      filter(sentiment %in% c("positive", "negative" )) %>% 
      group_by(line) %>% 
      summarize(
        positive = sum(sentiment == "positive", na.rm = TRUE), 
        negative = sum(sentiment == "negative", na.rm = TRUE), 
        netural = n() - positive - negative) %>%
      mutate(
        line,
        sentiment = case_when(
          positive > negative ~ "positive",
          positive < negative ~ "negative",
          TRUE ~ "neutral"
        )
      )
    sentiment %>% count(sentiment)
    
  })
  output$pie <- renderPlotly({ 
    input$goButton
    if (input$checkboxes) {
      
      df_pie = sentiment_analysis()
      pie <- plot_ly(type='pie', labels=df_pie$sentiment, values=df_pie$n, 
                     textinfo='label+percent',
                     insidetextorientation='radial')
      pie
  
      
    }
    
  })  
  output$bar <- renderPlot({
    input$goButton
    if(input$top){
      lines<- get_info(input$artist, input$song)$mus$text %>%
        str_split("\n") %>% 
        unlist() 
      lyric_df<- tibble(line = 1:length(lines), lines)
      
      token <-  lyric_df %>% unnest_tokens(word, lines) %>%
        anti_join(stop_words)
      token %>% group_by(word) %>% summarise(count=n()) %>% arrange(desc(count)) %>% 
        head(10) %>%
        ggplot(aes(x=reorder(word, -count), y=count, fill=word)) + geom_bar(stat="identity") +
        labs(x="Word", y="Count")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
