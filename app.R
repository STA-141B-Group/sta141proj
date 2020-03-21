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
library(devtools)
library(rsconnect)
library(purrr)
library(graphics)
library(stats)



key= Sys.getenv("KEY")
# function to access part of api database
get_info <- function(artist, title){
  fromJSON((
    str_glue("https://api.vagalume.com.br/search.php?apikey={key}&art={artist}&mus={title}&extra=relart,artpic" , artist = str_replace_all(artist , c(" " = "%20")),
             
             title = str_replace_all(title, c(" " = "%20")), key= Sys.getenv("KEY")
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
   
    # adding compents into side bar
    sidebarPanel(
      h5("Enter an Artist and Song:"),
      fluidPage(column(10, verbatimTextOutput("text"))),
      textInput("artist","Artist", width = NULL,
                placeholder = "e.g Justin Bieber", value="Justin Bieber"
      ),
      textInput("song","Song", width = NULL,
                placeholder = "e.g Baby", value="Baby"
      ),
      #adding checkboxes so user can choose what to view
      checkboxInput("checkboxes", "Sentiment Analysis Chart", value=TRUE),
      checkboxInput("top", "Top 10 words used", value = TRUE),
      checkboxInput("pic", "Artist Image", value = TRUE),
      checkboxInput("relsong", "Related songs", value = TRUE),
      actionButton("goButton", "Go!")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Lyric Analysis", plotOutput("bar"),plotlyOutput("pie")),
        tabPanel("Picture and Related Artists", fluidRow(column(8, tableOutput("table"))),
                 plotOutput("image")),
        tabPanel("Lyrics", verbatimTextOutput("lyric"))
      )
    )
  )
)


server <- function(input, output) {
  
  options(warn = -1)
  output$lyric <- renderText({
    input$goButton
    lyric <- get_info(input$artist, input$song)$mus$text
  })
  
  
  #test code:  fromJSON("https://api.vagalume.com.br/search.php?art=U2&mus=One&extra=relmus&nolyrics=1&apikey=660a4395f992ff67786584e238f501aa")$mus$related
  # get relared arist from api
  relsong_json <- reactive({
    relsong_url <- fromJSON(paste("https://api.vagalume.com.br/search.php?apikey=",key,"&art=",input$artist,"&mus=",input$song,"&extra=relmus&nolyrics=1"))$mus$related
  })
  #make table
  output$table <- renderTable({
    input$goButton
    if(input$relsong) {
      table <- as.data.frame(relsong_json())
      #add rel song and rel arist to table
      names(table$mus)[3] = 'Related Songs'
      names(table$art)[2] = 'Artists'
      c(table$mus[3], table$art[2])
    }
  })
  # searchf for image
  img_url = reactive({
    #get arist id to from get_info
    id = get_info(input$artist, input$song)$art$id
    img_url = fromJSON(paste("https://api.vagalume.com.br/image.php?bandID=",toString(id),"&apikey=",key,"",simplifyVector = FALSE))$images$url[1]
  })
  
  output$image = renderPlot({
    input$goButton
    if (input$pic) {
      #download image
      download.file(img_url(),'y.jpg', mode = 'wb')
      jj=readJPEG("y.jpg",native=FALSE)
      plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
      #display image
      rasterImage(jj,0,0,1,1)
    }
    
  })
  # create sentiment analysis
  sentiment_analysis <-  reactive({
    input$goButton
    nrc = read_csv(" lexicon.nrc.csv")
    lines<- get_info(input$artist, input$song)$mus$text %>%
      str_split("\n") %>%
      unlist()
    #crate a table of lyrics by line
    lyric_df<- tibble(line = 1:length(lines), lines)
    #remove stop words and seprate line by word
    token <-  lyric_df %>% unnest_tokens(word, lines) %>%
      anti_join(stop_words)
    #add info from lexicom
    sentiment <- token %>% left_join(nrc) %>%
      #filter non pos neg words
      filter(sentiment %in% c("positive", "negative" )) %>%
      # group by line and sum up pos and negative words
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
    #table of number of pos and neg and neutal
    #use for pie chart
    sentiment %>% count(sentiment)
  })
  
  output$pie <- renderPlotly({
    input$goButton
    if (input$checkboxes) {
      #make pie chrt using plotly
      df_pie = sentiment_analysis()
      pie <- plot_ly(type='pie', labels=df_pie$sentiment, values=df_pie$n,
                     textinfo='label+percent',
                     insidetextorientation='radial')  %>% layout(title="Sentiment Analysis \n \n")
      pie
    }
  }) 
  # make bar graph of top 10 words
  output$bar <- renderPlot({
    input$goButton
    
    if(input$top){
      #split lyrcs by line
      lines<- get_info(input$artist, input$song)$mus$text %>%
        str_split("\n") %>%
        unlist()
      lyric_df<- tibble(line = 1:length(lines), lines)
      # create token
      token <-  lyric_df %>% unnest_tokens(word, lines) %>%
        #remove stop words
        anti_join(stop_words)
      # count each lyric 
      token %>% group_by(word) %>% summarise(count=n()) %>% arrange(desc(count)) %>%
        #keep the to 10 lytics
        head(10) %>%
        #plot bar graph
        ggplot(aes(x=reorder(word, -count), y=count, fill=word)) + geom_bar(stat="identity") +
        labs(x="Word", y="Count") +  ggtitle("10 Top Lyrics")
      
    }
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)