library(dplyr)
library(ggplot2)
library(stringr)
library(tm)
library(SnowballC)
library(tidyr)
library(tidytext)
library(wordcloud)
library(plotly)
library(RColorBrewer)
library(textdata)
library(colortools)
library(magrittr)

geniusLyricsUnmodified <- read.csv('lyrics_with_dates.csv', stringsAsFactors = F)

geniusLyrics <- geniusLyricsUnmodified %>%
  mutate(Views = as.numeric(str_remove_all(Views, pattern = 'M'))) %>%
  mutate(Release.Date = as.Date(Release.Date, format = '%B %d, %Y')) %>%
  mutate(Lyrics = str_remove_all(string = Lyrics, pattern = '\\s*\\([^\\)]+\\)')) %>%
  mutate(Lyrics = str_remove_all(string = Lyrics, pattern = '\\s*\\[[^\\]]+\\]')) %>%
  mutate(Lyrics = iconv(Lyrics, to = 'ASCII', sub = '')) %>%
  mutate(Lyrics = trimws(Lyrics)) %>%
  filter(Title != 'Despacito (Remix)') %>%
  mutate(Year = as.factor(format(Release.Date, '%Y'))) %>%
  mutate(Position = row_number())

lyricsByYears <- geniusLyrics %>%
  group_by(Year) %>%
  filter(Position == min(Position))%>%
  as.data.frame()

colors <- c("#4BB446","#46B478","#46B4AF",
            "#4682B4","#464BB4","#7846B4",
            "#AF46B4","#B44682" ,"#B4464B",
            "#B47846","#B4AF46","#82B446")

compareAndCloud <- function(lyrics,colNames,cols,option=weightTf,setThresold=F){
  vs <- VectorSource(lyrics)
  cp <- VCorpus(vs)
  tdm <- TermDocumentMatrix(cp, control = list(
    removePunctuation = T,
    stopwords = T,
    removeNumbers = T,
    weighting = option
  ))
  if(setThresold){
    tdm <- removeSparseTerms(tdm,0.99) 
  }
  lyricMatrix <- as.matrix(tdm)
  colnames(lyricMatrix) <- colNames
  set.seed(2)
  comparison.cloud(lyricMatrix, 
                   max.words = 100,
                   colors= cols,
                   scale=c(1,0.5),
                   title.size=1,
                   title.colors = cols)
}

createComparisonCloud <- function(filtered,option=weightTf,setThresold=F){
  compareAndCloud(lyrics = filtered$Lyrics,colNames = filtered$Year, cols = colors,
                  option = option, setThresold = setThresold)
}

cloudPlot <- createComparisonCloud(lyricsByYears)
cloudPlot

visualizeTopNWords <- function(filtered,topNum){
  vs <- VectorSource(filtered$Lyrics)
  cp <- VCorpus(vs)
  
  tdm <- TermDocumentMatrix(cp, control = list(
    removePunctuation = T,
    stopwords = T,
    removeNumbers = T
  ))
  lyricsMatrix <- as.matrix(tdm)
  
  df_freq <- data.frame(terms=rownames(lyricsMatrix), 
                        freq=rowSums(lyricsMatrix), 
                        stringsAsFactors = F)
  
  
  top_num <- df_freq %>% 
    top_n(topNum, freq) %>%
    arrange(desc(freq))
  
  top_num %>%
    ggplot(aes(x=reorder(terms,freq),y = freq, fill=terms))+
    geom_bar(stat="identity")+
    labs(x="Words", y="Number of occurences",
         title="The most popular words in the most popular songs for filtered Years")+
    theme(legend.position = "none")
}

topNWords <- visualizeTopNWords(lyricsByYears,12)

commonWordsCount <- function(lyrics){
  vs <- VectorSource(lyrics)
  cp <- VCorpus(vs)
  tdm <- TermDocumentMatrix(cp, control = list(
    removePunctuation = T,
    stopwords = T,
    removeNumbers = T
  ))
  commonMatrix <- as.matrix(tdm)
  colnames(commonMatrix) <- c("first","second")
  rownames(tdm)
  commonWords <- data.frame(terms = rownames(commonMatrix),commonMatrix) %>% 
    filter(first > 0 & second > 0)
  return(nrow(commonWords))
}

compareCommons <- function(filtered){
  i <- 1
  commonCounts <- c()
  filtered <- filtered %>%
    arrange(desc(Year))
  ranges <- c()
  
  while(i < nrow(filtered)){
    first <- filtered[i,]
    second <- filtered[i+1,]
    combined <- c(first$Lyrics,second$Lyrics)
    commonCounts <- c(commonCounts, commonWordsCount(combined))
    ranges <- c(ranges,paste(second$Year,first$Year, sep = "&"))
    i = i + 2
  }
  
  commonDf <- data.frame(ranges, commonCounts)
  colnames(commonDf) <- c("Mutual","Count")
  commonDf$Mutual <- as.factor(commonDf$Mutual)
  return(commonDf)
}

commonWords <- compareCommons(lyricsByYears) %>%
  ggplot(aes(x = reorder(Mutual,Count), y = Count, fill = Mutual))+
  geom_bar(stat = "identity", fill = colors[1:6])+
  labs(x="The years of the songs", y="The number of common words",
       title="The distribution of common words in each range of succeding unique years")+
  theme(legend.position = "none")+
  coord_flip()


compareOldestNewest <- function(filtered){
  oldestLyrics <- filtered %>% 
    filter(Year == '1975')
  
  newestLyrics <- filtered %>%
    filter(Year == '2019')
  
  lyricsForBoth <- c(oldestLyrics$Lyrics, newestLyrics$Lyrics)
  colnames <- c('1975','2019')
  colors <- c('red','blue')
  compareAndCloud(lyricsForBoth,colNames = colnames,cols = colors)
}


compareOldestNewest(lyricsByYears)

tokenizedLyrics <- geniusLyrics %>%
  unnest_tokens(word, Lyrics)

# weight
afinnSentiment <- tokenizedLyrics %>%
  inner_join(get_sentiments("afinn"), by = "word")

# pos /neg
bingSentiments <- tokenizedLyrics %>%
  inner_join(get_sentiments("bing"), by = "word")

# emotion
loughranSentiments <- tokenizedLyrics %>%
  inner_join(get_sentiments("loughran"), by = "word")

# Mood Wheel for each song
moodWheel <- function(title) {
  return(
    afinnSentiment %>%
      filter(Title == title) %>%
      mutate(position = row_number()) %>%
      ggplot(aes(x = position, y = value, fill = value)) +
      geom_bar(stat = 'identity') +
      coord_polar(theta = "x") +
      scale_fill_gradient(low="blue", high="orange", guide = guide_colourbar(title = "Sentiment", barwidth = 0.5)) + 
      labs(y = "", title = "Mood Wheel") +
      theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())
  )
}

filterSongs <- function(years = c("2019"), n = 10) {
  filteredSongs <- geniusLyrics %>%
    filter(Year %in% years)
  
  return (
    visualizeTopNWords(filteredSongs, n)
  )
}

visualiseEmotions <- function() {
  emotions <- loughranSentiments %>%
    filter(sentiment != "positive" & sentiment != "negative")

  overallEmotions <- emotions %>%
    group_by(sentiment) %>%
    summarise(count = n())
  
  return (
    plot_ly(
      type = 'scatterpolar',
      fill = 'toself',
      r = overallEmotions$count,
      theta = overallEmotions$sentiment,
      name = 'Emotions In Songs'
    ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0, max(overallEmotions$count))
          )
        ),
        showlegend = F
      )
  )
}

visualiseChartPolarity <- function() {
  changeOverRanking <- bingSentiments %>%
    group_by(Position) %>%
    summarise(negatives = sum(sentiment == "negative"), positives = sum(sentiment == "positive"), Title = Title[1]) %>%
    mutate(proportion =  positives / negatives) %>%
    mutate(polarity = positives - negatives)
  
  return (
    plot_ly(
      changeOverRanking,
      x = ~Position,
      y = ~proportion,
      text = ~Title,
      type = "scatter",
      mode = "markers",
      color = ~polarity,
      colors = brewer.pal(5, "Reds"),
      sizes = c(5, 300),
      size = ~polarity,
      marker = list(opacity = 0.5)
    ) %>%
      layout(
        title = "Ratio Change",
        xaxis = list(showgrid = FALSE, zeroline = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, title = "Proportion")
      )
  )
}

server <- function(input, output) {
  output$common <- renderPlot({
    commonWords
  })
  output$topwords <- renderPlot({
    topNWords
  })
  output$cloud <- renderPlot({
    compareOldestNewest(lyricsByYears)
  })
  output$wheel <- renderPlot({
    moodWheel(input$mooWheel)
  })
  output$topN <- renderPlot({
    filterSongs(input$years, input$wordCount)
  })
  output$emotions <- renderPlotly({
    visualiseEmotions()
  })
  output$polarityInChart <- renderPlotly({
    visualiseChartPolarity()
  })
}

shinyApp(ui, server)
