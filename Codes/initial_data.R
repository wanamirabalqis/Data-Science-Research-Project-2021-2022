library(ggplot2)
library(dplyr)

url <- paste("/Users/balqishakim/Documents/UM/Semester 3 - MDS/Twitter.csv")
twitter <- read.csv(url)
head(twitter)
str(twitter)
summary(twitter)

twitter <- twitter %>% 
  group_by(veracity) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(twitter, aes(x="", y=veracity, fill=veracity)) +
  geom_bar(stat="identity", width=1) +
  geom_label(aes(label = labels),
            position = position_stack(vjust=0.5),
            show.legend = FALSE) +
  coord_polar("y", start=0)

sentiment <- as.factor(twitter$sentiment)
twitter <- twitter %>%
  group_by(sentiment) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(twitter, aes(x="", y=sentiment, fill=sentiment)) +
  geom_bar(stat="identity", width=1) +
  geom_label(aes(label = labels),
             position = position_stack(vjust=0.5),
             show.legend = FALSE) +
  coord_polar("y", start=0)



ggcorr(twitter)

install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("wordcloud2")
library(wordcloud2)Ye

install.packages("tm")
library(tm)

install.packages("slam")
library(slam)

#Create a vector containing only the text
tweet <- twitter$content
# Create a corpus  
docs <- Corpus(VectorSource(tweet))

dtm <- TermDocumentMatrix(tweet) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

set.seed(1234) # for reproducibility 
wordcloud(words = twitter$content, freq = df$freq, min.freq = 1,           max.words=200, random.order=FALSE, rot.per=0.35,            colors=brewer.pal(8, "Dark2"))
