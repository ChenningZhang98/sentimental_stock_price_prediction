###APAN 5205 Final Project
###Tweets sentiment analysis on predicting stock prices###
###Author: Junyu Zhang, Chenning Zhang, Eric Yang


###library
library(tidyquant)
library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)
library(utils)
library(GGally)
library(devtools)
library(dplyr)
library(tidyr)
library(ggplot2)
library(httr)
library(stringr)
library(twitteR)
library(magrittr)
library(SentimentAnalysis)
library(gridExtra)
library(rtweet)
###
aapl <- tq_get('AAPL',
               from = "2021-04-06",
               to = "2021-04-14",
               get = "stock.prices")

AAPL_4_7 = search_tweets(q = "AAPL", n = 2000, since= "2021-04-7",until = "2021-04-8")
AAPL_4_8 = search_tweets(q = "AAPL", n = 2000, since= "2021-04-8",until = "2021-04-9")
AAPL_4_9  = search_tweets(q = "AAPL", n = 2000, since= "2021-04-9",until = "2021-04-10")
AAPL_4_10  = search_tweets(q = "AAPL", n = 2000, since= "2021-04-10",until = "2021-04-11")
AAPL_4_11 = search_tweets(q = "AAPL", n = 2000, since= "2021-04-11",until = "2021-04-12")
AAPL_4_12 = search_tweets(q = "AAPL", n = 2000, since= "2021-04-12",until = "2021-04-13")
AAPL_4_13 = search_tweets(q = "AAPL", n = 2000, since= "2021-04-13",until = "2021-04-14")
AAPL = rbind(AAPL_4_7,AAPL_4_8,AAPL_4_9,AAPL_4_10,AAPL_4_11,AAPL_4_12,AAPL_4_13)
##read.csv('APPL_raw.csv')


tweetsdf <-as.data.frame(AAPL)

x <- tweetsdf
x$text <- enc2native(x$text)
x$text <- gsub("^[[:space:]]*","",x$text) 
x$text <- gsub("[[:space:]]*$","",x$text) 
x$text <- gsub(" +"," ",x$text) 
x$text <- gsub("'", "%%", x$text)
x$text <- iconv(x$text, "latin1", "ASCII", sub="") 
x$text <- gsub("<(.*)>", "", x$text) 
x$text <- gsub("\\ \\. ", " ", x$text) 
x$text <- gsub("  ", " ", x$text) 
x$text <- gsub("%%", "\'", x$text) 
x$text <- gsub("https(.*)*$", "", x$text) 
x$text <- gsub("\\n", "-", x$text) 
x$text <- gsub("--", "-", x$text) 
x$text <- gsub("&amp;", "&", x$text)
x$text[x$text == " "] <- "<no text>"

cleanTweets <- x %>% 
  select("text")





sentiment <- analyzeSentiment(cleanTweets)
sentiment2 <- sentiment$SentimentQDAP
sentiment3 <- convertToDirection(sentiment$SentimentQDAP)

date <- x$created
date <- str_extract(date, "\\d{4}-\\d{2}-\\d{2}")
date <- as.Date(date)
date <- as.Date(date, format = "%m/%d/%y")


df <- cbind(cleanTweets, sentiment2, sentiment3, date)
df <- df[complete.cases(df), ]
df2 <- df %>% 
  group_by(date) %>%
  summarize(meanSentiment = mean(sentiment2, na.rm=TRUE))


DT::datatable(df2, editable = TRUE)

freq <- df %>% 
  group_by(date,sentiment3) %>% 
  summarise(Freq=n())

#Convert data from long to wide
freq2 <- freq %>% 
  spread(key = sentiment3, value = Freq)

DT::datatable(freq2, editable = TRUE)


ggplot() + 
  geom_bar(mapping = aes(x = freq$date, y = freq$Freq, fill = freq$sentiment3), stat = "identity") +
  ylab('Sentiment Frequency') +
  xlab('Date')

aapl <- tq_get('AAPL',
               from = "2021-04-07",
               to = "2021-04-13",
               get = "stock.prices")
apple <- subset(aapl, select = c(date, close))

aapl$date <- as.Date(aapl$date)


mu <- mean(aapl$close)
sd <- sd(aapl$close)
aapl2 <- aapl %>% 
  mutate(zScore = (aapl$close-mu)/sd)



p1 <- ggplot(data=df2, aes(x=date,y=meanSentiment, group=1)) +
  geom_line()+
  geom_point() +
  ylab("Mean Twitter Sentiment Score")


p2 <- ggplot(data=aapl2, aes(x=date,y=zScore, group=1)) +
  geom_line()+
  geom_point() +
  ylab("Z-Score of closing stock price")
scale_x_date(date_breaks = "1 day", 
             limits = as.Date(c('2021-04-12','2021-04-14')))


plot1 <- p1
plot2 <- p2
grid.arrange(plot1, plot2, nrow=2)


ggplot() + 
  geom_line(mapping = aes(x = aapl2$date, y = aapl2$zScore), size = 1) + 
  geom_line(mapping = aes(x = df2$date, y = df2$meanSentiment*20), size = 1, color = "blue") +
  scale_x_date(name = "date", labels = NULL) +
  scale_y_continuous(name = "z-Score of Closing Stock Price", 
                     #Scale 2nd y-axis by factor of 20
                     sec.axis = sec_axis(~./20, name = "Sentiment Score")) + 
  theme(
    axis.title.y = element_text(color = "grey"),
    axis.title.y.right = element_text(color = "blue"))







plot(df2$date,df2$meanSentiment, type="l", col="red3",  xlab='Date', ylab='Mean Sentiment Score')

par(new=TRUE)

plot(aapl2$date,aapl2$zScore, type="l", axes=F, xlab=NA, ylab=NA, col="blue")
axis(side = 4)
mtext(side = 4, line = 3, 'Closing Stock Price z-Score')
legend("topright",
       legend=c("Mean Sentiment Score"),
       lty=c(1,0), col=c("red3"))
####Correlation Analysis 


aapl$date <- as.Date(aapl$date, format = "%Y-%m-%d")

aapl$date <- as.Date(strptime(aapl$date, format = "%Y-%m-%d"))


require(dplyr)
tweets_plus_stock <- left_join(df, aapl, by = "date")


weekday_tweets_plus_stock <- tweets_plus_stock
weekday_tweets_plus_stock <- subset(tweets_plus_stock, !is.na(close))

# mark tweets as positive or negative or neutral
weekday_tweets_plus_stock$positive <- as.numeric(weekday_tweets_plus_stock$sentiment2 > 
                                                   0)

weekday_tweets_plus_stock$negative <- as.numeric(weekday_tweets_plus_stock$sentiment2 < 
                                                   0)

weekday_tweets_plus_stock$neutral <- as.numeric(weekday_tweets_plus_stock$sentiment2 == 
                                                  0)


# Showing the sum positives, negatives and neutral tweets per day
require(plyr)
tweets_plus_stock_df <- ddply(weekday_tweets_plus_stock, c("date", "high", "low", 
                                                           "close"), plyr::summarise, pos.count = sum(positive), neg.count = sum(negative), 
                              neu.count = sum(neutral))

# a new feature with the sum tweets of the 3 sentiment categories
tweets_plus_stock_df$sum.count <- tweets_plus_stock_df$pos.count + tweets_plus_stock_df$neg.count + 
  tweets_plus_stock_df$neu.count

# negative tweets for each day
tweets_plus_stock_df$percentage.negatives <- round((tweets_plus_stock_df$neg.count/tweets_plus_stock_df$sum.count) * 
                                                     100)

##Correlation AAPL closing stock price vs Negative Tweets Sentiments
require(GGally)
ggpairs(tweets_plus_stock_df, columns = c(10, 4), ggplot2::aes(color = "red"), 
        title = "Correlation: Closing Stock Price VS Positive Tweets Sentiment", upper = list(continuous = wrap("cor", 
                                                                                                                size = 10)), lower = list(continuous = "smooth"))




##Regression model percentage of negative tweets vs stock closing price
library(stats)
cor(tweets_plus_stock_df$percentage.negative, tweets_plus_stock_df$close, 
    use = "complete")


glm_model_neg <- glm(tweets_plus_stock_df$close ~ tweets_plus_stock_df$percentage.negative)
summary(glm_model_neg)
require(lattice)
xyplot(tweets_plus_stock_df$close ~ tweets_plus_stock_df$percentage.negatives, 
       grid = TRUE, type = c("p", "r"), col.line = "red", lwd = 3, ylab = "Daily Change of Apple Share Price in $", 
       xlab = "% of Negative Tweets", main = "% of negative Tweets vs Daily
       Apple Share Price")
tweets_plus_stock_df$percentage.positives <- round((tweets_plus_stock_df$pos.count/tweets_plus_stock_df$sum.count) * 
                                                     100)




##Regression model percentage of positive tweets vs stock closing price
library(stats)
cor(tweets_plus_stock_df$percentage.positives, tweets_plus_stock_df$close, 
    use = "complete")
glm_model_pos <- glm(tweets_plus_stock_df$close ~ tweets_plus_stock_df$percentage.positives)
summary(glm_model_pos)
require(lattice)
xyplot(tweets_plus_stock_df$close ~ tweets_plus_stock_df$percentage.positives, 
       grid = TRUE, type = c("p", "r"), col.line = "blue", lwd = 3, ylab = "Daily Change of Apple Share Price in $", 
       xlab = "% of Positive Tweets", main = "% of positive Tweets vs Daily
       Apple Share Price")

######wordcloud####
library(tm)
corpus = Corpus(VectorSource(AAPL$text))
corpus = tm_map(corpus, FUN = content_transformer(tolower))
corpus = tm_map(corpus, FUN = removePunctuation)
corpus = tm_map(corpus, FUN = removeWords, c(stopwords('english')))
corpus = tm_map(corpus, FUN = stripWhitespace)
corpus_A = corpus



Textprocessing <- function(x)
{gsub("http[[:alnum:]]*",'', x)
  gsub('http\\S+\\s*', '', x) 
  gsub('#\\S+', '', x)  
  gsub('@\\S+', '', x)  
  gsub('[[:cntrl:]]', '', x) 
  gsub("\\d", '', x) 
  gsub(" +"," ",x) 
  gsub("[[:space:]]*$","",x) 
  gsub("^[[:space:]]*","",x) 
  iconv(x, "latin1", "ASCII", sub="") 
  gsub("<(.*)>", "", x)
  gsub("\\ \\. ", " ", x) 
  gsub("--", "-", x) 
  gsub("&amp;", "&", x) 
  gsub("  ", " ", x) 
  gsub("%%", "\'", x) 
}

corpus_A = tm_map(corpus_A, Textprocessing)


library(wordcloud)
AAPL_wordcloud <- wordcloud(corpus_A,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)