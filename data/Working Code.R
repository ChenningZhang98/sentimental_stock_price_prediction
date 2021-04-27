library(dplyr)
library(tidyr)
library(ggplot2)
library(httr)
library(stringr)
library(twitteR)
library(magrittr)
library(SentimentAnalysis)
require(gridExtra)
library(tidytext)


setwd('C:/Users/chenn/OneDrive/desktop/5205')
df = read.csv('AAPL_4_6.csv')
library(tidyquant)
library(ggplot2)
library(quantmod)

fanga = c('FB','AAPL','NFLX','GOOG','AMZN','TLSA')
stocks = getSymbols(fanga,from='2010-01-01')


aapl <- tq_get('AAPL',
               from = "2021-04-06",
               to = "2021-04-14",
               get = "stock.prices")


library(rtweet)
#tweets = search_tweets(q = 'hadleywickham',n = 10, include_rts = F)
AAPL_4_7 = search_tweets(q = "AAPL", n = 1500, since= "2021-04-7",until = "2021-04-8", include_rts = F)
AAPL_4_8 = search_tweets(q = "AAPL", n = 1500, since= "2021-04-8",until = "2021-04-9", include_rts = F)
AAPL_4_9  = search_tweets(q = "AAPL", n = 1500, since= "2021-04-9",until = "2021-04-10", include_rts = F)
AAPL_4_10  = search_tweets(q = "AAPL", n = 1500, since= "2021-04-10",until = "2021-04-11", include_rts = F)
AAPL_4_11 = search_tweets(q = "AAPL", n = 1500, since= "2021-04-11",until = "2021-04-12", include_rts = F)
AAPL_4_12 = search_tweets(q = "AAPL", n = 1500, since= "2021-04-12",until = "2021-04-13", include_rts = F)
AAPL_4_13 = search_tweets(q = "AAPL", n = 1500, since= "2021-04-13",until = "2021-04-14", include_rts = F)
AAPL = rbind(AAPL_4_7,AAPL_4_8,AAPL_4_9,AAPL_4_10,AAPL_4_11,AAPL_4_12,AAPL_4_13)



############################################
tweetsdf <-as.data.frame(AAPL)
x <- tweetsdf
x$text <- enc2native(x$text)
x$text <- gsub("^[[:space:]]*","",x$text) # Remove whitespaces
x$text <- gsub("[[:space:]]*$","",x$text)
x$text <- gsub(" +"," ",x$text) 
x$text <- gsub("'", "%%", x$text) #Replace apostrophes with %%
x$text <- iconv(x$text, "latin1", "ASCII", sub="") # Remove emoj
x$text <- gsub("<(.*)>", "", x$text) #Remove Unicodes 
x$text <- gsub("\\ \\. ", " ", x$text) 
x$text <- gsub("  ", " ", x$text) #Replace double space with single space
x$text <- gsub("%%", "\'", x$text) #Change %% back to apostrophes
x$text <- gsub("https(.*)*$", "", x$text) #Remove tweet URL
x$text <- gsub("\\n", "-", x$text) #Replace line breaks with "-"
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
             limits = as.Date(c('2021-04-07','2021-04-14')))


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
#################################################
####Correlation Analysis 


aapl$date <- as.Date(aapl$date, format = "%Y-%m-%d")

aapl$date <- as.Date(strptime(aapl$date, format = "%Y-%m-%d"))


require(dplyr)
tweets_plus_stock <- left_join(df, aapl, by = "date")


weekday_tweets_plus_stock <- tweets_plus_stock
weekday_tweets_plus_stock <- subset(tweets_plus_stock, !is.na(close))


# negative or neutral
weekday_tweets_plus_stock$positive <- as.numeric(weekday_tweets_plus_stock$sentiment2 > 
                                                   0)

weekday_tweets_plus_stock$negative <- as.numeric(weekday_tweets_plus_stock$sentiment2 < 
                                                   0)

weekday_tweets_plus_stock$neutral <- as.numeric(weekday_tweets_plus_stock$sentiment2 == 
                                                  0)


# Showing the sum positives, negatives and newtral tweets per day
require(plyr)
tweets_plus_stock_df <- ddply(weekday_tweets_plus_stock, c("date", "high", "low", 
                                                           "close"), plyr::summarise, pos.count = sum(positive), neg.count = sum(negative), 
                              neu.count = sum(neutral))

# We add a new feature with the sum tweets of the 3 sentiment categories
tweets_plus_stock_df$sum.count <- tweets_plus_stock_df$pos.count + tweets_plus_stock_df$neg.count + 
  tweets_plus_stock_df$neu.count

# We calculate the percentage of negative tweets for each day, and add it as
# a new feature
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
##############################################

apple <- subset(aapl, select = c(date, close))

aapl$date <- as.Date(aapl$date)

##wordcloud



library(tm)
corpus = Corpus(VectorSource(AAPL$text))
corpus = tm_map(corpus, FUN = content_transformer(tolower))
corpus = tm_map(corpus, FUN = removePunctuation)
corpus = tm_map(corpus, FUN = removeWords, c(stopwords('english')))
corpus = tm_map(corpus, FUN = stripWhitespace)
corpus_A = corpus



Textprocessing <- function(x)
{gsub("http[[:alnum:]]*",'', x)
  gsub('http\\S+\\s*', '', x) ## Remove URLs
  gsub('#\\S+', '', x) ## Remove Hashtags
  gsub('@\\S+', '', x) ## Remove Mentions
  gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
  gsub("\\d", '', x) ## Remove Controls and special characters
  gsub(" +"," ",x) #Remove extra whitespaces
  gsub("[[:space:]]*$","",x) # Remove trailing whitespaces
  gsub("^[[:space:]]*","",x) # Remove leading whitespaces
  iconv(x, "latin1", "ASCII", sub="") # Remove emojis
  gsub("<(.*)>", "", x) #Remove Unicodes like <U+A>
  gsub("\\ \\. ", " ", x) #Replace orphaned fullstops with space
  gsub("--", "-", x) #Remove double "-" from double line breaks
  gsub("&amp;", "&", x) #Fix ampersand &
  gsub("  ", " ", x) #Replace double space with single space
  gsub("%%", "\'", x) #Change %% back to apostrophes
}

corpus_A = tm_map(corpus_A, Textprocessing)


library(wordcloud)
AAPL_wordcloud <- wordcloud(corpus_A,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)
AAPL_words = AAPL%>%
  group_by(screen_name)%>%
  unnest_tokens(output = word,input = text)%>%
  ungroup()%>%
  mutate(row=1:n())
as.data.frame(AAPL_words)[1:50,c('screen_name','word')]

as.data.frame(get_sentiments('bing'))[1:50,]
get_sentiments('bing')%>%
  group_by(sentiment)%>%
  count()


library(ggplot2)
AAPL_words %>%
  inner_join(get_sentiments('bing'),by = 'word')%>%
  select('sentiment')%>%
  group_by(sentiment)%>%
  summarize(freq=n())%>%
  ungroup()%>%
  ggplot(aes(x=sentiment,y=freq))+geom_bar(position='dodge',stat='identity',fill=c('darkred','darkgreen'))+
  coord_flip()



## afinn dictionary 
afinn = read.table('affin.txt',header = F,sep = '\t',col.names = c('word','value'))

afinn %>%
  group_by(value)%>%
  count()

AAPL_words %>%
  inner_join(afinn,by = 'word')%>%
  select('value')%>%
  ggplot(aes(x=value))+geom_histogram()

AAPL_words %>%
  left_join(afinn,by = 'word')%>%
  group_by(screen_name)%>%
  summarize(value = mean(value,na.rm=T))%>%
  ungroup()%>%
  select('screen_name','value')%>%
  ggplot(aes(x=value))+geom_histogram()

AAPL_words %>%
  inner_join(afinn,by = 'word')%>%
  group_by(screen_name)%>%
  summarize(tweet_sentiment = mean(value,na.rm=T))%>%
  ungroup()

AAPL_words %>%
  inner_join(afinn,by = 'word')%>%
  group_by(screen_name)%>%
  summarize(tweet_sentiment = mean(value,na.rm=T))%>%
  ungroup()%>%
  summarize(Overall_Sentiment=mean(tweet_sentiment,na.rm=T))

##nrc dictionary
nrc = read.table(file = 'https://raw.githubusercontent.com/pseudorational/data/master/nrc_lexicon.txt',header = F,col.names = c('word','sentiment','num'),sep = '\t'); nrc = nrc[nrc$num!=0,]; nrc$num = NULL

nrc%>%
  group_by(sentiment)%>%
  count()


library(RColorBrewer)
AAPL_words %>%
  inner_join(get_sentiments('nrc'),by = 'word')%>%
  select('sentiment')%>%
  group_by(sentiment)%>%
  summarize(freq=n())%>%
  ungroup() %>%
  ggplot(aes(x=reorder(sentiment,desc(freq)),y=freq,fill=freq))+geom_bar(position='dodge',stat='identity')+xlab('Sentiment')+ylab('Frequency')+coord_flip()

df<- apply(AAPL_4_6,2,as.character)
write.csv(df,'AAPL_4_6.csv', row.names =FALSE)