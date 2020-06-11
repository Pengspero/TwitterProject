#Creating an application on twitter#
#Acessing Twitter From R#
install.packages("httr")
library("httr")
library("rtweet")
library("dplyr")
##Set up the authentification to connect to Twitter##
twitter_token<-create_token(
        app = "Analysis For DCFBHK",
        consumer_key = "Your consumer key",
        consumer_secret= "Your secret Key",
        set_renv = TRUE
)

###Searching for tweets###
target<-get_timeline("@joshuawongcf",n=10000)

##Analyze the tweets##
#remove the retweets and replies#
target_tweets_organic<-target[target$is_retweet==FALSE,]
#remove the replies##
traget_tweets_organic<-subset(target_tweets_organic,is.na(target_tweets_organic$reply_to_status_id))

#find the favourite and retweet counts of target#
target_tweets_organic<-target_tweets_organic%>%arrange(-favorite_count)
target_tweets_organic[1,5]

target_tweets_organic<-target_tweets_organic%>%arrange(-retweet_count)
target_tweets_organic[1,5]


###show the ratio of replies/retweets/organic tweets####
#keep only retweets#
target_retweets<-target[target$is_retweet==TRUE,]
#keep only the replies#
target_replies<-subset(target,!is.na(target$reply_to_status_id))
#create the data frame to summarize the organic tweets, retweets and replies#
data<-data.frame(category=c("Organic","Retweets","Replies"), count=c(2371,871,1177))

#Adding columns#
data$fraction=data$count/sum(data$count)
data$percentage=data$count/sum(data$count)*100
data$ymax=cumsum(data$fraction)
data$ymin=c(0,head(data$ymax,n=-1))
#rounding data to two decimal points
data.plot<-data[,-1]
data.plot<-round(data.plot,2)
data.plot$category=data$category

#plot the ratio#
#specify the legend#
type_of_tweets<-paste(data.plot$category,data.plot$percentage,"%")
#plotting#
ggplot(data.plot, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=1, fill=type_of_tweets)) +
        geom_rect() +
        coord_polar(theta="y") + 
        xlim(c(1, 4)) +
        theme_void() +
        theme(legend.position = "right")
dev.copy(jpeg,file="twitter ratio.jpeg",width=480,height=480)
dev.off()

#SHOW WHEN THE TWEETS ARE PUBLISHED#
colnames(target)[colnames(target)=="screen_name"]<-"Twitter_account"

p<-ts_plot(dplyr::group_by(target,Twitter_account,"year"))
p+ggplot2::theme_minimal()+
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))+
        ggplot2::labs(
                x=NULL,y=NULL,
                tittle="Frequency of Tweets from Trash",
                subtitle = "Tweets counts aggregated by year",
                caption = "\nSource:Data collect from Twitter API by PFL"
        )

##SHOW FROM WHERE THE TWEETS ARE PUBLISHED##
target_app<-target%>%
        select(source)%>%
        group_by(source)%>%
        summarize(count=n())
target_app<-subset(target_app,count>10)

data.plot1<-data.frame(category=target_app$source,
                       count=target_app$count)
data.plot1$fraction=data.plot1$count/sum(data.plot1$count)
data.plot1$percentage=data.plot1$count/sum(data.plot1$count)*100
data.plot1$ymax=cumsum(data.plot1$fraction)
data.plot1$ymin=c(0,head(data.plot1,n=1))
data.Plot1<-data.plot1[,-1]
data.Plot1<-round(data.Plot1,2)
data.Plot1$category=data.plot1$category
View(data.Plot1)

Source<-paste(data.Plot1$category,data.Plot1$percentage,"%")
dataplot1<-data.Plot1[,-1]
datapLot<-dataplot1[,-1]
dataPLOT<-datapLot[,-1]

sp<-ggplot(dataPLOT,aes(x="percentage",y=category,fill=Source))+geom_bar(width = 1,stat = "identity")
pie<-sp+coord_polar("y",start = 0)
pie


###SHOW THE MOST FREQUENT WORDS FOUND IN THE TWEETS###
##remove all the nonsense character#
target_tweets_organic$text<-gsub("https\\S*","",target_tweets_organic$text)
target_tweets_organic$text<-gsub("@\\S*","",target_tweets_organic$text)
target_tweets_organic$text<-gsub("amp","",target_tweets_organic$text)
target_tweets_organic$text<-gsub("[\r\n]","",target_tweets_organic$text)
target_tweets_organic$text<-gsub("[[:punct:]]","",target_tweets_organic$text)

#remove the stop words from the text#
install.packages("tokenizers")
library(tokenizers)
install.packages("tidytext")
library(tidytext)
tweets<-target_tweets_organic%>%
        select(text)%>%
        unnest_tokens(word,text)
tweets<-tweets%>%
        anti_join(stop_words)

tweets %>% # gives you a bar chart of the most frequent words found in the tweets
        count(word, sort = TRUE) %>%
        top_n(15) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(x = word, y = n)) +
        geom_col() +
        xlab(NULL) +
        coord_flip() +
        labs(y = "Count",
             x = "Unique words",
             title = "Most frequent words found in the tweets of Trash",
             subtitle = "Stop words removed from the list")


