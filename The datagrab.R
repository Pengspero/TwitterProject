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
with(data.plot,qplot(aes(ymax=ymax,ymin=ymin,fill=type_of_tweets)))+geom_rect()+
        coord_polar(theta='y')+
        theme_void()+
        theme(legend.position="right")
dev.copy(jpeg,file="twitter ratio.jpeg",width=480,height=480)
dev.off()
