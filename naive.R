library(tm)
library(NLP)
library(e1071)
library(Rstem)
library(SnowballC)
library(ggplot2)
library(RCurl)
library(ggmap)
library(RgoogleMaps)
library(xml2)
library(sentiment)
library(RTextTools)
library(RColorBrewer)
library(png)
library(plotrix)

df<-read.csv("test.csv")


class_emo = classify_emotion(df$review, algorithm="bayes", verbose = TRUE)

emotion = class_emo[,7]

emotion[is.na(emotion)] = "unknown"


barplot(table(emotion))


# classify polarity
class_pol = classify_polarity(df$review, algorithm="bayes",verbose = TRUE)
# get polarity best fit
polarity = class_pol[,4]

barplot(table(polarity))
conf.mat <- table("predictions" = emotion, Actual= polarity)
conf.mat
(accuracy <- sum(diag(conf.mat))/length(emotion) *100)


df<-cbind(df,data.frame(emotion,polarity))


#Emotion & Polarity from Review Topics
class_emo_title = classify_emotion(df$review, algorithm="bayes", verbose = TRUE)
emotion_title = class_emo_title[,7]
emotion_title[is.na(emotion_title)] = "unknown"
barplot(table(emotion_title))

class_pol_title = classify_polarity(df$review, algorithm="bayes")
polarity_title = class_pol_title[,4]
barplot(table(polarity_title))

df<-cbind(df,data.frame(emotion_title,polarity_title))

#Create Customer Id for all customers
df$cust_id<-rownames(df)

#Write the final dataframe to a csv for analysis
write.table(df, file="sentimentanalysis.txt")


