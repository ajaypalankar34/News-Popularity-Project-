# How does time of day reported affect popularity?

# Creating a variable for the hour in the day published
News_Final$TimeHr <- substr(News_Final$PublishDate, 12, 13)
News_Final$TimeHr <- as.integer(News_Final$TimeHr)

# Indexing hour of the day into night, morning, and evening
index_night <- which(News_Final$TimeHr >= 20 | News_Final$TimeHr < 4)
index_morning <- which(News_Final$TimeHr >= 4 & News_Final$TimeHr < 12)
index_evening <- which(News_Final$TimeHr >= 12 & News_Final$TimeHr < 20)

# Initializing the time of day variable for use
News_Final$Timeofday <- 1:93239

# Using indexes to fill out the Timeofday variable day 
News_Final$Timeofday[index_night] = "Night"
News_Final$Timeofday[index_morning] = "Morning"
News_Final$Timeofday[index_evening] = "Evening"

# Creating new variable combpop containing the combinded score from Facebook, Googleplus, and LinkedIn for each article
News_Final$combpop = News_Final$Facebook + News_Final$GooglePlus + News_Final$LinkedIn


#Finding the mean and median of combpop for Morning, Evening, and Night
mean(News_Final$combpop[which(News_Final$Timeofday == "Morning")])
mean(News_Final$combpop[which(News_Final$Timeofday == "Evening")])
mean(News_Final$combpop[which(News_Final$Timeofday == "Night")])

median(News_Final$combpop[which(News_Final$Timeofday == "Morning")])
median(News_Final$combpop[which(News_Final$Timeofday == "Evening")])
median(News_Final$combpop[which(News_Final$Timeofday == "Night")])

# The two are very different which tells us we have many outliers


# Creating boxplot of combpop for time of the day
boxplot(News_Final$combpop[which(News_Final$Timeofday == "Morning")], ylim = c(0, 50), main = "Popularity of articles posted in the morning", ylab = "popularity")
boxplot(News_Final$combpop[which(News_Final$Timeofday == "Evening")], ylim = c(0, 50), main = "Popularity of articles posted in the evening", ylab = "popularity" )
boxplot(News_Final$combpop[which(News_Final$Timeofday == "Night")], ylim = c(0, 50), main = "Popularity of articles posted at night", ylab = "popularity")

ggplot(data = subset(News_Final, Timeofday == "Evening" | Timeofday == "Morning")) + geom_boxplot(mapping = aes(fill = Timeofday, y = combpop)) + scale_y_continuous(limits = c(0,30)) + ggtitle("Popularity of articles posted in the morning and evening") + labs(y = "Popularity") + theme_dark()

news <- News_Final

# factorizing news source variable
table(news$Source)
news$Source <- as.factor(news$Source)

# factorizing news topic variable
table(news$Topic)
news$Topic <- as.factor(news$Topic)

# creating new variable, Popularity, as average of 3 platform popularity variables
news$Popularity <- (news$Facebook + news$GooglePlus + news$LinkedIn) / 3

source_names <- levels(news$Source)

#initialize source popularity vector
source_popularity <- c()

#initialize source number
source_number <- 1

# the goal is to populate the vector source_popularity with an average popularity 
# value for every article from that news source. to do this, we need to
# first create an index of every article's average popularity from a given news source,
# average these popularity values together, and then add this value to the
# popularity vector. we can use a while loop to do this like so:
while(length(source_popularity) < length(source_names)) {
  pop_index <- news$Popularity[which(news$Source == source_names[source_number])]
  pop_avg <- sum(pop_index) / length(pop_index)
  source_popularity <- append(source_popularity, pop_avg, after = length(source_popularity))
  source_number <- source_number + 1
}

# create new data frame with the source names, each source's popularity, and rank each source's popularity as a variable
news_pop = data.frame(source = source_names, popularity = source_popularity, rank = rank(source_popularity))

# subset the data to include only the 30 highest ranked news sources
news_most_pop <- subset(news_pop, rank > 5725)

# draw a barplot of this information! Code for figure 2
ggplot(data = news_most_pop, aes(x = reorder(source, -popularity), y = popularity, fill = popularity)) + geom_bar(stat = "identity") + labs(x = "News Source", y = "Average Popularity Score", title = "Most Popular News Sources") + theme(axis.text.x = element_text(face = "bold", angle = 90, vjust = 0.25), legend.position = "none")

news <- News_Final

# factorizing news source variable
table(news$Source)
news$Source <- as.factor(news$Source)

# factorizing news topic variable
table(news$Topic)
news$Topic <- as.factor(news$Topic)

news$Source <- factor(news$Source, levels = names(sort(table(news$Source), decreasing = TRUE)))

# subset news to include only the 10 most published news sources
news2 <- subset(news, Source == "Bloomberg" | Source == "Reuters" | Source == "ABC News" | Source == "New York Times" | Source == "The Guardian" | Source == "Business Insider" | Source == "Economic Times" | Source == "Forbes" | Source == "Washington Post" | Source == "CNN")

# create a barplot of this (Figure 3)
ggplot(data = news2) + geom_bar(mapping = aes(x = Source, fill = Source)) + labs(y = "Articles", title = "Most Popular News Sources") + theme(axis.text.x = element_text(angle = -45), legend.position = "none")

# subset news2 to include only articles on the economy topic
news_economy <- subset(news2, Topic == "economy")

# create boxplots of the headline sentiment scores for each source on the economy topic (Figure 4)
ggplot(data = news_economy) + geom_boxplot(mapping = aes(x = Source, y = SentimentHeadline), color = "red3") + labs(y = "Headline Sentiment Scores", title = "Headline Sentiment Scores: Economy") + theme(axis.text.x = element_text(angle = -45), legend.position = "none")

# subset news2 to include only articles on the microsoft topic
news_microsoft <- subset(news2, Topic == "microsoft")

# create boxplots of the headline sentiment scores for each source on the microsoft topic (Figure 5)
ggplot(data = news_microsoft) + geom_boxplot(mapping = aes(x = Source, y = SentimentHeadline), color = "blue3") + labs(y = "Headline Sentiment Scores", title = "Headline Sentiment Scores: Microsoft") + theme(axis.text.x = element_text(angle = -45), legend.position = "none")

# subset news2 to include only articles on the obama topic
news_obama <- subset(news2, Topic == "obama")

# create boxplots of the headline sentiment scores for each source on the economy topic (Figure 6)
ggplot(data = news_obama) + geom_boxplot(mapping = aes(x = Source, y = SentimentHeadline), color = "yellow3") + labs(y = "Headline Sentiment Scores", title = "Headline Sentiment Scores: Obama") + theme(axis.text.x = element_text(angle = -45), legend.position = "none")

# subset news2 to include only articles on the palestine topic
news_palestine <- subset(news2, Topic == "palestine")

# create boxplots of the headline sentiment scores for each source on the palestine topic (Figure 7)
ggplot(data = news_palestine) + geom_boxplot(mapping = aes(x = Source, y = SentimentHeadline), color = "green3") + labs(y = "Headline Sentiment Scores", title = "Headline Sentiment Scores: Palestine") + theme(axis.text.x = element_text(angle = -45), legend.position = "none")

# creating a News_Final variable to allow for R-Studio to read the comma separated value chart from the UCI Machine learning dataset

News_Final <- read.csv("News_Final.csv")

#create variables which will retrieve the total popularity scores of positive and negative articles for each platform based on sentiment headline score 

GooglePlus_Positive_Headline <- sum(News_Final$GooglePlus[which(News_Final$SentimentHeadline > 0)]) #Positive GooglePlus Articles

GooglePlus_Negative_Headline <- sum(News_Final$GooglePlus[which(News_Final$SentimentHeadline < 0)]) #Negative GooglePlus Articles

LinkedIn_Positive_Headline <- sum(News_Final$LinkedIn[which(News_Final$SentimentHeadline > 0)]) #Positive LinkedIn Articles

LinkedIn_Negative_Heaadline <- sum(News_Final$LinkedIn[which(News_Final$SentimentHeadline < 0)]) #Negative LinkedIn Articles

Facebook_Positive_Headline <- sum(News_Final$Facebook[which(News_Final$SentimentHeadline > 0)]) #Positive Facebook Articles

Facebook_Negative_Headline <- sum(News_Final$Facebook[which(News_Final$SentimentHeadline < 0)]) #Negative Facebook Articles 

#create variables which will retrieve the total popularity scores of positive and negative articles for each platform based on sentiment title score 

GooglePlus_Positive_Title <- sum(News_Final$GooglePlus[which(News_Final$SentimentTitle > 0)]) #Positive GooglePlus Articles

GooglePlus_Negative_Title <- sum(News_Final$GooglePlus[which(News_Final$SentimentTitle < 0)]) #Negative GooglePlus Articles

LinkedIn_Positive_Title <- sum(News_Final$LinkedIn[which(News_Final$SentimentTitle > 0)]) #Positive LinkedIn Articles

LinkedIn_Negative_Title <- sum(News_Final$LinkedIn[which(News_Final$SentimentTitle < 0)]) #Negative LinkedIn Articles
Facebook_Positive_Title <- sum(News_Final$Facebook[which(News_Final$SentimentTitle > 0)]) #Positive Facebook Articles
Facebook_Negative_Title <- sum(News_Final$Facebook[which(News_Final$SentimentTitle < 0)]) #Negative Facebook Articles

# Create a bar plot to represent the popularity scores for negative and positive articles among all Social media platforms

Negative_Positive_Popularity <- c("Google", "Google", "LinkedIn", "LinkedIn", "Facebook", "Facebook") # x-axis/ Social Media Platform Names

Popularity_score <- c(GooglePlus_Positive_Headline, GooglePlus_Negative_Headline, LinkedIn_Positive_Headline, LinkedIn_Negative_Heaadline, Facebook_Positive_Headline, Facebook_Negative_Headline) #Total Popularity Score for each of the positive and negative articles for each platform

legend <- c("Positive", "Negative") #Legend which differentiates between positive and negative articles for each social media platform

my_colors <- c("green", "red") #Colors used to represent popularity and negativity

barplot(Popularity_score, main = "Popularity of Negative vs Positive 
articles(SentimentHeadline)" , xlab = "Social media platform used", ylab = " Total Popularity Score", names.arg = Negative_Positive_Popularity, col = my_colors, beside = TRUE)

legend("topleft", legend = c("Positive", "Negative"), title = "Article Sentiment", fill = my_colors, cex = 0.8) #Legend used to help explain the bar plot

# Create a second bar plot to represent the popularity scores for negative and positive articles among all Social media platforms by Sentiment Title

Second_Popularity_Score <- c(GooglePlus_Positive_Title, GooglePlus_Negative_Title, LinkedIn_Positive_Title, LinkedIn_Negative_Title, Facebook_Positive_Title, Facebook_Negative_Title) #Total Popularity Score for each of the positive and negative articles for each platform by sentiment title

barplot(Second_Popularity_Score, main = "Popularity of Negative vs Positive articles(SentimentTitle)" , xlab = "Social media platform used", ylab = "Total Popularity Score", names.arg = Negative_Positive_Popularity, col = my_colors, beside = TRUE)

legend("topleft", legend = c("Positive", "Negative"), title = "Article Sentiment", fill = my_colors, cex = 0.8) #Legend used to help explain the bar plot

#Create variables which will receive the total popularity scores of economy, microsoft, palestine, and obama articles for each social media platform

economy_popularity_google <- sum(News_Final$GooglePlus[which(News_Final$Topic == "economy")]) #Popularity of economy articles on GooglePlus

microsoft_popularity_google <- sum(News_Final$GooglePlus[which(News_Final$Topic == "microsoft")]) #Popularity of microsoft articles on GooglePlus

palestine_popularity_google <- sum(News_Final$GooglePlus[which(News_Final$Topic == "palestine")])#Popularity of palestine articles on GooglePlus

obama_popularity_google <- sum(News_Final$GooglePlus[which(News_Final$Topic == "obama")]) #Popularity of obama articles on GooglePlus

economy_popularity_Facebook <- sum(News_Final$Facebook[which(News_Final$Topic == "economy")]) #Popularity of economy articles on Facebook
microsoft_popularity_Facebook <- sum(News_Final$Facebook[which(News_Final$Topic == "microsoft")]) #Popularity of microsoft articles on Facebook

palestine_popularity_Facebook <- sum(News_Final$Facebook[which(News_Final$Topic == "palestine")]) #Popularity of palestine articles on Facebook

obama_popularity_Facebook <- sum(News_Final$Facebook[which(News_Final$Topic == "obama")]) #Popularity of obama articles on Facebook

economy_popularity_LinkedIn <- sum(News_Final$LinkedIn[which(News_Final$Topic == "economy")]) #Popularity of economy articles on LinkedIn

microsoft_popularity_LinkedIn <- sum(News_Final$LinkedIn[which(News_Final$Topic == "microsoft")]) #Popularity of microsoft articles on LinkedIn

palestine_popularity_LinkedIn <- sum(News_Final$LinkedIn[which(News_Final$Topic == "palestine")]) #Popularity of palestine articles on LinkedIn

obama_popularity_LinkedIn <- sum(News_Final$LinkedIn[which(News_Final$Topic == "obama")]) #Popularity of obama articles on LinkedIn

# Create a bar plot which resembles the total popularity scores of articles of different topics among different platforms

Topic_Names <- c("eco", "micro", "palestine", "obama", "eco", "micro", "palestine", "obama", "economy", "micro", "palestine", "obama") #Topic names listed on x-axis

Topic_Popularity <- c(economy_popularity_google, microsoft_popularity_google, palestine_popularity_google, obama_popularity_google, economy_popularity_Facebook, microsoft_popularity_Facebook, palestine_popularity_Facebook, obama_popularity_Facebook, economy_popularity_LinkedIn, microsoft_popularity_LinkedIn, palestine_popularity_LinkedIn, obama_popularity_LinkedIn)
#Total Popularity Scores of each News Topic for each of the social media platforms

plot_colors <- c("green", "green", "green", "green", "blue", "blue", "blue", "blue", "red", "red", "red", "red") #colors used to represent the social media platforms

legend_colors <- c("green", "blue", "red") #Legend colors which correspond to plot_colors to help explain the colors in the bar plot

barplot(Topic_Popularity, main = "Total Popularity of News Topics" , xlab = " News Topic", ylab = " Total Popularity (Score)", names.arg = Topic_Names, col = plot_colors, beside = TRUE)

legend("topleft", legend = c("GooglePlus", "Facebook", "LinkedIn"), title = "Platform", fill = legend_colors, cex = 0.8) #legend which is used to explain the bar plot

# Taking the top 20 news sources from the data

sort(table(News_Final$Source), decreasing = T)[1:20]

# Assigning political bias to each news outlet from outside source
https://www.allsides.com/media-bias/media-bias-ratings?field_featured_bias_rating_value=All&field_news_source_type_tid[1]=1&field_news_source_type_tid[2]=2&field_news_source_type_tid[3]=3&field_news_source_type_tid[4]=4&field_news_bias_nid_1[1]=1&field_news_bias_nid_1[2]=2&field_news_bias_nid_1[3]=3&title=ZDNet

# Initializing the bias variable for use

News_Final$bias <- 1:93239

# Assigning news sources their bias

News_Final$bias[which((News_Final$Source == "Bloomberg") | (News_Final$Source == "ABC News") | (News_Final$Source == "New York Times") | (News_Final$Source == "The Guardian") | (News_Final$Source == "Economic Times") | (News_Final$Source == "Washington Post") | (News_Final$Source == "CNN"))] <- "Left"

News_Final$bias[which((News_Final$Source == "Reuters") | (News_Final$Source == "Business Insider") | (News_Final$Source == "Forbes") | (News_Final$Source == "Wall Street Journal") | (News_Final$Source == "Wall Street Journal") | (News_Final$Source == "WinBeta") | (News_Final$Source == "CNBC") | (News_Final$Source == "Reuters via Yahoo! Finance") | (News_Final$Source == "The Hill") | (News_Final$Source == "Financial Times") | (News_Final$Source == "USA TODAY") | (News_Final$Source == "ZDNet"))] <- "Center"

News_Final$bias[which((News_Final$Source == "Breitbart News") | (News_Final$Source == "Daily Mail") | (News_Final$Source == "Fox News") | (News_Final$Source == "Daily Caller"))] <- "Far Right"

News_Final$bias[which(News_Final$Source == "Huffington Post")] <- "Far Left"

News_Final$bias[which((News_Final$Source == "Washington Times") | (News_Final$Source == "MarketWatch"))] <- "Right"

# Calculating Means

Far_Left_H <- mean(News_Final$SentimentHeadline[which(News_Final$bias == "Far Left")])
[1] -0.02008842
Left_H <- mean(News_Final$SentimentHeadline[which(News_Final$bias == "Left")])
[1] -0.02457964
Center_H <- mean(News_Final$SentimentHeadline[which(News_Final$bias == "Center")])
[1] -0.02650299
Right_H <- mean(News_Final$SentimentHeadline[which(News_Final$bias == "Right")])
[1] -0.02020408
Far_Right_H <- mean(News_Final$SentimentHeadline[which(News_Final$bias == "Far Right")])
[1] -0.0353037
Far_Left_T <- mean(News_Final$SentimentTitle[which(News_Final$bias == "Far Left")])
[1] 0.004821679
Left_T <- mean(News_Final$SentimentTitle[which(News_Final$bias == "Left")])
[1] -0.006061708
Center_T <- mean(News_Final$SentimentTitle[which(News_Final$bias == "Center")])
[1] -0.002105816
Right_T <- mean(News_Final$SentimentTitle[which(News_Final$bias == "Right")])
[1] -0.02766623
Far_Right_T <- mean(News_Final$SentimentTitle[which(News_Final$bias == "Far Right")])
[1] -0.0140379

x_H <- c(Far_Left_H, Left_H, Center_H, Right_H, Far_Right_H)
names(x_H) <- c("Far Left", "Left", "Center", "Right", "Far Right")

barplot(x_H, main = "Positivity of Article Headline by Bias", ylab = "Bias", xlab = "Sentiment Score", col = c("Blue", "Blue", "Gray", "Red", "Red"))


x_T <- c(Far_Left_T, Left_T, Center_T, Right_T, Far_Right_T)
names(x_T) <- c("Far Left", "Left", "Center", "Right", "Far Right")

barplot(x_T, , main = "Positivity of Article Title by Bias", ylab = "Bias", xlab = "Sentiment Score", col = c("Blue", "Blue", "Gray", "Red", "Red") )

