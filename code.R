df <- read.csv("cereals_data.csv") # Importing Cereals dataset

dev.new(width=1000,height=1000,unit="px")
# Visualizing missing values in data
library(visdat)
vis_miss(df)

# Imputing the missing values by kNN method
library(VIM)
df <- kNN(df)

df <- df[1:16]
vis_miss(df) # No more missing values

# Basic Statistics
head(df,5)
str(df)
summary(df)

# Data Manipulation
df$mfr[df$mfr=="A"] <- "American Home Food Products"
df$mfr[df$mfr=="G"] <- "General Mills"
df$mfr[df$mfr=="K"] <- "Kelloggs"
df$mfr[df$mfr=="N"] <- "Nabisco"
df$mfr[df$mfr=="P"] <- "Post"
df$mfr[df$mfr=="Q"] <- "Quaker Oats"
df$mfr[df$mfr=="R"] <- "Ralston Purina"
df[,c(7,11)] <- df[,c(7,11)]/1000 # Converting milligrams to grams for uniformity of data
data <- df[,c(-3, -13:-15)]
dat <- data[,c(-1:-2,-11)]

# Data Visualization
library(ggplot2)
library(ggthemes)
library(tidyr)
ggplot(gather(dat),aes(value,col="Red")) + geom_boxplot(show.legend=F) +
  facet_wrap(~key,scales="free_x") + theme_solarized_2()

tab <- aggregate(dat,by=list(manufacturer=df$mfr),mean)
for (i in 2:10){
  tab[,i] <- round(tab[,i],digits=3)
}
library(gridExtra)
thm <- ttheme_default(core=list(bg_params=list(fill=blues9[1:7],col=NA)),
                      colhead=list(fg_params=list(col="white",fontface=3L),
                                   bg_params=list(fill="navyblue")))
grid.table(tab, theme=thm)

b1 <- ggplot(data,aes(calories,mfr,fill=mfr)) + geom_boxplot(outlier.shape=NA) +
  coord_flip() + stat_summary(fun=mean,geom="point",shape=4,size=3) +
  scale_color_brewer(palette="Dark2") +
  theme(axis.text.x=element_text(angle=90))
b2 <- ggplot(data,aes(rating,mfr,fill=mfr)) + geom_boxplot(outlier.shape=NA) +
  coord_flip() + stat_summary(fun=mean,geom="point",shape=4,size=3) +
  scale_color_brewer(palette="Dark2") +
  theme(axis.text.x=element_text(angle=90))
library(ggpubr)
ggarrange(b1,b2,labels=c("Calories","Ratings"),common.legend=T,legend="right") +
  theme_economist_white()

data1 <- data.frame(calories=data$calories,manufacturer=data$mfr)
data2 <- data.frame(nutrients=c(data$protein,data$fat,data$sodium,data$fiber,data$carbo,
                                data$sugars,data$potass),manufacturer=data$mfr)
data3 <- data.frame(rating=data$rating,manufacturer=data$mfr)
d1 <- ggplot(data1,aes(calories,fill=manufacturer)) + geom_density(alpha=0.3) +
  theme_economist()
d2 <- ggplot(data2,aes(nutrients,fill=manufacturer)) + geom_density(alpha=0.3) +
  theme_economist()
d3 <- ggplot(data3,aes(rating,fill=manufacturer)) + geom_density(alpha=0.3) +
  theme_economist()
ggarrange(d1,ggarrange(d2, d3, ncol=2, labels=c("Nutrients","Ratings"), legend=F),
          ncol=1, labels="Calories", legend="right") + theme_void()

library(corrplot)
library(RColorBrewer)
cm <- cor(data[,3:12],use="complete.obs")
corrplot(cm,type="lower",main="Correlation Matrix of Calories, Nutrients & Ratings",mar=
           c(0,0,1,0),tl.cex=0.9,tl.col="black",tl.srt=45,col=brewer.pal(n=10,name="BrBG"))

# Cluster Analysis
library(cluster)
library(factoextra)
rownames(data) <- data$name
df1 <- scale(data[3:12])
fviz_nbclust(df1,kmeans,method="wss") +
  geom_vline(xintercept=5,linetype=5,col="red") # k=5 according to Elbow method
set.seed(3000)
km.res <- kmeans(df1,5,nstart=30)
fviz_cluster(km.res,df1,palette="Dark2",ellipse=T,ellipse.type="norm",ellipse.level=0.99,
             ellipse.alpha=0.1,star.plot=T) + theme_solarized()
res.dist <- dist(df1,method="euclidean")
fviz_silhouette(silhouette(km.res$cluster,res.dist),palette="Dark2")
res.hc <- hclust(res.dist,method='ward.D2') # Ward method used for Dendogram
fviz_dend(res.hc,k=5,cex=0.5,kcolors="Dark2",color_labels_by_k=T,rect=T)

# Web Scraping text data
library(rvest)
url <- 'https://www.bbc.com/future/article/20180126-the-100-most-nutritious-foods'
webpage <- read_html(url)
elements = html_nodes(webpage,".body-text-card")
txt = html_text(elements)

# Data Cleaning
chunk_pasted <- paste(txt,collapse=" ")
clean_data1 <- tolower(chunk_pasted)
clean_data2 <- gsub(pattern="\\W",replace=" ",clean_data1)
clean_data3 <- gsub(pattern="\\d",replace=" ",clean_data2)
clean_data4 <- gsub(" g*",replace=" ",clean_data3)
library(tm)
clean_data5 <- removeWords(clean_data4,stopwords())
clean_data6 <- gsub(pattern="\\b[A-z]\\b{1}",replace=" ",clean_data5)
clean_data7 <- stripWhitespace(clean_data6)
clean_data8 <- gsub("iimagine",replace="imagine",clean_data7)
clean_data9 <- strsplit(clean_data8," ")
word <- clean_data9
word_freq <- table(word)

# Word Cloud
library(wordcloud2)
demoFreq <- as.data.frame(word_freq)
rownames(demoFreq) <- demoFreq$word
figPath <- system.file("examples/a1.png",package = "wordcloud2")
wordcloud2(demoFreq,figPath=figPath,size=5,color='random-light',backgroundColor="black")

dev.off(4)

# End.