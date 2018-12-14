library(tidyr)
library(dplyr)
library(ggplot2)

DemoGraphics <- as.data.frame(read.csv("~/Desktop/DS_ClassProject_Fall2018_data/Demographics.csv"))

DemoGraphics <- DemoGraphics %>% separate(TRANDATE, c("DATE", "TIME"), " ")

# Get hour of swipe 



count <- list("^00:", "^01:", "^02:", "^03:", '^04:', '^05:', '^06:', '^07:', '^08:', '^09:', '^10:', '^11:', '^12:', '^13:', '^14:', '^15:', '^16:', '^17:', '^18:', '^19:', '^20:', '^21:', '^22:', '^23:')

Time4 <- subset(DemoGraphics, grepl(count[5], TIME))
Time5 <- subset(DemoGraphics, grepl(count[6], TIME))
Time6 <- subset(DemoGraphics, grepl(count[7], TIME))
Time7 <- subset(DemoGraphics, grepl(count[8], TIME))
Time8 <- subset(DemoGraphics, grepl(count[9], TIME))
Time9 <- subset(DemoGraphics, grepl(count[10], TIME))
Time10 <- subset(DemoGraphics, grepl(count[11], TIME))
Time11 <- subset(DemoGraphics, grepl(count[12], TIME))
Time12 <- subset(DemoGraphics, grepl(count[13], TIME))
Time13 <- subset(DemoGraphics, grepl(count[14], TIME))
Time14 <- subset(DemoGraphics, grepl(count[15], TIME))
Time15 <- subset(DemoGraphics, grepl(count[16], TIME))
Time16 <- subset(DemoGraphics, grepl(count[17], TIME))
Time17 <- subset(DemoGraphics, grepl(count[18], TIME))
Time18 <- subset(DemoGraphics, grepl(count[19], TIME))
Time19 <- subset(DemoGraphics, grepl(count[20], TIME))
Time20 <- subset(DemoGraphics, grepl(count[21], TIME))
Time21 <- subset(DemoGraphics, grepl(count[22], TIME))
Time22 <- subset(DemoGraphics, grepl(count[23], TIME))
Time23 <- subset(DemoGraphics, grepl(count[24], TIME))

Time4$Hour <- 4
Time5$Hour <- 5
Time6$Hour <- 6
Time7$Hour <- 7
Time8$Hour <- 8
Time9$Hour <- 9
Time10$Hour <- 10
Time11$Hour <- 11
Time12$Hour <- 12
Time13$Hour <- 13
Time14$Hour <- 14
Time15$Hour <- 15
Time16$Hour <- 16
Time17$Hour <- 17
Time18$Hour <- 18
Time19$Hour <- 19
Time20$Hour <- 20
Time21$Hour <- 21
Time22$Hour <- 22
Time23$Hour <- 23

DemoGraphics <- rbind(Time4, Time5, Time6, Time7, Time8, Time9, Time10, Time11, Time12, Time13, Time14, Time15, Time16, Time17, Time18, Time19, Time20, Time21, Time22, Time23)

DemoGraphics <- DemoGraphics[order(as.numeric(row.names(DemoGraphics))),]

# Break into individual datasets by term

new = by(DemoGraphics, DemoGraphics[,"record_term"], function(x) x)


Fall2012 = new[[1]]
Spring2013 = new[[2]]
Summer2013 = new[[3]]
Fall2013 = new[[4]]
Spring2014 = new[[5]]
Summer2014 = new[[6]]
Fall2014 = new[[7]]
Spring2015 = new[[8]]
Summer2015 = new[[9]]
Fall2015 = new[[10]]

table(DemoGraphics$Hour)

Timebreak <- by(DemoGraphics, DemoGraphics[,"Hour"], function(x) x)
  
hold <- Timebreak[[1]]
hold2 <- as.data.frame(table(hold$sex))
hold2$Hour <- 4
CumulativeALevel <- hold2
for(i in 2:20)
{
  hold <- Timebreak[[i]]
  hold2 <- as.data.frame(table(hold$sex))
  hold2$Hour <- i + 3
  CumulativeALevel <- rbind(CumulativeALevel, hold2)
}

colorPalette = c("#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf", "#abd9e9", "#74add1", "#4575b4")
ggplot(data=CumulativeALevel, aes(x=Hour, y=Freq, fill=Var1)) + ggtitle("Frequency of academic level visits to SRC by semester") + geom_bar(stat="identity", position=position_dodge()) + scale_color_manual(values=colorPalette) + scale_fill_manual(values=colorPalette)

barplot(table(DemoGraphics$Hour), xlab = "Time of day", ylab = "Frequency")
