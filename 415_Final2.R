library(tidyr)
library(dplyr)
library(ggplot2)

DemoGraphics <- as.data.frame(read.csv("~/Desktop/DS_ClassProject_Fall2018_data/Demographics.csv"))

thing <- as.data.frame(table(DemoGraphics$acad_plan_descr))

thing <- thing[order(thing$Freq),]

thing1 <- thing[80:86,]

thing2 <- thing[163:169,]

thing3 <- thing[241:247,]

thing <- as.data.frame(tail(thing))

# Seperates by semester
new = by(DemoGraphics, DemoGraphics[,"record_term"], function(x) x)

Fall2012 = new[[1]] ## Fall 2012 [2012-08-17 to 2013-01-06] 
Spring2013 = new[[2]] ## Spring 2013 [2013-01-07 to 2013-05-05] 
Summer2013 = new[[3]] ## Summer 2013 [2013-05-06 to 2013-08-08]
Fall2013 = new[[4]] ## Fall 2013 [2013-08-09 to 2014-01-12] 
Spring2014 = new[[5]] ## Spring 2014 [2 014-01-13 to 2014-05-11] 
Summer2014 = new[[6]] ## Summer 2014 [2014-05-12 to 2014-08-14] 
Fall2014 = new[[7]] ## Fall 2014 [2014-08-15 to 2015-01-11] 
Spring2015 = new[[8]] ## Spring 2015 [2015-01-12 to 2015-05-10] 
Summer2015 = new[[9]] ## Summer 2015 [2015-05-11 to 2015-08-13] 
Fall2015 = new[[10]] ## Fall 2015 [INCOMPLETE. ONLY 3 DAYS]




# Plot the times on a graph. X axis has the date visited (8/16 to 1/6). Y axis has the time of day (515 to 2330)
# Highlight on the graph the peak times as specified by Yunshu Du
# Count the number of occurences at every time (x at empty, x at moderetly empty, x at crowded)
###### Do this for a range of people and see if there is any correlation

Fall2012ALfreq = by(Fall2012, Fall2012[,"acad_plan_descr"], function(x) x)
Spring2013ALfreq = by(Spring2013, Spring2013[,"acad_plan_descr"], function(x) x)

Fall2012_Plot = as.data.frame(table(Fall2012$acad_plan_descr))
Spring2013_Plot = as.data.frame(table(Spring2013$acad_plan_descr))

Fall2013ALfreq = by(Fall2013, Fall2013[,"acad_plan_descr"], function(x) x)
Spring2014ALfreq = by(Spring2014, Spring2014[,"acad_plan_descr"], function(x) x)

Fall2013_Plot = as.data.frame(table(Fall2013$acad_plan_descr))
Spring2014_Plot = as.data.frame(table(Spring2014$acad_plan_descr))

Fall2014ALfreq = by(Fall2014, Fall2014[,"acad_plan_descr"], function(x) x)
Spring2015ALfreq = by(Spring2015, Spring2015[,"acad_plan_descr"], function(x) x)

Fall2014_Plot = as.data.frame(table(Fall2014$acad_plan_descr))
Spring2015_Plot = as.data.frame(table(Spring2015$acad_plan_descr))
t

# Fall2012_Plot$Year = 2012
Fall2012_Plot$semester = "Fall"
# Spring2013_Plot$Year = 2013
Spring2013_Plot$semester = "Spring"
# Fall2013_Plot$Year = 2013
Fall2013_Plot$semester = "Fall"
# Spring2014_Plot$Year = 2014
Spring2014_Plot$semester = "Spring"
# Fall2014_Plot$Year = 2014
Fall2014_Plot$semester = "Fall"
# Spring2015_Plot$Year = 2015
Spring2015_Plot$semester = "Spring"

CumulativeALevel <- rbind(Fall2012_Plot, Spring2013_Plot, Fall2013_Plot, Spring2014_Plot, Fall2014_Plot, Spring2015_Plot)

CumulativeSplitFrame <- split(CumulativeALevel, CumulativeALevel$semester)

FallCumulative <- CumulativeSplitFrame[[1]]
SpringCumulative <- CumulativeSplitFrame[[2]]

FallCumulative <- aggregate(Freq ~ Var1, data=FallCumulative, sum)
SpringCumulative <- aggregate(Freq ~ Var1, data=SpringCumulative, sum)

FallCumulative$semester <- "Fall"
SpringCumulative$semester <- "Spring"

# Combne Fall and Spring datasets
CumulativeALevel <- rbind(FallCumulative, SpringCumulative)

CumulativeALevel <- subset(CumulativeALevel, Var1 %in% thing$Var1)

# Set Colors and make plot of results by semester
colorPalette = c("#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf", "#abd9e9", "#74add1", "#4575b4")
ggplot(data=CumulativeALevel, aes(x=semester, y=Freq, fill=Var1)) + ggtitle("Frequency of visits by most frequent majors to SRC by semester") + geom_bar(stat="identity", position=position_dodge()) + scale_color_manual(values=colorPalette) + scale_fill_manual(values=colorPalette)

############## By time ###################

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
hold2 <- as.data.frame(table(hold$acad_plan_descr))
hold2$Hour <- 4
CumulativeALevel <- hold2
for(i in 2:20)
{
  hold <- Timebreak[[i]]
  hold2 <- as.data.frame(table(hold$acad_plan_descr))
  hold2$Hour <- i + 3
  CumulativeALevel <- rbind(CumulativeALevel, hold2)
}

CumulativeALevel <- subset(CumulativeALevel, Var1 %in% thing$Var1)

# Set Colors and make plot of results by semester
colorPalette = c("#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf", "#abd9e9", "#74add1", "#4575b4")
ggplot(data=CumulativeALevel, aes(x=Hour, y=Freq, fill=Var1)) + ggtitle("Frequency X Hour/Major High") + geom_bar(stat="identity", position=position_dodge()) + scale_color_manual(values=colorPalette) + scale_fill_manual(values=colorPalette)

##################### Low ########################

hold <- Timebreak[[1]]
hold2 <- as.data.frame(table(hold$acad_plan_descr))
hold2$Hour <- 4
CumulativeALevel <- hold2
for(i in 2:20)
{
  hold <- Timebreak[[i]]
  hold2 <- as.data.frame(table(hold$acad_plan_descr))
  hold2$Hour <- i + 3
  CumulativeALevel <- rbind(CumulativeALevel, hold2)
}

CumulativeALevel <- subset(CumulativeALevel, Var1 %in% thing1$Var1)

# Set Colors and make plot of results by semester
colorPalette = c("#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf", "#abd9e9", "#74add1", "#4575b4")
ggplot(data=CumulativeALevel, aes(x=Hour, y=Freq, fill=Var1)) + ggtitle("Frequency X Hour/Major Low") + geom_bar(stat="identity", position=position_dodge()) + scale_color_manual(values=colorPalette) + scale_fill_manual(values=colorPalette)



#################### Med Low ###################
hold <- Timebreak[[1]]
hold2 <- as.data.frame(table(hold$acad_plan_descr))
hold2$Hour <- 4
CumulativeALevel <- hold2
for(i in 2:20)
{
  hold <- Timebreak[[i]]
  hold2 <- as.data.frame(table(hold$acad_plan_descr))
  hold2$Hour <- i + 3
  CumulativeALevel <- rbind(CumulativeALevel, hold2)
}

CumulativeALevel <- subset(CumulativeALevel, Var1 %in% thing2$Var1)

# Set Colors and make plot of results by semester
colorPalette = c("#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf", "#abd9e9", "#74add1", "#4575b4")
ggplot(data=CumulativeALevel, aes(x=Hour, y=Freq, fill=Var1)) + ggtitle("Frequency X Hour/Major Medium Low") + geom_bar(stat="identity", position=position_dodge()) + scale_color_manual(values=colorPalette) + scale_fill_manual(values=colorPalette)

#################### Med High ###################

hold <- Timebreak[[1]]
hold2 <- as.data.frame(table(hold$acad_plan_descr))
hold2$Hour <- 4
CumulativeALevel <- hold2
for(i in 2:20)
{
  hold <- Timebreak[[i]]
  hold2 <- as.data.frame(table(hold$acad_plan_descr))
  hold2$Hour <- i + 3
  CumulativeALevel <- rbind(CumulativeALevel, hold2)
}

CumulativeALevel <- subset(CumulativeALevel, Var1 %in% thing3$Var1)

# Set Colors and make plot of results by semester
colorPalette = c("#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf", "#abd9e9", "#74add1", "#4575b4")
ggplot(data=CumulativeALevel, aes(x=Hour, y=Freq, fill=Var1)) + ggtitle("Frequency X Hour/Major Medium High") + geom_bar(stat="identity", position=position_dodge()) + scale_color_manual(values=colorPalette) + scale_fill_manual(values=colorPalette)

Simpleres <- as.data.frame(table(DemoGraphics$academic_level))
Simpleres <- Simpleres[-c(2,4,5,6),]

colorPalette = c("#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf", "#abd9e9", "#74add1", "#4575b4")
ggplot(data=Simpleres, aes(x=Var1, y=Freq, fill=Var1)) + ggtitle("Academic Level X Frequency of SRC visits") + geom_bar(stat="identity", position=position_dodge()) + scale_color_manual(values=colorPalette) + scale_fill_manual(values=colorPalette)

barplot(DemoGraphics$Hour)
