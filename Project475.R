# Code for 475 Project - Rahul Singal

library(expss)
library(ggplot2)

DemoGraphics = as.data.frame(read.csv("~/Desktop/DS_ClassProject_Fall2018_data/Demographics.csv"))

# We do not care about the TRANSID, student_flag, adj_admit_type, admit_term, and HOUSING_TYPE, 
#                                                             so we will remove these 5 columns
DemoGraphics <- subset(DemoGraphics, select = -c(TRANSID, student_flag, adj_admit_type, admit_term, HOUSING_TYPE))

DemoGraphics$TRANDATE <- as.POSIXct(DemoGraphics$TRANDATE, format = "%Y-%m-%d %I:%M:%S")

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

Fall2012ALfreq = by(Fall2012, Fall2012[,"academic_level"], function(x) x)
Spring2013ALfreq = by(Spring2013, Spring2013[,"academic_level"], function(x) x)

Fall2012_Plot = as.data.frame(table(Fall2012$academic_level))
Spring2013_Plot = as.data.frame(table(Spring2013$academic_level))

Fall2013ALfreq = by(Fall2013, Fall2013[,"academic_level"], function(x) x)
Spring2014ALfreq = by(Spring2014, Spring2014[,"academic_level"], function(x) x)

Fall2013_Plot = as.data.frame(table(Fall2013$academic_level))
Spring2014_Plot = as.data.frame(table(Spring2014$academic_level))

Fall2014ALfreq = by(Fall2014, Fall2014[,"academic_level"], function(x) x)
Spring2015ALfreq = by(Spring2015, Spring2015[,"academic_level"], function(x) x)

Fall2014_Plot = as.data.frame(table(Fall2014$academic_level))
Spring2015_Plot = as.data.frame(table(Spring2015$academic_level))
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

# Remove non undergrads
CumulativeALevel <- CumulativeALevel[-c(2,4,5,6,10,12,13,14),]

# Set Colors and make plot of results by semester
colorPalette = c("#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf", "#abd9e9", "#74add1", "#4575b4")
ggplot(data=CumulativeALevel, aes(x=semester, y=Freq, fill=Var1)) + ggtitle("Frequency of academic level visits to SRC by semester") + geom_bar(stat="identity", position=position_dodge()) + scale_color_manual(values=colorPalette) + scale_fill_manual(values=colorPalette)

#Use as.character(student_date[150,])


