library(tidyr)
library(dplyr)

# Read the csv files containing the data
DemoGraphics = read.csv("~/Desktop/DS_ClassProject_Fall2018_data/Demographics.csv")
C_S = read.csv("~/Desktop/DS_ClassProject_Fall2018_data/Card_Swipes.csv")

DemoGraphics <- subset(DemoGraphics, select = -c(TRANSID, student_flag, adj_admit_type, admit_term, HOUSING_TYPE))

DemoGraphics <- DemoGraphics %>% separate(TRANDATE, c("DATE", "TIME"), " ")

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

tabDemo <- data.frame(table(DemoGraphics$Hour))

barplot(table(DemoGraphics$Hour), main = "Hour Freq")

#####################################################################


#Split data into individual data frames, each corresponding
# to a specific term date.
new = by(DemoGraphics, DemoGraphics[,"TIME"], function(x) x)



T20131 = new[[2]]
T20132 = new[[3]]
T20133 = new[[4]]
T20141 = new[[5]]
T20142 = new[[6]]
T20143 = new[[7]]
T20151 = new[[8]]
T20152 = new[[9]]
T20153 = new[[10]]

# For easier manipulation I'm splitting TRANDATE section into 
# two columns, one for Date and the other for Time. 
T20123 = T20123 %>% separate(TRANDATE, c("DATE", "TIME"), " ")
T20131 = T20131 %>% separate(TRANDATE, c("DATE", "TIME"), " ")
T20132 = T20132 %>% separate(TRANDATE, c("DATE", "TIME"), " ")
T20133 = T20133 %>% separate(TRANDATE, c("DATE", "TIME"), " ")
T20141 = T20141 %>% separate(TRANDATE, c("DATE", "TIME"), " ")
T20142 = T20142 %>% separate(TRANDATE, c("DATE", "TIME"), " ")
T20143 = T20143 %>% separate(TRANDATE, c("DATE", "TIME"), " ")
T20151 = T20151 %>% separate(TRANDATE, c("DATE", "TIME"), " ")
T20152 = T20152 %>% separate(TRANDATE, c("DATE", "TIME"), " ")
T20153 = T20153 %>% separate(TRANDATE, c("DATE", "TIME"), " ")

# Just for 20123 data so far, returns the ID# of top 25% most
# frequent visitors and bottom 75% visitors.
Tab20123 = data.frame(table(T20123$PATRONID.x))
n = 25
TabH20123 = Tab20123[Tab20123$Freq > quantile(Tab20123$Freq, prob=1-n/100),]
n = 75
TabL20123 = Tab20123[Tab20123$Freq > quantile(Tab20123$Freq, prob=1-n/100),]
  
# Make barplots that show how frequently each acaddemic level
# attends the gym
barplot(table(T20123$academic_level), main = "20123")
barplot(table(T20131$academic_level), main = "20131")
barplot(table(T20133$academic_level), main = "20133")
barplot(table(T20141$academic_level), main = "20141")
barplot(table(T20143$academic_level), main = "20143")
barplot(table(T20151$academic_level), main = "20151")
barplot(table(T20153$academic_level), main = "20153")

# Count time periods that occur during 05:00-05:59
sum(grepl('^05:', T20123$TIME))
# Creates a new dataframe out of only people who go between 5:00-5:59
PM520123 = subset(T20123, grepl('^05:', TIME))

#PM520123 <- grepl('^05:', T20123$TIME) 