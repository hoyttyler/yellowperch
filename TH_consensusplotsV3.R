#set Working Directory
setwd("~/Desktop/Grad School/4 Preservation/R/Data/singles")

#load packages
library(geomorph)
library(readxl)
library(dplyr)
library(ggplot2)

#upload excel file containing landmark coordniates
df <- read_excel("final.xlsx")

#Subset for day 0 and set landmarks as numeric
D0 <- subset(df, df$Period == "DOC")
D0$X <- as.numeric(D0$X)
D0$Y <- as.numeric(D0$Y)

#Subset for day 21 and set landmarks as numeric
D21 <- subset(df, df$Period == "21Days")
D21$X <- as.numeric(D21$X)
D21$Y <- as.numeric(D21$Y)

#Subset for day 47 and set landmarks as numeric
D47 <- subset(df, df$Period == "47Days")
D47$X <- as.numeric(D47$X)
D47$Y <- as.numeric(D47$Y)

#Subset for day 61 and set landmarks as numeric
D61 <- subset(df, df$Period == "61Days")
D61$X <- as.numeric(D61$X)
D61$Y <- as.numeric(D61$Y)

#calculate mean shape for day 0
D0 <- D0 %>%
  group_by(Landmark) %>%
  summarize(X = mean(X), Y = mean(Y))

#calculate mean shape for day 21
D21 <- D21 %>%
  group_by(Landmark) %>%
  summarize(X = mean(X), Y = mean(Y))

#calculate mean shape for day 47
D47 <- D47 %>%
  group_by(Landmark) %>%
  summarize(X = mean(X), Y = mean(Y))

#calculate mean shape for day 61
D61 <- D61 %>%
  group_by(Landmark) %>%
  summarize(X = mean(X), Y = mean(Y))

#subset first 13 landmarks
D0 <- D0[c(1:13),]
D21 <- D21[c(1:13),]
D47 <- D47[c(1:13),]
D61 <- D61[c(1:13),]

# Merge the data frames into one
merged_data <- rbind(D0, D21)

#add time period classifiers
DOC <- rep("Pre-preserved", times = 13)
Day21 <- rep("21-Days", times = 13)
Day47 <- rep("47Day", times = 13)
Day61 <- rep("61Day", times = 13)

# Combine into a single vector
Period <- c(DOC, Day21)

#mege data
final<- cbind(merged_data, Period)

#seperate landmarks by physical components for lines
connect <- final[c(1:10,12,14:23,25),]
eye <- final[c(13,26),]
gill <- final[c(12:14, ),]

#create plot comparing two timeframes
combined <- ggplot() +
  geom_polygon(data = connect, aes(X, Y, group = Period, col = Period, linetype = Period), fill = NA, size = 2) +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black")) 

g <- combined + 
  geom_point(data = eye, aes(X, Y, group = Period, col = Period, shape = Period), size = 4) +
  geom_point(data = connect, aes(X, Y, group = Period, col = Period, shape = Period), size = 4) 

h <- g +
  geom_polygon(data = gill, aes(X, Y, group = Period, col = Period, linetype = Period), fill = NA, size = 2) 

i <- h + scale_shape_manual(values = c("Pre-preserved" = 19, "21-Days" = 15))

i +   geom_point(data = eye, aes(X, Y, group = Period, col = Period, shape = Period), size = 4) +
  geom_point(data = gill, aes(X, Y, group = Period, col = Period, shape = Period), size = 4) +
  geom_point(data = connect, aes(X, Y, group = Period, col = Period, shape = Period), size = 4) +
  scale_color_manual(values = c("Pre-preserved" = "coral", "21-Days" = "blue")) +
  scale_linetype_manual(values = c("Pre-preserved" = "solid", "21-Days" = "solid")) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

###############################################
#repeats steps above for the rest of the timeframes
D21 <- subset(df, df$Period == "21Days")
D21$X <- as.numeric(D21$X)
D21$Y <- as.numeric(D21$Y)

D47 <- subset(df, df$Period == "47Days")
D47$X <- as.numeric(D47$X)
D47$Y <- as.numeric(D47$Y)

D21 <- D21 %>%
  group_by(Landmark) %>%
  summarize(X = mean(X), Y = mean(Y))

D47 <- D47 %>%
  group_by(Landmark) %>%
  summarize(X = mean(X), Y = mean(Y))

D21 <- D21[c(1:13),]
D47 <- D47[c(1:13),]

# Merge the data frames into one
merged_data <- rbind(D21, D47)

Day21 <- rep("21-Days", times = 13)
Day47 <- rep("47-Days", times = 13)

Period <- c(Day21, Day47)

final<- cbind(merged_data, Period)

connect <- final[c(1:10,12,14:23,25),]
eye <- final[c(13,26),]
gill <- final[c(11,12,24,25),]

combined <- ggplot() +
  geom_polygon(data = connect, aes(X, Y, group = Period, col = Period, linetype = Period), fill = NA, size = 2) +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"))# +

g <- combined + 
  geom_point(data = eye, aes(X, Y, group = Period, col = Period, shape = Period), size = 4) #+

h <- g +
  geom_polygon(data = gill, aes(X, Y, group = Period, col = Period, linetype = Period), fill = NA, size = 2) #+

i <- h + scale_shape_manual(values = c("21-Days" = 15, "47-Days" = 18, size = 3))

i + geom_point(data = eye, aes(X, Y, group = Period, col = Period, shape = Period), size = 4) +
  geom_point(data = gill, aes(X, Y, group = Period, col = Period, shape = Period), size = 4) +
  geom_point(data = connect, aes(X, Y, group = Period, col = Period, shape = Period), size = 4) +
  scale_color_manual(values = c("21-Days" = "blue", "47-Days" = "green")) +
  scale_linetype_manual(values = c("21-Days" = "solid", "47-Days" = "solid")) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())
###############################################
D47 <- subset(df, df$Period == "47Days")
D47$X <- as.numeric(D47$X)
D47$Y <- as.numeric(D47$Y)

D61 <- subset(df, df$Period == "61Days")
D61$X <- as.numeric(D61$X)
D61$Y <- as.numeric(D61$Y)

D47 <- D47 %>%
  group_by(Landmark) %>%
  summarize(X = mean(X), Y = mean(Y))

D61 <- D61 %>%
  group_by(Landmark) %>%
  summarize(X = mean(X), Y = mean(Y))

D47 <- D47[c(1:13),]
D61 <- D61[c(1:13),]

merged_data <- rbind(D47, D61)

Day47 <- rep("47-Days", times = 13)
Day61 <- rep("61-Days", times = 13)

Period <- c(Day47, Day61)

final<- cbind(merged_data, Period)

connect <- final[c(1:10,12,14:23,25),]
eye <- final[c(13,26),]
gill <- final[c(11,12,24,25),]

combined <- ggplot() +
  geom_polygon(data = connect, aes(X, Y, group = Period, col = Period, linetype = Period), fill = NA, size = 2) +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"))# +

g <- combined + 
  geom_point(data = eye, aes(X, Y, group = Period, col = Period, shape = Period), size = 4) #+

h <- g +
  geom_polygon(data = gill, aes(X, Y, group = Period, col = Period, linetype = Period), fill = NA, size = 2)# +

i <- h + scale_shape_manual(values = c("47-Days" = 18, "61-Days" = 17, size = 3))

i + geom_point(data = eye, aes(X, Y, group = Period, col = Period, shape = Period), size = 4) +
  geom_point(data = gill, aes(X, Y, group = Period, col = Period, shape = Period), size = 4) +
  geom_point(data = connect, aes(X, Y, group = Period, col = Period, shape = Period), size = 4) +
  scale_color_manual(values = c("47-Days" = "green", "61-Days" = "purple")) +
  scale_linetype_manual(values = c("47-Days" = "solid", "61-Days" = "solid")) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())
###############################################
D0 <- subset(df, df$Period == "DOC")
D0$X <- as.numeric(D0$X)
D0$Y <- as.numeric(D0$Y)

D61 <- subset(df, df$Period == "61Days")
D61$X <- as.numeric(D61$X)
D61$Y <- as.numeric(D61$Y)

D0 <- D0 %>%
  group_by(Landmark) %>%
  summarize(X = mean(X), Y = mean(Y))

D61 <- D61 %>%
  group_by(Landmark) %>%
  summarize(X = mean(X), Y = mean(Y))


D0 <- D0[c(1:13),]
D61 <- D61[c(1:13),]

merged_data <- rbind(D0, D61)

DOC <- rep("Pre-preserved", times = 13)
Day61 <- rep("61-Days", times = 13)

Period <- c(DOC, Day61)

final<- cbind(merged_data, Period)

connect <- final[c(1:10,12,14:23,25),]
eye <- final[c(13,26),]
gill <- final[c(11,12,24,25),]

combined <- ggplot() +
  geom_polygon(data = connect, aes(X, Y, group = Period, col = Period, linetype = Period), fill = NA, size = 2) +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black")) +
  guides(color = "none", linetype = "none")

g <- combined + 
  geom_point(data = eye, aes(X, Y, group = Period, col = Period, shape = Period), size = 4) +
  guides(color = "none", shape = "none")

h <- g +
  geom_polygon(data = gill, aes(X, Y, group = Period, col = Period, linetype = Period), fill = NA, size = 2) +
  guides(color = "none", linetype = "none")

i <- h + scale_shape_manual(values = c("Pre-preserved" = 19, "61-Days" = 17, size = 3))

i + geom_point(data = eye, aes(X, Y, group = Period, col = Period, shape = Period), size = 4) +
  geom_point(data = gill, aes(X, Y, group = Period, col = Period, shape = Period), size = 4) +
  geom_point(data = connect, aes(X, Y, group = Period, col = Period, shape = Period), size = 4) +
  scale_color_manual(values = c("Pre-preserved" = "coral", "61-Days" = "purple")) +
  scale_linetype_manual(values = c("Pre-preserved" = "solid", "61-Days" = "solid")) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

