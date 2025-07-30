setwd("~/Desktop/Grad School/4 Preservation/R/Data") #set working directory

#loads packages
library(readxl) 
library(dplyr) 
library(tidyverse)

#uploads data
Data1 <- read_excel("Master.xlsx") #loads data sheet

Data1$Lake = as.factor(Data1$Lake) #turns lake into factor
Data1$Period = as.factor(Data1$Period) #turns period into factor

#creates df with total length and classfiers
Length <- data.frame(TL = Data1$Total_length,
                     Group = Data1$Period,
                     Lake = Data1$Lake,
                     FishID = Data1$FishID)
#creates df with mass and classfiers
Mass <- data.frame(BM = Data1$Weight,
                   Group = Data1$Period,
                   Lake = Data1$Lake,
                   FishID = Data1$FishID)

#subsets df's to check each groups normality
data<- subset(Length, Length$Group == "21")
shapiro.test(data$TL)
data<- subset(Length, Length$Group == "0")
shapiro.test(data$TL)
data<- subset(Length, Length$Group == "47")
shapiro.test(data$TL)
data<- subset(Length, Length$Group == "61")
shapiro.test(data$TL)

data<- subset(Mass, Length$Group == "21")
shapiro.test((data$BM))
data<- subset(Mass, Length$Group == "0")
shapiro.test((data$BM))
data<- subset(Mass, Length$Group == "47")
shapiro.test((data$BM))
data<- subset(Mass, Length$Group == "61")
shapiro.test((data$BM))


shapiro.test((cs0$Csize))
shapiro.test((cs21$Csize))
shapiro.test((cs47$Csize))
shapiro.test((cs61$Csize))

#reformats ddata
Length_wide <- pivot_wider(Length, names_from = Group, values_from = TL)

#calculates mean lengths
mean(Length_wide$"0")
mean(Length_wide$"21")
# Conduct the repeated measures t-test using the t.test() function
L_0_21<- t.test(Length_wide$'0', Length_wide$'21', paired = TRUE)
L_0_21
L_21_47 <- t.test(Length_wide$'21', Length_wide$'47', paired = TRUE)
L_21_47
L_47_61 <- t.test(Length_wide$'47', Length_wide$'61', paired = TRUE)
L_0_61 <- t.test(Length_wide$'0', Length_wide$'61', paired = TRUE)

#creates table with lengths
Ltable <- rbind(L_0_21, 
                  L_21_47, L_47_61, L_0_61)

#edits rownames
rownames(Ltable) <- c("Fresh vs. 21 days",  
                      "21 days vs. 47 days", "47 days vs. 61 days",
                      "Fresh vs. 61 days")

#changes df format
mass_wide <- pivot_wider(Mass, names_from = Group, values_from = BM)


# Conduct the repeated measures t-test using the t.test() function
M_0_21<- t.test(log(mass_wide$'0'), log(mass_wide$'21'), paired = TRUE)
M_21_47 <- t.test(log(mass_wide$'21'), log(mass_wide$'47'), paired = TRUE)
M_47_61 <- t.test(log(mass_wide$'47'), log(mass_wide$'61'), paired = TRUE)
M_0_61 <- t.test(log(mass_wide$'0'), log(mass_wide$'61'), paired = TRUE)


#makes table
Mtable <- rbind(M_0_21,  
                M_21_47, M_47_61, M_0_61)

#edits rownames
rownames(Mtable) <- c("Fresh vs. 21 days",  
                        "21 days vs. 47 days", "47 days vs. 61 days",
                        "Fresh vs. 61 days")


#############################
#sets directory
setwd("~/Desktop/Grad School/4 Preservation/R/Data/singles") #sets working directory

#brings in packages
library(geomorph)
library(Morpho)
library(shapes)
library(RRPP)
library(lme4)
library(tidyverse)
library(shapes)


#Loading in .tps files
combined <- readmulti.tps(c("AL44WK3.tps", "AL45WK3.tps", "AL46WK3.tps", "AL47WK3.tps", "AL48WK3.tps", "AL49WK3.tps", "AL51WK3.tps", "AL54WK3.tps", "AL56WK3.tps", "AL59WK3.2.tps", "AL60WK3.tps", "AL61WK3.tps", "AL62WK3.tps", "AL67WK3.tps",
                            "MA41WK3.tps" ,"MA43WK3.tps", "MA44WK3.tps", "MA45WK3.2.tps","MA46WK3.tps", "MA47WK3.tps", "MA56WK3.tps", "MA58WK3.tps", "MA61WK3.tps", "MA64WK3.tps", "MA65WK3.tps","MA66WK3.2.tps","MA68WK3.tps",  "MA42WK3.2.tps",
                            "WL41WK3.tps", "WL43WK3.tps", "WL44WK3.tps", "WL45WK3.tps", "WL46WK3.tps", "WL47WK3.tps", "WL48WK3.tps", "WL49WK3.tps", "WL50WK3.tps", "WL51WK3.tps", "WL52WK3.tps", "WL53WK3.tps",  "WL54WK3.2.tps", "WL77WK3.tps",
                            
                            "AL44DOC.2.tps", "AL45DOC.2.tps", "AL46DOC.tps", "AL47DOC.tps", "AL48DOC.tps", "AL49DOC.tps", "AL51DOC.tps", "AL54DOC.tps","AL56DOC.tps", "AL59DOC.tps", "AL60DOC.tps", "AL61DOC.tps", "AL62DOC.tps", "AL67DOC.tps",
                            "MA41DOC.tps", "MA43DOC.tps", "MA44DOC.tps", "MA45DOC.tps", "MA46DOC.tps", "MA47DOC.tps", "MA56DOC.tps", "MA58DOC.tps", "MA61DOC.2.tps","MA64DOC.tps", "MA65DOC.tps", "MA66DOC.tps", "MA68DOC.tps",  "MA42DOC.tps",
                            "WL41DOC.tps", "WL43DOC.tps","WL44DOC.tps", "WL45DOC.tps", "WL46DOC.tps", "WL47DOC.tps", "WL48DOC.tps", "WL49DOC.tps", "WL50DOC.tps", "WL51DOC.tps", "WL52DOC.tps", "WL53DOC.tps", "WL54DOC.tps", "WL77DOC.2.tps",
                            
                            'AL44day47.tps', 'AL45day47.tps', 'AL46day47.tps','AL47day47.tps', 'AL48day47.tps', 'AL49day47.tps', 'AL51day47.tps', 'AL54day47.tps', 'AL56day47.tps', 'AL59day47.tps', 'AL60day47.tps', 'AL61day47.tps', 'AL62day47.tps', 'AL67day47.tps',
                            'MA41day47.tps', 'MA43day47.tps', 'MA44day47.tps', 'MA45day47.tps', 'MA46day47.tps', 'MA47day47.tps', 'MA56day47.tps', 'MA58day47.tps', 'MA61day47.tps', 'MA64day47.tps', 'MA65day47.tps', 'MA66day47.tps', 'MA68day47.tps', "MA42day47.tps",
                            'WL41day47.tps', 'WL43day47.tps', 'WL44day47.tps', 'WL45day47.tps', 'WL46day47.tps', 'WL47day47.tps', 'WL48day47.tps', 'WL49day47.tps', 'WL50day47.tps', 'WL51day47.tps', 'WL52day47.tps', 'WL53day47.tps', 'WL54day47.tps', 'WL77day47.tps',
                            
                            'AL44day61.tps', 'AL45day61.tps', 'AL46day61.tps', 'AL47day61.tps', 'AL48day61.tps', 'AL49day61.tps', 'AL51day61.tps', 'AL54day61.tps', 'AL56day61.tps', 'AL59day61.tps', 'AL60day61.tps', 'AL61day61.tps', 'AL62day61.tps', 'AL67day61.tps',
                            'MA41day61.tps','MA43day61.tps', 'MA44day61.tps','MA45day61.tps', 'MA46day61.tps', 'MA47day61.tps', 'MA56day61.tps', 'MA58day61.tps', 'MA61day61.tps', 'MA64day61.tps', 'MA65day61.tps','MA66day61.tps', 'MA68day61.tps', 'MA42day61.tps',
                            'WL41day61.tps', 'WL43day61.tps', 'WL44day61.tps', 'WL45day61.tps', 'WL46day61.tps', 'WL47day61.tps', 'WL48day61.tps','WL49day61.tps', 'WL50day61.tps', 'WL51day61.tps', 'WL52day61.tps', 'WL53day61.tps', 'WL54day61.tps', 'WL77day61.tps'))


#Generalized Procrustes Analysis (GPA)
myGPA<-gpagen(combined, curves = NULL, surfaces = NULL, PrinAxes = TRUE,
              max.iter = NULL, ProcD = TRUE, Proj = TRUE, print.progress = TRUE)

#classify each .tps file in object above into their TIme group
myGPA$Group <- c("21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days",
                 "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days",
                 "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days",
                 
                 "DOC", "DOC", "DOC", "DOC", "DOC", "DOC", "DOC", "DOC", "DOC", "DOC", "DOC", "DOC", "DOC", "DOC","DOC", "DOC", "DOC", "DOC", 
                 "DOC", "DOC", "DOC", "DOC", "DOC", "DOC", "DOC", "DOC", "DOC", "DOC","DOC", "DOC", "DOC", "DOC", "DOC", "DOC", "DOC", "DOC", 
                 "DOC", "DOC", "DOC", "DOC", "DOC", "DOC",
                 
                 "47Days", "47Days", "47Days", "47Days", "47Days", "47Days", "47Days", "47Days", "47Days", "47Days", "47Days", "47Days", "47Days", "47Days",
                 "47Days", "47Days", "47Days", "47Days", "47Days", "47Days", "47Days", "47Days", "47Days", "47Days", "47Days", "47Days", "47Days", "47Days",
                 "47Days", "47Days", "47Days", "47Days", "47Days", "47Days", "47Days", "47Days", "47Days", "47Days", "47Days", "47Days", "47Days", "47Days",
                 
                 "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days",
                 "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days",
                 "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days")

#classify each .tps file in object above into their Lake
myGPA$Lake <- c("Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia",
                "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", 
                "White", "White", "White", "White", "White", "White", "White", "White", "White", "White","White","White","White","White",
                
                "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia",
                "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", 
                "White", "White", "White", "White", "White", "White", "White", "White", "White", "White","White","White","White","White",
                
                "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia",
                "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", 
                "White", "White", "White", "White", "White", "White", "White", "White", "White", "White","White","White","White","White",
                
                "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia",
                "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", "Macatawa", 
                "White", "White", "White", "White", "White", "White", "White", "White", "White", "White","White","White","White","White")

#classify each as their respective ID
myGPA$FishID <- c("AL44", "AL45", "AL46", "AL47", "AL48", "AL49", "AL51", "AL54", "AL56", "AL59.2", "AL60", "AL61", "AL62", "AL67", "MA41", "MA43", "MA44", "MA45.2", "MA46", "MA47", "MA56", "MA58", "MA61", "MA64", "MA65", "MA66.2", "MA68", "MA42.2", "WL41", "WL43", "WL44", "WL45", "WL46", "WL47", "WL48", "WL49", "WL50", "WL51", "WL52", "WL53", "WL54.2", "WL77",
                  "AL44", "AL45", "AL46", "AL47", "AL48", "AL49", "AL51", "AL54", "AL56", "AL59.2", "AL60", "AL61", "AL62", "AL67", "MA41", "MA43", "MA44", "MA45.2", "MA46", "MA47", "MA56", "MA58", "MA61", "MA64", "MA65", "MA66.2", "MA68", "MA42.2", "WL41", "WL43", "WL44", "WL45", "WL46", "WL47", "WL48", "WL49", "WL50", "WL51", "WL52", "WL53", "WL54.2", "WL77",
                  "AL44", "AL45", "AL46", "AL47", "AL48", "AL49", "AL51", "AL54", "AL56", "AL59.2", "AL60", "AL61", "AL62", "AL67", "MA41", "MA43", "MA44", "MA45.2", "MA46", "MA47", "MA56", "MA58", "MA61", "MA64", "MA65", "MA66.2", "MA68", "MA42.2", "WL41", "WL43", "WL44", "WL45", "WL46", "WL47", "WL48", "WL49", "WL50", "WL51", "WL52", "WL53", "WL54.2", "WL77",
                  "AL44", "AL45", "AL46", "AL47", "AL48", "AL49", "AL51", "AL54", "AL56", "AL59.2", "AL60", "AL61", "AL62", "AL67", "MA41", "MA43", "MA44", "MA45.2", "MA46", "MA47", "MA56", "MA58", "MA61", "MA64", "MA65", "MA66.2", "MA68", "MA42.2", "WL41", "WL43", "WL44", "WL45", "WL46", "WL47", "WL48", "WL49", "WL50", "WL51", "WL52", "WL53", "WL54.2", "WL77")

myGPA$Group<-as.factor(myGPA$Group) #converts to factor
myGPA$Lake<-as.factor(myGPA$Lake) #coverts to factor

#creates new df for centroid size with Lake, Group, and FishID classifiers
csizedf <- data.frame(Csize = myGPA$Csize,
                      Group = myGPA$Group,
                      Lake = myGPA$Lake,
                      FishID = myGPA$FishID)

#checks normality of centroid size
data<- subset(csizedf, csizedf$Group == "DOC")
shapiro.test(data$Csize)

data<- subset(csizedf, csizedf$Group == "21Days")
shapiro.test(data$Csize)

data<- subset(csizedf, csizedf$Group == "47Days")
shapiro.test(data$Csize)

data<- subset(csizedf, csizedf$Group == "61Days")
shapiro.test(data$Csize)


###############
###############
#converts to proper format
ALL <- pivot_wider(csizedf, names_from = Group, values_from = Csize)
# Conduct the repeated measures t-test using the t.test() function
ALL_0_21<- t.test(ALL$DOC, ALL$'21Days', paired = TRUE)
ALL_21_47 <- t.test(ALL$'21Days', ALL$'47Days', paired = TRUE)
ALL_47_61 <- t.test(ALL$'47Days', ALL$'61Days', paired = TRUE)
ALL_0_61 <- t.test(ALL$DOC, ALL$'61Days', paired = TRUE)

#makes tables
Ctable <- rbind(ALL_0_21, 
                ALL_21_47, ALL_47_61,
                ALL_0_61)

#edits rownames
rownames(Ctable) <- c("Fresh vs. 21 days",  
                      "21 days vs. 47 days", "47 days vs. 61 days",
                      "Fresh vs. 61 days")



#combine all tables
combined_table <- rbind(Ctable, Ltable, Mtable)


# Convert the matrix to a dataframe
df <- as.data.frame(combined_table)

#subset results
f <- df[,c(1,2,3,4)]

#creates period names
Period <- c("Fresh vs. 21 days",  
"21 days vs. 47 days", "47 days vs. 61 days",
"Fresh vs. 61 days",
"Fresh vs. 21 days",  
"21 days vs. 47 days", "47 days vs. 61 days",
"Fresh vs. 61 days",
"Fresh vs. 21 days",  
"21 days vs. 47 days", "47 days vs. 61 days",
"Fresh vs. 61 days")

#creates official measurement names
Measurement <- c("Centroid Size", "Centroid Size", "Centroid Size", "Centroid Size",
                 "Total Length", "Total Length", "Total Length", "Total Length",
                 "Wet Mass", "Wet Mass", "Wet Mass", "Wet Mass")

#creates df
x<-data.frame(Measurement, Period,f)

#renames column names
colnames(x)[3:6] <- c("t-stat", "DF", "P-value", "CI")

#formats for table
x$DF <- str_extract(x$DF, "\\d+\\.?\\d*")
x$'t-stat' <- str_extract(x$'t-stat', "-?\\d+\\.?\\d*")
x$'CI' <- gsub("^c\\(|\\)$", "", x$CI)

x <- x %>%
  mutate(across(c(`t-stat`, `P-value`, CI),
                ~ ifelse(is.numeric(.), round(., digits = 4), .)))

# Print the modified data frame
print(summary_df)



#create table (kable) and save 
library(magick)
library(webshot)
library(knitr)
library(kableExtra)
f %>%
  kbl(caption = "Size by Period", row.names = TRUE) %>%
  kable_classic(full_width = F, html_font = "Cambria") 





