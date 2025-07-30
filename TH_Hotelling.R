#sets working directory
setwd("~/Desktop/Grad School/4 Preservation/R/Data/singles")

#loads packages
library(geomorph)
library(Morpho)
library(shapes)
library(RRPP)
library(readxl)
library(vctrs)

#residuals<- read_excel("residuals.xlsx")

#brings in tps files
#14 of each
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
                            'WL41day61.tps', 'WL43day61.tps', 'WL44day61.tps', 'WL45day61.tps', 'WL46day61.tps', 'WL47day61.tps', 'WL48day61.tps','WL49day61.tps', 'WL50day61.tps', 'WL51day61.tps', 'WL52day61.tps', 'WL53day61.tps', 'WL54day61.tps', 'WL77day61.tps'
))

#Generalized Procrustes Analysis (GPA)
myGPA<-gpagen(combined, curves = NULL, surfaces = NULL, PrinAxes = TRUE,
              max.iter = NULL, ProcD = TRUE, Proj = TRUE, print.progress = TRUE)

#classify each .tps file in object above into their group
myGPA$Time=c(   "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days", "21Days",
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
                "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days", "61Days"
)

#Add time factor (preserved or day of capture)
is.factor(myGPA$Time)
myGPA$Time<-as.factor(myGPA$Time)
myGPA$Time


#classify each .tps file in object above into their group
myGPA$Lake=c("Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia", "Arcadia",
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

#turns lake identifier into factor
myGPA$Lake<-as.factor(myGPA$Lake)
myGPA$Lake

#adds FishID identifiers
myGPA$FishID <- c("AL44", "AL45", "AL46", "AL47", "AL48", "AL49", "AL51", "AL54", "AL56", "AL59.2", "AL60", "AL61", "AL62", "AL67", "MA41", "MA43", "MA44", "MA45.2", "MA46", "MA47", "MA56", "MA58", "MA61", "MA64", "MA65", "MA66.2", "MA68", "MA42.2", "WL41", "WL43", "WL44", "WL45", "WL46", "WL47", "WL48", "WL49", "WL50", "WL51", "WL52", "WL53", "WL54.2", "WL77",
                  "AL44", "AL45", "AL46", "AL47", "AL48", "AL49", "AL51", "AL54", "AL56", "AL59.2", "AL60", "AL61", "AL62", "AL67", "MA41", "MA43", "MA44", "MA45.2", "MA46", "MA47", "MA56", "MA58", "MA61", "MA64", "MA65", "MA66.2", "MA68", "MA42.2", "WL41", "WL43", "WL44", "WL45", "WL46", "WL47", "WL48", "WL49", "WL50", "WL51", "WL52", "WL53", "WL54.2", "WL77",
                  "AL44", "AL45", "AL46", "AL47", "AL48", "AL49", "AL51", "AL54", "AL56", "AL59.2", "AL60", "AL61", "AL62", "AL67", "MA41", "MA43", "MA44", "MA45.2", "MA46", "MA47", "MA56", "MA58", "MA61", "MA64", "MA65", "MA66.2", "MA68", "MA42.2", "WL41", "WL43", "WL44", "WL45", "WL46", "WL47", "WL48", "WL49", "WL50", "WL51", "WL52", "WL53", "WL54.2", "WL77",
                  "AL44", "AL45", "AL46", "AL47", "AL48", "AL49", "AL51", "AL54", "AL56", "AL59.2", "AL60", "AL61", "AL62", "AL67", "MA41", "MA43", "MA44", "MA45.2", "MA46", "MA47", "MA56", "MA58", "MA61", "MA64", "MA65", "MA66.2", "MA68", "MA42.2", "WL41", "WL43", "WL44", "WL45", "WL46", "WL47", "WL48", "WL49", "WL50", "WL51", "WL52", "WL53", "WL54.2", "WL77")

#checks outliers
plotOutliers(myGPA$coords)


#calculate residuals
res <- procD.lm(coords ~ log(Csize), data = myGPA, iter = 1)$residuals # removing allometry
means <- procD.lm(coords ~ 1, data = myGPA, iter = 1)$fitted # overall mean shape
Y.adj <- means + res # These are shapes with allometry removed
myGPA$coords.adj <- arrayspecs(Y.adj, 13, 2) # using p = number of landmarks and k = 2 for 2D data

#calculate PC's
pca_results<- gm.prcomp(myGPA$coords.adj)
summary(pca_results)

# Create a data frame with the PCA scores for the first two principal components
scores <- data.frame(PC1 = pca_results$x[,1], PC2 = pca_results$x[,2], PC3 = pca_results$x[,3], PC4 = pca_results$x[,4], PC5 = pca_results$x[,5], PC6 = pca_results$x[,6],
                     PC7 = pca_results$x[,7], PC8 = pca_results$x[,8], PC9 = pca_results$x[,9], PC10 = pca_results$x[,10], PC11 = pca_results$x[,11],
                     PC12 = pca_results$x[,12], PC13 = pca_results$x[,13], PC14 = pca_results$x[,14], PC15 = pca_results$x[,15], PC16 = pca_results$x[,16],
                     PC17 = pca_results$x[,17], PC18 = pca_results$x[,18], PC19 = pca_results$x[,19], PC20 = pca_results$x[,20], PC21 = pca_results$x[,21], PC22 = pca_results$x[,22], Group = myGPA$Time, Lake = myGPA$Lake)


#tests interaction between PC1 and cetroid size
test <- procD.lm(scores$PC1 ~ log(Csize), data = myGPA, iter = 1)
test

#loads package
library(GeometricMorphometricsMix)

#All lakes combined
scores0 <- subset(scores, scores$Group == "DOC")
scores21 <- subset(scores, scores$Group == "21Days")
scores47 <- subset(scores, scores$Group == "47Days")
scores61 <- subset(scores, scores$Group == "61Days")

#subsets scores0 df
scores0 <- scores0[,1:22]
scores21 <- scores21[,1:22]
scores47 <- scores47[,1:22]
scores61 <- scores61[,1:22]

#turns into numeric
scores0[] <- lapply(scores0, as.numeric)
scores21[] <- lapply(scores21, as.numeric)
scores47[] <- lapply(scores47, as.numeric)
scores61[] <- lapply(scores61, as.numeric)

#edit row names to make identical between df's
RN <- as.numeric(1:42)
rownames(scores0) <- RN
rownames(scores21) <- RN
rownames(scores47) <- RN
rownames(scores61) <- RN

#turns into matrix
scores0 <- as.matrix(scores0)
scores21 <- as.matrix(scores21)
scores47 <- as.matrix(scores47)
scores61 <- as.matrix(scores61)

#runs repeated measures tests comparing between timeframes
scoresd0_d21<- repeated_measures_test(scores0, scores21, rnames = T)
scoresd0_d61<- repeated_measures_test(scores0, scores61, rnames = T)
scores21_d47<- repeated_measures_test(scores21, scores47, rnames = T)
scores47_d61<- repeated_measures_test(scores47, scores61, rnames = T)

# create a data frame for each result
df1 <- data.frame(t(scoresd0_d21))
df4 <- data.frame(t(scoresd0_d61))
df2 <- data.frame(t(scores21_d47))
df3 <- data.frame(t(scores47_d61))

# combine the data frames horizontally
Combinedtable <- rbind(df1, df2, 
                       df3, df4)

rownames(Combinedtable) <- c("Fresh vs. 21 days", 
                             "21 days vs. 47 days", "47 days vs. 61 days", "Fresh vs. 61 days")

# View the resulting table
Combinedtable

#loads packages
library(magick)
library(webshot)
library(knitr)
library(kableExtra)
#creates table
Combinedtable %>%
  kbl(caption = "Shape Differences (HotellingT2)", row.names = TRUE) %>%
  kable_classic(full_width = F, html_font = "Cambria") 

