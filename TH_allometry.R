setwd("~/Desktop/Grad School/4 Preservation/R/Data/singles")

library(geomorph)
library(Morpho)
library(shapes)
library(RRPP)

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
                            'WL41day61.tps', 'WL43day61.tps', 'WL44day61.tps', 'WL45day61.tps', 'WL46day61.tps', 'WL47day61.tps', 'WL48day61.tps','WL49day61.tps', 'WL50day61.tps', 'WL51day61.tps', 'WL52day61.tps', 'WL53day61.tps', 'WL54day61.tps', 'WL77day61.tps'))

#Generalized Procrustes Analysis (GPA)
myGPA<-gpagen(combined, curves = NULL, surfaces = NULL, PrinAxes = TRUE,
              max.iter = NULL, ProcD = TRUE, Proj = TRUE, print.progress = TRUE)

#classify each .tps file in object above into their group
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

#classify each .tps file in object above into their group
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


myGPA$FishID <- c("AL44", "AL45", "AL46", "AL47", "AL48", "AL49", "AL51", "AL54", "AL56", "AL59.2", "AL60", "AL61", "AL62", "AL67", "MA41", "MA43", "MA44", "MA45.2", "MA46", "MA47", "MA56", "MA58", "MA61", "MA64", "MA65", "MA66.2", "MA68", "MA42.2", "WL41", "WL43", "WL44", "WL45", "WL46", "WL47", "WL48", "WL49", "WL50", "WL51", "WL52", "WL53", "WL54.2", "WL77",
                  "AL44", "AL45", "AL46", "AL47", "AL48", "AL49", "AL51", "AL54", "AL56", "AL59.2", "AL60", "AL61", "AL62", "AL67", "MA41", "MA43", "MA44", "MA45.2", "MA46", "MA47", "MA56", "MA58", "MA61", "MA64", "MA65", "MA66.2", "MA68", "MA42.2", "WL41", "WL43", "WL44", "WL45", "WL46", "WL47", "WL48", "WL49", "WL50", "WL51", "WL52", "WL53", "WL54.2", "WL77",
                  "AL44", "AL45", "AL46", "AL47", "AL48", "AL49", "AL51", "AL54", "AL56", "AL59.2", "AL60", "AL61", "AL62", "AL67", "MA41", "MA43", "MA44", "MA45.2", "MA46", "MA47", "MA56", "MA58", "MA61", "MA64", "MA65", "MA66.2", "MA68", "MA42.2", "WL41", "WL43", "WL44", "WL45", "WL46", "WL47", "WL48", "WL49", "WL50", "WL51", "WL52", "WL53", "WL54.2", "WL77",
                  "AL44", "AL45", "AL46", "AL47", "AL48", "AL49", "AL51", "AL54", "AL56", "AL59.2", "AL60", "AL61", "AL62", "AL67", "MA41", "MA43", "MA44", "MA45.2", "MA46", "MA47", "MA56", "MA58", "MA61", "MA64", "MA65", "MA66.2", "MA68", "MA42.2", "WL41", "WL43", "WL44", "WL45", "WL46", "WL47", "WL48", "WL49", "WL50", "WL51", "WL52", "WL53", "WL54.2", "WL77")

myGPA$Group<-as.factor(myGPA$Group)
myGPA$Lake<-as.factor(myGPA$Lake)

gdf <- geomorph.data.frame(myGPA, Group = myGPA$Group, Lake = myGPA$Lake)

# Group Allometries
fit <- procD.lm(coords ~ Csize * Lake, data=gdf, iter=0,
                print.progress = FALSE)
# CAC (should not change from last time; model change has no effect)
plotAllometry(fit, size = gdf$Csize, logsz = TRUE, method = "CAC",
              pch = 19)
# Predline
plotAllometry(fit, size = gdf$Csize, logsz = TRUE, method = "PredLine",
              pch = 19, col = as.numeric(interaction(gdf$Lake)))
# RegScore
plotAllometry(fit, size = gdf$Csize, logsz = TRUE, method = "RegScore",
              pch = 19, col = as.numeric(interaction(gdf$Lake)))


# Multivariate regression of shape on log-transformed centroid size
allometry_model <- procD.lm(gdf$coords~ log(gdf$Csize))

allometry_model_group <- procD.lm(gdf$coords ~ log(gdf$Csize) * gdf$Lake)


plot(allometry_model, 
     type = "regression", 
     predictor = log(gdf$Csize), 
     reg.type = "PredLine")  # color by group


allometry_model <- procD.lm(gdf$coords ~ log(gdf$Csize))
summary(allometry_model)

###############
allometry_model <- procD.lm(coords ~ log(Csize), data = gdf)
reg_scores <- allometry_model$LM$RegScore
library(ggplot2)

ggplot(gdf, aes(x = log(Csize), y = reg_scores)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    x = "log(Centroid Size)",
    y = "Shape (Regression Scores)",
    title = "Allometric Relationship Between Size and Shape"
  ) +
  theme_minimal()

