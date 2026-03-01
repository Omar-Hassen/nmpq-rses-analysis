library(ggstatsplot)
library(tableone) 
#preparing and understanding my data
mydata <- read.csv("NomophobiaQ.csv" ,stringsAsFactors = T)
str(mydata)
mydata <- mydata[, !names(mydata) %in% c("X." , "University" )]
str(mydata) 
mydata[mydata == ""] <- NA
mydata <- droplevels(mydata)
str(mydata)
shapiro.test(mydata$Nomophobia.Score)
shapiro.test(mydata$Self.Esteem.Score)
shapiro.test(mydata$Age)
shapiro.test(mydata$Phone_usage.hours.)
shapiro.test(mydata$Extraversion.Score)
shapiro.test(mydata$Agreeableness.Score)
shapiro.test(mydata$Conscientiousness.Score)
shapiro.test(mydata$Neuroticism.Score)
shapiro.test(mydata$Openness.to.Experience.Score)
# Only Nomophobia and Phone Usage are normally-distributed
summary(mydata)
#self-esteem grading needs to be adjusted manually
mydata$Self.Esteem.Grading <- factor(mydata$Self.Esteem.Grading,levels = c("Low", "Normal", "High"), ordered = TRUE)
#Dempgraphics and descriptives
dput(names(mydata))
Descriptives <- c("Gender", "Age", "Academic_year", 
                  "Relationship.Status", "Phone_usage.hours.", 
                  "Predominant.Smartphone.Usage.Habit.original", "Nomophobia.Score", 
                  "Nomophobia.Level.detailed", "Self.Esteem.Score", "Self.Esteem.Grading", 
                  "Extraversion.Score", "Agreeableness.Score", "Conscientiousness.Score", 
                  "Neuroticism.Score", "Openness.to.Experience.Score")
CatVars <- c("Gender", "Academic_year", 
             "Relationship.Status", 
             "Predominant.Smartphone.Usage.Habit.original", 
             "Nomophobia.Level.detailed", "Self.Esteem.Grading")
NomVars <- c("Age", "Self.Esteem.Score", "Extraversion.Score", "Agreeableness.Score", "Conscientiousness.Score", 
             "Neuroticism.Score", "Openness.to.Experience.Score")
Tab1 <- CreateTableOne(vars = Descriptives, data = mydata, factorVars = CatVars)
Table1 <- print(Tab1, nonnormal = NomVars)
write.csv(Table1, "NomoTable1.csv")
#Correlations calculation
ggscatterstats(data = mydata ,
               x = "Nomophobia.Score" , y = "Self.Esteem.Score" , 
               type = "nonparametric" )
cor.test(mydata$Nomophobia.Score , mydata$Age , method = "spearman" , exact = F)
cor.test(mydata$Nomophobia.Score , mydata$Phone_usage.hours. , method = "spearman" , exact = F)
cor.test(mydata$Nomophobia.Score , mydata$Extraversion.Score , method = "spearman" , exact = F)
cor.test(mydata$Nomophobia.Score , mydata$Agreeableness.Score , method = "spearman" , exact = F)
cor.test(mydata$Nomophobia.Score , mydata$Conscientiousness.Score , method = "spearman" , exact = F)
cor.test(mydata$Nomophobia.Score , mydata$Neuroticism.Score , method = "spearman" , exact = F)
cor.test(mydata$Nomophobia.Score , mydata$Openness.to.Experience.Score , method = "spearman" , exact = F)
#perfoming simple regression model for the main outcome of interest
RegNomSel <- lm(Nomophobia.Score~Self.Esteem.Score)
summary(RegNomSel)
#Distribution of The Predominant Smartphone Usage Habit (PSUH) and the associated Nomophobia Scores
 Tab3 <- CreateTableOne(vars = "Nomophobia.Score" , data = mydata , 
                        strata = "Predominant.Smartphone.Usage.Habit.original")
 Table3 <- print(Tab3, nonnormal = NomVars ,   test = TRUE    , pDigits = 3)
 write.csv(Table3, "NomoTable3.csv")
 #Getting the P-value (results are reported with caution as some groups have as few as 1 subjects)
 kwTest <- kruskal.test(
   Nomophobia.Score ~ Predominant.Smartphone.Usage.Habit.original)
 kwTest$p.value
 
 