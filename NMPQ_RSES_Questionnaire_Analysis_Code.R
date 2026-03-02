library(ggstatsplot)
library(tableone) 
#preparing and understanding my data--------------------------------------------
mydata <- read.csv("NomophobiaQ.csv" ,stringsAsFactors = T)
str(mydata)
mydata <- mydata[, !names(mydata) %in% c("X." , "University")]
str(mydata) 
mydata[mydata == ""] <- NA
mydata <- droplevels(mydata)
str(mydata)
#Categorizing the data variables------------------------------------------------
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
NumVars <- c("Nomophobia.Score" , "Phone_usage.hours." , "Age", "Self.Esteem.Score", "Extraversion.Score", "Agreeableness.Score", "Conscientiousness.Score", 
             "Neuroticism.Score", "Openness.to.Experience.Score")
shapiro_results <- lapply(mydata[NumVars], shapiro.test)
print(shapiro_results)
# Only Nomophobia and Phone Usage are normally-distributed
NonNormVars <- c("Age", "Self.Esteem.Score", "Extraversion.Score", "Agreeableness.Score", "Conscientiousness.Score", 
                 "Neuroticism.Score", "Openness.to.Experience.Score")
summary(mydata)
#self-esteem grading needs to be adjusted manually
mydata$Self.Esteem.Grading <- factor(mydata$Self.Esteem.Grading,levels = c("Low", "Normal", "High"), ordered = TRUE)
#Dempgraphics and descriptives--------------------------------------------------
Tab1 <- CreateTableOne(vars = Descriptives, data = mydata, factorVars = CatVars)
Table1 <- print(Tab1, nonnormal = NonNormVars)
write.csv(Table1, "NomoTable1.csv")
#Correlations calculation-------------------------------------------------------
ggscatterstats(data = mydata ,
               x = "Nomophobia.Score" , y = "Self.Esteem.Score" , 
               type = "nonparametric" )
Cor_results <- lapply(mydata[NumVars[-1]], function(x){
  cor.test(mydata$Nomophobia.Score , x , method = "spearman" , exact = F)
})
print(Cor_results)

#perfoming a simple regression model for the main outcome of interest-----------
#(Although correlation was measured using Spearman’s method due to the non-normality of all variables, a linear regression model was used, as suggested by Lumley et al. (2002), given the sufficiently large sample size (N=365).
RegNomSel <- lm(Nomophobia.Score~Self.Esteem.Score)
summary(RegNomSel)
#Distribution of The Predominant Smartphone Usage Habit (PSUH) and the associated Nomophobia Scores----
Tab3 <- CreateTableOne(vars = "Nomophobia.Score" , data = mydata , 
                       strata = "Predominant.Smartphone.Usage.Habit.original")
Table3 <- print(Tab3, nonnormal = NonNormVars)
write.csv(Table3, "NomoTable3.csv")
#Getting the P-value (results are reported with caution as some groups have n=1 subject)
Anova <- aov(data = mydata ,
             Nomophobia.Score ~ Predominant.Smartphone.Usage.Habit.original)
print(summary(Anova))
#Post-hoc Analysis--------------------------------------------------------------
##the relationship between the exposure to short-form content the most and Nomophobia score 
ShortContent <- CreateTableOne(vars = "Nomophobia.Score" , data = mydata , 
                               strata = "Watching.reels.or.Stories")
print(ShortContent, nonnormal = NonNormVars)
##the relationship between the exposure to short-form videos (specifically) the most and Nomophobia
ShortVids <- CreateTableOne(vars = "Nomophobia.Score" , data = mydata , 
                            strata = "watching.reels")
print(ShortVids, nonnormal = NonNormVars)
##the relationship between the exposure to Social Media Stories the most and Nomophobia
Stories <- CreateTableOne(vars = "Nomophobia.Score" , data = mydata , 
                          strata = "stories.only")
print(Stories, nonnormal = NonNormVars)
##Testing if exposure to short-form content the most is a dependent factor
ShortContentDep <- CreateTableOne(vars = Descriptives , data = mydata , 
                                  strata = "Watching.reels.or.Stories")
ShortContentDepTab <- print(ShortContentDep, nonnormal = NonNormVars)
write.csv(ShortContentDepTab, "ShortContentDepTab.csv")
##Testing if  other variables are associated with Gender
GenderStrata <- CreateTableOne(vars = Descriptives[-1] , data = mydata , 
                               strata = "Gender")
print(GenderStrata, nonnormal = NonNormVars)
##Testing if other variables are associated with being an intern vs. a student
InternStrata <- CreateTableOne(vars = Descriptives[-3] , data = mydata , 
                               strata = "Students_Interns")
print(InternStrata , nonnormal = NonNormVars)
##Testing if other variables are associated with relationship status
RelationStrata <- CreateTableOne(vars = Descriptives[-4] , data = mydata , 
                                 strata = "Relationship.Status")
print(RelationStrata, nonnormal = NonNormVars)
