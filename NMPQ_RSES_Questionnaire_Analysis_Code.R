library(ggstatsplot)
library(ggplot2)
library(tidyverse)
library(gtsummary)
library(finalfit)
library(officer)
library(flextable)
library(rstatix)
library(car)
#Understanding and manipulating my data--------------------------------------------
mydata <- read.csv("NomophobiaQ.csv" ,stringsAsFactors = T)
mydata <- mydata[, !names(mydata) %in% c("X." , "University")]
mydata[mydata == ""] <- NA
mydata <- droplevels(mydata)
mydata$Mod_Sev_Nomo[mydata$Nomophobia.Level.detailed =='Mild'] ="Mild"
mydata$Mod_Sev_Nomo[mydata$Nomophobia.Level.detailed =='Moderate'] ="Moderate or Severe"
mydata$Mod_Sev_Nomo[mydata$Nomophobia.Level.detailed =='Severe'] ="Moderate or Severe"
mydata$Mod_Sev_Nomo = as.factor(mydata$Mod_Sev_Nomo)
#Grouping the variables
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
#Only Nomophobia and Phone Usage are normally-distributed
NonNormVars <- c("Age", "Self.Esteem.Score", "Extraversion.Score", "Agreeableness.Score", "Conscientiousness.Score", 
                 "Neuroticism.Score", "Openness.to.Experience.Score")
#Group smartphone usage habits levels with less than 5 occurrences into "Other"
mydata$Predominant.Smartphone.Usage.Habit.original <- fct_lump_min(
  mydata$Predominant.Smartphone.Usage.Habit.original, 
  min = 5, 
  other_level = "Other")
#Sorting smartphone usage habits levels from the most frequent to the least
mydata$Predominant.Smartphone.Usage.Habit.original <- fct_infreq(mydata$Predominant.Smartphone.Usage.Habit.original)
#Self-esteem grading needs to be adjusted manually
mydata$Self.Esteem.Grading <- factor(mydata$Self.Esteem.Grading,levels = c("Low", "Normal", "High"), ordered = TRUE)
#Dempgraphics and descriptives--------------------------------------------------
table1 <- tbl_summary(
  mydata,
  type = list(where(is.numeric) ~ "continuous"),
  missing = "no",
  include = all_of(Descriptives),
  statistic = list(
    Age ~ "{median} ({p25} - {p75})",
    Nomophobia.Score ~ "{mean} ({sd})",
    Self.Esteem.Score ~ "{median} ({p25} - {p75})",
    Phone_usage.hours. ~ "{mean} ({sd})",
    Extraversion.Score ~ "{median} ({p25} - {p75})",
    Agreeableness.Score ~ "{median} ({p25} - {p75})",
    Conscientiousness.Score ~ "{median} ({p25} - {p75})",
    Neuroticism.Score ~ "{median} ({p25} - {p75})",
    Openness.to.Experience.Score ~ "{median} ({p25} - {p75})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  digits = all_continuous() ~ 2,
  label = list(
    Openness.to.Experience.Score = "Openness-to-Experience Score",
    Age = "Age (years)", Neuroticism.Score = "Neuroticism Score",
    Academic_year = "Academic Year",
    Relationship.Status = "Relationship Status	", Phone_usage.hours.	 = "Average Daily Phone Usage (hours)",
    Nomophobia.Score = "Nomophobia Score", Predominant.Smartphone.Usage.Habit.original = "Predominant Smartphone Usage Habit",
    Nomophobia.Level.detailed	 = "Nomophobia Grading", Self.Esteem.Score	 = "Self-Esteem Score",
    Self.Esteem.Grading		 = "Self-Esteem Grading", Extraversion.Score		 = "Extraversion Score",
    Agreeableness.Score = "Agreeableness Score",
    Conscientiousness.Score	 = "Conscientiousness Score"
  )
) %>%
  add_n() %>% 
#Removing the N = 365 from the main data column
  modify_header( label = "**Variable**",
                 n = "**N**",
                 stat_0 = "**Summary Statistics**") %>% 
#Renaming the N column
  modify_header(n ~ "**N**") %>% 
  bold_labels()
#Correlations calculation-------------------------------------------------------
scatter_cor<- ggscatterstats(data = mydata ,
               x = "Nomophobia.Score" , y = "Self.Esteem.Score" , 
               type = "nonparametric",
               xlab = "Nomophobia Scores", ylab = "Self-Esteem Scores",
               xsidehistogram.args = list(fill = "#009E73", color = "black", na.rm = TRUE),
               ysidehistogram.args = list(fill = "#a03100", color = "black", na.rm = TRUE),
               ggplot.component = list(
                 scale_x_continuous(n.breaks = 25),
                 scale_y_continuous(breaks = seq( 0, 30, by = 2)))
)
Cor_results <- lapply(mydata[NumVars[-1]], function(x){
  cor.test(mydata$Nomophobia.Score , x , method = "spearman" , exact = F)
})
print(Cor_results)
#Academic Year by Predominant Smartphone Usage Pattern
barchart<-mydata %>%
  filter(!is.na(Predominant.Smartphone.Usage.Habit.original)) %>%
  mutate(
    Academic_year = fct_recode(Academic_year, 
                               "1" = "1st Year", "2" = "2nd Year", "3" = "3rd Year", 
                               "4" = "4th Year", "5" = "5th Year", "Intern" = "Internship"),
    Predominant_Smartphone_Usage_Pattern = fct_rev(fct_infreq(Predominant.Smartphone.Usage.Habit.original))
  ) %>%
  ggplot(aes(x = Academic_year, fill = Predominant_Smartphone_Usage_Pattern)) +
  geom_bar(width=0.6) + 
  scale_fill_manual(
    values = c(
      "Short-Form Video Content"  = "#E63946",
      "Social Media Feeds"        = "#F4A261",
      "Long-Form Video Content"   = "#E9C46A",
      "Instant Messaging"         = "#2A9D8F",
      "Mobile Gaming"             = "#264653",
      "General Web Browsing"      = "#457B9D",
      "Social Media Stories"      = "#A8DADC",
      "Digital Reading"           = "#9B59B6",
      "E-Commerce"                = "#34495E",
      "Audio Streaming"           = "#2ECC71",
      "Other"                     = "#BDC3C7" 
    )
  ) +
  scale_y_continuous(breaks = seq(0, 90, by = 10)) + 
  theme_minimal()+
  scale_x_discrete(expand = expansion(mult = c(0.07, 0.07))) + 
  labs(
    fill = "Predominant Smartphone Usage Pattern", 
    x = "Academic Year",
    y = "Count")

#performing a simple regression model for the main outcome of interest-----------
#(Although correlation was measured using Spearman’s method due to the non-normality of all variables, a linear regression model was used, as suggested by Lumley et al. (2002), given the sufficiently large sample size (N=365).
RegNomSel <- lm(Nomophobia.Score ~ Self.Esteem.Score, data = mydata)
summary(RegNomSel)
#Checking the homogeneity of Variance to determine the test of choice
leveneTest(Nomophobia.Score ~ Predominant.Smartphone.Usage.Habit.original, data = mydata)
#P value=0.4 >> the variance is homogeneous >> use Fisher's ANOVA 
#Distribution of The Predominant Smartphone Usage Habit (PSUH) and the associated Nomophobia Scores----
table2 <-tbl_summary(
  mydata,
  by = Predominant.Smartphone.Usage.Habit.original,
  include =  "Nomophobia.Score",
  statistic = list(
    Nomophobia.Score ~ "{mean} ({sd})"  ),
  digits = all_continuous() ~ 2,
  label = list(
    Nomophobia.Score = "Nomophobia Score")
) %>%
  add_n() %>%
  add_p(test = Nomophobia.Score ~ "oneway.test",
        test.args = Nomophobia.Score ~ list(var.equal = TRUE) #Stating that the variance is homogeneous to use Fisher's not Welch's
  ) %>%
  add_significance_stars()
#Visualization of the findings
boxplot<-mydata %>%
  filter(!is.na(Predominant.Smartphone.Usage.Habit.original)) %>%
  mutate(
#Shortening the names
  Habit_Short = fct_recode(Predominant.Smartphone.Usage.Habit.original,
                             "Short Video"  = "Short-Form Video Content",
                             "Social Feeds" = "Social Media Feeds",
                             "Long Video"   = "Long-Form Video Content",
                             "Messaging"    = "Instant Messaging",
                             "Gaming"       = "Mobile Gaming",
                             "Web Browsing" = "General Web Browsing",
                             "Stories"      = "Social Media Stories",
                             "Reading"      = "Digital Reading"
    ),
  Habit_Short = fct_infreq(Habit_Short)
  ) %>% 
  ggplot(aes(x = Habit_Short, y = Nomophobia.Score, fill = Habit_Short)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) + 
  geom_jitter(color = "grey30", width = 0.2, alpha = 0.5, size = 1.2, shape = 18) +
  scale_fill_manual(
    values = c(
      "Short Video"  = "#E63946",
      "Social Feeds" = "#F4A261",
      "Long Video"   = "#E9C46A",
      "Messaging"    = "#2A9D8F",
      "Gaming"       = "#264653",
      "Web Browsing" = "#457B9D",
      "Stories"      = "#A8DADC",
      "Reading"      = "#9B59B6",
      "Other"        = "#BDC3C7" 
    )
  ) +
  scale_y_continuous(breaks = seq(0, 150, by = 10)) + 
  theme_minimal() +
  labs(
    x = "Predominant Smartphone Usage Habit",
    y = "Nomophobia Score"
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.07, 0.07))) + 
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    legend.position = "none"
  )
  
#ANOVA test Post-hoc Analysis---------------------------------------------------
#activities that involve Reward Circuitry (i.e., social validation and interactions, infinite scroll and rapid achievement) were grouped into one group
#Activities that don't involve social interaction or infinite scroll were grouped together
mydata <- mydata %>%
  mutate(Dopamine_Level = case_when(
    Predominant.Smartphone.Usage.Habit.original %in% c("Short-Form Video Content", 
                                                       "Social Media Stories", 
                                                       "Mobile Gaming","Instant Messaging",
                                                       "Social Media Feeds") ~ "Highly Rewarding Activities",
    TRUE ~ "Less Rewarding Activities" #Everything else >> Low-Dopamine
  ))
#Converting it to a factor for analysis
mydata$Dopamine_Level <- as.factor(mydata$Dopamine_Level)
#Running the t-test to compare Nomophobia scores between High and Low Dopamine groups
dopamine_test<-tbl_summary(
  mydata,
  by = Dopamine_Level,
  include =  "Nomophobia.Score",
  statistic = list(
    Nomophobia.Score ~ "{mean} ({sd})"  ),
  digits = all_continuous() ~ 2,
  label = list(
    Nomophobia.Score = "Nomophobia Score")
) %>%
  add_n()%>%
  add_p(test = Nomophobia.Score ~ "t.test",
        pvalue_fun = label_style_pvalue(digits = 3)) %>%
  add_significance_stars()
#Post-hoc Analysis--------------------------------------------------------------
#the relationship between the exposure to short-form content the most and Nomophobia score 
ShortContent <-tbl_summary(
  mydata,
  by = Watching.reels.or.Stories,
  include =  "Nomophobia.Score",
  statistic = list(
    Nomophobia.Score ~ "{mean} ({sd})"  ),
  digits = all_continuous() ~ 2,
  label = list(
    Nomophobia.Score = "Nomophobia Score")
) %>%
  add_n() %>%
  add_p(test = Nomophobia.Score ~ "t.test",
        pvalue_fun = label_style_pvalue(digits = 3)) %>%
  add_significance_stars()
#the relationship between the exposure to short-form videos (specifically) the most and Nomophobia
ShortVids <-tbl_summary(
  mydata,
  by = watching.reels,
  include =  "Nomophobia.Score",
  statistic = list(
    Nomophobia.Score ~ "{mean} ({sd})"  ),
  digits = all_continuous() ~ 2,
  label = list(
    Nomophobia.Score = "Nomophobia Score")
) %>%
  add_n()%>%
  add_p(test = Nomophobia.Score ~ "t.test",
        pvalue_fun = label_style_pvalue(digits = 3)) %>%
  add_significance_stars()
#the relationship between the exposure to Social Media Stories the most and Nomophobia
Stories<-tbl_summary(
  mydata,
  by = stories.only,
  include =  "Nomophobia.Score",
  statistic = list(
    Nomophobia.Score ~ "{mean} ({sd})"  ),
  digits = all_continuous() ~ 2,
  label = list(
    Nomophobia.Score = "Nomophobia Score")
) %>%
  add_n()%>%
  add_p(test = Nomophobia.Score ~ "t.test",
        pvalue_fun = label_style_pvalue(digits = 3)) %>%
  add_significance_stars()
#Testing if exposure to short-form content the most is a dependent factor
ShortContentDep <- tbl_summary(
  mydata,
  by = "Watching.reels.or.Stories",
  type = list(where(is.numeric) ~ "continuous"),
  missing = "no",
  include = all_of(Descriptives),
  statistic = list(
    Age ~ "{median} ({p25} - {p75})",
    Nomophobia.Score ~ "{mean} ({sd})",
    Self.Esteem.Score ~ "{median} ({p25} - {p75})",
    Phone_usage.hours. ~ "{mean} ({sd})",
    Extraversion.Score ~ "{median} ({p25} - {p75})",
    Agreeableness.Score ~ "{median} ({p25} - {p75})",
    Conscientiousness.Score ~ "{median} ({p25} - {p75})",
    Neuroticism.Score ~ "{median} ({p25} - {p75})",
    Openness.to.Experience.Score ~ "{median} ({p25} - {p75})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  digits = all_continuous() ~ 2,
  label = list(
    Openness.to.Experience.Score = "Openness-to-Experience Score",
    Age = "Age (years)", Neuroticism.Score = "Neuroticism Score",
    Academic_year = "Academic Year",
    Relationship.Status = "Relationship Status	", Phone_usage.hours.	 = "Average Daily Phone Usage (hours)",
    Nomophobia.Score = "Nomophobia Score", Predominant.Smartphone.Usage.Habit.original = "Predominant Smartphone Usage Habit",
    Nomophobia.Level.detailed	 = "Nomophobia Grading", Self.Esteem.Score	 = "Self-Esteem Score",
    Self.Esteem.Grading		 = "Self-Esteem Grading", Extraversion.Score		 = "Extraversion Score",
    Agreeableness.Score = "Agreeableness Score",
    Conscientiousness.Score	 = "Conscientiousness Score"
  )
) %>%
  add_p(
    test = list(
      all_of(NonNormVars) ~ "wilcox.test",
      c(Nomophobia.Score, Phone_usage.hours.) ~ "t.test"),
    pvalue_fun = label_style_pvalue(digits = 3)) %>%
  add_significance_stars() %>% 
  add_n() %>%
  bold_labels()
#Testing if  other variables are associated with Gender
GenderStrata  <- tbl_summary(
  mydata,
  by = "Gender",
  type = list(where(is.numeric) ~ "continuous"),
  missing = "no",
  include = all_of(Descriptives),
  statistic = list(
    Age ~ "{median} ({p25} - {p75})",
    Nomophobia.Score ~ "{mean} ({sd})",
    Self.Esteem.Score ~ "{median} ({p25} - {p75})",
    Phone_usage.hours. ~ "{mean} ({sd})",
    Extraversion.Score ~ "{median} ({p25} - {p75})",
    Agreeableness.Score ~ "{median} ({p25} - {p75})",
    Conscientiousness.Score ~ "{median} ({p25} - {p75})",
    Neuroticism.Score ~ "{median} ({p25} - {p75})",
    Openness.to.Experience.Score ~ "{median} ({p25} - {p75})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  digits = all_continuous() ~ 2,
  label = list(
    Openness.to.Experience.Score = "Openness-to-Experience Score",
    Age = "Age (years)", Neuroticism.Score = "Neuroticism Score",
    Academic_year = "Academic Year",
    Relationship.Status = "Relationship Status	", Phone_usage.hours.	 = "Average Daily Phone Usage (hours)",
    Nomophobia.Score = "Nomophobia Score", Predominant.Smartphone.Usage.Habit.original = "Predominant Smartphone Usage Habit",
    Nomophobia.Level.detailed	 = "Nomophobia Grading", Self.Esteem.Score	 = "Self-Esteem Score",
    Self.Esteem.Grading		 = "Self-Esteem Grading", Extraversion.Score		 = "Extraversion Score",
    Agreeableness.Score = "Agreeableness Score",
    Conscientiousness.Score	 = "Conscientiousness Score"
  )
) %>%
  add_p(
    test = list(
      all_of(NonNormVars) ~ "wilcox.test",
      c(Nomophobia.Score, Phone_usage.hours.) ~ "t.test",
      all_categorical() ~ "chisq.test"),
    pvalue_fun = label_style_pvalue(digits = 3)) %>%
  add_significance_stars() %>% 
  add_n() %>%
  bold_labels()
#Testing if other variables are associated with being an intern vs. a student
InternStrata <- tbl_summary(
  mydata,
  by = "Students_Interns",
  type = list(where(is.numeric) ~ "continuous"),
  missing = "no",
  include = all_of(Descriptives),
  statistic = list(
    Age ~ "{median} ({p25} - {p75})",
    Nomophobia.Score ~ "{mean} ({sd})",
    Self.Esteem.Score ~ "{median} ({p25} - {p75})",
    Phone_usage.hours. ~ "{mean} ({sd})",
    Extraversion.Score ~ "{median} ({p25} - {p75})",
    Agreeableness.Score ~ "{median} ({p25} - {p75})",
    Conscientiousness.Score ~ "{median} ({p25} - {p75})",
    Neuroticism.Score ~ "{median} ({p25} - {p75})",
    Openness.to.Experience.Score ~ "{median} ({p25} - {p75})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  digits = all_continuous() ~ 2,
  label = list(
    Openness.to.Experience.Score = "Openness-to-Experience Score",
    Age = "Age (years)", Neuroticism.Score = "Neuroticism Score",
    Academic_year = "Academic Year",
    Relationship.Status = "Relationship Status	", Phone_usage.hours.	 = "Average Daily Phone Usage (hours)",
    Nomophobia.Score = "Nomophobia Score", Predominant.Smartphone.Usage.Habit.original = "Predominant Smartphone Usage Habit",
    Nomophobia.Level.detailed	 = "Nomophobia Grading", Self.Esteem.Score	 = "Self-Esteem Score",
    Self.Esteem.Grading		 = "Self-Esteem Grading", Extraversion.Score		 = "Extraversion Score",
    Agreeableness.Score = "Agreeableness Score",
    Conscientiousness.Score	 = "Conscientiousness Score"
  )
) %>%
  add_p(
    test = list(
      all_of(NonNormVars) ~ "wilcox.test",
      c(Nomophobia.Score, Phone_usage.hours.) ~ "t.test",
      all_categorical() ~ "chisq.test"),
    pvalue_fun = label_style_pvalue(digits = 3)) %>%
  add_significance_stars() %>% 
  add_n() %>%
  bold_labels()
#Testing if other variables are associated with relationship status
RelationStrata <- tbl_summary(
  mydata,
  by = "Relationship.Status",
  type = list(where(is.numeric) ~ "continuous"),
  missing = "no",
  include = all_of(Descriptives),
  statistic = list(
    Age ~ "{median} ({p25} - {p75})",
    Nomophobia.Score ~ "{mean} ({sd})",
    Self.Esteem.Score ~ "{median} ({p25} - {p75})",
    Phone_usage.hours. ~ "{mean} ({sd})",
    Extraversion.Score ~ "{median} ({p25} - {p75})",
    Agreeableness.Score ~ "{median} ({p25} - {p75})",
    Conscientiousness.Score ~ "{median} ({p25} - {p75})",
    Neuroticism.Score ~ "{median} ({p25} - {p75})",
    Openness.to.Experience.Score ~ "{median} ({p25} - {p75})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  digits = all_continuous() ~ 2,
  label = list(
    Openness.to.Experience.Score = "Openness-to-Experience Score",
    Age = "Age (years)", Neuroticism.Score = "Neuroticism Score",
    Academic_year = "Academic Year",
    Relationship.Status = "Relationship Status	", Phone_usage.hours.	 = "Average Daily Phone Usage (hours)",
    Nomophobia.Score = "Nomophobia Score", Predominant.Smartphone.Usage.Habit.original = "Predominant Smartphone Usage Habit",
    Nomophobia.Level.detailed	 = "Nomophobia Grading", Self.Esteem.Score	 = "Self-Esteem Score",
    Self.Esteem.Grading		 = "Self-Esteem Grading", Extraversion.Score		 = "Extraversion Score",
    Agreeableness.Score = "Agreeableness Score",
    Conscientiousness.Score	 = "Conscientiousness Score"
  )
) %>%
  add_p(
    test = list(
      all_of(NonNormVars) ~ "wilcox.test",
      c(Nomophobia.Score, Phone_usage.hours.) ~ "t.test",
      all_categorical() ~ "chisq.test"),
    pvalue_fun = label_style_pvalue(digits = 3)) %>%
  add_significance_stars() %>% 
  add_n() %>%
  bold_labels()
#Comparing between mild and moderate-to-severe nomophobia against the other variables
Mode_Sev <- tbl_summary(
  mydata,
  by = "Mod_Sev_Nomo",
  type = list(where(is.numeric) ~ "continuous"),
  missing = "no",
  include = all_of(Descriptives),
  statistic = list(
    Age ~ "{median} ({p25} - {p75})",
    Nomophobia.Score ~ "{mean} ({sd})",
    Self.Esteem.Score ~ "{median} ({p25} - {p75})",
    Phone_usage.hours. ~ "{mean} ({sd})",
    Extraversion.Score ~ "{median} ({p25} - {p75})",
    Agreeableness.Score ~ "{median} ({p25} - {p75})",
    Conscientiousness.Score ~ "{median} ({p25} - {p75})",
    Neuroticism.Score ~ "{median} ({p25} - {p75})",
    Openness.to.Experience.Score ~ "{median} ({p25} - {p75})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  digits = all_continuous() ~ 2,
  label = list(
    Openness.to.Experience.Score = "Openness-to-Experience Score",
    Age = "Age (years)", Neuroticism.Score = "Neuroticism Score",
    Academic_year = "Academic Year",
    Relationship.Status = "Relationship Status	", Phone_usage.hours.	 = "Average Daily Phone Usage (hours)",
    Nomophobia.Score = "Nomophobia Score", Predominant.Smartphone.Usage.Habit.original = "Predominant Smartphone Usage Habit",
    Nomophobia.Level.detailed	 = "Nomophobia Grading", Self.Esteem.Score	 = "Self-Esteem Score",
    Self.Esteem.Grading		 = "Self-Esteem Grading", Extraversion.Score		 = "Extraversion Score",
    Agreeableness.Score = "Agreeableness Score",
    Conscientiousness.Score	 = "Conscientiousness Score"
  )
) %>%
  add_p(
    test = list(
      all_of(NonNormVars) ~ "wilcox.test",
      c(Nomophobia.Score, Phone_usage.hours.) ~ "t.test",
      all_categorical() ~ "chisq.test"),
    pvalue_fun = label_style_pvalue(digits = 3)) %>%
  add_significance_stars() %>% 
  add_n() %>%
  bold_labels()
#Outputs-----------------------------------------------------------------------
#Figures
scatter_cor
barchart
boxplot
#Tables
Table1 <- as_flex_table(table1)
Table2 <- as_flex_table(table2)
Table3 <- as_flex_table(dopamine_test)
Table4 <- as_flex_table(ShortContent)
Table5 <- as_flex_table(ShortVids)
Table6 <- as_flex_table(Stories)
Table7 <- as_flex_table(ShortContentDep)
Table8 <- as_flex_table(GenderStrata)
Table9 <- as_flex_table(InternStrata)
Table10 <- as_flex_table(RelationStrata)
Table11 <- as_flex_table(Mode_Sev)
#Exporting the tables to a Word Document---------------------------------------
report <- read_docx()
report <- body_add_par(report, "Table1. Properties and descriptives of the sample", style = "heading 2")
report <- body_add_flextable(report, Table1)
report <- body_add_par(report, "Table2. Distribution of The Predominant Smartphone Usage Habit (PSUH) and the associated Nomophobia Scores", style = "heading 2")
report <- body_add_flextable(report, Table2)
report <- body_add_par(report, "Table3. The difference between Nomophobia scores among High and Low Reward Activity Groups", style = "heading 2")
report <- body_add_flextable(report, Table3)
report <- body_add_par(report, "Table4. the relationship between the exposure to short-form content the most and Nomophobia score", style = "heading 2")
report <- body_add_flextable(report, Table4)
report <- body_add_par(report, "Table5. the relationship between the exposure to short-form videos (specifically) the most and Nomophobia", style = "heading 2")
report <- body_add_flextable(report, Table5)
report <- body_add_par(report, "Table6. the relationship between the exposure to Social Media Stories the most and Nomophobia", style = "heading 2")
report <- body_add_flextable(report, Table6)
report <- body_add_par(report, "Table7. Testing if exposure to short-form content the most is a dependent factor", style = "heading 2")
report <- body_add_flextable(report, Table7)
report <- body_add_par(report, "Table8. Testing if  other variables are associated with Gender", style = "heading 2")
report <- body_add_flextable(report, Table8)
report <- body_add_par(report, "Table9. Testing if other variables are associated with being a student or intern", style = "heading 2")
report <- body_add_flextable(report, Table9)
report <- body_add_par(report, "Table10. Testing if other variables are associated with relationship status", style = "heading 2")
report <- body_add_flextable(report, Table10)
report <- body_add_par(report, "Table11. Testing if other variables are associated with having moderate-to-severe Nomophobia", style = "heading 2")
report <- body_add_flextable(report, Table11)
print(report, target = "Nomophobia_Tables.docx")
