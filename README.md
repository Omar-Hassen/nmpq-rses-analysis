# Nomophobia Among Medical Students and its Relation to Self-Esteem and Personality Traits: A National cross-sectional study
Analysis of a questionnaire to assess the prevalence of Nomophobia and its correlation with Self-esteem and personality traits in a sample of medical students.
The study evaluates whether higher levels of nomophobia are associated with lower self-esteem and specific personality profiles based on the Big Five model.
## Study Design & Population
### Design:
Analytical cross-sectional study   
### Population:
Egyptian medical students (age 18–26)  
### Sample Size:
N = 365, Calculated using β = 0.2 and α = 0.05 based on expected correlations from existing literature (Target: 362; Achieved: 366, with 1 exclusion for age).
## Instruments Used
Nomophobia Questionnaire (NMP-Q)<br/>
Brief Big Five Inventory (BFI-10)<br/>
Rosenberg Self-Esteem Scale (RSES)
## Data Availability
Due to ethical and privacy considerations, raw participant data are not publicly available.
## Statistical Analysis
All data analyses were performed using R version 4.5.2 via RStudio and the packages tidyverse, gtsummary, ggstatsplot, rstatix, car, flextable, and officer.
### Descriptive statistics
Mean ± SD for normally distributed variables; median (IQR) for non-normal variables; categorical variables as counts and percentages
### Normality testing 
Shapiro-Wilk test
### Correlation analysis
Spearman’s rank correlation
### Regression modeling
Although correlation was measured using Spearman’s method due to the non-normality of the data, a linear regression model was used, as suggested by [Lumley et al. (2002)](https://doi.org/10.1146/annurev.publhealth.23.100901.140546), given the sufficiently large sample size (N=365). The sample was deemed “sufficiently large” if it had around 100 subjects to use linear regression in non-normally distributed data if the sample was around 100 subjects
### Group comparisons
Pearson’s chi-square test, Student’s t-test, or Wilcoxon rank-sum test as appropriate
### Subgroup analysis
One-way ANOVA for phone usage patterns with Levene’s test for homogeneity; small categories grouped
### Behavioral grouping
Classification into “Highly Rewarding Activities” vs “Less Rewarding Activities”
### Stratified analysis
By gender, training level, relationship status, and nomophobia severity
### Visualization
ggplot2 and ggstatsplot
## Citation
The corresponding study will be added upon publication
