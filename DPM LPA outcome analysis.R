library(BayesFactor)
library(partitions)

# import data
hard_2year <- read.csv("~/Documents/ABCD model fit analysis/Bayesian infinite LPA analysis/for_hard_analysis_2year (0 missing).csv", row.names=1)
hard_3year <- read.csv("~/Documents/ABCD model fit analysis/Bayesian infinite LPA analysis/for_hard_analysis_3year (0 missing).csv", row.names=1)

# 2 year (note that r2 values are based on hard assignment, which isn't what we actually report)
Bayes_ANOVA_and_posthoc(hard_2year[, c("externalizing", "rule.breaking", "aggression", "internalizing", "positive.urgency", "negative.urgency")], hard_2year$z_hat)

# 3 year
Bayes_ANOVA_and_posthoc(hard_3year[, c("externalizing", "rule.breaking", "aggression", "internalizing")], hard_3year$z_hat)
