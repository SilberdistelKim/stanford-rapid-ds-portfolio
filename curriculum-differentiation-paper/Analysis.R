# install.packages("MatchIt")
# install.packages("cobalt")
# install.packages("cem")
# install.packages("hrbrthemes")
# install.packages("ggplot2")
# install.packages("margins")
# install.packages("effects")
library("MatchIt")
library("cobalt")
library("cem")
library("dplyr")
library("nnet")
library("margins")
library("effects")

# 1. Data preparation ----

# load("C:/Users/silbe/OneDrive/Yongwook/r2_Data/US/ELS 2002/ELS 2002_student.Rdata")
# data <- els_02_12_byf3pststu_v1_0
# 
# els = data[c("F3PS1LVL", "BYA04", "bytxcstd", "bytxcqu", "BYSES1", "BYSES1QU",
#              "byfcomp", "byrace", "bysex", "byparasp", "bystexp", "BYS86G",
#              "bysctrl", "byurban", "byregion",
#              "byschprg", "F1RGPP2")]
# 
# save(els, file = "ELS2002.Rdata")

load("C:/Users/silbe/OneDrive/Yongwook/a1_Categorical Data Analysis/Project/2_Analysis/ELS2002.Rdata")

els = within(els,{
  F3PS1LVL[F3PS1LVL <= -4] = NA
  BYA04[BYA04 <= 1 | BYA04 == 4] = NA
  bytxcstd[bytxcstd == -8] = NA
  bytxcqu[bytxcqu == -8] = NA
  BYSES1[BYSES1 <= -4] = NA
  BYSES1QU[BYSES1QU <= -4] = NA
  byfcomp[byfcomp <= -4] = NA
  byrace[byrace <= -4] = NA
  bysex[bysex <= -4] = NA
  byparasp[byparasp <= -4] = NA
  bystexp[bystexp <= -4] = NA
  BYS86G[BYS86G <= -1] = NA
  byschprg[byschprg == -8] = NA
  F1RGPP2[F1RGPP2 <= -4] = NA
})

els$abil = els$bytxcstd
els$ses = els$BYSES1

els$ps = recode_factor(els$F3PS1LVL, `-3` = "No PS", `1` = "Univ", `2` = "Col", `3` = "Col")
els$diff = recode_factor(els$BYA04, `3` = "No diff", `2` = "Diff/Group")
els$abilq = recode_factor(els$bytxcqu, `1` = "Low", `2` = "Low", `3` = "High", `4` = "High")
els$sesq = recode_factor(els$BYSES1QU, `1` = "Low", `2` = "Low", `3` = "High", `4` = "High")
els$twoprnt = recode_factor(els$byfcomp, `2` = "Others", `3` = "Others", `4` = "Others", `5` = "Others", `6` = "Others", `7` = "Others", `8` = "Others", `9` = "Others",
                            `1` = "Two parent")
els$race = recode_factor(els$byrace, `7` = "White", `3` = "Black", `4` = "Hispanic", `5` = "Hispanic", `2` = "Asian",
                         `1` = "Others", `6` = "Others")
els$female = recode_factor(els$bysex, `1` = "Male", `2` = "Female")
els$pexp = recode_factor(els$byparasp, `1` = "No PS", `2` = "No PS", `3` = "Col", `4` = "Col",
                         `5` = "Univ", `6` = "Univ", `7` = "Univ")
els$sexp = recode_factor(els$bystexp, `-1` = "No PS", `1` = "No PS", `2` = "No PS", `3` = "Col", `4` = "Col",
                         `5` = "Univ", `6` = "Univ", `7` = "Univ")
els$disc = recode_factor(els$BYS86G, `1` = "Never", `2` = "Sometimes", `3` = "Often")
els$schtype = recode_factor(els$bysctrl, `1` = "Public", `2` = "Catholic", `3` = "Private")
els$urbanicity = recode_factor(els$byurban, `1` = "Urban", `2` = "Suburban", `3` = "Rural")
els$region = recode_factor(els$byregion, `1` = "NE", `2` = "MW", `3` = "S", `4` = "W")
els$track = recode_factor(els$byschprg, `1` = "General", `2` = "Academic", `3` = "Vocational")
els$gpa = as.numeric(recode(as.character(els$F1RGPP2), "0" = "0.5", "1" = "1.25", "2" = "1.75", "3" = "2.25", "4" = "2.75", "5" = "3.25", "6" = "3.75"))

# els = els[complete.cases(els[, c("abil", "ses", "ps", "diff", "abilq", "sesq", "twoprnt", "race", "female", "pexp", "sexp", "disc", "schtype", "urbanicity", "region", "track", "gpa")]),]

# 2. Estimation of the treatment effect before matching ----

# 2.1. Diff x Ability before matching ----

# 2.1.1. Diff x Ability before matching without controls ----

fit.b1 = multinom(ps ~ diff + abilq + diff:abilq, data = els)
summary(fit.b1)

exp(coef(fit.b1)) # Odds ratio

confint(fit.b1) # 95% CI
confint(fit.b1, level = 0.90) # 90% CI

z = summary(fit.b1)$coefficients / summary(fit.b1)$standard.errors
pval.b1 = (1 - pnorm(abs(z), 0, 1)) * 2
pval.b1 # p-value 

logLik(fit.b1) # Log likelihood
null = update(fit.b1, . ~ 1, trace = FALSE)
1 - (logLik(fit.b1) / logLik(null)) # McFadden's R-squared

model.b1 = model.frame(ps ~ diff + abilq + diff:abilq, data = els)
dim(model.b1) # Number of observations
table(model.b1$diff) # Number of the control and treated

# 2.1.2. Diff x Ability before matching with controls ----

fit.b1c = multinom(ps ~ diff + abilq + diff:abilq 
                   + ses + gpa + track + female + race + twoprnt + sexp + disc + schtype + urbanicity + region, 
                   data = els)
summary(fit.b1c)

exp(coef(fit.b1c)) # Odds ratio

confint(fit.b1c) # 95% CI
confint(fit.b1c, level = 0.90) # 90% CI

z = summary(fit.b1c)$coefficients / summary(fit.b1c)$standard.errors
pval.b1c = (1 - pnorm(abs(z), 0, 1)) * 2
pval.b1c # p-value

logLik(fit.b1c) # Log likelihood
null = update(fit.b1c, . ~ 1, trace = FALSE)
1 - (logLik(fit.b1c) / logLik(null)) # McFadden's R-squared

model.b1c = model.frame(ps ~ diff + abilq + diff:abilq 
                        + ses + gpa + track + female + race + twoprnt + sexp + disc + schtype + urbanicity + region, 
                        data = els)
dim(model.b1c) # Number of observations
table(model.b1c$diff) # Number of the control and treated

# 2.2. Diff x SES before matching ----

# 2.2.1. Diff x SES before matching without controls ----

fit.b2 = multinom(ps ~ diff + sesq + diff:sesq, data = els)
summary(fit.b2)

exp(coef(fit.b2)) # Odds ratio

confint(fit.b2) # 95% CI
confint(fit.b2, level = 0.90) # 90% CI

z = summary(fit.b2)$coefficients / summary(fit.b2)$standard.errors
pval.b2 = (1 - pnorm(abs(z), 0, 1)) * 2
pval.b2 # p-value

logLik(fit.b2) # Log likelihood
null = update(fit.b2, . ~ 1, trace = FALSE)
1 - (logLik(fit.b2) / logLik(null)) # McFadden's R-squared

model.b2 = model.frame(ps ~ diff + sesq + diff:sesq, data = els)
dim(model.b2) # Number of observations
table(model.b2$diff) # Number of the control and treated

pre.imbalance = imbalance(group = els$diff, data = els, drop = c("ps","diff", "abilq", "sesq"))

# 2.2.2. Diff x SES before matching with controls ----

fit.b2c = multinom(ps ~ diff + sesq + diff:sesq 
                   + abil + gpa + track + female + race + twoprnt + sexp + disc + schtype + urbanicity + region, 
                   data = els)
summary(fit.b2c)

exp(coef(fit.b2c)) # Odds ratio

confint(fit.b2c) # 95% CI
confint(fit.b2c, level = 0.90) # 90% CI

z = summary(fit.b2c)$coefficients / summary(fit.b2c)$standard.errors
pval.b2c = (1 - pnorm(abs(z), 0, 1)) * 2
pval.b2c # p-value

logLik(fit.b2c) # Log likelihood
null = update(fit.b2c, . ~ 1, trace = FALSE)
1 - (logLik(fit.b2c) / logLik(null)) # McFadden's R-squared

model.b2c = model.frame(ps ~ diff + sesq + diff:sesq 
                        + abil + gpa + track + female + race + twoprnt + sexp + disc + schtype + urbanicity + region, 
                        data = els)
dim(model.b2c) # Number of observations
table(model.b2c$diff) # Number of the control and treated

# 3. Nearest 1-to-1 matching within a caliper ----

els.complete = els[complete.cases(els[, c("abil", "ses", "ps", "diff", "abilq", "sesq", "twoprnt", "race", "female", "pexp", "sexp", "disc", "schtype", "urbanicity", "region", "track", "gpa")]),]

matched = matchit(formula = diff ~ abil + ses + female + race + twoprnt + pexp + sexp + disc + schtype + urbanicity + region
                  + ses:ses + ses:race + ses:schtype + ses:pexp + ses:sexp + ses:disc + ses:urbanicity + ses:region
                  + abil:abil + abil:schtype + abil:pexp + abil:sexp + abil:disc + abil:female
                  + schtype:female + schtype:race + schtype:pexp + schtype:sexp + schtype:disc 
                  + schtype:urbanicity + schtype:region + urbanicity:female + urbanicity:region,
                  data = els.complete, method = "nearest", distance = "logit", caliper = 0.25)

# Logistic model specified in Stata 
# matched = matchit(formula = diff ~ abil + ses + female + race + twoprnt + pexp + sexp + disc + schtype + urbanicity + region
#                   + ses:ses + ses:schtype + ses:pexp + ses:sexp
#                   + abil:abil + abil:schtype + abil:pexp + abil:sexp + abil:female
#                   + schtype:female + schtype:race + schtype:urbanicity + schtype:region,
#                   data = els, method = "nearest", distance = "logit", caliper = 0.25)

pre.balance = summary(matched)$sum.all
post.balance = summary(matched)$sum.matched

els.matched = match.data(matched)
table(els.matched$diff)

save(els.matched, file = "ELS2002_matched.Rdata")

load("C:/Users/silbe/OneDrive/Yongwook/a1_Categorical Data Analysis/Project/2_Analysis/ELS2002_matched.Rdata")

# 4. Estimation of the treatment effect after matching ----

# 4.1. Diff x Ability after matching ----

# 4.1.1. Diff x Ability after matching without controls ----

fit.a1 = multinom(ps ~ diff + abilq + diff:abilq, data = els.matched)
summary(fit.a1)

exp(coef(fit.a1)) # Odds ratio

confint(fit.a1) # 95% CI
confint(fit.a1, level = 0.90) # 90% CI

z = summary(fit.a1)$coefficients / summary(fit.a1)$standard.errors
pval.a1 = (1 - pnorm(abs(z), 0, 1)) * 2
pval.a1 # p-value 

logLik(fit.a1) # Log likelihood
null = update(fit.a1, . ~ 1, trace = FALSE)
1 - (logLik(fit.a1) / logLik(null)) # McFadden's R-squared

model.a1 = model.frame(ps ~ diff + abilq + diff:abilq, data = els.matched)
dim(model.a1) # Number of observations
table(model.a1$diff) # Number of the control and treated

# 4.1.2. Diff x Ability after matching with controls ----

fit.a1c = multinom(ps ~ diff + abilq + diff:abilq 
                   + ses + gpa + track + female + race + twoprnt + sexp + disc + schtype + urbanicity + region, 
                   data = els.matched)
summary(fit.a1c)

exp(coef(fit.a1c)) # Odds ratio

summary(marginal_effects(fit.a1c))

confint(fit.a1c) # 95% CI
confint(fit.a1c, level = 0.90) # 90% CI

z = summary(fit.a1c)$coefficients / summary(fit.a1c)$standard.errors
pval.a1c = (1 - pnorm(abs(z), 0, 1)) * 2
pval.a1c # p-value

logLik(fit.a1c) # Log likelihood
null = update(fit.a1c, . ~ 1, trace = FALSE)
1 - (logLik(fit.a1c) / logLik(null)) # McFadden's R-squared

model.a1c = model.frame(ps ~ diff + abilq + diff:abilq 
                        + ses + gpa + track + female + race + twoprnt + sexp + disc + schtype + urbanicity + region, 
                        data = els.matched)
dim(model.a1c) # Number of observations
table(model.a1c$diff) # Number of the control and treated

# 4.2. Diff x SES after matching ----

# 4.2.1. Diff x SES after matching without controls ----

fit.a2 = multinom(ps ~ diff + sesq + diff:sesq, data = els.matched)
summary(fit.a2)

exp(coef(fit.a2)) # Odds ratio

confint(fit.a2) # 95% CI
confint(fit.a2, level = 0.90) # 90% CI

z = summary(fit.a2)$coefficients / summary(fit.a2)$standard.errors
pval.a2 = (1 - pnorm(abs(z), 0, 1)) * 2
pval.a2 # p-value

logLik(fit.a2) # Log likelihood
null = update(fit.a2, . ~ 1, trace = FALSE)
1 - (logLik(fit.a2) / logLik(null)) # McFadden's R-squared

model.a2 = model.frame(ps ~ diff + sesq + diff:sesq, data = els.matched)
dim(model.a2) # Number of observations
table(model.a2$diff) # Number of the control and treated

# 4.2.2. Diff x SES after matching with controls ----

fit.a2c = multinom(ps ~ diff + sesq + diff:sesq 
                   + abil + gpa + track + female + race + twoprnt + sexp + disc + schtype + urbanicity + region, 
                   data = els.matched)
summary(fit.a2c)

exp(coef(fit.a2c)) # Odds ratio

summary(marginal_effects(fit.a2c))

confint(fit.a2c) # 95% CI
confint(fit.a2c, level = 0.90) # 90% CI

z = summary(fit.a2c)$coefficients / summary(fit.a2c)$standard.errors
pval.a2c = (1 - pnorm(abs(z), 0, 1)) * 2
pval.a2c # p-value

logLik(fit.a2c) # Log likelihood
null = update(fit.a2c, . ~ 1, trace = FALSE)
1 - (logLik(fit.a2c) / logLik(null)) # McFadden's R-squared

model.a2c = model.frame(ps ~ diff + sesq + diff:sesq 
                        + abil + gpa + track + female + race + twoprnt + sexp + disc + schtype + urbanicity + region, 
                        data = els.matched)
dim(model.a2c) # Number of observations
table(model.a2c$diff) # Number of the control and treated

predictorEffects(fit.a2c, ~ diff) # Estimated probability by treatment and SES

# 5. Graphs ----

lable = data.frame(old = c("abil", "ses", "female_female", "race_White", "race_Black", "race_Hispanic", "race_Asian", "race_Others",
                           "twoprnt_Two parent", "pexp_No PS", "pexp_Col", "pexp_Univ", "sexp_No PS", "sexp_Col", "sexp_Univ",
                           "disc_Never", "disc_Sometimes", "disc_Often", "schtype_Public", "schtype_Catholic", "schtype_Private", 
                           "urbanicity_Urban", "urbanicity_Suburban", "urbanicity_Rural", "region_NE", "region_MW", "region_S", "region_W"),
                   new = c("Ability", "SES", "Gender", "White", "Black", "Hispanic", "Asian", "Other races",
                           "Two-parent family", "No PS education (parent)", "Some college (parent)", "4yr university (parent)", 
                           "No PS education (student)", "Some college (student)", "4yr university (student)",
                           "No discussion with parents", "Some discussion with parents", "Frequent discussion with parents", 
                           "Public school", "Catholic school", "Private school", 
                           "Urban", "Suburban", "Rural", "Northeast", "Midwest", "South", "West"))

dev.new() # Covariate imbalance
love.plot(matched, binary = "std", thresholds = c(m = .05), drop.distance = TRUE, var.names = lable, 
          sample.names = c("Unmatched", "Matched"), colors = c("black", "orange"))

dev.new() # Distribution of the estimated probability
bal.plot(matched, var.name = "distance", which = "both", type = "histogram", mirror = TRUE, 
         sample.names = c("Unmatched", "Matched"), colors = c("#F46524", "#27C7BD"))

dev.new() # Diff x Ability after matching with controls
plot(predictorEffects(fit.a1c, ~ diff), axes = list(grid = TRUE, x = list(rug = FALSE)))

dev.new() # Diff x SES after matching with controls
plot(predictorEffects(fit.a2c, ~ diff), axes = list(grid = TRUE, x = list(rug = FALSE)))
