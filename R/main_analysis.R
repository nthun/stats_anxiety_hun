
# loading packages
library(lavaan) # for calculating Cronbach alpha
library(semTools)  # for calculating Cronbach alpha
library(tidyverse)


anxiety <- read_csv(file = "data/stat_anxiety_hun.csv")


## ---------------------------------------- 1. Preliminary analyses ---------------------------------

## ----------------------- 1.1. Internal consistency (alpha) calculation ------------------


# 1.1. Statistics Anxiety Rating Scale (STARS; Cruise et al., 1985) -------


## Internal consistency (alpha) calculation -------------------------
# calculating Cronbach alpha for STARS (item selection is based on Papousek et al., 2012)
stars_tnc <- '
test_and_class =~ stars_1 + stars_4 + stars_8 + stars_10 + 
                  stars_13 + stars_15 + stars_21 + stars_22' 

fit_stars_tnc <- cfa(stars_tnc, data = anxiety, estimator = 'MLR')
stars_tnc_rel <- as.data.frame(reliability(fit_stars_tnc))


stars_interpret <- '
interpretation =~ stars_2 + stars_5 + stars_6 + stars_7 + 
                  stars_9 + stars_11 + stars_12 + stars_14 +
                  stars_17 + stars_18 + stars_20' 

fit_stars_interpret <- cfa(stars_interpret, data = anxiety, estimator = 'MLR')
stars_interpret_rel <- as.data.frame(reliability(fit_stars_interpret))


stars_fear_of_asking <- '
fear_of_asking =~ stars_3 + stars_16 + stars_19 + stars_23' 

fit_stars_fear_of_asking <- cfa(stars_fear_of_asking, data = anxiety, estimator = 'MLR')
stars_fear_of_asking_rel <- as.data.frame(reliability(fit_stars_fear_of_asking))


reliabilities_stars <- cbind(stars_tnc_rel, stars_interpret_rel, stars_fear_of_asking_rel)
reliabilities_stars2 <- round(reliabilities_stars[1,], 3)



# STARS - The estimation of four alternative CFA solutions ----------------

# (1) one-factor solution
stars_onemodel <- '
stars =~ stars_1 + stars_4 + stars_8 + stars_10 + 
                  stars_13 + stars_15 + stars_21 + stars_22 +
         stars_2 + stars_5 + stars_6 + stars_7 + 
                  stars_9 + stars_11 + stars_12 + stars_14 +
                  stars_17 + stars_18 + stars_20 +
         stars_3 + stars_16 + stars_19 + stars_23' 

fit_stars_one <- cfa(stars_onemodel, data = anxiety, estimator = 'MLR')
summary(fit_stars_one, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_stars_one)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                              "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                              "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                              "srmr")], 3)
# Fit indices: (CFI = .701, TLI = .672, RMSEA = .119 [90% CI = 0.114 - 0.125]).



# (2) ﬁrst-order (including the 3 speciﬁc factors) solution
stars_first_order_model <- '
test_and_class =~ stars_1 + stars_4 + stars_8 + stars_10 + 
                  stars_13 + stars_15 + stars_21 + stars_22 

interpretation =~ stars_2 + stars_5 + stars_6 + stars_7 + 
                  stars_9 + stars_11 + stars_12 + stars_14 +
                  stars_17 + stars_18 + stars_20 

fear_of_asking =~ stars_3 + stars_16 + stars_19 + stars_23

test_and_class ~~ interpretation
test_and_class ~~ fear_of_asking
interpretation ~~ fear_of_asking
' 
# In the three-factor CFA solution, items were set to load only on their a priori speciﬁc factors, cross-loadings were set to be zero, and factors were allowed to correlate with one another.

fit_stars_first <- cfa(stars_first_order_model, data = anxiety, estimator = 'MLR')
summary(fit_stars_first, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_stars_first)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                   "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                   "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                   "srmr")], 3)
# Fit indices: (CFI = .906, TLI = .895, RMSEA = .067 [90% CI = 0.061 - 0.074]).



# (3)  second-order (including the 3 speciﬁc factors and a higher-order stat anxiety factor)
stars_second_order_model <- '
stat_anxiety =~ test_and_class + interpretation + fear_of_asking

test_and_class =~ stars_1 + stars_4 + stars_8 + stars_10 + 
                  stars_13 + stars_15 + stars_21 + stars_22 

interpretation =~ stars_2 + stars_5 + stars_6 + stars_7 + 
                  stars_9 + stars_11 + stars_12 + stars_14 +
                  stars_17 + stars_18 + stars_20 

fear_of_asking =~ stars_3 + stars_16 + stars_19 + stars_23' 
# In the second-order model, speciﬁcations were the same as in the ﬁrst-order model, but the correlations between the factors were replaced by a second-order global statistics anxiety factor.

fit_stars_second <- cfa(stars_second_order_model, data = anxiety, estimator = 'MLR')
summary(fit_stars_second, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_stars_second)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                     "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                     "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                     "srmr")], 3)
# Fit indices: (CFI = .906, TLI = .895, RMSEA = .067 [90% CI = 0.061 - 0.074]).



# (4) bifactor solution (including the 3 speciﬁc factors and a co-existing stat anxiety factor)
stars_bifactor_model <- '
test_and_class =~ stars_1 + stars_4 + stars_8 + stars_10 + 
                  stars_13 + stars_15 + stars_21 + stars_22 

interpretation =~ stars_2 + stars_5 + stars_6 + stars_7 + 
                  stars_9 + stars_11 + stars_12 + stars_14 +
                  stars_17 + stars_18 + stars_20 

fear_of_asking =~ stars_3 + stars_16 + stars_19 + stars_23

stat_anxiety =~ stars_1 + stars_2 + stars_3 + stars_4 + stars_5 + stars_6 + stars_7 + stars_8 + stars_9 + stars_10 + 
                stars_11 + stars_12 + stars_13 + stars_14 + 
                stars_15 + stars_16 + stars_17 + stars_18 + stars_19 + 
                stars_20 + stars_21 + stars_22 + stars_23

# stat_anxiety    ~~ 1*stat_anxiety
# test_and_class  ~~ 1*test_and_class
# interpretation  ~~ 1*interpretation
# fear_of_asking  ~~ 1*fear_of_asking

# stat_anxiety    ~~ 0*test_and_class
# stat_anxiety    ~~ 0*interpretation
# stat_anxiety    ~~ 0*fear_of_asking
# test_and_class  ~~ 0*interpretation
# test_and_class  ~~ 0*fear_of_asking
# interpretation  ~~ 0*fear_of_asking
'

# In bifactor-CFA solution, items were set to load on their respective S-factors as well as on the statistics anxiety G-factor, 
# and following typical bifactor speciﬁcations (Reise, 2012) factors were speciﬁed as orthogonal (i.e., not allowed to correlate with one another).

fit_stars_bifactor <- cfa(stars_bifactor_model, data = anxiety, estimator = 'MLR')
summary(fit_stars_bifactor, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_stars_bifactor)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                      "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                      "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                      "srmr")], 3)
# Fit indices: (CFI = .926, TLI = .911, RMSEA = .062 [90% CI = 0.055 - 0.068]).


lavInspect(fit_stars_bifactor, "cov.lv")



# In the comparison of ﬁrst-order and bifactor models, we followed 
# the guidelines of Morin et al. (2016a) and apart from goodness-of-ﬁt, 
# we also carefully examined the standardized parameter estimates 
# with an emphasis on the size of the correlations between the factors.


# Measurement Invariance Testing ------------------------------------------

# Testing measurement invariance for genders
anxiety2 <- anxiety %>%
    mutate(gender1 = case_when(!is.na(gender) & gender == "Female" ~ "1",
                              !is.na(gender) & gender == "Male" ~ "2",
                                TRUE ~ "0")) %>%
    filter(gender1 %in% c(1, 2))
table(anxiety2$gender1, useNA = "always")

config_gender <- cfa(stars_bifactor_model, data = anxiety2, group = "gender1", estimator = 'MLR')
weak_gender <- cfa(stars_bifactor_model, data = anxiety2, group = "gender1", estimator = 'MLR', group.equal = "loadings")
strong_gender <- cfa(stars_bifactor_model, data = anxiety2, group = "gender1", estimator = 'MLR', group.equal = c("loadings", "intercepts"))
strict_gender <- cfa(stars_bifactor_model, data = anxiety2, group = "gender1", estimator = 'MLR', group.equal = c("loadings", "intercepts", "residuals"))
latent_vc_gender <- cfa(stars_bifactor_model, data = anxiety2, group = "gender1", estimator = 'MLR', group.equal = c("loadings", "intercepts", "residuals", "lv.variances", "lv.covariances"))
mean_gender <- cfa(stars_bifactor_model, data = anxiety2, group = "gender1", estimator = 'MLR', group.equal = c("loadings", "intercepts", "residuals", "lv.variances", "lv.covariances", "means"))
anova(config_gender, weak_gender, strong_gender, strict_gender, latent_vc_gender, mean_gender)



fit.stats_gender <- rbind(fitmeasures(config_gender, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                          fitmeasures(weak_gender, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                          fitmeasures(strong_gender, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                          fitmeasures(strict_gender, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                          fitmeasures(latent_vc_gender, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                          fitmeasures(mean_gender, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")))
rownames(fit.stats_gender) <- c("config", "weak", "strong", "strict", "latent_varcov", "latent_means")
round(fit.stats_gender,3)



# Testing measurement invariance for universities
table(anxiety$university)


config_uni <- cfa(stars_bifactor_model, data = anxiety, group = "university", estimator = 'MLR')
weak_uni <- cfa(stars_bifactor_model, data = anxiety, group = "university", estimator = 'MLR', group.equal = "loadings")
strong_uni <- cfa(stars_bifactor_model, data = anxiety, group = "university", estimator = 'MLR', group.equal = c("loadings", "intercepts"))
strict_uni <- cfa(stars_bifactor_model, data = anxiety, group = "university", estimator = 'MLR', group.equal = c("loadings", "intercepts", "residuals"))
latent_vc_uni <- cfa(stars_bifactor_model, data = anxiety2, group = "university", estimator = 'MLR', group.equal = c("loadings", "intercepts", "residuals", "lv.variances", "lv.covariances"))
mean_uni <- cfa(stars_bifactor_model, data = anxiety2, group = "university", estimator = 'MLR', group.equal = c("loadings", "intercepts", "residuals", "lv.variances", "lv.covariances", "means"))
anova(config_uni, weak_uni, strong_uni, strict_uni, latent_vc_uni, mean_uni)


fit.stats_uni <- rbind(fitmeasures(config_uni, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                       fitmeasures(weak_uni, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                       fitmeasures(strong_uni, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                       fitmeasures(strict_uni, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                       fitmeasures(latent_vc_uni, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                       fitmeasures(mean_uni, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")))
rownames(fit.stats_uni) <- c("config", "weak", "strong", "strict", "latent_varcov", "latent_means")
round(fit.stats_uni,3)


# Testing measurement invariance for stat experience
table(anxiety2$stat_experience, useNA = "always")


config_stat_exp <- cfa(stars_bifactor_model, data = anxiety2, group = "stat_experience", estimator = 'MLR')
weak_stat_exp <- cfa(stars_bifactor_model, data = anxiety2, group = "stat_experience", estimator = 'MLR', group.equal = "loadings")
strong_stat_exp <- cfa(stars_bifactor_model, data = anxiety2, group = "stat_experience", estimator = 'MLR', group.equal = c("loadings", "intercepts"))
strict_stat_exp <- cfa(stars_bifactor_model, data = anxiety2, group = "stat_experience", estimator = 'MLR', group.equal = c("loadings", "intercepts", "residuals"))
latent_vc_exp <- cfa(stars_bifactor_model, data = anxiety2, group = "stat_experience", estimator = 'MLR', group.equal = c("loadings", "intercepts", "residuals", "lv.variances", "lv.covariances"))
mean_exp <- cfa(stars_bifactor_model, data = anxiety2, group = "stat_experience", estimator = 'MLR', group.equal = c("loadings", "intercepts", "residuals", "lv.variances", "lv.covariances", "means"))
anova(config_stat_exp, weak_stat_exp, strong_stat_exp, strict_stat_exp, latent_vc_exp, mean_exp)


fit.stats_stat <- rbind(fitmeasures(config_stat_exp, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                        fitmeasures(weak_stat_exp, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                        fitmeasures(strong_stat_exp, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                        fitmeasures(strict_stat_exp, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                        fitmeasures(latent_vc_exp, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                        fitmeasures(mean_exp, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")))
rownames(fit.stats_stat) <- c("config", "weak", "strong", "strict", "latent_varcov", "latent_means")
round(fit.stats_stat,3)




# 
# # STARS McDonald's omega calculation
# ## Standardized Parameter Estimates from the correlational m --------
# 
# # calculating composite reliability, McDonald's omega
# 
# # reliability check
# stars_corr_model_rel <- '
# # regressions
# 
# testnclass =~ t1*stars_1 + t2*stars_4 + t3*stars_8 + t4*stars_10 + 
#                   t5*stars_13 + t6*stars_15 + t7*stars_21 + t8*stars_22 
# 
# # Error Variance
# stars_1~~et1*stars_1
# stars_4~~et2*stars_4
# stars_8~~et3*stars_8
# stars_10~~et4*stars_10
# stars_13~~et5*stars_13
# stars_15~~et6*stars_15
# stars_21~~et7*stars_21
# stars_22~~et8*stars_22
# 
# #Reliability
# omega.test := 
# ((t1 + t2 + t3 + t4 + t5 + t6 + t7 + t8)^2) 
# / 
# ((t1 + t2 + t3 + t4 + t5 + t6 + t7 + t8)^2 + 
# (et1 + et2 + et3 + et4 + et5 + et6 + et7 + et8))
# 
# #Average Variance Extracted (AVE)
# ave_t := 
# ((t1^2) + (t2^2) + (t3^2) + (t4^2) + (t5^2) + (t6^2) + (t7^2) + (t8^2)) / 8
# 
# 
# 
# interpret =~ i1*stars_2 + i2*stars_5 + i3*stars_6 + i4*stars_7 + 
#              i5*stars_9 + i6*stars_11 + i7*stars_12 + i8*stars_14 +
#              i9*stars_17 + i10*stars_18 + i11*stars_20
# 
# # Error Variance
# stars_2~~ei1*stars_2
# stars_5~~ei2*stars_5
# stars_6~~ei3*stars_6
# stars_7~~ei4*stars_7
# stars_9~~ei5*stars_9
# stars_11~~ei6*stars_11
# stars_12~~ei7*stars_12
# stars_14~~ei8*stars_14
# stars_17~~ei9*stars_17
# stars_18~~ei10*stars_18
# stars_20~~ei11*stars_20
# 
# #Reliability
# omega.interpret := 
# ((i1 + i2 + i3 + i4 + i5 + i6 + i7 + i8 + i9 + i10 + i11)^2) 
# / 
# ((i1 + i2 + i3 + i4 + i5 + i6 + i7 + i8 + i9 + i10 + i11)^2 + 
# (ei1 + ei2 + ei3 + ei4 + ei5 + ei6 + ei7 + ei8 + ei9 + ei10 + ei11))
# 
# #Average Variance Extracted (AVE)
# ave_i := 
# ((i1^2) + (i2^2) + (i3^2) + (i4^2) + (i5^2) + (i6^2) + (i7^2) + (i8^2) + (i9^2) + (i10^2) + (i11^2)) / 11
# 
# 
# 
# fearask =~ f1*stars_3 + f2*stars_16 + f3*stars_19 + f4*stars_23 
# 
# # Error Variance
# stars_3~~ef1*stars_3
# stars_16~~ef2*stars_16
# stars_19~~ef3*stars_19
# stars_23~~ef4*stars_23
# 
# #Reliability
# omega.fear := 
# ((f1 + f2 + f3 + f4)^2) 
# / 
# ((f1 + f2 + f3 + f4)^2 + 
# (ef1 + ef2 + ef3 + ef4))
# 
# #Average Variance Extracted (AVE)
# ave_t := 
# ((f1^2) + (f2^2) + (f3^2) + (f4^2)) / 4
# 
# 
# 
# # correlations
# testnclass ~~ interpret
# testnclass ~~ fearask
# interpret ~~ fearask
# '
# 
# fit_stars <- cfa(stars_corr_model_rel, data = anxiety, estimator = 'MLR', std.lv = TRUE)
# summary(fit_stars, fit.measures = TRUE, standardized = TRUE, rsquare=T)
# 
# 
# # Fit indices
# round(fitMeasures(fit_stars)[c("chisq.scaled", "df.scaled", "cfi.scaled", "tli.scaled",
#                                "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr", "aic", "bic")], 3)
# round(fitMeasures(fit_stars)["pvalue.scaled"], 3)







# 1.2. State-Trait Inventory for Cog. and Som. Anxiety (STICSA) -----------

## Internal consistency (alpha) calculation -------------------------
# calculating Cronbach alpha for STICSA (item selection is based on Ree et al., 2008)
sticsa_som <- '
somatic =~ sticsa_1 + sticsa_2 + sticsa_6 + sticsa_7 + sticsa_8 + 
           sticsa_12 + sticsa_14 + sticsa_15 + sticsa_18 + sticsa_20 + sticsa_21' 

fit_sticsa_som <- cfa(sticsa_som, data = anxiety, estimator = 'MLR')
sticsa_som_rel <- as.data.frame(reliability(fit_sticsa_som))


sticsa_cog <- '
cognitive =~ sticsa_3 + sticsa_4 + sticsa_5 + sticsa_9 + sticsa_10 + 
             sticsa_11 + sticsa_13 + sticsa_16 + sticsa_17 + sticsa_19' 

fit_sticsa_cog <- cfa(sticsa_cog, data = anxiety, estimator = 'MLR')
sticsa_cog_rel <- as.data.frame(reliability(fit_sticsa_cog))


reliabilities_sticsa <- cbind(sticsa_som_rel, sticsa_cog_rel)
reliabilities_sticsa2 <- round(reliabilities_sticsa[1,], 3)




# STICSA - estimation of one-factor and first-order CFA solutions ---------

# (1) one-factor solution
sticsa_onemodel <- '
sticsa =~ sticsa_1 + sticsa_2 + sticsa_6 + sticsa_7 + sticsa_8 + 
           sticsa_12 + sticsa_14 + sticsa_15 + sticsa_18 + sticsa_20 + sticsa_21 +
         sticsa_3 + sticsa_4 + sticsa_5 + sticsa_9 + sticsa_10 + 
             sticsa_11 + sticsa_13 + sticsa_16 + sticsa_17 + sticsa_19' 

fit_sticsa_one <- cfa(sticsa_onemodel, data = anxiety, estimator = 'MLR')
summary(fit_sticsa_one, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_sticsa_one)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                   "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                   "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                   "srmr")], 3)
# Fit indices: (CFI = .845, TLI = .828, RMSEA = .074 [90% CI = 0.068 - 0.081]).



# (2) ﬁrst-order (including the 2 speciﬁc factors) solution
sticsa_first_order_model <- '
somatic =~ sticsa_1 + sticsa_2 + sticsa_6 + sticsa_7 + sticsa_8 + 
           sticsa_12 + sticsa_14 + sticsa_15 + sticsa_18 + sticsa_20 + sticsa_21 

cognitive =~ sticsa_3 + sticsa_4 + sticsa_5 + sticsa_9 + sticsa_10 + 
             sticsa_11 + sticsa_13 + sticsa_16 + sticsa_17 + sticsa_19

somatic ~~ cognitive
' 
# In the three-factor CFA solution, items were set to load only on their a priori speciﬁc factors, cross-loadings were set to be zero, and factors were allowed to correlate with one another.

fit_sticsa_first <- cfa(sticsa_first_order_model, data = anxiety, estimator = 'MLR')
summary(fit_sticsa_first, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_sticsa_first)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                     "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                     "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                     "srmr")], 3)
# Fit indices: (CFI = .916, TLI = .906, RMSEA = .055 [90% CI = 0.048 - 0.062]).





# 
# ## Standardized Parameter Estimates from the correlational m --------
# 
# # calculating composite reliability, McDonald's omega
# # reliability check
# sticsa_corr_model_rel <- '
# # regressions
# 
# somatic =~ s1*sticsa_1 + s2*sticsa_2 + s3*sticsa_6 + s4*sticsa_7 + s5*sticsa_8 + 
#            s6*sticsa_12 + s7*sticsa_14 + s8*sticsa_15 + s9*sticsa_18 + s10*sticsa_20 + s11*sticsa_21
# 
# # Error Variance
# sticsa_1~~es1*sticsa_1
# sticsa_2~~es2*sticsa_2
# sticsa_6~~es3*sticsa_6
# sticsa_7~~es4*sticsa_7
# sticsa_8~~es5*sticsa_8
# sticsa_12~~es6*sticsa_12
# sticsa_14~~es7*sticsa_14
# sticsa_15~~es8*sticsa_15
# sticsa_18~~es9*sticsa_18
# sticsa_20~~es10*sticsa_20
# sticsa_21~~es11*sticsa_21
# 
# 
# #Reliability
# omega.som := 
# ((s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9 + s10 + s11)^2) 
# / 
# ((s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9 + s10 + s11)^2 + 
# (es1 + es2 + es3 + es4 + es5 + es6 + es7 + es8 + es9 + es10 + es11))
# 
# #Average Variance Extracted (AVE)
# ave_som := 
# ((s1^2) + (s2^2) + (s3^2) + (s4^2) + (s5^2) + (s6^2) + (s7^2) + (s8^2) + (s9^2) + (s10^2) + (s11^2)) / 11
# 
# 
# 
# cognitive =~ c1*sticsa_3 + c2*sticsa_4 + c3*sticsa_5 + c4*sticsa_9 + c5*sticsa_10 + 
#              c6*sticsa_11 + c7*sticsa_13 + c8*sticsa_16 + c9*sticsa_17 + c10*sticsa_19 
# 
# 
# # Error Variance
# sticsa_3~~ec1*sticsa_3
# sticsa_4~~ec2*sticsa_4
# sticsa_5~~ec3*sticsa_5
# sticsa_9~~ec4*sticsa_9
# sticsa_10~~ec5*sticsa_10
# sticsa_11~~ec6*sticsa_11
# sticsa_13~~ec7*sticsa_13
# sticsa_16~~ec8*sticsa_16
# sticsa_17~~ec9*sticsa_17
# sticsa_19~~ec10*sticsa_19
# 
# #Reliability
# omega.cog := 
# ((c1 + c2 + c3 + c4 + c5 + c6 + c7 + c8 + c9 + c10)^2) 
# / 
# ((c1 + c2 + c3 + c4 + c5 + c6 + c7 + c8 + c9 + c10)^2 + 
# (ec1 + ec2 + ec3 + ec4 + ec5 + ec6 + ec7 + ec8 + ec9 + ec10))
# 
# #Average Variance Extracted (AVE)
# ave_cog := 
# ((c1^2) + (c2^2) + (c3^2) + (c4^2) + (c5^2) + (c6^2) + (c7^2) + (c8^2) + (c9^2) + (c10^2)) / 10
# 
# # correlations
# somatic ~~ cognitive
# '
# 
# fit_sticsa <- cfa(sticsa_corr_model_rel, data = anxiety, estimator = 'MLR', std.lv = TRUE)
# summary(fit_sticsa, fit.measures = TRUE, standardized = TRUE, rsquare=T)
# 
# 
# # Fit indices
# round(fitMeasures(fit_sticsa)[c("chisq.scaled", "df.scaled", "cfi.scaled", "tli.scaled",
#                                "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr", "aic", "bic")], 3)
# round(fitMeasures(fit_sticsa)["pvalue.scaled"], 3)




# 1.3. Revised Test Anxiety Scale (R-TAS) ---------------------------------


## Internal consistency (alpha) calculation -------------------------
# calculating Cronbach alpha for R-TAS (item selection is based on Benson & El‐Zahhar, 1994)
rtas_wor <- '
worry =~ rtas_1 + rtas_2 + rtas_3 + rtas_8 + rtas_11 + rtas_24' 

fit_rtas_wor <- cfa(rtas_wor, data = anxiety, estimator = 'MLR')
rtas_wor_rel <- as.data.frame(reliability(fit_rtas_wor))


rtas_ten <- '
tension =~ rtas_4 + rtas_5 + rtas_6 + rtas_13 + rtas_25' 

fit_rtas_ten <- cfa(rtas_ten, data = anxiety, estimator = 'MLR')
rtas_ten_rel <- as.data.frame(reliability(fit_rtas_ten))


rtas_irr <- '
irrelevant =~ rtas_7 + rtas_9 + rtas_15 + rtas_16' 

fit_rtas_irr <- cfa(rtas_irr, data = anxiety, estimator = 'MLR')
rtas_irr_rel <- as.data.frame(reliability(fit_rtas_irr))


rtas_bod <- '
bodily =~ rtas_10 + rtas_17 + rtas_18 + rtas_21 + rtas_22' 

fit_rtas_bod <- cfa(rtas_bod, data = anxiety, estimator = 'MLR')
rtas_bod_rel <- as.data.frame(reliability(fit_rtas_bod))



reliabilities_rtas <- cbind(rtas_wor_rel, rtas_ten_rel, rtas_irr_rel, rtas_bod_rel)
reliabilities_rtas2 <- round(reliabilities_rtas[1,], 3)



# R-TAS - The estimation of alternative CFA solutions ---------------------

# (1) one-factor solution
rtas_onemodel <- '
rtas =~ rtas_1 + rtas_2 + rtas_3 + rtas_8 + rtas_11 + rtas_24 +
        rtas_4 + rtas_5 + rtas_6 + rtas_13 + rtas_25 + 
        rtas_7 + rtas_9 + rtas_15 + rtas_16 +
        rtas_10 + rtas_17 + rtas_18 + rtas_21 + rtas_22' 

fit_rtas_one <- cfa(rtas_onemodel, data = anxiety, estimator = 'MLR')
summary(fit_rtas_one, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_rtas_one)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                   "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                   "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                   "srmr")], 3)
# Fit indices: (CFI = .723, TLI = .691, RMSEA = .109 [90% CI = 0.102 - 0.115]).



# (2) ﬁrst-order (including the 4 speciﬁc factors) solution
rtas_first_order_model <- '
worry =~ rtas_1 + rtas_2 + rtas_3 + rtas_8 + rtas_11 + rtas_24 

tension =~ rtas_4 + rtas_5 + rtas_6 + rtas_13 + rtas_25 

irrelevant =~ rtas_7 + rtas_9 + rtas_15 + rtas_16

bodily =~ rtas_10 + rtas_17 + rtas_18 + rtas_21 + rtas_22

worry ~~ tension
worry ~~ irrelevant
worry ~~ bodily
tension ~~ irrelevant
tension ~~ bodily
irrelevant ~~ bodily
' 
# In the three-factor CFA solution, items were set to load only on their a priori speciﬁc factors, cross-loadings were set to be zero, and factors were allowed to correlate with one another.

fit_rtas_first <- cfa(rtas_first_order_model, data = anxiety, estimator = 'MLR')
summary(fit_rtas_first, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_rtas_first)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                     "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                     "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                     "srmr")], 3)
# Fit indices: (CFI = .960, TLI = .953, RMSEA = .042 [90% CI = 0.034 - 0.051]).



# (3)  second-order (including the 4 speciﬁc factors and a higher-order stat anxiety factor)
rtas_second_order_model <- '
stat_anxiety =~ worry + tension + irrelevant + bodily

worry =~ rtas_1 + rtas_2 + rtas_3 + rtas_8 + rtas_11 + rtas_24 

tension =~ rtas_4 + rtas_5 + rtas_6 + rtas_13 + rtas_25 

irrelevant =~ rtas_7 + rtas_9 + rtas_15 + rtas_16

bodily =~ rtas_10 + rtas_17 + rtas_18 + rtas_21 + rtas_22' 
# In the second-order model, speciﬁcations were the same as in the ﬁrst-order model, but the correlations between the factors were replaced by a second-order global statistics anxiety factor.

fit_rtas_second <- cfa(rtas_second_order_model, data = anxiety, estimator = 'MLR')
summary(fit_rtas_second, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_rtas_second)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                      "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                      "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                      "srmr")], 3)
# Fit indices: (CFI = .939, TLI = .930, RMSEA = .052 [90% CI = 0.044 - 0.060]).







# 
# ## Standardized Parameter Estimates from the correlational m --------
# # calculating composite reliability, McDonald's omega
# 
# # reliability check
# rtas_corr_model_rel <- '
# # regressions
# 
# worry =~ w1*rtas_1 + w2*rtas_2 + w3*rtas_3 + w4*rtas_8 + w5*rtas_11 + w6*rtas_24
# 
# # Error Variance
# rtas_1~~ew1*rtas_1
# rtas_2~~ew2*rtas_2
# rtas_3~~ew3*rtas_3
# rtas_8~~ew4*rtas_8
# rtas_11~~ew5*rtas_11
# rtas_24~~ew6*rtas_24
# 
# #Reliability
# omega.wor := 
# ((w1 + w2 + w3 + w4 + w5 + w6)^2) 
# / 
# ((w1 + w2 + w3 + w4 + w5 + w6)^2 + 
# (ew1 + ew2 + ew3 + ew4 + ew5 + ew6))
# 
# #Average Variance Extracted (AVE)
# ave_wor := 
# ((w1^2) + (w2^2) + (w3^2) + (w4^2) + (w5^2) + (w6^2)) / 6
# 
# 
# 
# tension =~ t1*rtas_4 + t2*rtas_5 + t3*rtas_6 + t4*rtas_13 + t5*rtas_25
# 
# # Error Variance
# rtas_4~~et1*rtas_4
# rtas_5~~et2*rtas_5
# rtas_6~~et3*rtas_6
# rtas_13~~et4*rtas_13
# rtas_25~~et5*rtas_25
# 
# #Reliability
# omega.ten := 
# ((t1 + t2 + t3 + t4 + t5)^2) 
# / 
# ((t1 + t2 + t3 + t4 + t5)^2 + 
# (et1 + et2 + et3 + et4 + et5))
# 
# #Average Variance Extracted (AVE)
# ave_ten := 
# ((t1^2) + (t2^2) + (t3^2) + (t4^2) + (t5^2)) / 5
# 
# 
# 
# irrelevant =~ i1*rtas_7 + i2*rtas_9 + i3*rtas_15 + i4*rtas_16
# 
# # Error Variance
# rtas_7~~ei1*rtas_7
# rtas_9~~ei2*rtas_9
# rtas_15~~ei3*rtas_15
# rtas_16~~ei4*rtas_16
# 
# #Reliability
# omega.irr := 
# ((i1 + i2 + i3 + i4)^2) 
# / 
# ((i1 + i2 + i3 + i4)^2 + 
# (ei1 + ei2 + ei3 + ei4))
# 
# #Average Variance Extracted (AVE)
# ave_irr := 
# ((i1^2) + (i2^2) + (i3^2) + (i4^2)) / 4
# 
# 
# 
# bodily =~ b1*rtas_10 + b2*rtas_17 + b3*rtas_18 + b4*rtas_21 + b5*rtas_22
# 
# # Error Variance
# rtas_10~~eb1*rtas_10
# rtas_17~~eb2*rtas_17
# rtas_18~~eb3*rtas_18
# rtas_21~~eb4*rtas_21
# rtas_22~~eb5*rtas_22
# 
# #Reliability
# omega.bod := 
# ((b1 + b2 + b3 + b4 + b5)^2) 
# / 
# ((b1 + b2 + b3 + b4 + b5)^2 + 
# (eb1 + eb2 + eb3 + eb4 + eb5))
# 
# #Average Variance Extracted (AVE)
# ave_bod := 
# ((b1^2) + (b2^2) + (b3^2) + (b4^2) + (b5^2)) / 5
# 
# 
# 
# # correlations
# worry ~~ tension
# worry ~~ irrelevant
# worry ~~ bodily
# tension ~~ irrelevant
# tension ~~ bodily
# irrelevant ~~ bodily
# '
# 
# fit_rtas <- cfa(rtas_corr_model_rel, data = anxiety, estimator = 'MLR', std.lv = TRUE)
# summary(fit_rtas, fit.measures = TRUE, standardized = TRUE, rsquare=T)
# 
# 
# # Fit indices
# round(fitMeasures(fit_rtas)[c("chisq.scaled", "df.scaled", "cfi.scaled", "tli.scaled",
#                                "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr", "aic", "bic")], 3)
# round(fitMeasures(fit_rtas)["pvalue.scaled"], 3)




# 1.4. Brief Fear of Negative Eval. Scale-Straightforward (BFNE-S) --------


## Internal consistency (alpha) calculation ----------------------------


# calculating Cronbach alpha for BFNE-S (item selection is based on Carleton et al., 2011)
bfnes <- '
bfnes =~ bfne_1 + bfne_2 + bfne_3 + bfne_4 + bfne_5 + bfne_6 + bfne_7 + bfne_8' 

fit_bfnes <- cfa(bfnes, data = anxiety, estimator = 'MLR')
bfnes_rel <- as.data.frame(reliability(fit_bfnes))

# reliabilities_bfnes <- cbind(sticsa_som_rel, sticsa_cog_rel)
reliabilities_bfnes2 <- round(bfnes_rel[1,], 3)


fit_bfnes_one <- cfa(bfnes, data = anxiety, estimator = 'MLR')
summary(fit_bfnes_one, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_bfnes_one)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                  "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                  "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                  "srmr")], 3)
# Fit indices: (CFI = .969, TLI = .957, RMSEA = .090 [90% CI = 0.072 - 0.109]).





## Standardized Parameter Estimates from the correlational ----------

# calculating composite reliability, McDonald's omega
# reliability check
bfnes_corr_model_rel <- '
# regressions

bfnes =~ b1*bfne_1 + b2*bfne_2 + b3*bfne_3 + b4*bfne_4 + b5*bfne_5 + b6*bfne_6 + b7*bfne_7 + b8*bfne_8

# Error Variance
bfne_1~~eb1*bfne_1
bfne_2~~eb2*bfne_2
bfne_3~~eb3*bfne_3
bfne_4~~eb4*bfne_4
bfne_5~~eb5*bfne_5
bfne_6~~eb6*bfne_6
bfne_7~~eb7*bfne_7
bfne_8~~eb8*bfne_8

#Reliability
omega.bfnes := 
((b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8)^2) 
/ 
((b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8)^2 + 
(eb1 + eb2 + eb3 + eb4 + eb5 + eb6 + eb7 + eb8))

#Average Variance Extracted (AVE)
ave_bfnes := 
((b1^2) + (b2^2) + (b3^2) + (b4^2) + (b5^2) + (b6^2) + (b7^2) + (b8^2)) / 8
'

fit_bfnes <- cfa(bfnes_corr_model_rel, data = anxiety, estimator = 'MLR', std.lv = TRUE)
summary(fit_bfnes, fit.measures = TRUE, standardized = TRUE, rsquare=T)


# Fit indices
round(fitMeasures(fit_bfnes)[c("chisq.scaled", "df.scaled", "cfi.scaled", "tli.scaled",
                                "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr", "aic", "bic")], 3)
round(fitMeasures(fit_bfnes)["pvalue.scaled"], 3)
