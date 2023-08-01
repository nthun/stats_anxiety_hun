
# loading packages
library(lavaan) # for calculating Cronbach alpha
library(semTools)  # for calculating Cronbach alpha and measurement invariance
library(tidyverse)


anxiety <- read_csv(file = "data/stat_anxiety_hun.csv")


## ---------------------------------------- Preliminary analyses ---------------------------------

# 1. Internal consistency (alpha) calculation ---------------------------

# 1.1 State-Trait Inventory for Cog. and Som. Anxiety (STICSA) -----------
# calculating Cronbach alpha for STICSA (item selection is based on Ree et al., 2008)
sticsa_som <- '
somatic =~ sticsa_1 + sticsa_2 + sticsa_6 + sticsa_7 + sticsa_8 + 
           sticsa_12 + sticsa_14 + sticsa_15 + sticsa_18 + sticsa_20 + sticsa_21' 

fit_sticsa_som <- cfa(sticsa_som, data = anxiety, estimator = 'WLSMV')
sticsa_som_rel <- as.data.frame(reliability(fit_sticsa_som))


sticsa_cog <- '
cognitive =~ sticsa_3 + sticsa_4 + sticsa_5 + sticsa_9 + sticsa_10 + 
             sticsa_11 + sticsa_13 + sticsa_16 + sticsa_17 + sticsa_19' 

fit_sticsa_cog <- cfa(sticsa_cog, data = anxiety, estimator = 'WLSMV')
sticsa_cog_rel <- as.data.frame(reliability(fit_sticsa_cog))


reliabilities_sticsa <- cbind(sticsa_som_rel, sticsa_cog_rel)
reliabilities_sticsa2 <- round(reliabilities_sticsa[1,], 3)



# 1.2 Revised Test Anxiety Scale (R-TAS) ---------------------------------

# calculating Cronbach alpha for R-TAS (item selection is based on Benson & El‐Zahhar, 1994)
rtas_wor <- '
worry =~ rtas_1 + rtas_2 + rtas_3 + rtas_8 + rtas_11 + rtas_24' 

fit_rtas_wor <- cfa(rtas_wor, data = anxiety, estimator = 'WLSMV')
rtas_wor_rel <- as.data.frame(reliability(fit_rtas_wor))


rtas_ten <- '
tension =~ rtas_4 + rtas_5 + rtas_6 + rtas_13 + rtas_25' 

fit_rtas_ten <- cfa(rtas_ten, data = anxiety, estimator = 'WLSMV')
rtas_ten_rel <- as.data.frame(reliability(fit_rtas_ten))


rtas_irr <- '
irrelevant =~ rtas_7 + rtas_9 + rtas_15 + rtas_16' 

fit_rtas_irr <- cfa(rtas_irr, data = anxiety, estimator = 'WLSMV')
rtas_irr_rel <- as.data.frame(reliability(fit_rtas_irr))


rtas_bod <- '
bodily =~ rtas_10 + rtas_17 + rtas_18 + rtas_21 + rtas_22' 

fit_rtas_bod <- cfa(rtas_bod, data = anxiety, estimator = 'WLSMV')
rtas_bod_rel <- as.data.frame(reliability(fit_rtas_bod))



reliabilities_rtas <- cbind(rtas_wor_rel, rtas_ten_rel, rtas_irr_rel, rtas_bod_rel)
reliabilities_rtas2 <- round(reliabilities_rtas[1,], 3)



# 1.3 Brief Fear of Negative Eval. Scale-Straightforward (BFNE-S) --------
# calculating Cronbach alpha for BFNE-S (item selection is based on Carleton et al., 2011)
bfnes <- '
bfnes =~ bfne_1 + bfne_2 + bfne_3 + bfne_4 + bfne_5 + bfne_6 + bfne_7 + bfne_8' 

fit_bfnes <- cfa(bfnes, data = anxiety, estimator = 'WLSMV')
bfnes_rel <- as.data.frame(reliability(fit_bfnes))

# reliabilities_bfnes <- cbind(sticsa_som_rel, sticsa_cog_rel)
reliabilities_bfnes2 <- round(bfnes_rel[1,], 3)



# 1.4 Statistics Anxiety Rating Scale (STARS; Cruise et al., 1985) --------
# calculating Cronbach alpha for STARS (item selection is based on Papousek et al., 2012)
stars_tnc <- '
test_and_class =~ stars_1 + stars_4 + stars_8 + stars_10 + 
                  stars_13 + stars_15 + stars_21 + stars_22' 

fit_stars_tnc <- cfa(stars_tnc, data = anxiety, estimator = 'WLSMV')
stars_tnc_rel <- as.data.frame(reliability(fit_stars_tnc))


stars_interpret <- '
interpretation =~ stars_2 + stars_5 + stars_6 + stars_7 + 
                  stars_9 + stars_11 + stars_12 + stars_14 +
                  stars_17 + stars_18 + stars_20' 

fit_stars_interpret <- cfa(stars_interpret, data = anxiety, estimator = 'WLSMV')
stars_interpret_rel <- as.data.frame(reliability(fit_stars_interpret))


stars_fear_of_asking <- '
fear_of_asking =~ stars_3 + stars_16 + stars_19 + stars_23' 

fit_stars_fear_of_asking <- cfa(stars_fear_of_asking, data = anxiety, estimator = 'WLSMV')
stars_fear_of_asking_rel <- as.data.frame(reliability(fit_stars_fear_of_asking))


reliabilities_stars <- cbind(stars_tnc_rel, stars_interpret_rel, stars_fear_of_asking_rel)
reliabilities_stars2 <- round(reliabilities_stars[1,], 3)


# Cronbach alphas for STICSA, R-TAS, BFNE-S, STARS
reliabilities_sticsa2
reliabilities_rtas2
reliabilities_bfnes2
reliabilities_stars2



# 2. model fit indices and composite reliability, McDonald's omega ------------------

# 2.1 State-Trait Inventory for Cog. and Som. Anxiety (STICSA) -----------

# 2.1.1 STICSA - estimation of first-order CFA solution ---------
# ﬁrst-order (including the 2 speciﬁc factors) solution
sticsa_first_order_model <- '
somatic =~ sticsa_1 + sticsa_2 + sticsa_6 + sticsa_7 + sticsa_8 + 
           sticsa_12 + sticsa_14 + sticsa_15 + sticsa_18 + sticsa_20 + sticsa_21 

cognitive =~ sticsa_3 + sticsa_4 + sticsa_5 + sticsa_9 + sticsa_10 + 
             sticsa_11 + sticsa_13 + sticsa_16 + sticsa_17 + sticsa_19

somatic ~~ cognitive
' 
# In the two-factor CFA solution, items were set to load only on their a priori speciﬁc factors, cross-loadings were set to be zero, and factors were allowed to correlate with one another.

fit_sticsa_first <- cfa(sticsa_first_order_model, data = anxiety, estimator = 'WLSMV')
summary(fit_sticsa_first, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_sticsa_first)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                     "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                     "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                     "srmr")], 3)
# Fit indices: (chisq.scaled = 328.675* (188), CFI = .951, TLI = .946, RMSEA = .045 [90% CI = 0.037 - 0.053]).



# 2.1.2 STICSA composite reliability --------------------------------------------
# calculating composite reliability, McDonald's omega

sticsa_corr_model_rel <- '
# regressions

somatic =~ s1*sticsa_1 + s2*sticsa_2 + s3*sticsa_6 + s4*sticsa_7 + s5*sticsa_8 +
           s6*sticsa_12 + s7*sticsa_14 + s8*sticsa_15 + s9*sticsa_18 + s10*sticsa_20 + s11*sticsa_21

# Error Variance
sticsa_1~~es1*sticsa_1
sticsa_2~~es2*sticsa_2
sticsa_6~~es3*sticsa_6
sticsa_7~~es4*sticsa_7
sticsa_8~~es5*sticsa_8
sticsa_12~~es6*sticsa_12
sticsa_14~~es7*sticsa_14
sticsa_15~~es8*sticsa_15
sticsa_18~~es9*sticsa_18
sticsa_20~~es10*sticsa_20
sticsa_21~~es11*sticsa_21


#Reliability
omega.som :=
((s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9 + s10 + s11)^2)
/
((s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9 + s10 + s11)^2 +
(es1 + es2 + es3 + es4 + es5 + es6 + es7 + es8 + es9 + es10 + es11))



cognitive =~ c1*sticsa_3 + c2*sticsa_4 + c3*sticsa_5 + c4*sticsa_9 + c5*sticsa_10 +
             c6*sticsa_11 + c7*sticsa_13 + c8*sticsa_16 + c9*sticsa_17 + c10*sticsa_19


# Error Variance
sticsa_3~~ec1*sticsa_3
sticsa_4~~ec2*sticsa_4
sticsa_5~~ec3*sticsa_5
sticsa_9~~ec4*sticsa_9
sticsa_10~~ec5*sticsa_10
sticsa_11~~ec6*sticsa_11
sticsa_13~~ec7*sticsa_13
sticsa_16~~ec8*sticsa_16
sticsa_17~~ec9*sticsa_17
sticsa_19~~ec10*sticsa_19

#Reliability
omega.cog :=
((c1 + c2 + c3 + c4 + c5 + c6 + c7 + c8 + c9 + c10)^2)
/
((c1 + c2 + c3 + c4 + c5 + c6 + c7 + c8 + c9 + c10)^2 +
(ec1 + ec2 + ec3 + ec4 + ec5 + ec6 + ec7 + ec8 + ec9 + ec10))


# correlations
somatic ~~ cognitive
'

fit_sticsa <- cfa(sticsa_corr_model_rel, data = anxiety, estimator = 'WLSMV', std.lv = TRUE)
summary(fit_sticsa, fit.measures = TRUE, standardized = TRUE, rsquare=T)
# omega.som = 0.862
# omega.cog = 0.874




# 2.2. Revised Test Anxiety Scale (R-TAS) ---------------------------------

# 2.2.1 R-TAS - estimation of first-order CFA solution --------------------
# ﬁrst-order (including the 4 speciﬁc factors) solution
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
# In the four-factor CFA solution, items were set to load only on their a priori speciﬁc factors, cross-loadings were set to be zero, and factors were allowed to correlate with one another.

fit_rtas_first <- cfa(rtas_first_order_model, data = anxiety, estimator = 'WLSMV')
summary(fit_rtas_first, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_rtas_first)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                     "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                     "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                     "srmr")], 3)
# Fit indices: (chisq.scaled = 246.029* (164), CFI = .963, TLI = .957, RMSEA = .037 [90% CI = 0.027 - 0.046]).



# 2.2.2 R-TAS composite reliability ---------------------------------------
# calculating composite reliability, McDonald's omega

# reliability check
rtas_corr_model_rel <- '
# regressions

worry =~ w1*rtas_1 + w2*rtas_2 + w3*rtas_3 + w4*rtas_8 + w5*rtas_11 + w6*rtas_24

# Error Variance
rtas_1~~ew1*rtas_1
rtas_2~~ew2*rtas_2
rtas_3~~ew3*rtas_3
rtas_8~~ew4*rtas_8
rtas_11~~ew5*rtas_11
rtas_24~~ew6*rtas_24

#Reliability
omega.wor :=
((w1 + w2 + w3 + w4 + w5 + w6)^2)
/
((w1 + w2 + w3 + w4 + w5 + w6)^2 +
(ew1 + ew2 + ew3 + ew4 + ew5 + ew6))



tension =~ t1*rtas_4 + t2*rtas_5 + t3*rtas_6 + t4*rtas_13 + t5*rtas_25

# Error Variance
rtas_4~~et1*rtas_4
rtas_5~~et2*rtas_5
rtas_6~~et3*rtas_6
rtas_13~~et4*rtas_13
rtas_25~~et5*rtas_25

#Reliability
omega.ten :=
((t1 + t2 + t3 + t4 + t5)^2)
/
((t1 + t2 + t3 + t4 + t5)^2 +
(et1 + et2 + et3 + et4 + et5))



irrelevant =~ i1*rtas_7 + i2*rtas_9 + i3*rtas_15 + i4*rtas_16

# Error Variance
rtas_7~~ei1*rtas_7
rtas_9~~ei2*rtas_9
rtas_15~~ei3*rtas_15
rtas_16~~ei4*rtas_16

#Reliability
omega.irr :=
((i1 + i2 + i3 + i4)^2)
/
((i1 + i2 + i3 + i4)^2 +
(ei1 + ei2 + ei3 + ei4))



bodily =~ b1*rtas_10 + b2*rtas_17 + b3*rtas_18 + b4*rtas_21 + b5*rtas_22

# Error Variance
rtas_10~~eb1*rtas_10
rtas_17~~eb2*rtas_17
rtas_18~~eb3*rtas_18
rtas_21~~eb4*rtas_21
rtas_22~~eb5*rtas_22

#Reliability
omega.bod :=
((b1 + b2 + b3 + b4 + b5)^2)
/
((b1 + b2 + b3 + b4 + b5)^2 +
(eb1 + eb2 + eb3 + eb4 + eb5))



# correlations
worry ~~ tension
worry ~~ irrelevant
worry ~~ bodily
tension ~~ irrelevant
tension ~~ bodily
irrelevant ~~ bodily
'

fit_rtas <- cfa(rtas_corr_model_rel, data = anxiety, estimator = 'WLSMV', std.lv = TRUE)
summary(fit_rtas, fit.measures = TRUE, standardized = TRUE, rsquare=T)
# omega.wor = 0.835
# omega.ten = 0.876
# omega.irr = 0.776
# omega.bod = 0.750



# 2.3. Brief Fear of Negative Eval. Scale-Straightforward (BFNE-S) --------

# 2.3.1 BFNE-S - estimation of one-factor CFA solution ---------
bfnes <- '
bfnes =~ bfne_1 + bfne_2 + bfne_3 + bfne_4 + bfne_5 + bfne_6 + bfne_7 + bfne_8' 

fit_bfnes_one <- cfa(bfnes, data = anxiety, estimator = 'WLSMV')
summary(fit_bfnes_one, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_bfnes_one)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                  "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                  "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                  "srmr")], 3)
# Fit indices: (chisq.scaled = 52.446* (20), CFI = .983, TLI = .976, RMSEA = .066 [90% CI = 0.045 - 0.089]).



# 2.3.2 BFNE-S composite reliability ---------------------------------------
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
'

fit_bfnes <- cfa(bfnes_corr_model_rel, data = anxiety, estimator = 'WLSMV', std.lv = TRUE)
summary(fit_bfnes, fit.measures = TRUE, standardized = TRUE, rsquare=T)
# omega.bfnes = 0.951



# 2.4 Statistics Anxiety Rating Scale (STARS; Cruise et al., 1985) -------

# 2.4.1 STARS - The estimation of four alternative CFA solutions ----------------

# (1) one-factor solution -------------------------------------------------
stars_onemodel <- '
stars =~ stars_1 + stars_4 + stars_8 + stars_10 + 
                  stars_13 + stars_15 + stars_21 + stars_22 +
         stars_2 + stars_5 + stars_6 + stars_7 + 
                  stars_9 + stars_11 + stars_12 + stars_14 +
                  stars_17 + stars_18 + stars_20 +
         stars_3 + stars_16 + stars_19 + stars_23' 

fit_stars_one <- cfa(stars_onemodel, data = anxiety, estimator = 'WLSMV')
summary(fit_stars_one, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_stars_one)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                   "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                   "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                   "srmr")], 3)
# Fit indices: (chisq.scaled = 1065.459* (230), CFI = .745, TLI = .719, RMSEA = .099 [90% CI = 0.093 - 0.105]).



# (2) ﬁrst-order solution -------------------------------------------------
# (including the 3 speciﬁc factors)

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

fit_stars_first <- cfa(stars_first_order_model, data = anxiety, estimator = 'WLSMV')
summary(fit_stars_first, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_stars_first)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                     "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                     "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                     "srmr")], 3)
# Fit indices: (chisq.scaled = 494.814* (227), CFI = .918, TLI = .909, RMSEA = .056 [90% CI = 0.049 - 0.063]).


# (3) second-order solution -----------------------------------------------
# (including the 3 speciﬁc factors and a higher-order stat anxiety factor)

stars_second_order_model <- '
stat_anxiety =~ test_and_class + interpretation + fear_of_asking

test_and_class =~ stars_1 + stars_4 + stars_8 + stars_10 + 
                  stars_13 + stars_15 + stars_21 + stars_22 

interpretation =~ stars_2 + stars_5 + stars_6 + stars_7 + 
                  stars_9 + stars_11 + stars_12 + stars_14 +
                  stars_17 + stars_18 + stars_20 

fear_of_asking =~ stars_3 + stars_16 + stars_19 + stars_23' 
# In the second-order model, speciﬁcations were the same as in the ﬁrst-order model, but the correlations between the factors were replaced by a second-order global statistics anxiety factor.

fit_stars_second <- cfa(stars_second_order_model, data = anxiety, estimator = 'WLSMV')
summary(fit_stars_second, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_stars_second)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                      "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                      "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                      "srmr")], 3)
# Fit indices: (chisq.scaled = 494.814* (227), CFI = .918, TLI = .909, RMSEA = .056 [90% CI = 0.049 - 0.063]).


# (4) bifactor solution ---------------------------------------------------
# (including the 3 speciﬁc factors and a co-existing stat anxiety factor)

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



# restrictions (uncorrelated factors)
stat_anxiety    ~~ 0*test_and_class
stat_anxiety    ~~ 0*interpretation
stat_anxiety    ~~ 0*fear_of_asking
test_and_class  ~~ 0*interpretation
test_and_class  ~~ 0*fear_of_asking
interpretation  ~~ 0*fear_of_asking
'

# In bifactor-CFA solution, items were set to load on their respective S-factors as well as on the statistics anxiety G-factor, 
# and following typical bifactor speciﬁcations (Reise, 2012) factors were speciﬁed as orthogonal (i.e., not allowed to correlate with one another).

fit_stars_bifactor <- cfa(stars_bifactor_model, data = anxiety, estimator = 'WLSMV')
summary(fit_stars_bifactor, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_stars_bifactor)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                        "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                        "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                        "srmr")], 3)
# Fit indices: (chisq.scaled = 299.310* (207), CFI = .972, TLI = .966, RMSEA = .035 [90% CI = 0.026 - 0.043]).

anova(fit_stars_one, fit_stars_first, fit_stars_second, fit_stars_bifactor)

# 2.4.2 STARS composite reliability ---------------------------------------
# calculating composite reliability, McDonald's omega for the bifactor model

# reliability check
stars_bifact_model_rel <- '
# regressions

test_and_class =~ t1*stars_1 + t2*stars_4 + 
                  t3*stars_8 + t4*stars_10 + t5*stars_13 + 
                  t6*stars_15 + t7*stars_21 + 
                  t8*stars_22

# Error Variance
stars_1~~et1*stars_1
stars_4~~et2*stars_4
stars_8~~et3*stars_8
stars_10~~et4*stars_10
stars_13~~et5*stars_13
stars_15~~et6*stars_15
stars_21~~et7*stars_21
stars_22~~et8*stars_22

#Reliability
omega.test :=
((t1 + t2 + t3 + t4 + t5 + t6 + t7 + t8)^2)
/
((t1 + t2 + t3 + t4 + t5 + t6 + t7 + t8)^2 +
(et1 + et2 + et3 + et4 + et5 + et6 + et7 + et8))



interpretation =~ i1*stars_2 + i2*stars_5 + i3*stars_6 + i4*stars_7 +
                  i5*stars_9 + i6*stars_11 + i7*stars_12 + i8*stars_14 +
                  i9*stars_17 + i10*stars_18 + i11*stars_20

# Error Variance
stars_2~~ei1*stars_2
stars_5~~ei2*stars_5
stars_6~~ei3*stars_6
stars_7~~ei4*stars_7
stars_9~~ei5*stars_9
stars_11~~ei6*stars_11
stars_12~~ei7*stars_12
stars_14~~ei8*stars_14
stars_17~~ei9*stars_17
stars_18~~ei10*stars_18
stars_20~~ei11*stars_20

#Reliability
omega.interpret :=
((i1 + i2 + i3 + i4 + i5 + i6 + i7 + i8 + i9 + i10 + i11)^2)
/
((i1 + i2 + i3 + i4 + i5 + i6 + i7 + i8 + i9 + i10 + i11)^2 +
(ei1 + ei2 + ei3 + ei4 + ei5 + ei6 + ei7 + ei8 + ei9 + ei10 + ei11))



fear_of_asking =~ f1*stars_3 + f2*stars_16 + f3*stars_19 + f4*stars_23

# Error Variance
stars_3~~ef1*stars_3
stars_16~~ef2*stars_16
stars_19~~ef3*stars_19
stars_23~~ef4*stars_23

#Reliability
omega.fear :=
((f1 + f2 + f3 + f4)^2)
/
((f1 + f2 + f3 + f4)^2 +
(ef1 + ef2 + ef3 + ef4))



stat_anxiety =~ tg1*stars_1 + tg2*stars_4 + tg3*stars_8 + tg4*stars_10 +
                tg5*stars_13 + tg6*stars_15 + tg7*stars_21 + 
                tg8*stars_22 +
                ig1*stars_2 + ig2*stars_5 + ig3*stars_6 + ig4*stars_7 +
                ig5*stars_9 + ig6*stars_11 + ig7*stars_12 + ig8*stars_14 +
                ig9*stars_17 + ig10*stars_18 + ig11*stars_20 +
                fg1*stars_3 + fg2*stars_16 + fg3*stars_19 + fg4*stars_23

# # Error Variance
# stars_1~~etg1*stars_1
# stars_4~~etg2*stars_4
# stars_8~~etg3*stars_8
# stars_10~~etg4*stars_10
# stars_13~~etg5*stars_13
# stars_15~~etg6*stars_15
# stars_21~~etg7*stars_21
# stars_22~~etg8*stars_22
# 
# stars_2~~eig1*stars_2
# stars_5~~eig2*stars_5
# stars_6~~eig3*stars_6
# stars_7~~eig4*stars_7
# stars_9~~eig5*stars_9
# stars_11~~eig6*stars_11
# stars_12~~eig7*stars_12
# stars_14~~eig8*stars_14
# stars_17~~eig9*stars_17
# stars_18~~eig10*stars_18
# stars_20~~eig11*stars_20
# 
# stars_3~~efg1*stars_3
# stars_16~~efg2*stars_16
# stars_19~~efg3*stars_19
# stars_23~~efg4*stars_23


#Reliability
omega.all :=
((tg1 + tg2 + tg3 + tg4 + tg5 + tg6 + tg7 + tg8 + 
ig1 + ig2 + ig3 + ig4 + ig5 + ig6 + ig7 + ig8 + ig9 + ig10 + ig11 + 
fg1 + fg2 + fg3 + fg4)^2)
/
((tg1 + tg2 + tg3 + tg4 + tg5 + tg6 + tg7 + tg8 + 
ig1 + ig2 + ig3 + ig4 + ig5 + ig6 + ig7 + ig8 + ig9 + ig10 + ig11 + 
fg1 + fg2 + fg3 + fg4)^2 +
(et1 + et2 + et3 + et4 + et5 + et6 + et7 + et8 + 
ei1 + ei2 + ei3 + ei4 + ei5 + ei6 + ei7 + ei8 + ei9 + ei10 + ei11 + 
ef1 + ef2 + ef3 + ef4))
# (etg1 + etg2 + etg3 + etg4 + etg5 + etg6 + etg7 + etg8 + 
# eig1 + eig2 + eig3 + eig4 + eig5 + eig6 + eig7 + eig8 + eig9 + eig10 + eig11 + 
# efg1 + efg2 + efg3 + efg4))


# restrictions (uncorrelated factors)
stat_anxiety    ~~ 0*test_and_class
stat_anxiety    ~~ 0*interpretation
stat_anxiety    ~~ 0*fear_of_asking
test_and_class  ~~ 0*interpretation
test_and_class  ~~ 0*fear_of_asking
interpretation  ~~ 0*fear_of_asking
'

fit_stars <- cfa(stars_bifact_model_rel, data = anxiety, estimator = 'WLSMV', std.lv = TRUE)
summary(fit_stars, fit.measures = TRUE, standardized = TRUE, rsquare=T)
# omega.test = 0.606
# omega.interprt = 0.698
# omega.fear = 0.836
# omega.all = 0.943
round(fitMeasures(fit_stars)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                        "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                        "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                        "srmr")], 3)


# 3. Measurement Invariance Testing ---------------------------------------

table(anxiety$gender)          # 296 vs 55 vs 2 vs 4 
table(anxiety$stat_now)        # 63 vs 295

table(anxiety$university)      # 248 vs 129
table(anxiety$stat_experience) # 125 vs 233



# Testing measurement invariance for universities
table(anxiety$university)

config_uni <- cfa(stars_bifactor_model, data = anxiety, group = "university", estimator = 'WLSMV')
weak_uni <- cfa(stars_bifactor_model, data = anxiety, group = "university", estimator = 'WLSMV', group.equal = "loadings")
strong_uni <- cfa(stars_bifactor_model, data = anxiety, group = "university", estimator = 'WLSMV', group.equal = c("loadings", "intercepts"))
strict_uni <- cfa(stars_bifactor_model, data = anxiety, group = "university", estimator = 'WLSMV', group.equal = c("loadings", "intercepts", "residuals"))
latent_vc_uni <- cfa(stars_bifactor_model, data = anxiety, group = "university", estimator = 'WLSMV', group.equal = c("loadings", "intercepts", "residuals", "lv.variances", "lv.covariances"))
mean_uni <- cfa(stars_bifactor_model, data = anxiety, group = "university", estimator = 'WLSMV', group.equal = c("loadings", "intercepts", "residuals", "lv.variances", "lv.covariances", "means"))

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
table(anxiety$stat_experience, useNA = "always")
anxiety2 <- anxiety %>% 
    filter(stat_experience %in% c("Experience with statistics", "No statistics experience"))
table(anxiety2$stat_experience, useNA = "always")


config_stat_exp <- cfa(stars_bifactor_model, data = anxiety2, group = "stat_experience", estimator = 'WLSMV')
weak_stat_exp <- cfa(stars_bifactor_model, data = anxiety2, group = "stat_experience", estimator = 'WLSMV', group.equal = "loadings")
strong_stat_exp <- cfa(stars_bifactor_model, data = anxiety2, group = "stat_experience", estimator = 'WLSMV', group.equal = c("loadings", "intercepts"))
strict_stat_exp <- cfa(stars_bifactor_model, data = anxiety2, group = "stat_experience", estimator = 'WLSMV', group.equal = c("loadings", "intercepts", "residuals"))
latent_vc_exp <- cfa(stars_bifactor_model, data = anxiety2, group = "stat_experience", estimator = 'WLSMV', group.equal = c("loadings", "intercepts", "residuals", "lv.variances", "lv.covariances"))
mean_exp <- cfa(stars_bifactor_model, data = anxiety2, group = "stat_experience", estimator = 'WLSMV', group.equal = c("loadings", "intercepts", "residuals", "lv.variances", "lv.covariances", "means"))

anova(config_stat_exp, weak_stat_exp, strong_stat_exp, strict_stat_exp, latent_vc_exp, mean_exp)

fit.stats_stat <- rbind(fitmeasures(config_stat_exp, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                        fitmeasures(weak_stat_exp, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                        fitmeasures(strong_stat_exp, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                        fitmeasures(strict_stat_exp, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                        fitmeasures(latent_vc_exp, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                        fitmeasures(mean_exp, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")))
rownames(fit.stats_stat) <- c("config", "weak", "strong", "strict", "latent_varcov", "latent_means")
round(fit.stats_stat,3)




# 4. Correlations ---------------------------------------------------------

# STARS bifactor, STICSA first-order, R-TAS first-order, BFNE-S one-factor

sem_model <- '
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



# restrictions (uncorrelated factors)
stat_anxiety    ~~ 0*test_and_class
stat_anxiety    ~~ 0*interpretation
stat_anxiety    ~~ 0*fear_of_asking
test_and_class  ~~ 0*interpretation
test_and_class  ~~ 0*fear_of_asking
interpretation  ~~ 0*fear_of_asking

# STICSA
somatic =~ sticsa_1 + sticsa_2 + sticsa_6 + sticsa_7 + sticsa_8 + 
           sticsa_12 + sticsa_14 + sticsa_15 + sticsa_18 + sticsa_20 + sticsa_21 

cognitive =~ sticsa_3 + sticsa_4 + sticsa_5 + sticsa_9 + sticsa_10 + 
             sticsa_11 + sticsa_13 + sticsa_16 + sticsa_17 + sticsa_19

somatic ~~ cognitive


# R-TAS
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



# BFNE-S
bfnes =~ bfne_1 + bfne_2 + bfne_3 + bfne_4 + bfne_5 + bfne_6 + bfne_7 + bfne_8


# Correlations between measures
stat_anxiety ~~ somatic
stat_anxiety ~~ cognitive
stat_anxiety ~~ worry
stat_anxiety ~~ tension
stat_anxiety ~~ irrelevant
stat_anxiety ~~ bodily
stat_anxiety ~~ bfnes

test_and_class ~~ somatic
test_and_class ~~ cognitive
test_and_class ~~ worry
test_and_class ~~ tension
test_and_class ~~ irrelevant
test_and_class ~~ bodily
test_and_class ~~ bfnes

interpretation ~~ somatic
interpretation ~~ cognitive
interpretation ~~ worry
interpretation ~~ tension
interpretation ~~ irrelevant
interpretation ~~ bodily
interpretation ~~ bfnes

fear_of_asking ~~ somatic
fear_of_asking ~~ cognitive
fear_of_asking ~~ worry
fear_of_asking ~~ tension
fear_of_asking ~~ irrelevant
fear_of_asking ~~ bodily
fear_of_asking ~~ bfnes

somatic ~~ worry
somatic ~~ tension
somatic ~~ irrelevant
somatic ~~ bodily
somatic ~~ bfnes

cognitive ~~ worry
cognitive ~~ tension
cognitive ~~ irrelevant
cognitive ~~ bodily
cognitive ~~ bfnes

worry ~~ bfnes
tension ~~ bfnes
irrelevant ~~ bfnes
bodily ~~ bfnes

'

fit_stars_sem <- cfa(sem_model, data = anxiety, estimator = 'WLSMV')
summary(fit_stars_sem, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_stars_sem)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                        "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                        "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                        "srmr")], 3)
# Fit indices: (chisq.scaled = 2784.689* (2412), CFI = .964, TLI = .962, RMSEA = .021 [90% CI = 0.017 - 0.024]).



