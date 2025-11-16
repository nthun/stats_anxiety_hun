
# loading packages
library(lavaan) # for calculating Cronbach alpha, McDonald's omega, CFA, Measurement Invariance, correlations
library(semTools)  # for calculating Cronbach alpha
library(tidyverse)


anxiety <- read_csv(file = "data/stat_anxiety_hun.csv")

## ---------------------------------------- Preliminary analyses ---------------------------------

# 1. Internal consistency (alpha) calculation ---------------------------

# 1.1 State-Trait Inventory for Cog. and Som. Anxiety (STICSA) -----------
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



# 1.2 Revised Test Anxiety Scale (R-TAS) ---------------------------------

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



# 1.3 Brief Fear of Negative Eval. Scale-Straightforward (BFNE-S) --------
# calculating Cronbach alpha for BFNE-S (item selection is based on Carleton et al., 2011)
bfnes <- '
bfnes =~ bfne_1 + bfne_2 + bfne_3 + bfne_4 + bfne_5 + bfne_6 + bfne_7 + bfne_8' 

fit_bfnes <- cfa(bfnes, data = anxiety, estimator = 'MLR')
bfnes_rel <- as.data.frame(reliability(fit_bfnes))

# reliabilities_bfnes <- cbind(sticsa_som_rel, sticsa_cog_rel)
reliabilities_bfnes2 <- round(bfnes_rel[1,], 3)



# 1.4 Revised Maths Anxiety Rating Scale (Baloğlu&Zelhart (2007) ----------
# calculating Cronbach alpha for R-MARS (item selection is based on Baloğlu & Zelhart, 2007)
rmars_mta <- '
math_test_anx =~ rmars_21 + rmars_22 + rmars_23 + rmars_24 + rmars_25 + 
                  rmars_26 + rmars_27 + rmars_28 + rmars_29 + rmars_30' 

fit_rmars_mta <- cfa(rmars_mta, data = anxiety, estimator = 'MLR')
rmars_mta_rel <- as.data.frame(reliability(fit_rmars_mta))


rmars_nta <- '
num_task_anx =~ rmars_31 + rmars_32 + rmars_33 + rmars_34 + rmars_35 + 
                rmars_41 + rmars_42 + rmars_43 + rmars_44'

fit_rmars_nta <- cfa(rmars_nta, data = anxiety, estimator = 'MLR')
rmars_nta_rel <- as.data.frame(reliability(fit_rmars_nta))


rmars_mca <- '
math_class_anx =~ rmars_36 + rmars_37 + rmars_38 + rmars_39 + rmars_40' 

fit_rmars_mca <- cfa(rmars_mca, data = anxiety, estimator = 'MLR')
rmars_mca_rel <- as.data.frame(reliability(fit_rmars_mca))


reliabilities_rmars <- cbind(rmars_mta_rel, rmars_nta_rel, rmars_mca_rel)
reliabilities_rmars2 <- round(reliabilities_rmars[1,], 3)



# 1.5 Statistics Anxiety Rating Scale (STARS; Cruise et al., 1985) --------
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



# Cronbach alphas for STICSA, R-TAS, BFNE-S, R-MARS, STARS
reliabilities_sticsa2
reliabilities_rtas2
reliabilities_bfnes2
reliabilities_rmars2
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

fit_sticsa_first <- cfa(sticsa_first_order_model, data = anxiety, estimator = 'MLR', std.lv = TRUE)
summary(fit_sticsa_first, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_sticsa_first)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                     "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                     "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                     "srmr")], 3)
# Fit indices: (chisq.scaled = 397.870* (188), CFI = .916, TLI = .906, RMSEA = .055 [90% CI = 0.048 - 0.062]).


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

fit_sticsa <- cfa(sticsa_corr_model_rel, data = anxiety, estimator = 'MLR', std.lv = TRUE)
summary(fit_sticsa, fit.measures = TRUE, standardized = TRUE, rsquare=T)
# omega.som = 0.864
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

fit_rtas_first <- cfa(rtas_first_order_model, data = anxiety, estimator = 'MLR', std.lv = TRUE)
summary(fit_rtas_first, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_rtas_first)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                     "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                     "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                     "srmr")], 3)
# Fit indices: (chisq.scaled = 272.694* (164), CFI = .960, TLI = .953, RMSEA = .042 [90% CI = 0.034 - 0.051]).


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

fit_rtas <- cfa(rtas_corr_model_rel, data = anxiety, estimator = 'MLR', std.lv = TRUE)
summary(fit_rtas, fit.measures = TRUE, standardized = TRUE, rsquare=T)
# omega.wor = 0.836
# omega.ten = 0.880
# omega.irr = 0.776
# omega.bod = 0.751


# 2.3. Brief Fear of Negative Eval. Scale-Straightforward (BFNE-S) --------

# 2.3.1 BFNE-S - estimation of one-factor CFA solution ---------
bfnes <- '
bfnes =~ bfne_1 + bfne_2 + bfne_3 + bfne_4 + bfne_5 + bfne_6 + bfne_7 + bfne_8' 

fit_bfnes_one <- cfa(bfnes, data = anxiety, estimator = 'MLR', std.lv = TRUE)
summary(fit_bfnes_one, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_bfnes_one)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                  "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                  "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                  "srmr")], 3)
# Fit indices: (chisq.scaled = 80.497* (20), CFI = .969, TLI = .957, RMSEA = .090 [90% CI = 0.072 - 0.109]).


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

fit_bfnes <- cfa(bfnes_corr_model_rel, data = anxiety, estimator = 'MLR', std.lv = TRUE)
summary(fit_bfnes, fit.measures = TRUE, standardized = TRUE, rsquare=T)
# omega.bfnes = 0.950


# 2.4 Revised Maths Anxiety Rating Scale (Baloğlu&Zelhart (2007) ----------
# 2.4.1 (1) R-MARS - estimation of first-order CFA solution --------------------
# ﬁrst-order solution (including the 3 speciﬁc factors) 
rmars_first_order_model <- '
math_test_anx =~ rmars_21 + rmars_22 + rmars_23 + rmars_24 + rmars_25 + 
    rmars_26 + rmars_27 + rmars_28 + rmars_29 + rmars_30

num_task_anx =~ rmars_31 + rmars_32 + rmars_33 + rmars_34 + rmars_35 + 
    rmars_41 + rmars_42 + rmars_43 + rmars_44

math_class_anx =~ rmars_36 + rmars_37 + rmars_38 + rmars_39 + rmars_40


math_test_anx ~~ num_task_anx
math_test_anx ~~ math_class_anx
num_task_anx ~~ math_class_anx
' 
# In the three-factor CFA solution, items were set to load only on their a priori speciﬁc factors, cross-loadings were set to be zero, and factors were allowed to correlate with one another.

fit_rmars_first <- cfa(rmars_first_order_model, data = anxiety, estimator = 'MLR', std.lv = TRUE)
summary(fit_rmars_first, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_rmars_first)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                     "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                     "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                     "srmr")], 3)
# Fit indices: (chisq.scaled = 1001.655* (249), CFI = .761, TLI = .735, RMSEA = .134 [90% CI = 0.125 - 0.142]).

# (2) one-factor solution --------------------
rmars_one_factor_model <- '
rmars_one =~ rmars_21 + rmars_22 + rmars_23 + rmars_24 + rmars_25 + 
    rmars_26 + rmars_27 + rmars_28 + rmars_29 + rmars_30 +
    
    rmars_31 + rmars_32 + rmars_33 + rmars_34 + rmars_35 + 
    rmars_41 + rmars_42 + rmars_43 + rmars_44 +
    
    rmars_36 + rmars_37 + rmars_38 + rmars_39 + rmars_40' 

fit_rmars_one <- cfa(rmars_one_factor_model, data = anxiety, estimator = 'MLR', std.lv = TRUE)
summary(fit_rmars_one, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_rmars_one)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                     "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                     "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                     "srmr")], 3)
# Fit indices: (chisq.scaled = 1910.457* (252), CFI = .474, TLI = .424, RMSEA = .197 [90% CI = 0.189 - 0.206]).

# (3) second-order solution --------------------
# second-order solution (including the 3 speciﬁc factors and a higher-order math anxiety factor)
rmars_second_order_model <- '
math_anxiety =~ math_test_anx + num_task_anx + math_class_anx

math_test_anx =~ rmars_21 + rmars_22 + rmars_23 + rmars_24 + rmars_25 + 
    rmars_26 + rmars_27 + rmars_28 + rmars_29 + rmars_30

num_task_anx =~ rmars_31 + rmars_32 + rmars_33 + rmars_34 + rmars_35 + 
    rmars_41 + rmars_42 + rmars_43 + rmars_44

math_class_anx =~ rmars_36 + rmars_37 + rmars_38 + rmars_39 + rmars_40
' 

fit_rmars_second <- cfa(rmars_second_order_model, data = anxiety, estimator = 'MLR', std.lv = TRUE)
summary(fit_rmars_second, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_rmars_second)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                     "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                     "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                     "srmr")], 3)
# Fit indices: (chisq.scaled = 1001.655* (249), CFI = .761, TLI = .735, RMSEA = .134 [90% CI = 0.125 - 0.142]).

# (4) bifactor solution ---------------------------------------------------
# (including the 3 speciﬁc factors and a co-existing math anxiety factor)
rmars_bifactor_model <- '
math_test_anx =~ rmars_21 + rmars_22 + rmars_23 + rmars_24 + rmars_25 + 
    rmars_26 + rmars_27 + rmars_28 + rmars_29 + rmars_30

num_task_anx =~ rmars_31 + rmars_32 + rmars_33 + rmars_34 + rmars_35 +
    rmars_41 + rmars_42 + rmars_43 + rmars_44

math_class_anx =~ rmars_36 + rmars_37 + rmars_38 + rmars_39 + rmars_40

math_anxiety =~ rmars_21 + rmars_22 + rmars_23 + rmars_24 + rmars_25 + 
    rmars_26 + rmars_27 + rmars_28 + rmars_29 + rmars_30 +
    
    rmars_31 + rmars_32 + rmars_33 + rmars_34 + rmars_35 + 
    rmars_41 + rmars_42 + rmars_43 + rmars_44 +
    
    rmars_36 + rmars_37 + rmars_38 + rmars_39 + rmars_40


# restrictions (uncorrelated factors)
math_anxiety    ~~ 0*math_test_anx
math_anxiety    ~~ 0*num_task_anx
math_anxiety    ~~ 0*math_class_anx
math_test_anx  ~~ 0*num_task_anx
math_test_anx  ~~ 0*math_class_anx
num_task_anx  ~~ 0*math_class_anx
' 

fit_rmars_bifactor <- cfa(rmars_bifactor_model, data = anxiety, estimator = 'MLR', std.lv = TRUE)
summary(fit_rmars_bifactor, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_rmars_bifactor)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                      "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                      "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                      "srmr")], 3)
# Fit indices: (chisq.scaled = 540.307* (228), CFI = .901, TLI = .880, RMSEA = .090 [90% CI = 0.080 - 0.100]).

# Sample size (169) is probably too small to reach acceptable level of goodness of fit indices (which are sensitive to sample size)
# Myers et al. (2011): 
# "Common rules of thumb for determining adequate N for a particular application of CFA include, but are not limited to: 
# N ≥ 200, 
# ratio of N to the number of variables in a model (p), N/p ≥ 10; 
# the ratio of N to the number of model parameters (q), N/q ≥ 5"
# Myers, N. D., Ahn, S., & Jin, Y. (2011). Sample size and power estimates for a confirmatory factor analytic model in exercise and sport: A Monte Carlo approach. Research quarterly for exercise and sport, 82(3), 412-423.

# In the model npar = 51, ratio (169/51) = 3,31  
# which is less then the recommended 5; based on this recommendation, (51*5=) 255 would be the minimum necessary sample size




# 2.4.2 R-MARS composite reliability ---------------------------------------
# calculating composite reliability, McDonald's omega

# reliability check
rmars_corr_model_rel <- '
# regressions

math_test_anx =~ a1*rmars_21 + a2*rmars_22 + a3*rmars_23 + a4*rmars_24 + a5*rmars_25 + 
    a6*rmars_26 + a7*rmars_27 + a8*rmars_28 + a9*rmars_29 + a10*rmars_30

# Error Variance
rmars_21~~ea1*rmars_21
rmars_22~~ea2*rmars_22
rmars_23~~ea3*rmars_23
rmars_24~~ea4*rmars_24
rmars_25~~ea5*rmars_25
rmars_26~~ea6*rmars_26
rmars_27~~ea7*rmars_27
rmars_28~~ea8*rmars_28
rmars_29~~ea9*rmars_29
rmars_30~~ea10*rmars_30


#Reliability
omega.test :=
((a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10)^2)
/
((a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10)^2 +
(ea1 + ea2 + ea3 + ea4 + ea5 + ea6 + ea7 + ea8 + ea9 + ea10))



num_task_anx =~ t1*rmars_31 + t2*rmars_32 + t3*rmars_33 + t4*rmars_34 + t5*rmars_35 + 
                t6*rmars_41 + t7*rmars_42 + t8*rmars_43 + t9*rmars_44

# Error Variance
rmars_31~~et1*rmars_31
rmars_32~~et2*rmars_32
rmars_33~~et3*rmars_33
rmars_34~~et4*rmars_34
rmars_35~~et5*rmars_35
rmars_41~~et6*rmars_41
rmars_42~~et7*rmars_42
rmars_43~~et8*rmars_43
rmars_44~~et9*rmars_44

#Reliability
omega.task :=
((t1 + t2 + t3 + t4 + t5 + t6 + t7 + t8 + t9)^2)
/
((t1 + t2 + t3 + t4 + t5 + t6 + t7 + t8 + t9)^2 +
(et1 + et2 + et3 + et4 + et5 + et6 + et7 + et8 + et9))


math_class_anx =~ c1*rmars_36 + c2*rmars_37 + c3*rmars_38 + c4*rmars_39 + c5*rmars_40

# Error Variance
rmars_36~~ec1*rmars_36
rmars_37~~ec2*rmars_37
rmars_38~~ec3*rmars_38
rmars_39~~ec4*rmars_39
rmars_40~~ec5*rmars_40

#Reliability
omega.class :=
((c1 + c2 + c3 + c4 + c5)^2)
/
((c1 + c2 + c3 + c4 + c5)^2 +
(ec1 + ec2 + ec3 + ec4 + ec5))



math_anxiety =~ teg1*rmars_21 + teg2*rmars_22 + teg3*rmars_23 + teg4*rmars_24 +
                teg5*rmars_25 + teg6*rmars_26 + teg7*rmars_27 + teg8*rmars_28 +
                teg9*rmars_29 + teg10*rmars_30 + 
                
                tag1*rmars_31 + tag2*rmars_32 + tag3*rmars_33 + tag4*rmars_34 + 
                tag5*rmars_35 + tag6*rmars_41 + tag7*rmars_42 + tag8*rmars_43 + tag9*rmars_44 +
                
                clg1*rmars_36 + clg2*rmars_37 + clg3*rmars_38 + clg4*rmars_39 + clg5*rmars_40

# # Error Variance
# rmars_21~~ea1*rmars_21
# rmars_22~~ea2*rmars_22
# rmars_23~~ea3*rmars_23
# rmars_24~~ea4*rmars_24
# rmars_25~~ea5*rmars_25
# rmars_26~~ea6*rmars_26
# rmars_27~~ea7*rmars_27
# rmars_28~~ea8*rmars_28
# rmars_29~~ea9*rmars_29
# rmars_30~~ea10*rmars_30
# 
# rmars_31~~et1*rmars_31
# rmars_32~~et2*rmars_32
# rmars_33~~et3*rmars_33
# rmars_34~~et4*rmars_34
# rmars_35~~et5*rmars_35
# rmars_41~~et6*rmars_41
# rmars_42~~et7*rmars_42
# rmars_43~~et8*rmars_43
# rmars_44~~et9*rmars_44
# 
# rmars_36~~ec1*rmars_36
# rmars_37~~ec2*rmars_37
# rmars_38~~ec3*rmars_38
# rmars_39~~ec4*rmars_39
# rmars_40~~ec5*rmars_40


#Reliability
omega.all :=
((teg1 + teg2 + teg3 + teg4 + teg5 + teg6 + teg7 + teg8 + teg9 + teg10 + 
tag1 + tag2 + tag3 + tag4 + tag5 + tag6 + tag7 + tag8 + tag9 +  
clg1 + clg2 + clg3 + clg4 + clg5)^2)
/
((teg1 + teg2 + teg3 + teg4 + teg5 + teg6 + teg7 + teg8 + teg9 + teg10 +
tag1 + tag2 + tag3 + tag4 + tag5 + tag6 + tag7 + tag8 + tag9 + 
clg1 + clg2 + clg3 + clg4 + clg5)^2 +
(ea1 + ea2 + ea3 + ea4 + ea5 + ea6 + ea7 + ea8 + ea9 + ea10 +
et1 + et2 + et3 + et4 + et5 + et6 + et7 + et8 + et9 +
ec1 + ec2 + ec3 + ec4 + ec5))


# restrictions (uncorrelated factors)
math_anxiety    ~~ 0*math_test_anx
math_anxiety    ~~ 0*num_task_anx
math_anxiety    ~~ 0*math_class_anx
math_test_anx  ~~ 0*num_task_anx
math_test_anx  ~~ 0*math_class_anx
num_task_anx  ~~ 0*math_class_anx
'

fit_rmars <- cfa(rmars_corr_model_rel, data = anxiety, estimator = 'MLR', std.lv = TRUE)
summary(fit_rmars, fit.measures = TRUE, standardized = TRUE, rsquare=T)
# omega.test = 0.927
# omega.task = 0.908
# omega.class = 0.857


# 2.5 Statistics Anxiety Rating Scale (STARS; Cruise et al., 1985) -------

# 2.5.1 STARS - The estimation of four alternative CFA solutions ----------------

# (1) one-factor solution -------------------------------------------------
stars_onemodel <- '
stars =~ stars_1 + stars_4 + stars_8 + stars_10 + 
                  stars_13 + stars_15 + stars_21 + stars_22 +
         stars_2 + stars_5 + stars_6 + stars_7 + 
                  stars_9 + stars_11 + stars_12 + stars_14 +
                  stars_17 + stars_18 + stars_20 +
         stars_3 + stars_16 + stars_19 + stars_23' 

fit_stars_one <- cfa(stars_onemodel, data = anxiety, estimator = 'MLR', std.lv = TRUE)
summary(fit_stars_one, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_stars_one)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                   "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                   "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                   "srmr")], 3)
# Fit indices: (chisq.scaled = 1450.592* (230), CFI = .701, TLI = .672, RMSEA = .119 [90% CI = 0.114 - 0.125]).


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

fit_stars_first <- cfa(stars_first_order_model, data = anxiety, estimator = 'MLR', std.lv = TRUE)
summary(fit_stars_first, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_stars_first)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                     "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                     "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                     "srmr")], 3)
# Fit indices: (chisq.scaled = 612.642* (227), CFI = .906, TLI = .895, RMSEA = .067 [90% CI = 0.061 - 0.074]).

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

fit_stars_second <- cfa(stars_second_order_model, data = anxiety, estimator = 'MLR', std.lv = TRUE)
summary(fit_stars_second, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_stars_second)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                      "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                      "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                      "srmr")], 3)
# Fit indices: (chisq.scaled = 612.642* (227), CFI = .906, TLI = .895, RMSEA = .067 [90% CI = 0.061 - 0.074]).

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

fit_stars_bifactor <- cfa(stars_bifactor_model, data = anxiety, estimator = 'MLR', std.lv = TRUE)
summary(fit_stars_bifactor, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_stars_bifactor)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                        "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                        "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                        "srmr")], 3)
# Fit indices: (chisq.scaled = 363.429* (207), CFI = .962, TLI = .953, RMSEA = .045 [90% CI = 0.038 - 0.052]).

anova(fit_stars_one, fit_stars_first, fit_stars_bifactor, scaled=TRUE)

round(fitMeasures(fit_stars_one)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                        "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                        "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                        "srmr")], 4)
round(fitMeasures(fit_stars_first)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                        "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                        "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                        "srmr")], 4)
round(fitMeasures(fit_stars_bifactor)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                        "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                        "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                        "srmr")], 4)



# 2.5.2 STARS composite reliability ---------------------------------------
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
omega.test := ((t1 + t2 + t3 + t4 + t5 + t6 + t7 + t8)^2) / ((t1 + t2 + t3 + t4 + t5 + t6 + t7 + t8)^2 + (et1 + et2 + et3 + et4 + et5 + et6 + et7 + et8))



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
omega.interpret := ((i1 + i2 + i3 + i4 + i5 + 
i6 + i7 + i8 + i9 + i10 + i11)^2) / ((i1 + i2 + i3 + 
i4 + i5 + i6 + i7 + i8 + i9 + i10 + i11)^2 + (ei1 + ei2 + 
ei3 + ei4 + ei5 + ei6 + ei7 + ei8 + ei9 + ei10 + ei11))



fear_of_asking =~ f1*stars_3 + f2*stars_16 + f3*stars_19 + f4*stars_23

# Error Variance
stars_3~~ef1*stars_3
stars_16~~ef2*stars_16
stars_19~~ef3*stars_19
stars_23~~ef4*stars_23

#Reliability
omega.fear := ((f1 + f2 + f3 + f4)^2) / ((f1 + f2 + f3 + f4)^2 + (ef1 + ef2 + ef3 + ef4))


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
omega.all := ((tg1 + tg2 + tg3 + tg4 + tg5 + tg6 + tg7 + tg8 + 
ig1 + ig2 + ig3 + ig4 + ig5 + ig6 + ig7 + ig8 + ig9 + ig10 + ig11 + 
fg1 + fg2 + fg3 + fg4)^2) / ((tg1 + tg2 + tg3 + tg4 + tg5 + 
tg6 + tg7 + tg8 + 
ig1 + ig2 + ig3 + ig4 + ig5 + ig6 + ig7 + ig8 + ig9 + ig10 + ig11 + 
fg1 + fg2 + fg3 + fg4)^2 + (et1 + et2 + et3 + et4 + et5 + 
et6 + et7 + et8 + 
ei1 + ei2 + ei3 + ei4 + ei5 + ei6 + ei7 + ei8 + ei9 + ei10 + ei11 + 
ef1 + ef2 + ef3 + ef4))


# restrictions (uncorrelated factors)
stat_anxiety    ~~ 0*test_and_class
stat_anxiety    ~~ 0*interpretation
stat_anxiety    ~~ 0*fear_of_asking
test_and_class  ~~ 0*interpretation
test_and_class  ~~ 0*fear_of_asking
interpretation  ~~ 0*fear_of_asking
'

fit_stars <- cfa(stars_bifact_model_rel, data = anxiety, estimator = 'MLR', std.lv = TRUE)
summary(fit_stars, fit.measures = TRUE, standardized = TRUE, rsquare=T)
# omega.test = 0.699
# omega.interprt = 0.554
# omega.fear = 0.843
# omega.all = 0.944

round(fitMeasures(fit_stars)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                        "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                        "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                        "srmr")], 3)

# Fit with bootstrap SEs
set.seed(1)  # for reproducibility
fit_stars <- cfa(
    stars_bifact_model_rel,
    data = anxiety,
    estimator = "ML",           
    std.lv   = TRUE,             
    se       = "bootstrap",      
    bootstrap = 2000,            
    test     = "bootstrap")

# Extract the ω estimates with CIs
pe <- parameterEstimates(fit_stars, ci = TRUE, boot.ci.type = "bca.simple")

# ω’s are the defined parameters (op == ":="). Pull label, est, CI:
omega_ci <- subset(pe, op == ":=")[, c("label","est","ci.lower","ci.upper")]
omega_ci


# 3. Measurement Invariance Testing ---------------------------------------

table(anxiety$gender)          # 296 vs 55 vs 2 vs 4 
table(anxiety$stat_now)        # 63 vs 295

table(anxiety$university)      # 248 vs 129
table(anxiety$stat_experience) # 125 vs 233



# Testing measurement invariance for universities
table(anxiety$university)

config_uni <- cfa(stars_bifactor_model, data = anxiety, group = "university", estimator = 'MLR')
weak_uni <- cfa(stars_bifactor_model, data = anxiety, group = "university", estimator = 'MLR', group.equal = "loadings")
strong_uni <- cfa(stars_bifactor_model, data = anxiety, group = "university", estimator = 'MLR', group.equal = c("loadings", "intercepts"))
strict_uni <- cfa(stars_bifactor_model, data = anxiety, group = "university", estimator = 'MLR', group.equal = c("loadings", "intercepts", "residuals"))
latent_vc_uni <- cfa(stars_bifactor_model, data = anxiety, group = "university", estimator = 'MLR', group.equal = c("loadings", "intercepts", "residuals", "lv.variances", "lv.covariances"))
mean_uni <- cfa(stars_bifactor_model, data = anxiety, group = "university", estimator = 'MLR', group.equal = c("loadings", "intercepts", "residuals", "lv.variances", "lv.covariances", "means"))


anova(config_uni, weak_uni, strong_uni, strict_uni, latent_vc_uni, mean_uni, scaled=TRUE)

fit.stats_uni <- rbind(fitmeasures(config_uni, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                       fitmeasures(weak_uni, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                       fitmeasures(strong_uni, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                       fitmeasures(strict_uni, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                       fitmeasures(latent_vc_uni, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                       fitmeasures(mean_uni, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")))
rownames(fit.stats_uni) <- c("config", "weak", "strong", "strict", "latent_varcov", "latent_means")
round(fit.stats_uni,4)


# Testing measurement invariance for stat experience
table(anxiety$stat_experience, useNA = "always")
anxiety2 <- anxiety %>% 
    filter(stat_experience %in% c("Experience with statistics", "No statistics experience"))
table(anxiety2$stat_experience, useNA = "always")

config_stat_exp <- cfa(stars_bifactor_model, data = anxiety2, group = "stat_experience", estimator = 'MLR')
weak_stat_exp <- cfa(stars_bifactor_model, data = anxiety2, group = "stat_experience", estimator = 'MLR', group.equal = "loadings")
strong_stat_exp <- cfa(stars_bifactor_model, data = anxiety2, group = "stat_experience", estimator = 'MLR', group.equal = c("loadings", "intercepts"))
strict_stat_exp <- cfa(stars_bifactor_model, data = anxiety2, group = "stat_experience", estimator = 'MLR', group.equal = c("loadings", "intercepts", "residuals"))
latent_vc_exp <- cfa(stars_bifactor_model, data = anxiety2, group = "stat_experience", estimator = 'MLR', group.equal = c("loadings", "intercepts", "residuals", "lv.variances", "lv.covariances"))
mean_exp <- cfa(stars_bifactor_model, data = anxiety2, group = "stat_experience", estimator = 'MLR', group.equal = c("loadings", "intercepts", "residuals", "lv.variances", "lv.covariances", "means"))

anova(config_stat_exp, weak_stat_exp, strong_stat_exp, strict_stat_exp, latent_vc_exp, mean_exp, scaled=TRUE)

fit.stats_stat <- rbind(fitmeasures(config_stat_exp, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                        fitmeasures(weak_stat_exp, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                        fitmeasures(strong_stat_exp, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                        fitmeasures(strict_stat_exp, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                        fitmeasures(latent_vc_exp, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")),
                        fitmeasures(mean_exp, fit.measures = c("chisq.scaled", "pvalue.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")))
rownames(fit.stats_stat) <- c("config", "weak", "strong", "strict", "latent_varcov", "latent_means")
round(fit.stats_stat,4)



# 4. Correlations ---------------------------------------------------------

# 4.1 Correlations of latent variables ------------------------------------
# STARS bifactor, STICSA first-order, R-TAS first-order, BFNE-S one-factor, grade, CRT

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


# # R-MARS
# math_test_anx =~ rmars_21 + rmars_22 + rmars_23 + rmars_24 + rmars_25 +
#     rmars_26 + rmars_27 + rmars_28 + rmars_29 + rmars_30
# 
# num_task_anx =~ rmars_31 + rmars_32 + rmars_33 + rmars_34 + rmars_35 +
#     rmars_41 + rmars_42 + rmars_43 + rmars_44
# 
# math_class_anx =~ rmars_36 + rmars_37 + rmars_38 + rmars_39 + rmars_40
# 
# 
# math_test_anx ~~ num_task_anx
# math_test_anx ~~ math_class_anx
# num_task_anx ~~ math_class_anx



# Correlations between measures
stat_anxiety ~~ somatic
stat_anxiety ~~ cognitive
stat_anxiety ~~ worry
stat_anxiety ~~ tension
stat_anxiety ~~ irrelevant
stat_anxiety ~~ bodily
stat_anxiety ~~ bfnes
stat_anxiety ~~ grade
stat_anxiety ~~ crt_sum_correct
# stat_anxiety ~~ math_test_anx
# stat_anxiety ~~ num_task_anx
# stat_anxiety ~~ math_class_anx

test_and_class ~~ somatic
test_and_class ~~ cognitive
test_and_class ~~ worry
test_and_class ~~ tension
test_and_class ~~ irrelevant
test_and_class ~~ bodily
test_and_class ~~ bfnes
test_and_class ~~ grade
test_and_class ~~ crt_sum_correct
# test_and_class ~~ math_test_anx
# test_and_class ~~ num_task_anx
# test_and_class ~~ math_class_anx

interpretation ~~ somatic
interpretation ~~ cognitive
interpretation ~~ worry
interpretation ~~ tension
interpretation ~~ irrelevant
interpretation ~~ bodily
interpretation ~~ bfnes
interpretation ~~ grade
interpretation ~~ crt_sum_correct
# interpretation ~~ math_test_anx
# interpretation ~~ num_task_anx
# interpretation ~~ math_class_anx

fear_of_asking ~~ somatic
fear_of_asking ~~ cognitive
fear_of_asking ~~ worry
fear_of_asking ~~ tension
fear_of_asking ~~ irrelevant
fear_of_asking ~~ bodily
fear_of_asking ~~ bfnes
fear_of_asking ~~ grade
fear_of_asking ~~ crt_sum_correct
# fear_of_asking ~~ math_test_anx
# fear_of_asking ~~ num_task_anx
# fear_of_asking ~~ math_class_anx

somatic ~~ worry
somatic ~~ tension
somatic ~~ irrelevant
somatic ~~ bodily
somatic ~~ bfnes
somatic ~~ grade
somatic ~~ crt_sum_correct
# somatic ~~ math_test_anx
# somatic ~~ num_task_anx
# somatic ~~ math_class_anx

cognitive ~~ worry
cognitive ~~ tension
cognitive ~~ irrelevant
cognitive ~~ bodily
cognitive ~~ bfnes
cognitive ~~ grade
cognitive ~~ crt_sum_correct
# cognitive ~~ math_test_anx
# cognitive ~~ num_task_anx
# cognitive ~~ math_class_anx

worry ~~ bfnes
worry ~~ grade
worry ~~ crt_sum_correct
# worry ~~ math_test_anx
# worry ~~ num_task_anx
# worry ~~ math_class_anx
tension ~~ bfnes
tension ~~ grade
tension ~~ crt_sum_correct
# tension ~~ math_test_anx
# tension ~~ num_task_anx
# tension ~~ math_class_anx
irrelevant ~~ bfnes
irrelevant ~~ grade
irrelevant ~~ crt_sum_correct
# irrelevant ~~ math_test_anx
# irrelevant ~~ num_task_anx
# irrelevant ~~ math_class_anx
bodily ~~ bfnes
bodily ~~ grade
bodily ~~ crt_sum_correct
# bodily ~~ math_test_anx
# bodily ~~ num_task_anx
# bodily ~~ math_class_anx

bfnes ~~ grade
bfnes ~~ crt_sum_correct
# bfnes ~~ math_test_anx
# bfnes ~~ num_task_anx
# bfnes ~~ math_class_anx

grade ~~ crt_sum_correct
# grade ~~ math_test_anx
# grade ~~ num_task_anx
# grade ~~ math_class_anx

# crt_sum_correct ~~ math_test_anx
# crt_sum_correct ~~ num_task_anx
# crt_sum_correct ~~ math_class_anx
'


fit_stars_sem <- cfa(sem_model, data = anxiety, estimator = 'MLR', std.lv = TRUE)
summary(fit_stars_sem, fit.measures = TRUE, standardized = TRUE)
round(fitMeasures(fit_stars_sem)[c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                                   "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                   "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                   "srmr")], 3)
# Fit indices: (chisq.scaled = 3696.046* (2534), CFI = .898, TLI = .892, RMSEA = .039 [90% CI = 0.036 - 0.042]).


# 4.1.1 SEM with Spearman correlations ------------------------------------
vars_in_model <- c(
    # STARS
    paste0("stars_", 1:23),
    
    # STICSA
    paste0("sticsa_", 1:21),
    
    # R-TAS
    paste0("rtas_", c(1:11, 13, 15:18, 21, 22, 24, 25)),
    
    # BFNE-S
    paste0("bfne_", 1:8),
    
    # grade + CRT
    "grade",
    "crt_sum_correct"
)

anxiety_sub <- anxiety[, vars_in_model]

cors_spearman <- cor(
    anxiety_sub,
    use   = "pairwise.complete.obs",
    method = "spearman"
)


fit_stars_sem_spear <- cfa(
    sem_model,
    sample.cov  = cors_spearman,
    sample.nobs = nrow(anxiety_sub),
    std.lv      = TRUE,
    estimator   = "ML"
)

summary(fit_stars_sem_spear, fit.measures = TRUE, standardized = TRUE)



# 4.2 Correlations of measured variables ----------------------------------

# Preparation for correlation table

# calculating measured variables
anxiety2 <- anxiety %>%
    mutate(somatic = (sticsa_1 + sticsa_2 + sticsa_6 + sticsa_7 + sticsa_8 + 
                          sticsa_12 + sticsa_14 + sticsa_15 + sticsa_18 + sticsa_20 + sticsa_21)/11, 
           cognitive = (sticsa_3 + sticsa_4 + sticsa_5 + sticsa_9 + sticsa_10 + 
                            sticsa_11 + sticsa_13 + sticsa_16 + sticsa_17 + sticsa_19)/10,
           worry = (rtas_1 + rtas_2 + rtas_3 + rtas_8 + rtas_11 + rtas_24)/6,
           tension = (rtas_4 + rtas_5 + rtas_6 + rtas_13 + rtas_25)/5,
           irrelevant = (rtas_7 + rtas_9 + rtas_15 + rtas_16)/4,
           bodily = (rtas_10 + rtas_17 + rtas_18 + rtas_21 + rtas_22)/5,
           bfnes = (bfne_1 + bfne_2 + bfne_3 + bfne_4 + bfne_5 + bfne_6 + bfne_7 + bfne_8)/8,
           rmars = (rmars_21 + rmars_22 + rmars_23 + rmars_24 + rmars_25 + 
                        rmars_26 + rmars_27 + rmars_28 + rmars_29 + rmars_30 + 
                        rmars_31 + rmars_32 + rmars_33 + rmars_34 + rmars_35 + 
                        rmars_36 + rmars_37 + rmars_38 + rmars_39 + rmars_40 + 
                        rmars_41 + rmars_42 + rmars_43 + rmars_44)/24,
           math_test_anx = (rmars_21 + rmars_22 + rmars_23 + rmars_24 + rmars_25 + 
                                rmars_26 + rmars_27 + rmars_28 + rmars_29 + rmars_30)/10,
           num_task_anx = (rmars_31 + rmars_32 + rmars_33 + rmars_34 + rmars_35 + 
                               rmars_41 + rmars_42 + rmars_43 + rmars_44)/9,
           math_class_anx = (rmars_36 + rmars_37 + rmars_38 + rmars_39 + rmars_40)/5,
           test_and_class = (stars_1 + stars_4 + stars_8 + stars_10 + 
                                 stars_13 + stars_15 + stars_21 + stars_22)/8,
           interpretation = (stars_2 + stars_5 + stars_6 + stars_7 + 
                                 stars_9 + stars_11 + stars_12 + stars_14 +
                                 stars_17 + stars_18 + stars_20)/11,
           fear_of_asking = (stars_3 + stars_16 + stars_19 + stars_23)/4)
    
# check variables normality
shapiro.test(anxiety2$somatic)
shapiro.test(anxiety2$cognitive)
shapiro.test(anxiety2$worry)
shapiro.test(anxiety2$tension)
shapiro.test(anxiety2$irrelevant)
shapiro.test(anxiety2$bodily)
shapiro.test(anxiety2$bfnes)
shapiro.test(anxiety2$math_test_anx)
shapiro.test(anxiety2$num_task_anx)
shapiro.test(anxiety2$math_class_anx)
shapiro.test(anxiety2$test_and_class)
shapiro.test(anxiety2$interpretation)
shapiro.test(anxiety2$fear_of_asking)

# significant Shapiro-Wilk normality tests --> Spearman correlations are necessary

# corr_input <- c("rmars_21", "rmars_22", "rmars_23", "rmars_24", "rmars_25",
#                 "rmars_26", "rmars_27", "rmars_28", "rmars_29", "rmars_30",
#                 "rmars_31", "rmars_32", "rmars_33", "rmars_34", "rmars_35",
#                 "rmars_41", "rmars_42", "rmars_43", "rmars_44",
#                 "rmars_36", "rmars_37", "rmars_38", "rmars_39", "rmars_40")

corr_input <- c("test_and_class", "interpretation", "fear_of_asking",
                "somatic", "cognitive",
                "worry", "tension", "irrelevant", "bodily",
                "bfnes",
                "math_test_anx", "num_task_anx", "math_class_anx",
                "grade", "crt_sum_correct")

corr_table <- anxiety2 %>% 
    select(., one_of(corr_input))

# function to create Spearman corr table with stars indicating significance levels
# source of the following function: http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package

corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
    #Compute correlation matrix
    require(Hmisc)
    x <- as.matrix(x)
    correlation_matrix<-rcorr(x, type=method[1])
    R <- correlation_matrix$r # Matrix of correlation coeficients
    p <- correlation_matrix$P # Matrix of p-value 
    
    ## Define notions for significance levels; spacing is important.
    # mystars <- ifelse(p < .0001, "***", ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   "))))
    mystars <- ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   "))
    
    ## trunctuate the correlation matrix to two decimal
    R <- format(round(cbind(rep(-1.11, ncol(x)), R), 3))[,-1]
    
    ## build a new matrix that includes the correlations with their appropriate stars
    Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
    diag(Rnew) <- paste(diag(R), " ", sep="")
    rownames(Rnew) <- colnames(x)
    colnames(Rnew) <- paste(colnames(x), "", sep="")
    
    ## remove upper triangle of correlation matrix
    if(removeTriangle[1]=="upper"){
        Rnew <- as.matrix(Rnew)
        Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
        Rnew <- as.data.frame(Rnew)
    }
    
    ## remove lower triangle of correlation matrix
    else if(removeTriangle[1]=="lower"){
        Rnew <- as.matrix(Rnew)
        Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
        Rnew <- as.data.frame(Rnew)
    }
    
    ## remove last column and return the correlation matrix
    Rnew <- cbind(Rnew[1:length(Rnew)-1])
    if (result[1]=="none") return(Rnew)
    else{
        if(result[1]=="html") print(xtable(Rnew), type="html")
        else print(xtable(Rnew), type="latex") 
    }
} 


# 4.2.1 Correlations of measured variables - all  -------------------------

# Calculating correlations and creating correlation matrix
correlation_table <- corstars(corr_table, method = "spearman")

