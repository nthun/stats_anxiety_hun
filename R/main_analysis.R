
# loading packages
library(lavaan) # for calculating Cronbach alpha
library(semTools)  # for calculating Cronbach alpha



anxiety <- read.csv(file = "data/stat_anxiety_hun.csv")


# II. PRELIMINARY ANALYSIS ------------------------------------------------


# # Statistics Anxiety Rating Scale (STARS; Cruise et al., 1985) ----------

# 1. Internal consistency (alpha) calculation -----------------------------
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



# 2. Standardized Parameter Estimates from the correlational model ----------

# calculating composite reliability, McDonald's omega

# reliability check
stars_corr_model_rel <- '
# regressions

testnclass =~ t1*stars_1 + t2*stars_4 + t3*stars_8 + t4*stars_10 + 
                  t5*stars_13 + t6*stars_15 + t7*stars_21 + t8*stars_22 

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

#Average Variance Extracted (AVE)
ave_t := 
((t1^2) + (t2^2) + (t3^2) + (t4^2) + (t5^2) + (t6^2) + (t7^2) + (t8^2)) / 8



interpret =~ i1*stars_2 + i2*stars_5 + i3*stars_6 + i4*stars_7 + 
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

#Average Variance Extracted (AVE)
ave_i := 
((i1^2) + (i2^2) + (i3^2) + (i4^2) + (i5^2) + (i6^2) + (i7^2) + (i8^2) + (i9^2) + (i10^2) + (i11^2)) / 11



fearask =~ f1*stars_3 + f2*stars_16 + f3*stars_19 + f4*stars_23 

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

#Average Variance Extracted (AVE)
ave_t := 
((f1^2) + (f2^2) + (f3^2) + (f4^2)) / 4



# correlations
testnclass ~~ interpret
testnclass ~~ fearask
interpret ~~ fearask
'

fit_stars <- cfa(stars_corr_model_rel, data = anxiety, estimator = 'MLR', std.lv = TRUE)
summary(fit_stars, fit.measures = TRUE, standardized = TRUE, rsquare=T)


# Fit indices
round(fitMeasures(fit_stars)[c("chisq.scaled", "df.scaled", "cfi.scaled", "tli.scaled",
                               "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr", "aic", "bic")], 3)
round(fitMeasures(fit_stars)["pvalue.scaled"], 3)



# Revised Maths Anxiety Rating Scale  (R-MARS; Baloğlu & Zelhart,  --------

colnames(anxiety)

# 1. Internal consistency (alpha) calculation -----------------------------
# calculating Cronbach alpha for R-MARS (item selection is based on XXXX)
rmars_mtest <- '
mathtest =~ rmars_1 + rmars_2 + rmars_3 + rmars_4 + rmars_5 +  
            rmars_6 + rmars_7 + rmars_8 + rmars_9 + rmars_10' 

fit_rmars_mtest <- cfa(rmars_mtest, data = anxiety, estimator = 'MLR')
stars_mtest_rel <- as.data.frame(reliability(fit_rmars_mtest))


rmars_ntest <- '
numericaltest =~ rmars_11 + rmars_12 + rmars_13 + rmars_14 + rmars_15' 

fit_rmars_ntest <- cfa(rmars_ntest, data = anxiety, estimator = 'MLR')
rmars_ntest_rel <- as.data.frame(reliability(fit_rmars_ntest))


rmars_mcourse <- '
mathcourse =~ rmars_16 + rmars_17 + rmars_18 + rmars_19 + rmars_20' 

fit_rmars_mcourse <- cfa(rmars_mcourse, data = anxiety, estimator = 'MLR')
rmars_mcourse_rel <- as.data.frame(reliability(fit_rmars_mcourse))


reliabilities_rmars <- cbind(stars_mtest_rel, rmars_ntest_rel, rmars_mcourse_rel)
reliabilities_rmars2 <- round(reliabilities_rmars[1,], 3)



# 2. Standardized Parameter Estimates from the correlational model ----------

# calculating composite reliability, McDonald's omega

# reliability check
rmars_corr_model_rel <- '
# regressions

mathtest =~ mt1*rmars_1 + mt2*rmars_2 + mt3*rmars_3 + mt4*rmars_4 + mt5*rmars_5 +  
            mt6*rmars_6 + mt7*rmars_7 + mt8*rmars_8 + mt9*rmars_9 + mt10*rmars_10

# Error Variance
rmars_1~~emt1*rmars_1
rmars_2~~emt2*rmars_2
rmars_3~~emt3*rmars_3
rmars_4~~emt4*rmars_4
rmars_5~~emt5*rmars_5
rmars_6~~emt6*rmars_6
rmars_7~~emt7*rmars_7
rmars_8~~emt8*rmars_8
rmars_9~~emt9*rmars_9
rmars_10~~emt10*rmars_10

#Reliability
omega.mtest := 
((mt1 + mt2 + mt3 + mt4 + mt5 + mt6 + mt7 + mt8 + mt9 + mt10)^2) 
/ 
((mt1 + mt2 + mt3 + mt4 + mt5 + mt6 + mt7 + mt8 + mt9 + mt10)^2 + 
(emt1 + emt2 + emt3 + emt4 + emt5 + emt6 + emt7 + emt8 + emt9 + emt10))

#Average Variance Extracted (AVE)
ave_t := 
((mt1^2) + (mt2^2) + (mt3^2) + (mt4^2) + (mt5^2) + 
(mt6^2) + (mt7^2) + (mt8^2) + (mt9^2) + (mt10^2)) / 10



numtest =~ n1*rmars_11 + n2*rmars_12 + n3*rmars_13 + n4*rmars_14 + n5*rmars_15

# Error Variance
rmars_11~~en1*rmars_11
rmars_12~~en2*rmars_12
rmars_13~~en3*rmars_13
rmars_14~~en4*rmars_14
rmars_15~~en5*rmars_15


#Reliability
omega.ntest := 
((n1 + n2 + n3 + n4 + n5)^2) 
/ 
((n1 + n2 + n3 + n4 + n5)^2 + 
(en1 + en2 + en3 + en4 + en5))

#Average Variance Extracted (AVE)
ave_n := 
((n1^2) + (n2^2) + (n3^2) + (n4^2) + (n5^2)) / 5



mathcourse =~ c1*rmars_16 + c2*rmars_17 + c3*rmars_18 + c4*rmars_19 + c5*rmars_20 

# Error Variance
rmars_16~~ec1*rmars_16
rmars_17~~ec2*rmars_17
rmars_18~~ec3*rmars_18
rmars_19~~ec4*rmars_19
rmars_20~~ec5*rmars_20

#Reliability
omega.mcourse := 
((c1 + c2 + c3 + c4 + c5)^2) 
/ 
((c1 + c2 + c3 + c4 + c5)^2 + 
(ec1 + ec2 + ec3 + ec4 + ec5))

#Average Variance Extracted (AVE)
ave_mc := 
((c1^2) + (c2^2) + (c3^2) + (c4^2) + (c5^2)) / 5



# correlations
mathtest ~~ numtest
mathtest ~~ mathcourse
numtest ~~ mathcourse
'

fit_rmars <- cfa(rmars_corr_model_rel, data = anxiety, estimator = 'MLR', std.lv = TRUE)
summary(fit_rmars, fit.measures = TRUE, standardized = TRUE, rsquare=T)


# Fit indices
round(fitMeasures(fit_rmars)[c("chisq.scaled", "df.scaled", "cfi.scaled", "tli.scaled",
                               "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr", "aic", "bic")], 3)
round(fitMeasures(fit_rmars)["pvalue.scaled"], 3)



# State-Trait Inventory for Cognitive and Somatic Anxiety (STICSA; --------

# 1. Internal consistency (alpha) calculation -----------------------------
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



# 2. Standardized Parameter Estimates from the correlational model ----------

# calculating composite reliability, McDonald's omega

# reliability check
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

#Average Variance Extracted (AVE)
ave_som := 
((s1^2) + (s2^2) + (s3^2) + (s4^2) + (s5^2) + (s6^2) + (s7^2) + (s8^2) + (s9^2) + (s10^2) + (s11^2)) / 11



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

#Average Variance Extracted (AVE)
ave_cog := 
((c1^2) + (c2^2) + (c3^2) + (c4^2) + (c5^2) + (c6^2) + (c7^2) + (c8^2) + (c9^2) + (c10^2)) / 10

# correlations
somatic ~~ cognitive
'

fit_sticsa <- cfa(sticsa_corr_model_rel, data = anxiety, estimator = 'MLR', std.lv = TRUE)
summary(fit_sticsa, fit.measures = TRUE, standardized = TRUE, rsquare=T)


# Fit indices
round(fitMeasures(fit_sticsa)[c("chisq.scaled", "df.scaled", "cfi.scaled", "tli.scaled",
                               "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr", "aic", "bic")], 3)
round(fitMeasures(fit_sticsa)["pvalue.scaled"], 3)




# Revised Test Anxiety Scale (R-TAS; Benson & El‐Zahhar, 1994 -------------

# 1. Internal consistency (alpha) calculation -----------------------------
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



# 2. Standardized Parameter Estimates from the correlational model ----------

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

#Average Variance Extracted (AVE)
ave_wor := 
((w1^2) + (w2^2) + (w3^2) + (w4^2) + (w5^2) + (w6^2)) / 6



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

#Average Variance Extracted (AVE)
ave_ten := 
((t1^2) + (t2^2) + (t3^2) + (t4^2) + (t5^2)) / 5



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

#Average Variance Extracted (AVE)
ave_irr := 
((i1^2) + (i2^2) + (i3^2) + (i4^2)) / 4



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

#Average Variance Extracted (AVE)
ave_bod := 
((b1^2) + (b2^2) + (b3^2) + (b4^2) + (b5^2)) / 5



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


# Fit indices
round(fitMeasures(fit_rtas)[c("chisq.scaled", "df.scaled", "cfi.scaled", "tli.scaled",
                               "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr", "aic", "bic")], 3)
round(fitMeasures(fit_rtas)["pvalue.scaled"], 3)

