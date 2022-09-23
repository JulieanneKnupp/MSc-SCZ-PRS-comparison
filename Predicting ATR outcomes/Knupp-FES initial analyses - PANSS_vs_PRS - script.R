# create a new folder on your computer and copy your dataset into it
# set working directory to the folder containing your dataset using setwd(/..../yourfolder/yourdataset.xls)
# or use the set working directory button under the Session tab

# load the following packages: lme4, lmerTest, SNPassoc, gdata
library('lme4')
library('lmerTest')
library('gdata')
library('piecewiseSEM')
library('languageR') # has functions for plotting lmer outputs
library('fmsb') # package used to determine best model when outcome variable is binary
library('ggpubr') # for plotting correlation graphs
# import your dataset using the read.csv function as shown below
# replace 'example_data' in the scripting with the name of your dataset file
datafile <- read.csv(file = './example_database2_PRS.csv',header = T, sep = ';',dec = ".", na.strings = 'NA')
# now we need to do some simple data cleanup and formatting to make things easier for later on
# we should assign the class 'factor' to all variables that are factors but have been imported as integers
# and vice versa for numeric data that has been imported as factors
# use the structure (str) function to check this
str(datafile)
# use the function as.factor to assign variables to class 'factor'
as.factor(datafile$Gender) -> datafile$Gender
as.factor(datafile$Ethnic.group) -> datafile$Ethnic.group
as.factor(datafile$V09_Full_Remission) -> datafile$V09_Full_Remission
as.factor(datafile$TreatRef) -> datafile$TreatRef
as.factor(datafile$EPSAE) -> datafile$EPSAE
as.factor(datafile$Relapse) -> datafile$Relapse
as.numeric(datafile$african_san) -> datafile$african_san
as.numeric(datafile$african_non_san) -> datafile$african_non_san
as.numeric(datafile$european) -> datafile$european
as.numeric(datafile$south_asian) -> datafile$south_asian
as.numeric(datafile$east_asian) -> datafile$east_asian
#as.numeric(datafile$Change_in_PANSS_T) -> datafile$Change_in_PANSS_T
as.numeric(datafile$PANSSP_baseline) -> datafile$PANSSP_baseline
as.numeric(datafile$PANSSN_baseline) -> datafile$PANSSN_baseline
as.numeric(datafile$PANSSG_baseline) -> datafile$PANSSG_baseline
as.numeric(datafile$PANSST_baseline) -> datafile$PANSST_baseline
as.numeric(datafile$PANSSP_scores) -> datafile$PANSSP_scores
as.numeric(datafile$PANSSN_scores) -> datafile$PANSSN_scores
as.numeric(datafile$PANSSG_scores) -> datafile$PANSSG_scores
as.numeric(datafile$PANSST_scores) -> datafile$PANSST_scores
as.numeric(datafile$PRSice2_T) -> datafile$PRSice2_T
as.numeric(datafile$PRSice2_E) -> datafile$PRSice2_E
as.numeric(datafile$PRSice_G) -> datafile$PRSice_G
as.numeric(datafile$PRSice_N) -> datafile$PRSice_N
as.numeric(datafile$PRSice_P) -> datafile$PRSice_P
as.numeric(datafile$PRS_CS) -> datafile$PRS_CS
as.numeric(datafile$PRS_CSx) -> datafile$PRS_CSx
as.numeric(datafile$LDpred2) -> datafile$LDpred2
as.numeric(datafile$Lasso_E) -> datafile$Lasso_E
as.numeric(datafile$Lasso_G) -> datafile$Lasso_G
as.numeric(datafile$Lasso_N) -> datafile$Lasso_N
as.numeric(datafile$Lasso_P) -> datafile$Lasso_P
as.numeric(datafile$lassosum_T) -> datafile$lassosum_T

str(datafile)

working_data<- data.frame(datafile[1:103,1:ncol(datafile)]) # a subset of the data for when not conisdering an association over time
str(working_data)
# we use 'lmer' from lme4 to do a mixed effects model for the association with change in PANSS

# set R to output the results to an output folder
# first we create the folder and then navigate to it
dir.create('./Output_PANSS_vs_PRS')
setwd('./Output_PANSS_vs_PRS/')

# Test for association between PRS and PANSSN
PANSSN_model_PRS_CS <- lmer(formula = log(PANSSN_scores) ~ Weeks_PANSS*scale(datafile$PRS_CS) +Age +Gender+ log(PANSSN_baseline) +scale(african_san) + scale(african_non_san)+ scale(european) +scale(south_asian) + scale(east_asian) + (1|SampleID), data = datafile, na.action = na.exclude, REML = F)
PANSSN_model_PRS_CSx <- lmer(formula = log(PANSSN_scores) ~ Weeks_PANSS*scale(datafile$PRS_CSx) +Age +Gender+ log(PANSSN_baseline) +scale(african_san) + scale(african_non_san)+ scale(european) +scale(south_asian) + scale(east_asian) + (1|SampleID), data = datafile, na.action = na.exclude, REML = F)
PANSSN_model_PRSice2_N <- lmer(formula = log(PANSSN_scores) ~ Weeks_PANSS*scale(datafile$PRSice_N) +Age +Gender+ log(PANSSN_baseline) +scale(african_san) + scale(african_non_san)+ scale(european) +scale(south_asian) + scale(east_asian) + (1|SampleID), data = datafile, na.action = na.exclude, REML = F)
PANSSN_model_Lasso_N <- lmer(formula = log(PANSSN_scores) ~ Weeks_PANSS*scale(datafile$Lasso_N) +Age +Gender+ log(PANSSN_baseline) +scale(african_san) + scale(african_non_san)+ scale(european) +scale(south_asian) + scale(east_asian) + (1|SampleID), data = datafile, na.action = na.exclude, REML = F)
PANSSN_model_LDpred2 <- lmer(formula = log(PANSSN_scores) ~ Weeks_PANSS*scale(datafile$LDpred2) +Age +Gender+ log(PANSSN_baseline) +scale(african_san) + scale(african_non_san)+ scale(european) +scale(south_asian) + scale(east_asian) + (1|SampleID), data = datafile, na.action = na.exclude, REML = F)
# Test for best fitting model. The highest conditional R-squared, lowest AIC and lowest dAIC is the best model to report on
anova(PANSSN_model_PRS_CS, PANSSN_model_LDpred2, PANSSN_model_Lasso_N, PANSSN_model_PRSice2_N, PANSSN_model_PRS_CSx)
AICc(PANSSN_model_Lasso_N)
AICc(PANSSN_model_LDpred2)
AICc(PANSSN_model_PRS_CS)
AICc(PANSSN_model_PRS_CSx)
AICc(PANSSN_model_PRSice2_N)
# best fit model will be analysed further
PANSSN_sum <- summary(PANSSN_model_LDpred2) 
PANSSN_sum
PANSSN_confints <- confint.merMod(PANSSN_model_LDpred2)
lmerPlotInt.fnc(PANSSN_model_LDpred2, yname = "scale(datafile$LDpred2)",xname = "Weeks_PANSS",intxyname = "Weeks_PANSS:scale(datafile$LDpred2)")

# Test for association between PRS and PANSSP
PANSSP_model_PRS_CS <- lmer(formula = log(PANSSP_scores) ~ Weeks_PANSS*scale(datafile$PRS_CS) +Age +Gender+ log(PANSSP_baseline) +scale(african_san) + scale(african_non_san)+ scale(european) +scale(south_asian) + scale(east_asian) + (1|SampleID), data = datafile, na.action = na.exclude, REML = F)
PANSSP_model_PRS_CSx <- lmer(formula = log(PANSSP_scores) ~ Weeks_PANSS*scale(datafile$PRS_CSx) +Age +Gender+ log(PANSSP_baseline) +scale(african_san) + scale(african_non_san)+ scale(european) +scale(south_asian) + scale(east_asian) + (1|SampleID), data = datafile, na.action = na.exclude, REML = F)
PANSSP_model_PRSice2_P <- lmer(formula = log(PANSSP_scores) ~ Weeks_PANSS*scale(datafile$PRSice_P) +Age +Gender+ log(PANSSP_baseline) +scale(african_san) + scale(african_non_san)+ scale(european) +scale(south_asian) + scale(east_asian) + (1|SampleID), data = datafile, na.action = na.exclude, REML = F)
PANSSP_model_Lasso_P <- lmer(formula = log(PANSSP_scores) ~ Weeks_PANSS*scale(datafile$Lasso_P) +Age +Gender+ log(PANSSP_baseline) +scale(african_san) + scale(african_non_san)+ scale(european) +scale(south_asian) + scale(east_asian) + (1|SampleID), data = datafile, na.action = na.exclude, REML = F)
PANSSP_model_LDpred2 <- lmer(formula = log(PANSSP_scores) ~ Weeks_PANSS*scale(datafile$LDpred2) +Age +Gender+ log(PANSSP_baseline) +scale(african_san) + scale(african_non_san)+ scale(european) +scale(south_asian) + scale(east_asian) + (1|SampleID), data = datafile, na.action = na.exclude, REML = F)
# Test for best fitting model. The highest conditional R-squared, lowest AIC and lowest dAIC is the best model to report on
anova(PANSSP_model_PRS_CS, PANSSP_model_LDpred2, PANSSP_model_Lasso_P, PANSSP_model_PRSice2_P, PANSSP_model_PRS_CSx)
AICc(PANSSP_model_Lasso_P)
AICc(PANSSP_model_LDpred2)
AICc(PANSSP_model_PRS_CS)
AICc(PANSSP_model_PRS_CSx)
AICc(PANSSP_model_PRSice2_P)
# best fit model will be analysed further
PANSSP_sum <- summary(PANSSP_model_PRS_CSx) 
PANSSP_confints <- confint.merMod(PANSSP_model_PRS_CSx)
lmerPlotInt.fnc(PANSSP_model_PRS_CSx, yname = "scale(datafile$PRS_CSx)",xname = "Weeks_PANSS",intxyname = "Weeks_PANSS:scale(datafile$PRS_CSx)")


# Test for association between PRS and PANSSG
PANSSG_model_PRS_CS <- lmer(formula = log(PANSSG_scores) ~ Weeks_PANSS*scale(datafile$PRS_CS) +Age +Gender+ log(PANSSG_baseline) +scale(african_san) + scale(african_non_san)+ scale(european) +scale(south_asian) + scale(east_asian) + (1|SampleID), data = datafile, na.action = na.exclude, REML = F)
PANSSG_model_PRS_CSx <- lmer(formula = log(PANSSG_scores) ~ Weeks_PANSS*scale(datafile$PRS_CSx) +Age +Gender+ log(PANSSG_baseline) +scale(african_san) + scale(african_non_san)+ scale(european) +scale(south_asian) + scale(east_asian) + (1|SampleID), data = datafile, na.action = na.exclude, REML = F)
PANSSG_model_PRSice2_G <- lmer(formula = log(PANSSG_scores) ~ Weeks_PANSS*scale(datafile$PRSice_G) +Age +Gender+ log(PANSSG_baseline) +scale(african_san) + scale(african_non_san)+ scale(european) +scale(south_asian) + scale(east_asian) + (1|SampleID), data = datafile, na.action = na.exclude, REML = F)
PANSSG_model_Lasso_G <- lmer(formula = log(PANSSG_scores) ~ Weeks_PANSS*scale(datafile$Lasso_G) +Age +Gender+ log(PANSSG_baseline) +scale(african_san) + scale(african_non_san)+ scale(european) +scale(south_asian) + scale(east_asian) + (1|SampleID), data = datafile, na.action = na.exclude, REML = F)
PANSSG_model_LDpred2 <- lmer(formula = log(PANSSG_scores) ~ Weeks_PANSS*scale(datafile$LDpred2) +Age +Gender+ log(PANSSG_baseline) +scale(african_san) + scale(african_non_san)+ scale(european) +scale(south_asian) + scale(east_asian) + (1|SampleID), data = datafile, na.action = na.exclude, REML = F)
# Test for best fitting model. The highest conditional R-squared, lowest AIC and lowest dAIC is the best model to report on
anova(PANSSG_model_PRS_CS, PANSSG_model_LDpred2, PANSSG_model_Lasso_G, PANSSG_model_PRSice2_G, PANSSG_model_PRS_CSx)
AICc(PANSSG_model_Lasso_G)
AICc(PANSSG_model_LDpred2)
AICc(PANSSG_model_PRS_CS)
AICc(PANSSG_model_PRS_CSx)
AICc(PANSSG_model_PRSice2_G)
# best fit model will be analysed further
PANSSG_sum <- summary(PANSSG_model_Lasso_G) 
PANSSG_confints <- confint.merMod(PANSSG_model_Lasso_G)
lmerPlotInt.fnc(PANSSG_model_Lasso_G, yname = "scale(datafile$Lasso_G)",xname = "Weeks_PANSS",intxyname = "Weeks_PANSS:scale(datafile$Lasso_G)")


# Test for association between PRS and PANSST
PANSST_model_PRS_CS <- lmer(formula = log(PANSST_scores) ~ Weeks_PANSS*scale(datafile$PRS_CS) +Age +Gender+ log(PANSST_baseline) +scale(african_san) + scale(african_non_san)+ scale(european) +scale(south_asian) + scale(east_asian) + (1|SampleID), data = datafile, na.action = na.exclude, REML = F)
PANSST_model_PRS_CSx <- lmer(formula = log(PANSST_scores) ~ Weeks_PANSS*scale(datafile$PRS_CSx) +Age +Gender+ log(PANSST_baseline) +scale(african_san) + scale(african_non_san)+ scale(european) +scale(south_asian) + scale(east_asian) + (1|SampleID), data = datafile, na.action = na.exclude, REML = F)
PANSST_model_PRSice2_T <- lmer(formula = log(PANSST_scores) ~ Weeks_PANSS*scale(datafile$PRSice2_T) +Age +Gender+ log(PANSST_baseline) +scale(african_san) + scale(african_non_san)+ scale(european) +scale(south_asian) + scale(east_asian) + (1|SampleID), data = datafile, na.action = na.exclude, REML = F)
PANSST_model_Lasso_T <- lmer(formula = log(PANSST_scores) ~ Weeks_PANSS*scale(datafile$lassosum_T) +Age +Gender+ log(PANSST_baseline) +scale(african_san) + scale(african_non_san)+ scale(european) +scale(south_asian) + scale(east_asian) + (1|SampleID), data = datafile, na.action = na.exclude, REML = F)
PANSST_model_LDpred2 <- lmer(formula = log(PANSST_scores) ~ Weeks_PANSS*scale(datafile$LDpred2) +Age +Gender+ log(PANSST_baseline) +scale(african_san) + scale(african_non_san)+ scale(european) +scale(south_asian) + scale(east_asian) + (1|SampleID), data = datafile, na.action = na.exclude, REML = F)
# Test for best fitting model. The highest conditional R-squared, lowest AIC and lowest dAIC is the best model to report on
anova(PANSST_model_PRS_CS, PANSST_model_LDpred2, PANSST_model_Lasso_T, PANSST_model_PRSice2_T, PANSST_model_PRS_CSx)
AICc(PANSST_model_Lasso_T)
AICc(PANSST_model_LDpred2)
AICc(PANSST_model_PRS_CS)
AICc(PANSST_model_PRS_CSx)
AICc(PANSST_model_PRSice2_T)
# best fit model will be analysed further
PANSST_sum <- summary(PANSST_model_PRS_CSx) 
PANSST_confints <- confint.merMod(PANSST_model_PRS_CSx)
lmerPlotInt.fnc(PANSST_model_PRS_CSx, yname = "scale(datafile$PRS_CSx)",xname = "Weeks_PANSS",intxyname = "Weeks_PANSS:scale(datafile$PRS_CSx)")


# write an output file with the results from the best fitting models
sink('PANSS_vs_PRS.txt', append = F) 
print("PANSSN RESULTS")
print(PANSSN_sum)
print(PANSSN_confints)
print("PANSSP RESULTS")
print(PANSSP_sum)
print(PANSSP_confints)
print("PANSSG RESULTS")
print(PANSSG_sum)
print(PANSSG_confints)
print("PANSST RESULTS")
print(PANSST_sum)
print(PANSST_confints)
sink(file = NULL)

setwd('..')





