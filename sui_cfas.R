#loading lavaan package
library(lavaan)
library(scales)

# Input whether running on Linux or Mac
linux <- 1

# set working directory
if (linux == 1) {
  work_dir <- '/home/cglab/projects/sui/data/'
} else {
  work_dir <- ''
}

setwd(work_dir)

#reading in data
sui <- read.csv(paste0(paste(work_dir, sep = "/"), 'abcd5.1_tmri_mid_llosVn_lrwdVn_opfc_subc_net_hses_wide_sui.csv'), header=TRUE)
# scale deprivation indicators
sui$SPedud1 <- scale(sui$Pedud1)
sui$SFamDep1 <- scale(sui$FamDep1)
sui$SIPR1 <- scale(sui$IPR1)
sui$SLowEdu1 <- scale(sui$LowEdu1)
sui$SSingPH1 <- scale(sui$SingPH1)
sui$SUnempR1 <- scale(sui$UnempR1)
# reverse score
sui$SIPR1r <- sui$SIPR1*-1
## drop old threat and aware scores from smaller sample
# sui <- subset(sui, select = -c(Threat,Aware, NoAcpt, Implse, Goals))
######################################################################3333
######################################################################3333
# Harsh SES 4 item CFA 
cfa.harsh <- '
#factor loadings
HSES1g =~ SLowEdu1 + SPedud1 + SFamDep1 + SUnempR1 + SSingPH1 + SIPR1r

SIPR1r ~~ SPedud1
SSingPH1 ~~ SUnempR1
'
fit.h <- cfa(cfa.harsh, data=sui, std.lv=FALSE, missing="ML")
summary(fit.h, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

# Extract the latent factor scores
harsh.scores <- lavPredict(fit.h)

# apppend latent scores and sub id
latent_scores <- cbind(harsh.scores, subID=sui$subID)

# save latent factor summary score to csv
write.csv(latent_scores, "sui_lf_scores_hses1g.csv", row.names=FALSE)
