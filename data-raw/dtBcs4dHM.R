# clean data in bcs70 to use in EM algorithm

# load packages
library(here)
library(data.table)
library(magrittr)

# load ukheEm (including bcs70 data)
# library(ukheEm)

# load data
load(here("data/bcs70.rda"))
load(here("data/bcs70labels.rda"))

# list of variables to be possible instruments
workVars <- bcs70labels$bcs1986x[varNames %like% "^c5a", varNames]
names(workVars) <- bcs70labels$bcs1986x[varNames %like% "^c5a", varLabels]
jobVars <- bcs70labels$bcs1986x[varNames %like% "^c5d", varNames]
names(jobVars) <- bcs70labels$bcs1986x[varNames %like% "^c5d", varLabels]
adultLifeVars <- bcs70labels$bcs1986x[varNames %like% "^c5e", varNames]
names(adultLifeVars) <- bcs70labels$bcs1986x[varNames %like% "^c5e", varLabels]
parQualVars <- bcs70labels$bcs1986x[varNames %like% "^t6", varNames]
names(parQualVars) <- bcs70labels$bcs1986x[varNames %like% "^t6", varLabels]

oldWageVars <- c(
  "t11.2", "t11.9", "c6.14", "sex86"
)

# noncognitive measures
locVarsN <- paste0("c5l", c(1:3, 5, 7, 8, 10, 12:15, 17, 19, 20, 23, 25, 26))
locVarsY <- paste0("c5l", c(9, 22))
ghqVarsP <- paste0("c5i", 1:6)
ghqVarsN <- paste0("c5i", 7:12)

noncogVars <- c("bcsid", "f22score", locVarsN, locVarsY, ghqVarsN, ghqVarsP)

dtNonCog <- bcs70$bcs1986x[, ..noncogVars] %>%
  .[, selfEsteemScore := as.numeric(f22score)] %>%
  .[, locScoreN := rowSums(.SD == "No"), .SDcols = locVarsN] %>%
  .[, locScoreY := rowSums(.SD == "Yes"), .SDcols = locVarsN] %>%
  .[, locScore := locScoreN + locScoreY] %>%
  .[, ghqScoreN := rowSums(sapply(.SD, function(x) (x %in% c("More than usual", "Rather more tn usual")))), .SDcols = ghqVarsN] %>%
  .[, ghqScoreP := rowSums(sapply(.SD, function(x) (x %in% c("Not at all", "Less than usual")))), .SDcols = ghqVarsP] %>%
  .[, ghqScore := ghqScoreP + ghqScoreN]

# merge datasets
dtBcs <- merge(
  bcs70$bcs1986x[, .(
    bcsid, attSchl = q46.1, parInc = oe2,
    .SD
  ), .SDcols = c(workVars, jobVars, adultLifeVars, parQualVars, oldWageVars)],
  bcs70$bcs1986derived[, .(
    bcsid = BCSID,
    readScore = as.numeric(BD4RREAD)
  )],
  by = "bcsid"
) %>%
  merge(
    bcs70$bcs1986_arithmetic_data[, .(
      bcsid,
      mathScore = as.numeric(mathscore)
    )],
    by = "bcsid", all = TRUE
  ) %>%
  merge(
    bcs70$bcs1996x[, .(bcsid, degree = fcase(b960219 == "23", T,
                                             default = F),
                       wkPay = wklypay)],
    by = "bcsid", all = TRUE
  ) %>%
  merge(dtNonCog[, .(bcsid, selfEsteemScore, locScore, ghqScore)],
        by = "bcsid", all = TRUE)


setnames(dtBcs, function(x) stringr::str_remove(x, ".SD."))

# parents quals
posResp <- c("Father", "Mother", "Both")

dtBcs[t6.10 %in% posResp, parHiQual := 0] # no quals
dtBcs[t6.9 %in% posResp, parHiQual := 1] # no quals
dtBcs[t6.7 %in% posResp, parHiQual := 2] # other quals
dtBcs[t6.1 %in% posResp, parHiQual := 3] # apprenticeship
dtBcs[t6.2 %in% posResp, parHiQual := 4] # O-levels
dtBcs[t6.3 %in% posResp, parHiQual := 5] # A-levels
dtBcs[t6.4 %in% posResp, parHiQual := 6] # nurse
dtBcs[t6.5 %in% posResp, parHiQual := 7] # teacher
dtBcs[t6.6 %in% posResp, parHiQual := 8] # degree
dtBcs[, parHiQualF := factor(
  parHiQual, levels = 0:8, labels = c(
    "Unknown", "None", "Other", "Apprent.", "O-levels", "A-levels", "Nurse",
    "Teacher", "Degree+"
  )
)]

oldWageVars <- c(
  "t11.2", "t11.9", "c6.14", "sex86", "parHiQualF"
)

newWageVars <- c(
  "fathSocCls", "mothSocCls", "ethnGrp", "sex", "parHiQualF"
)

setnames(dtBcs, oldWageVars, newWageVars)

# combine maths and reading score
dtBcs[, cogScore := fcase(
  !is.na(mathScore) & !is.na(readScore), mathScore + readScore / 2,
  !is.na(readScore) & is.na(mathScore), readScore,
  !is.na(mathScore) & is.na(readScore), mathScore
)]

cols2keep <- c(
  "bcsid", "degree", "wkPay", "cogScore", "parInc",
  workVars, jobVars, adultLifeVars, parQualVars, newWageVars
)

# drop observations with missing data
dtBcsNoNA <- dtBcs[, ..cols2keep]
dtBcsNoNA <- dtBcsNoNA[complete.cases(dtBcsNoNA)]

# define log pay after university
dtBcsNoNA[, logWkPay := log(wkPay)]

# keep only individuals with wages between 1st and 99th percentiles
wageLims <- dtBcsNoNA[, quantile(wkPay, probs = c(.01, .99), na.rm = TRUE)]
dtBcs4dHM <- dtBcsNoNA[wkPay %between% wageLims]

# using binned parental income data
# needs to be formatted to calculate likelihood directly
# each observation has left and right values corresponding to lower and upper
# bounds of relevant bin

# lower bound
dtBcs4dHM[, left := fcase(
  parInc == "<50    pw/Under 2600 pa", 10,
  parInc == "50-99  pw/2600-5199  pa", 50,
  parInc == "100-149pw/5200-7799  pa", 100,
  parInc == "150-199pw/7800-10399 pa", 150,
  parInc == "200-249pw/10400-12999pa", 200,
  parInc == "250-299pw/13000-15599pa", 250,
  parInc == "300-349pw/15600-18199pa", 300,
  parInc == "350-399pw/18200-20799pa", 350,
  parInc == "400-449pw/20800-23399pa", 400,
  parInc == "450-499pw/23400-25999pa", 450,
  parInc == "500&over /26000&over", 500
)]

# upper bound
dtBcs4dHM[, right := fcase(
  parInc == "<50    pw/Under 2600 pa", 50,
  parInc == "50-99  pw/2600-5199  pa", 99,
  parInc == "100-149pw/5200-7799  pa", 149,
  parInc == "150-199pw/7800-10399 pa", 199,
  parInc == "200-249pw/10400-12999pa", 249,
  parInc == "250-299pw/13000-15599pa", 299,
  parInc == "300-349pw/15600-18199pa", 349,
  parInc == "350-399pw/18200-20799pa", 399,
  parInc == "400-449pw/20800-23399pa", 449,
  parInc == "450-499pw/23400-25999pa", 499,
  parInc == "500&over /26000&over", 600
)]

# take the log of the bounds
dtBcs4dHM[, c("left", "right") := .(log(left), log(right))]

# combine maths and reading score
dtBcs4dHM[, cogScore := fcase(
  !is.na(mathScore) & !is.na(readScore), mathScore + readScore / 2,
  !is.na(readScore) & is.na(mathScore), readScore,
  !is.na(mathScore) & is.na(readScore), mathScore
)]

oldNpVars <- c(
  # about work
  "c5a1", "c5a2", "c5a3", "c5a4", "c5a5", "c5a6", "c5a7", "c5a8", "c5a9",
  # job related
  "c5d1", "c5d2", "c5d3", "c5d4", "c5d5", "c5d6", "c5d7", "c5d8",
  "c5d9", "c5d10", "c5d11", "c5d12", "c5d13", "c5d14", "c5d15", "c5d16",
  # adult life
  "c5e1", "c5e2", "c5e3", "c5e4", "c5e5", "c5e6", "c5e7", "c5e8", "c5e9",
  "c5e10", "c5e11", "c5e12", "c5e13", "c5e14", "c5e15"
)

newNpVars <- c(
  # about work
  "qualHelpJob", "whoKnowHelpJob", "deterHelpJob", "luckGetJob",
  "educOnlyDelayEmp", "bestLvSchlASAP", "planCareerUseless", "takeAnyJob",
  "expGrQuals",
  # job related
  "helpOth", "hiPay", "goodBoss", "wrkOutside", "selfEmp", "jobVaries",
  "jobEasy", "jobAmbi", "jobMath", "tradeProf", "jobQuiet", "jobLT", "jobChall",
  "jobTravel", "jobBuild", "jobRegHrs",
  # adult life
  "moreFun", "ftJob", "selfResp", "notBossed", "canVote", "noDoss", "moveOut",
  "getMarr", "nightClub", "localComm", "xFilms", "drinkAlc", "politics", "kids",
  "doWhatIWant"
  # personality ones to add
)

setnames(dtBcs4dHM, oldNpVars, newNpVars)

# save to \data
use_data(dtBcs4dHM, overwrite = TRUE)

