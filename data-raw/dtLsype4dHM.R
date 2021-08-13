## --------------------------------------------------------------------------- #
## dtLsype4dHM.R
##
## Project: UKHeExp
## Purpose: code to prepare `dtLsype` dataset goes here
## Author: Oliver Cassagneau-Francis
## Date: Tue Aug 10 11:54:30 2021
## --------------------------------------------------------------------------- #
## Notes:

load_all()

library(data.table)
library(magrittr)

lsype1YP <- lapply(lsype1YP, as.data.table)
lsype1FB <- lapply(lsype1FB, as.data.table)

# renaming
benefitVars <- names(lsype1YP[[4]])[grep('Benefits', names(lsype1YP[[4]]))]
costVars <- names(lsype1YP[[4]])[grep('Costs', names(lsype1YP[[4]]))]

newBenefitVars <- c(
  'betterJob', 'betterPay', 'betterOpp', 'need4Career', 'showSkills',
  'delayWorkGd', 'socialLifeGd', 'leaveHomeGd', 'keepStudy', 'moreQuals',
  'persDevel', 'moreConfid', 'moreRespect', 'betterLife', 'prepareLife'
)

newCostVars <- c(
  'tooExpensive', 'getDebt', 'parentsMoney', 'notFinancIndep', 'notWorkEarn',
  'costsGen', 'noGuaranteeGdJb', 'notNeed4Career', 'lessExp', 'tooHard',
  'leaveHomeBad', 'tooLong', 'wasteTime', 'feesEtc', 'stress'
)

old <- c(benefitVars[1:15], costVars[1:15])

new <- c(newBenefitVars, newCostVars)

setnames(lsype1YP[[4]], old, new)

# variables to keep from different waves
w4YPcols2keep <- c(
  'NSID', new, 'W4MainActYP', 'W4AlevNoYP', 'W4SexYP'
)

w5YPcols2keep <- c(
  'NSID', 'W5AlevNoYP', 'W5EMA1YP', 'W5debtattYP', 'W5actYP', 'W5ScollegeYP',
  'W5EducYP'
)

w6YPcols2keep <- c(
  'NSID', 'W6TCurrentAct'
)

w7YPcols2keep <- c(
  'NSID', 'W7TCurrentAct'
)

w8YPcols2keep <- c(
  'NSID', 'W8DACTIVITY', 'W8DDEGP', 'W8GROW', 'W8WRKHRS', 'W8NETW',
  # 'W8USLW', 'W8HOSJ',
  'W8DHANVQH'
)

w4FBcols2keep <- c(
  'NSID', 'w4SOCMajorMP', 'w4SOCMajorSP', 'w4ethgrpMP', 'w4ethgrpSP',
  'w4hiqualgMP', 'w4hiqualgSP', 'w4sexMP', 'w4sexSP'
)

# cognitive measure
# while waiting to get hold of the NPD I will use as measures of cognitive ability:
# - self-reported "good marks" [W1-3YYS12YP]
# - whether they are "good" at maths, english, science and ICT [W1g<subj>YP]
dtCog <- lsype1YP[[1]][, .(
  NSID,
  gdMath = W1gmathYP,
  gdMath_num = fcase(
    W1gmathYP == "No good at all", 0,
    W1gmathYP == "Not very good", 1,
    W1gmathYP == "Fairly good", 2,
    W1gmathYP == "Very good", 3
  ),
  gdEngl = W1gengYP,
  gdEngl_num = fcase(
    W1gengYP == "No good at all", 0,
    W1gengYP == "Not very good", 1,
    W1gengYP == "Fairly good", 2,
    W1gengYP == "Very good", 3
  ),
  gdSci = W1gsciYP,
  gdSci_num = fcase(
    W1gsciYP == "No good at all", 0,
    W1gsciYP == "Not very good", 1,
    W1gsciYP == "Fairly good", 2,
    W1gsciYP == "Very good", 3
  ),
  gdIct = W1gictYP,
  gdIct_num = fcase(
    W1gictYP == "No good at all", 0,
    W1gictYP == "Not very good", 1,
    W1gictYP == "Fairly good", 2,
    W1gictYP == "Very good", 3
  ),
  gdMarksW1 = W1yys12YP,
  gdMarksW1_num = fcase(
    W1yys12YP == "Strongly disagree", 0,
    W1yys12YP == "Disagree", 1,
    W1yys12YP == "Agree", 2,
    W1yys12YP == "Strongly agree", 3
  )
)] %>%
  merge(
    lsype1YP[[2]][, .(
      NSID,
      gdMarksW2 = W2YYS12YP,
      gdMarksW2_num = fcase(
        W2YYS12YP == "Strongly disagree", 0,
        W2YYS12YP == "Disagree", 1,
        W2YYS12YP == "Agree", 2,
        W2YYS12YP == "Strongly agree", 3
      )
    )]
  ) %>%
  merge(
    lsype1YP[[3]][, .(
      NSID,
      gdMarksW3 = W3yys12YP,
      gdMarksW3_num = fcase(
        W3yys12YP == "Strongly disagree", 0,
        W3yys12YP == "Disagree", 1,
        W3yys12YP == "Agree", 2,
        W3yys12YP == "Strongly agree", 3
      )
    )]
  )

stdise <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

dtCog[, cogScore := stdise(rowSums(.SD)),
      .SDcols = c("gdMath_num", "gdEngl_num", "gdSci_num", "gdIct_num",
                  "gdMarksW1_num", "gdMarksW2_num", "gdMarksW3_num")]

# nested merge() to join wave on NSID
dtLsype4dHM <- lsype1YP[[4]][, ..w4YPcols2keep] %>%
  merge(
    lsype1YP[[5]][, ..w5YPcols2keep],
    by = 'NSID', all = TRUE
  ) %>%
  merge(
    lsype1YP[[6]][, ..w6YPcols2keep],
    by = 'NSID', all = TRUE
  ) %>%
  merge(
    lsype1YP[[7]][, ..w7YPcols2keep],
    by = 'NSID', all = TRUE
  ) %>%
  merge(
    lsype1YP[[8]][, ..w8YPcols2keep],
    by = 'NSID', all = TRUE
  ) %>%
  merge(
    lsype1FB[[4]][, ..w4FBcols2keep],
    by = 'NSID', all = TRUE
  ) %>%
  merge(
    dtCog[, .(
      NSID,
      gdMath_num, gdSci_num, gdIct_num, gdEngl_num,
      gdMarksW1_num, gdMarksW2_num, gdMarksW3_num,
      cogScore
    )],
    by = "NSID", all = TRUE
  )

# drop missing variables etc
dtLsype4dHM <- dtLsype4dHM[
  !(is.na(W8GROW) & is.na(W8NETW)) & !is.na(betterJob) & !is.na(cogScore) &
    W4MainActYP == 'Going to a school or college full time',
]

wageQs <- dtLsype4dHM[, quantile(W8NETW, probs = c(.01, .99), na.rm = TRUE)]
minWage <- wageQs[[1]]
maxWage <- wageQs[[2]]

dtLsype4dHM <- dtLsype4dHM[W8NETW > minWage & W8NETW < maxWage, ]

dtLsype4dHM[, logHourWage := log(W8GROW / W8WRKHRS)]

dtLsype4dHM[W8DHANVQH %in% c('NVQ Level 4', 'NVQ Level 5'),
           c('logWageUni', 'degree') := .(logHourWage, TRUE)]

dtLsype4dHM[!(W8DHANVQH %in% c('NVQ Level 4', 'NVQ Level 5')),
           c('logWageHS', 'degree') := .(logHourWage, FALSE)]

dtLsype4dHM <- dtLsype4dHM[logHourWage < Inf & logHourWage > -Inf, ]

cols2keep <- c("NSID", newBenefitVars, newCostVars,
               c('w4SOCMajorMP', 'w4ethgrpMP',
                 'w4hiqualgMP', 'W5AlevNoYP', 'W5EMA1YP',
                 'W4AlevNoYP', 'w4sexMP', 'W4SexYP',
                 "cogScore"), "logHourWage", "degree")

dtLsype4dHM <- dtLsype4dHM[, ..cols2keep]
dtLsype4dHM <- dtLsype4dHM[complete.cases(dtLsype4dHM)]

usethis::use_data(dtLsype4dHM, overwrite = TRUE)
