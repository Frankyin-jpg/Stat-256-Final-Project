# ---
# title: "777"
# author: "Frank Yin"
# format: html
# editor: visual
# ---

## Importing raw data

library(haven)
marcps_w <- read_sas("marcps_w.sas7bdat")

names(marcps_w)

summary(marcps_w)

## 1. Data cleaning and replicating Table 1

library(dplyr)

##--------------------------------------------------
## Helper: weighted mean (PROC SUMMARY replacement)
##--------------------------------------------------
wmean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

##--------------------------------------------------
## DATA STEP TRANSLATION (raw dataset = marcps_w)
##--------------------------------------------------

one <- marcps_w %>%
  ## Restrict ages 21–58
  filter(AGE >= 21, AGE <= 58) %>%
  
  mutate(
    ## Wage variables
    LNWKWAGE = ifelse(WKSWORK > 0 & WSAL_VAL > 0,
                      log(WSAL_VAL / WKSWORK),
                      NA_real_),

    WKWAGE   = ifelse(WKSWORK > 0, WSAL_VAL / WKSWORK, NA_real_),
    TOTWAGE  = WSAL_VAL,
    JOBWAGE  = ERN_VAL,

    ## Weights (divide by 100)
    FNLWGT   = FNLWGT   / 100,
    MARSUPWT = MARSUPWT / 100,
    FNLWGT2  = FNLWGT2  / 100,

    ## Age groups and dummies
    AGEGRP = 10 * floor(AGE / 10),
    AGE20  = as.numeric(AGEGRP == 20),
    AGE30  = as.numeric(AGEGRP == 30),
    AGE40  = as.numeric(AGEGRP == 40),
    AGE50  = as.numeric(AGEGRP == 50),

    SAMPLE = ifelse(AGE < 40, "young", "old"),

    ## Race
    RACEGRP = ifelse(RACE >= 3, 3, RACE),
    WHITE   = as.numeric(RACE == 1),
    BLACK   = as.numeric(RACE == 2),
    OTHER   = as.numeric(RACE == 3),

    ## CPI-W index normalized to 1988
    CPIW = dplyr::case_when(
      YEAR == 88 ~ 117,
      YEAR == 89 ~ 122.6,
      YEAR == 90 ~ 129.0,
      YEAR == 91 ~ 134.3,
      YEAR == 92 ~ 138.2,
      YEAR == 93 ~ 142.1,
      YEAR == 94 ~ 145.6,
      YEAR == 95 ~ 149.8,
      YEAR == 96 ~ 154.1,
      YEAR == 97 ~ 157.6,
      TRUE       ~ NA_real_
    ) / 117,

    RLWKWAGE = WKWAGE / CPIW,
    WKWAGE   = WKWAGE / CPIW,

    ## Outlier removal (<25 or >2000 real weekly wage)
    LNWKWAGE = ifelse(RLWKWAGE < 25 | RLWKWAGE > 2000, NA_real_, LNWKWAGE),
    WKWAGE   = ifelse(RLWKWAGE < 25 | RLWKWAGE > 2000, NA_real_, WKWAGE),

    ## Federal benefits
    DI   = as.numeric((SS_VAL / (CPIW * 52)) > 75),
    OAS  = as.numeric(SS_YN == 1 & DI == 0),
    SSI  = as.numeric(SSI_YN == 1),
    SSIORDI = as.numeric(SSI == 1 | DI == 1),
    OASDISSI = as.numeric(SS_YN == 1 | SSI_YN == 1),

    OTHERDIS = as.numeric(DIS_YN == 1),

    ## VA variables
    VETCOMP = as.numeric(VET_TYP1 == 1),
    VETSURV = as.numeric(VET_TYP2 == 1),
    VETPENS = as.numeric(VET_TYP3 == 1),
    VETEDUC = as.numeric(VET_TYP4 == 1),
    VETOTHR = as.numeric(VET_TYP5 == 1),
    VETQVA  = as.numeric(VET_QVA == 1),
    ANYVA   = as.numeric(VET_YN == 1),

    ## Disability classifications
    FGDI   = as.numeric(DIS_SC1 == 3 | DIS_SC2 == 3),
    MILDI  = as.numeric(DIS_SC1 == 4 | DIS_SC2 == 4),
    USRRDI = as.numeric(DIS_SC1 == 6 | DIS_SC2 == 6),
    AFDC   = 0,

    OTHERFED = as.numeric(FGDI == 1 | MILDI == 1 | USRRDI == 1 | AFDC == 1),

    ANYFED   = as.numeric(OASDISSI == 1 | ANYVA == 1 | OTHERFED == 1),
    MEANSTST = as.numeric(
      OASDISSI == 1 |
        (ANYVA == 1 & VETQVA == 1) |
        AFDC == 1
    ),

    ## Instruments
    VETCOMP2 = as.numeric(VETCOMP == 1 | FGDI == 1 | MILDI == 1 | USRRDI == 1),
    VETCOMP3 = as.numeric(ANYVA == 1 & VETQVA == 0),

    ## Demographics
    MARRIED = as.numeric(MARITAL >= 1 & MARITAL <= 3),
    WIDOWED = as.numeric(MARITAL == 4),
    DIVSEP  = as.numeric(MARITAL >= 5 & MARITAL <= 6),

    VETERAN  = as.numeric(VET >= 1 & VET <= 5),
    VIETSERV = as.numeric(VET == 1),
    KORASERV = as.numeric(VET == 2),
    OTHRSERV = as.numeric(VET >= 3 & VET <= 5),

    ## Trend
    TREND  = YEAR - 87,
    TREND2 = (YEAR - 87)^2,

    DIS_YR92 = as.numeric(YEAR == 92) * DISABL1,
    DIS_YR93 = as.numeric(YEAR == 93) * DISABL1,
    DIS_YR94 = as.numeric(YEAR == 94) * DISABL1,
    DIS_YR95 = as.numeric(YEAR == 95) * DISABL1,
    DIS_YR96 = as.numeric(YEAR == 96) * DISABL1,
    DIS_YR97 = as.numeric(YEAR == 97) * DISABL1,

    DYR_9497 = DISABL1 * as.numeric(YEAR >= 94 & YEAR <= 97),

    ## Year dummies
    YR89 = as.numeric(YEAR == 89),
    YR90 = as.numeric(YEAR == 90),
    YR91 = as.numeric(YEAR == 91),
    YR92 = as.numeric(YEAR == 92),
    YR93 = as.numeric(YEAR == 93),
    YR94 = as.numeric(YEAR == 94),
    YR95 = as.numeric(YEAR == 95),
    YR96 = as.numeric(YEAR == 96),
    YR97 = as.numeric(YEAR == 97),

    TREND_D = TREND * DISABL1,

    AGE2  = AGE^2,
    POSTHS = as.numeric(SOMECO == 1 | COLGRAD == 1),

    SOUTH = as.numeric(REGION >= 5 & REGION <= 7),
    WEST  = as.numeric(REGION >= 8 & REGION <= 9)
  ) %>%
  
  ## Keep even years only
  filter(YEAR %% 2 == 0)

##--------------------------------------------------
## PROC SUMMARY equivalent
##--------------------------------------------------

two <- one %>%
  group_by(DISABL1, SEX, SAMPLE, YEAR) %>%
  summarise(
    AGE     = wmean(AGE, FNLWGT2),
    WHITE   = wmean(WHITE, FNLWGT2),
    POSTHS  = wmean(POSTHS, FNLWGT2),
    WORKING = wmean(WORKING, FNLWGT2),
    WKSWORK = wmean(WKSWORK, FNLWGT2),
    WKWAGE  = wmean(WKWAGE, FNLWGT2),
    SSIORDI = wmean(SSIORDI, FNLWGT2),
    .groups = "drop"
  ) %>%
  arrange(desc(DISABL1), SEX, desc(SAMPLE), YEAR)

print(two)

## 2. Replicating Table 2

library(dplyr)


one <- marcps_w %>%
  # if (21 <= AGE <= 58);
  filter(AGE >= 21, AGE <= 58) %>%
  
  mutate(
    # SAMPLE = 'young' or 'old'
    sample = ifelse(AGE < 40, "young", "old"),

    # Wage variables
    lnwkwage = ifelse(WKSWORK > 0 & WSAL_VAL > 0,
                      log(WSAL_VAL / WKSWORK),
                      NA_real_),
    wkwage   = ifelse(WKSWORK > 0 & WSAL_VAL > 0,
                      WSAL_VAL / WKSWORK,
                      NA_real_),
    totwage  = WSAL_VAL,
    jobwage  = ERN_VAL,

    # Age groups
    agegrp = 10L * floor(AGE / 10),
    age20  = as.numeric(agegrp == 20),
    age30  = as.numeric(agegrp == 30),
    age40  = as.numeric(agegrp == 40),
    age50  = as.numeric(agegrp == 50),

    # Race group
    racegrp = ifelse(RACE >= 3, 3L, RACE),

    # Education groups
    educgrp = case_when(
      LESSHS == 1 ~ 1L,
      HSGRAD == 1 ~ 2L,
      SOMECO == 1 | COLGRAD == 1 ~ 3L,
      TRUE ~ NA_integer_
    ),

    # Work indicator
    workly = as.numeric(WKSWORK > 0),

    # Labor force codes
    lfin1 = ifelse(WKSWORK == 0, WORKING, NA_real_),
    lfin2 = ifelse(WKSWORK < 50, WORKING, NA_real_),
    lfout1 = ifelse(WKSWORK >= 50, 1 - WORKING, NA_real_),
    changer = ifelse(WKSWORK == 0, NA_real_, NA_real_),

    # Trend variables
    trend  = YEAR - 87,
    trend2 = (YEAR - 87)^2,

    dis_trend = trend * DISABL1,
    dis_yr89 = as.numeric(YEAR == 89) * DISABL1,
    dis_yr90 = as.numeric(YEAR == 90) * DISABL1,
    dis_yr91 = as.numeric(YEAR == 91) * DISABL1,
    dis_yr92 = as.numeric(YEAR == 92) * DISABL1,
    dis_yr93 = as.numeric(YEAR == 93) * DISABL1,
    dis_yr94 = as.numeric(YEAR == 94) * DISABL1,
    dis_yr95 = as.numeric(YEAR == 95) * DISABL1,
    dis_yr96 = as.numeric(YEAR == 96) * DISABL1,
    dis_yr97 = as.numeric(YEAR == 97) * DISABL1,

    dyr_9497 = as.numeric(YEAR >= 94 & YEAR <= 97) * DISABL1,

    # CPI-W coding
    cpiw = case_when(
      YEAR == 88 ~ 117,
      YEAR == 89 ~ 122.6,
      YEAR == 90 ~ 129.0,
      YEAR == 91 ~ 134.3,
      YEAR == 92 ~ 138.2,
      YEAR == 93 ~ 142.1,
      YEAR == 94 ~ 145.6,
      YEAR == 95 ~ 149.8,
      YEAR == 96 ~ 154.1,
      YEAR == 97 ~ 157.6,
      TRUE ~ NA_real_
    ),
    cpiw88 = cpiw / 117,
    rlwkwage = wkwage / cpiw88
  ) %>%
  mutate(
    # Kill wage outliers
    lnwkwage = ifelse(rlwkwage < 25 | rlwkwage > 2000, NA_real_, lnwkwage),
    wkwage   = ifelse(rlwkwage < 25 | rlwkwage > 2000, NA_real_, wkwage)
  ) %>%
  select(
    AGE, agegrp, YEAR, SEX, DISABL1, totwage, jobwage, sample,
    WORKING, UNEMPL, NILF, JOBLOSER, WKSWORK, workly,
    HSGRAD, SOMECO, COLGRAD, REGION, CENTRALC, BALMSA, trend,
    dis_yr89, dis_yr90, dis_yr91, dis_yr92, dis_yr93, dis_yr94,
    dis_yr95, dis_yr96, dis_yr97, dyr_9497, FNLWGT,
    age20, age30, age40, age50, NOWEEKS, racegrp, lnwkwage, REGION,
    HG_ST60, LESSHS, HSGRAD, SOMECO, COLGRAD, dis_trend,
    cpiw, cpiw88, rlwkwage, wkwage, FNLWGT2, educgrp
  )

one <- one %>%
  arrange(sample, desc(YEAR), desc(DISABL1))

one_men   <- subset(one, SEX == 1)
one_women <- subset(one, SEX == 2)

men_young   <- subset(one_men, sample == "young")
men_old     <- subset(one_men, sample == "old")
women_young <- subset(one_women, sample == "young")
women_old   <- subset(one_women, sample == "old")

### OLS for no trend models

# Men, Young
mod_men_young_wks_no_trend <- lm(
  WKSWORK ~ factor(YEAR) + factor(agegrp) + factor(racegrp) +
    factor(educgrp) + factor(REGION) +
    factor(YEAR):factor(agegrp) +
    factor(YEAR):factor(racegrp) +
    factor(YEAR):factor(educgrp) +
    factor(YEAR):factor(REGION) +
    DISABL1 +
    dis_yr89 + dis_yr90 + dis_yr91 +
    dis_yr92 + dis_yr93 + dis_yr94 +
    dis_yr95 + dis_yr96 + dis_yr97,
  data = men_young,
  weights = FNLWGT2
)

# Men, Old
mod_men_old_wks_no_trend <- lm(
  WKSWORK ~ factor(YEAR) + factor(agegrp) + factor(racegrp) +
    factor(educgrp) + factor(REGION) +
    factor(YEAR):factor(agegrp) +
    factor(YEAR):factor(racegrp) +
    factor(YEAR):factor(educgrp) +
    factor(YEAR):factor(REGION) +
    DISABL1 +
    dis_yr89 + dis_yr90 + dis_yr91 +
    dis_yr92 + dis_yr93 + dis_yr94 +
    dis_yr95 + dis_yr96 + dis_yr97,
  data = men_old,
  weights = FNLWGT2
)

# Woman, Young
mod_women_young_wks_no_trend <- lm(
  WKSWORK ~ factor(YEAR) + factor(agegrp) + factor(racegrp) +
    factor(educgrp) + factor(REGION) +
    factor(YEAR):factor(agegrp) +
    factor(YEAR):factor(racegrp) +
    factor(YEAR):factor(educgrp) +
    factor(YEAR):factor(REGION) +
    DISABL1 +
    dis_yr89 + dis_yr90 + dis_yr91 +
    dis_yr92 + dis_yr93 + dis_yr94 +
    dis_yr95 + dis_yr96 + dis_yr97,
  data = women_young,
  weights = FNLWGT2
)

# Woman, Old
mod_women_old_wks_no_trend <- lm(
  WKSWORK ~ factor(YEAR) + factor(agegrp) + factor(racegrp) +
    factor(educgrp) + factor(REGION) +
    factor(YEAR):factor(agegrp) +
    factor(YEAR):factor(racegrp) +
    factor(YEAR):factor(educgrp) +
    factor(YEAR):factor(REGION) +
    DISABL1 +
    dis_yr89 + dis_yr90 + dis_yr91 +
    dis_yr92 + dis_yr93 + dis_yr94 +
    dis_yr95 + dis_yr96 + dis_yr97,
  data = women_old,
  weights = FNLWGT2
)

### GLM with Trend

# Men, Young
mod_men_young_wks_trend <- lm(
  WKSWORK ~ factor(YEAR) + factor(agegrp) + factor(racegrp) +
    factor(educgrp) + factor(REGION) +
    factor(YEAR):factor(agegrp) +
    factor(YEAR):factor(racegrp) +
    factor(YEAR):factor(educgrp) +
    factor(YEAR):factor(REGION) +
    DISABL1 +
    dis_yr92 + dis_yr93 + dis_yr94 +
    dis_yr95 + dis_yr96 + dis_yr97 +
    dis_trend,
  data = men_young,
  weights = FNLWGT2
)

# Men, Old
mod_men_old_wks_trend <- lm(
  WKSWORK ~ factor(YEAR) + factor(agegrp) + factor(racegrp) +
    factor(educgrp) + factor(REGION) +
    factor(YEAR):factor(agegrp) +
    factor(YEAR):factor(racegrp) +
    factor(YEAR):factor(educgrp) +
    factor(YEAR):factor(REGION) +
    DISABL1 +
    dis_yr92 + dis_yr93 + dis_yr94 +
    dis_yr95 + dis_yr96 + dis_yr97 +
    dis_trend,
  data = men_old,
  weights = FNLWGT2
)

# Women, Young
mod_women_young_wks_trend <- lm(
  WKSWORK ~ factor(YEAR) + factor(agegrp) + factor(racegrp) +
    factor(educgrp) + factor(REGION) +
    factor(YEAR):factor(agegrp) +
    factor(YEAR):factor(racegrp) +
    factor(YEAR):factor(educgrp) +
    factor(YEAR):factor(REGION) +
    DISABL1 +
    dis_yr92 + dis_yr93 + dis_yr94 +
    dis_yr95 + dis_yr96 + dis_yr97 +
    dis_trend,
  data = women_young,
  weights = FNLWGT2
)

# Woman, Old
mod_women_old_wks_trend <- lm(
  WKSWORK ~ factor(YEAR) + factor(agegrp) + factor(racegrp) +
    factor(educgrp) + factor(REGION) +
    factor(YEAR):factor(agegrp) +
    factor(YEAR):factor(racegrp) +
    factor(YEAR):factor(educgrp) +
    factor(YEAR):factor(REGION) +
    DISABL1 +
    dis_yr92 + dis_yr93 + dis_yr94 +
    dis_yr95 + dis_yr96 + dis_yr97 +
    dis_trend,
  data = women_old,
  weights = FNLWGT2
)

## 3. Replicating Table 3

library(dplyr)

one <- marcps_w %>%
  # 21 <= AGE <= 58
  filter(AGE >= 21, AGE <= 58) %>%
  
  mutate(
    ## SAMPLE: young vs old
    sample = ifelse(AGE < 40, "young", "old"),
    
    ## Wage variables
    lnwkwage = ifelse(WKSWORK > 0 & WSAL_VAL > 0,
                      log(WSAL_VAL / WKSWORK),
                      NA_real_),
    wkwage   = ifelse(WKSWORK > 0 & WSAL_VAL > 0,
                      WSAL_VAL / WKSWORK,
                      NA_real_),
    totwage  = WSAL_VAL,
    jobwage  = ERN_VAL,
    
    ## Age group & dummies
    agegrp = 10L * floor(AGE / 10),
    age20  = as.numeric(agegrp == 20),
    age30  = as.numeric(agegrp == 30),
    age40  = as.numeric(agegrp == 40),
    age50  = as.numeric(agegrp == 50),
    
    ## Race group: collapse 3+
    racegrp = ifelse(RACE >= 3, 3L, RACE),
    
    ## Educ group: 1 = <HS, 2 = HS, 3 = some college/college grad
    educgrp = case_when(
      LESSHS == 1 ~ 1L,
      HSGRAD == 1 ~ 2L,
      SOMECO == 1 | COLGRAD == 1 ~ 3L,
      TRUE ~ NA_integer_
    ),
    
    ## Worked at all
    workly = as.numeric(WKSWORK > 0),
    
    ## Labor-force variants
    lfin1  = ifelse(WKSWORK == 0, WORKING, NA_real_),
    lfin2  = ifelse(WKSWORK < 50, WORKING, NA_real_),
    lfout1 = ifelse(WKSWORK >= 50, 1 - WORKING, NA_real_),
    changer = ifelse(WKSWORK == 0, NA_real_, NA_real_),
    
    ## Time trend
    trend  = YEAR - 87,
    trend2 = (YEAR - 87)^2,
    
    dis_trend = trend * DISABL1,
    dis_yr89  = as.numeric(YEAR == 89) * DISABL1,
    dis_yr90  = as.numeric(YEAR == 90) * DISABL1,
    dis_yr91  = as.numeric(YEAR == 91) * DISABL1,
    dis_yr92  = as.numeric(YEAR == 92) * DISABL1,
    dis_yr93  = as.numeric(YEAR == 93) * DISABL1,
    dis_yr94  = as.numeric(YEAR == 94) * DISABL1,
    dis_yr95  = as.numeric(YEAR == 95) * DISABL1,
    dis_yr96  = as.numeric(YEAR == 96) * DISABL1,
    dis_yr97  = as.numeric(YEAR == 97) * DISABL1,
    
    dyr_9497 = as.numeric(YEAR >= 94 & YEAR <= 97) * DISABL1,
    
    ## --- dis_* interactions by race/region/age/education (macro %code in SAS) ---
    dis_racegrp1  = DISABL1 * (racegrp == 1),
    dis_racegrp2  = DISABL1 * (racegrp == 2),
    dis_racegrp3  = DISABL1 * (racegrp == 3),
    
    dis_region1 = DISABL1 * (REGION == 1),
    dis_region2 = DISABL1 * (REGION == 2),
    dis_region3 = DISABL1 * (REGION == 3),
    dis_region4 = DISABL1 * (REGION == 4),
    dis_region5 = DISABL1 * (REGION == 5),
    dis_region6 = DISABL1 * (REGION == 6),
    dis_region7 = DISABL1 * (REGION == 7),
    dis_region8 = DISABL1 * (REGION == 8),
    dis_region9 = DISABL1 * (REGION == 9),
    
    dis_agegrp20 = DISABL1 * (agegrp == 20),
    dis_agegrp30 = DISABL1 * (agegrp == 30),
    dis_agegrp40 = DISABL1 * (agegrp == 40),
    dis_agegrp50 = DISABL1 * (agegrp == 50),
    
    dis_educgrp1 = DISABL1 * (educgrp == 1),
    dis_educgrp2 = DISABL1 * (educgrp == 2),
    dis_educgrp3 = DISABL1 * (educgrp == 3),
    
    ## --- year_d* dummies (macro %code2 in SAS) ---
    year_d89 = as.numeric(YEAR == 89),
    year_d90 = as.numeric(YEAR == 90),
    year_d91 = as.numeric(YEAR == 91),
    year_d92 = as.numeric(YEAR == 92),
    year_d93 = as.numeric(YEAR == 93),
    year_d94 = as.numeric(YEAR == 94),
    year_d95 = as.numeric(YEAR == 95),
    year_d96 = as.numeric(YEAR == 96),
    year_d97 = as.numeric(YEAR == 97),
    
    ## CPI-W and real weekly wage
    cpiw = case_when(
      YEAR == 88 ~ 117,
      YEAR == 89 ~ 122.6,
      YEAR == 90 ~ 129.0,
      YEAR == 91 ~ 134.3,
      YEAR == 92 ~ 138.2,
      YEAR == 93 ~ 142.1,
      YEAR == 94 ~ 145.6,
      YEAR == 95 ~ 149.8,
      YEAR == 96 ~ 154.1,
      YEAR == 97 ~ 157.6,
      TRUE       ~ NA_real_
    ),
    cpiw88   = cpiw / 117,
    rlwkwage = wkwage / cpiw88
  ) %>%
  # Kill wage outliers
  mutate(
    lnwkwage = ifelse(rlwkwage < 25 | rlwkwage > 2000, NA_real_, lnwkwage),
    wkwage   = ifelse(rlwkwage < 25 | rlwkwage > 2000, NA_real_, wkwage)
  ) %>%
  # Cell identifiers
  mutate(
    classid  = 1000 * agegrp + 100 * racegrp + 10 * educgrp + REGION,
    sampsex  = paste0(sample, SEX),
    classid2 = paste0(classid, sample, SEX),
    count    = 1L
  ) %>%
  # Keep only variables listed in KEEP
  select(
    AGE, agegrp, YEAR, SEX, DISABL1, totwage, jobwage, sample,
    WORKING, UNEMPL, NILF, JOBLOSER, WKSWORK, workly,
    HSGRAD, SOMECO, COLGRAD, REGION, CENTRALC, BALMSA, trend,
    dis_yr89, dis_yr90, dis_yr91, dis_yr92, dis_yr93, dis_yr94,
    dis_yr95, dis_yr96, dis_yr97, dyr_9497, FNLWGT,
    dis_racegrp1, dis_racegrp2, dis_racegrp3,
    dis_region1, dis_region2, dis_region3, dis_region4, dis_region5,
    dis_region6, dis_region7, dis_region8, dis_region9,
    dis_educgrp1, dis_educgrp2, dis_educgrp3,
    dis_agegrp20, dis_agegrp30, dis_agegrp40, dis_agegrp50,
    age20, age30, age40, age50, NOWEEKS, racegrp, lnwkwage, REGION,
    HG_ST60, LESSHS, HSGRAD, SOMECO, COLGRAD, dis_trend, count,
    cpiw, cpiw88, rlwkwage, wkwage, FNLWGT2, educgrp, classid,
    sampsex, classid2,
    year_d89, year_d90, year_d91, year_d92, year_d93, year_d94,
    year_d95, year_d96, year_d97
  )

# PROC MEANS
summary(one)

# PROC SORT BY SAMPLE DESC YEAR DESC DISABL1
one <- one %>%
  arrange(sample, desc(YEAR), desc(DISABL1))

### Col-1. Baseline Specification

# subsets
one_men      <- subset(one, SEX == 1)
one_women_y  <- subset(one, SEX == 2 & sample == "young")

# men, sample = young / old (BY sample)
men_young <- subset(one_men, sample == "young")
men_old   <- subset(one_men, sample == "old")

# Men, baseline spec (Table 3 col 1)
mod_men_young_col1 <- lm(
  WKSWORK ~
    factor(YEAR) + factor(agegrp) + factor(racegrp) +
    factor(educgrp) + factor(REGION) +
    factor(YEAR):factor(agegrp) +
    factor(YEAR):factor(racegrp) +
    factor(YEAR):factor(educgrp) +
    factor(YEAR):factor(REGION) +
    DISABL1 +
    dis_yr92 + dis_yr93 + dis_yr94 +
    dis_yr95 + dis_yr96 + dis_yr97,
  data    = men_young,
  weights = FNLWGT2
)

mod_men_old_col1 <- lm(
  WKSWORK ~
    factor(YEAR) + factor(agegrp) + factor(racegrp) +
    factor(educgrp) + factor(REGION) +
    factor(YEAR):factor(agegrp) +
    factor(YEAR):factor(racegrp) +
    factor(YEAR):factor(educgrp) +
    factor(YEAR):factor(REGION) +
    DISABL1 +
    dis_yr92 + dis_yr93 + dis_yr94 +
    dis_yr95 + dis_yr96 + dis_yr97,
  data    = men_old,
  weights = FNLWGT2
)

# Women, baseline (only young)
mod_women_young_col1 <- lm(
  WKSWORK ~
    factor(YEAR) + factor(agegrp) + factor(racegrp) +
    factor(educgrp) + factor(REGION) +
    factor(YEAR):factor(agegrp) +
    factor(YEAR):factor(racegrp) +
    factor(YEAR):factor(educgrp) +
    factor(YEAR):factor(REGION) +
    DISABL1 +
    dis_yr92 + dis_yr93 + dis_yr94 +
    dis_yr95 + dis_yr96 + dis_yr97,
  data    = one_women_y,
  weights = FNLWGT2
)

### Col-2. " No controls"

# Men, no control (Table 3 col 2)
mod_men_young_col2 <- lm(
  WKSWORK ~ factor(YEAR) + DISABL1 +
    dis_yr92 + dis_yr93 + dis_yr94 +
    dis_yr95 + dis_yr96 + dis_yr97,
  data    = men_young,
  weights = FNLWGT2
)

mod_men_old_col2 <- lm(
  WKSWORK ~ factor(YEAR) + DISABL1 +
    dis_yr92 + dis_yr93 + dis_yr94 +
    dis_yr95 + dis_yr96 + dis_yr97,
  data    = men_old,
  weights = FNLWGT2
)

# Women, young, no control
mod_women_young_col2 <- lm(
  WKSWORK ~ factor(YEAR) + DISABL1 +
    dis_yr92 + dis_yr93 + dis_yr94 +
    dis_yr95 + dis_yr96 + dis_yr97,
  data    = one_women_y,
  weights = FNLWGT2
)

### Col-3. Regression controls with disabilityXcovariates  interaction

# Men, young, regression controls
mod_men_young_col3 <- lm(
  WKSWORK ~
    factor(YEAR) + factor(agegrp) + factor(racegrp) +
    factor(educgrp) + factor(REGION) +
    factor(YEAR):factor(agegrp) +
    factor(YEAR):factor(racegrp) +
    factor(YEAR):factor(educgrp) +
    factor(YEAR):factor(REGION) +
    dis_racegrp1 + dis_racegrp2 +
    dis_region1  + dis_region2 + dis_region3 + dis_region4 +
    dis_region5  + dis_region6 + dis_region7 + dis_region8 +
    dis_educgrp1 + dis_educgrp2 +
    dis_agegrp20 +
    DISABL1 +
    dis_yr92 + dis_yr93 + dis_yr94 +
    dis_yr95 + dis_yr96 + dis_yr97,
  data    = men_young,
  weights = FNLWGT2
)

# Men, old, regression controls
mod_men_old_col3 <- lm(
  WKSWORK ~
    factor(YEAR) + factor(agegrp) + factor(racegrp) +
    factor(educgrp) + factor(REGION) +
    factor(YEAR):factor(agegrp) +
    factor(YEAR):factor(racegrp) +
    factor(YEAR):factor(educgrp) +
    factor(YEAR):factor(REGION) +
    dis_racegrp1 + dis_racegrp2 +
    dis_region1  + dis_region2 + dis_region3 + dis_region4 +
    dis_region5  + dis_region6 + dis_region7 + dis_region8 +
    dis_educgrp1 + dis_educgrp2 +
    dis_agegrp40 +
    DISABL1 +
    dis_yr92 + dis_yr93 + dis_yr94 +
    dis_yr95 + dis_yr96 + dis_yr97,
  data    = men_old,
  weights = FNLWGT2
)

# Women, young, regression controls
mod_women_young_col3 <- lm(
  WKSWORK ~
    factor(YEAR) + factor(agegrp) + factor(racegrp) +
    factor(educgrp) + factor(REGION) +
    factor(YEAR):factor(agegrp) +
    factor(YEAR):factor(racegrp) +
    factor(YEAR):factor(educgrp) +
    factor(YEAR):factor(REGION) +
    dis_racegrp1 + dis_racegrp2 +
    dis_region1  + dis_region2 + dis_region3 + dis_region4 +
    dis_region5  + dis_region6 + dis_region7 + dis_region8 +
    dis_educgrp1 + dis_educgrp2 +
    dis_agegrp20 +
    DISABL1 +
    dis_yr92 + dis_yr93 + dis_yr94 +
    dis_yr95 + dis_yr96 + dis_yr97,
  data    = one_women_y,
  weights = FNLWGT2
)

### Col-4. Cell Level DD

library(broom)

# Keep only relevant sample for column 4
one_col4 <- one %>%
  filter(SEX == 1 | (SEX == 2 & sample == "young"))

# 7.1. Run cell-level regressions by classid2
cell_regs <- one_col4 %>%
  group_by(classid2) %>%
  do(
    {
      fit <- lm(
        WKSWORK ~ year_d89 + year_d90 + year_d91 + year_d92 +
          year_d93 + year_d94 + year_d95 + year_d96 +
          DISABL1 + dis_yr92 + dis_yr93 + dis_yr94 +
          dis_yr95 + dis_yr96 + dis_yr97,
        data    = .,
        weights = FNLWGT2
      )
      broom::tidy(fit)
    }
  ) %>%
  ungroup()

### 
