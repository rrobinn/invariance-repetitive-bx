# Data Dictionary
## RBSEC.csv

|variable                 |class     |description |
|:---|:---|:-----------|
|AGE                      |numeric   | Age in months|
|REP1:REP9                |numeric   | Frequency of items endorsed on Repetitive Motor subscale. Binarized based on median split|
|SELF1:SELF7              |numeric   | Frequency of items endorsed on Self-directed subscale. Binarized based on median split|
|RES1:RES8                |numeric   | Frequency of items endorsed on Restricted Interest subscale. Binarized based on median split|
|RIT1:RIT10               |numeric   | Frequency of items endorsed on Ritual & Routine subscale. Binarized based on median split|
|PROJECT                  |character | Which project individual was enrolled in|
|SEX                      |character | Gender identified by parent|
|TABLET                   |factor    | Dummy variable for PROJECT=TABLET|
|BSLERP                   |factor    | Dummy variable for PROJECT=BSLERP|
|MALE                     |factor    | Dummy variable for SEX=MALE|
|AGER                     |numeric   | Age in months, rounded to nearest month|
|AGE18                    |numeric   | Age in months, centered at grand mean (18 months)|
|AGE18YR                  |numeric   | Age in years, centered at grand mean (18 months)|
|AGE18YR2                 |numeric   | Age in years, centered at grand mean (18 months), squared (to test quadratic effect of age)|
|AGEYR                    |numeric   | Age in years (not centered)|
|AGESEX1                  |numeric   | AGE18*SEX (to test interaction b/w Age and Sex)|
|AGESEX2                  |numeric   | AGE18^2*SEX (to test interaction b/w Quadratic Effect of Age and Sex)|
|AGEXTAB                  |numeric   | AGE18*TABLET (to test interaction b/w Age and Cohort)|
|AGE2XTAB                 |numeric   | AGE18^2*TABLET (to test interaction b/w Quadratic Age and Cohort)|
|ID2                      |factor    | ID|



