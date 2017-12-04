install.packages("sqldf")
install.packages("lubridate")
install.packages("forcats")
install.packages("BiocGenerics")
library(sqldf)
library(dplyr)
library(lubridate)
library(plyr)
library(forcats)
library(BiocGenerics)
library(tm)

options(max.print=1000000)


#---------JOIN ICUSTAY AND ADMISSIONS DATA-------------
setwd("C:/Users/amber/Desktop/Data_Practicum/DATA")
icustays<-read.csv("ICUSTAYS.csv")
admissions<-read.csv("ADMISSIONS.csv")
admissions<-sqldf('select A.*, B.* FROM icustays A left join admissions B on A.HADM_ID=B.HADM_ID')
names(admissions)
admissions<-admissions[,c(1:12, 16:31)] # remove duplicate variables of hadm_id, subject_id, and row_id
sum(is.na(admissions$HADM_ID)) # make sure there are no missing values for the hadm_id column
admissions<-admissions[!duplicated(admissions$HADM_ID),] # keep only the first icu visit of each patient visit
anyDuplicated(admissions$HADM_ID) # make sure all the duplicates have been removed (ie subsequent visits to the icu)
admissions<-admissions[admissions$HAS_CHARTEVENTS_DATA==1,] # remove rows where patients have no chartevents
names(admissions)

#----------JOIN DATASET AND DIAGNOSES------------------
diagnoses_icd<-read.csv("DIAGNOSES_ICD.csv")
d_icd_diagnoses<-read.csv("D_ICD_DIAGNOSES.csv")
head(diagnoses_icd)
head(d_icd_diagnoses)
summary(diagnoses_icd)


table(as.factor(diagnoses_icd$SEQ_NUM))

# subset to only include diagnoses 1, 2, etc
diagnoses_1<-diagnoses_icd[diagnoses_icd$SEQ_NUM==1,]
diagnoses_2<-diagnoses_icd[diagnoses_icd$SEQ_NUM==2,]
diagnoses_3<-diagnoses_icd[diagnoses_icd$SEQ_NUM==3,]
diagnoses_4<-diagnoses_icd[diagnoses_icd$SEQ_NUM==4,]
diagnoses_5<-diagnoses_icd[diagnoses_icd$SEQ_NUM==5,]
diagnoses_6<-diagnoses_icd[diagnoses_icd$SEQ_NUM==6,]
diagnoses_7<-diagnoses_icd[diagnoses_icd$SEQ_NUM==7,]
diagnoses_8<-diagnoses_icd[diagnoses_icd$SEQ_NUM==8,]
diagnoses_9<-diagnoses_icd[diagnoses_icd$SEQ_NUM==9,]
diagnoses_10<-diagnoses_icd[diagnoses_icd$SEQ_NUM==10,]
diagnoses_11<-diagnoses_icd[diagnoses_icd$SEQ_NUM==11,]
diagnoses_12<-diagnoses_icd[diagnoses_icd$SEQ_NUM==12,]
diagnoses_13<-diagnoses_icd[diagnoses_icd$SEQ_NUM==13,]
diagnoses_14<-diagnoses_icd[diagnoses_icd$SEQ_NUM==14,]
diagnoses_15<-diagnoses_icd[diagnoses_icd$SEQ_NUM==15,]
diagnoses_16<-diagnoses_icd[diagnoses_icd$SEQ_NUM==16,]
diagnoses_17<-diagnoses_icd[diagnoses_icd$SEQ_NUM==17,]
diagnoses_18<-diagnoses_icd[diagnoses_icd$SEQ_NUM==18,]
diagnoses_19<-diagnoses_icd[diagnoses_icd$SEQ_NUM==19,]
diagnoses_20<-diagnoses_icd[diagnoses_icd$SEQ_NUM==20,]
# combine admissions and diagnoses tables
admissions<-sqldf('select A.ROW_ID, A.SUBJECT_ID, A.HADM_ID, A.ICUSTAY_ID, A.ADMITTIME, A.DISCHTIME, 
                 A.DEATHTIME, A.ADMISSION_TYPE, A.ADMISSION_LOCATION, A.DISCHARGE_LOCATION, 
                  A.INSURANCE, A.LANGUAGE, A.RELIGION, A.MARITAL_STATUS, A.ETHNICITY, A.DIAGNOSIS,
                  A.HOSPITAL_EXPIRE_FLAG, A.HAS_CHARTEVENTS_DATA, B.ICD9_CODE AS DIAGNOSES_1, 
                  C.ICD9_CODE AS DIAGNOSES_2, D.ICD9_CODE AS DIAGNOSES_3, E.ICD9_CODE AS DIAGNOSES_4, 
                  F.ICD9_CODE AS DIAGNOSES_5, G.ICD9_CODE AS DIAGNOSES_6, H.ICD9_CODE AS DIAGNOSES_7, 
                  I.ICD9_CODE AS DIAGNOSES_8, J.ICD9_CODE AS DIAGNOSES_9, K.ICD9_CODE AS DIAGNOSES_10,
                  L.ICD9_CODE AS DIAGNOSES_11, M.ICD9_CODE AS DIAGNOSES_12, N.ICD9_CODE AS DIAGNOSES_13,
                  O.ICD9_CODE AS DIAGNOSES_14, P.ICD9_CODE AS DIAGNOSES_15, Q.ICD9_CODE AS DIAGNOSES_16,
                  R.ICD9_CODE AS DIAGNOSES_17, S.ICD9_CODE AS DIAGNOSES_18, T.ICD9_CODE AS DIAGNOSES_19,
                  U.ICD9_CODE AS DIAGNOSES_20
                  from admissions A left join diagnoses_1 B on A.HADM_ID=B.HADM_ID 
                  left join diagnoses_2 C on A.HADM_ID=C.HADM_ID 
                  left join diagnoses_3 D on A.HADM_ID=D.HADM_ID
                  left join diagnoses_4 E on A.HADM_ID=E.HADM_ID
                  left join diagnoses_5 F on A.HADM_ID=F.HADM_ID 
                  left join diagnoses_6 G on A.HADM_ID=G.HADM_ID
                  left join diagnoses_7 H on A.HADM_ID=H.HADM_ID
                  left join diagnoses_8 I on A.HADM_ID=I.HADM_ID 
                  left join diagnoses_9 J on A.HADM_ID=J.HADM_ID
                  left join diagnoses_10 K on A.HADM_ID=K.HADM_ID
                  left join diagnoses_11 L on A.HADM_ID=L.HADM_ID 
                  left join diagnoses_12 M on A.HADM_ID=M.HADM_ID
                  left join diagnoses_13 N on A.HADM_ID=N.HADM_ID
                  left join diagnoses_14 O on A.HADM_ID=O.HADM_ID 
                  left join diagnoses_15 P on A.HADM_ID=P.HADM_ID
                  left join diagnoses_16 Q on A.HADM_ID=Q.HADM_ID
                  left join diagnoses_17 R on A.HADM_ID=R.HADM_ID 
                  left join diagnoses_18 S on A.HADM_ID=S.HADM_ID
                  left join diagnoses_19 T on A.HADM_ID=T.HADM_ID
                  left join diagnoses_20 U on A.HADM_ID=U.HADM_ID
                  ')
head(admissions)
head(diagnoses_icd, 20)
#verify changes were done correctly
admissions[admissions$HADM_ID==172335,]

DATA<-admissions
rm(admissions)
rm(diagnoses_1)
rm(diagnoses_2)
rm(diagnoses_3)
rm(diagnoses_4)
rm(diagnoses_5)
rm(diagnoses_6)
rm(diagnoses_7)
rm(diagnoses_8)
rm(diagnoses_9)
rm(diagnoses_10)
rm(diagnoses_11)
rm(diagnoses_12)
rm(diagnoses_13)
rm(diagnoses_14)
rm(diagnoses_15)
rm(diagnoses_16)
rm(diagnoses_17)
rm(diagnoses_18)
rm(diagnoses_19)
rm(diagnoses_20)

pattern <- "^46|^47|^48|^49|^50|^51" # all the icd-9 codes for respiratory illness fall between 460 and 519

a<-DATA[(grep(pattern, DATA$DIAGNOSES_1)),]
b<-DATA[(grep(pattern, DATA$DIAGNOSES_2)),]
c<-DATA[(grep(pattern, DATA$DIAGNOSES_3)),]
d<-DATA[(grep(pattern, DATA$DIAGNOSES_4)),]
e<-DATA[(grep(pattern, DATA$DIAGNOSES_5)),]
f<-DATA[(grep(pattern, DATA$DIAGNOSES_6)),]
g<-DATA[(grep(pattern, DATA$DIAGNOSES_7)),]
h<-DATA[(grep(pattern, DATA$DIAGNOSES_8)),]
i<-DATA[(grep(pattern, DATA$DIAGNOSES_9)),]
j<-DATA[(grep(pattern, DATA$DIAGNOSES_10)),]
k<-DATA[(grep(pattern, DATA$DIAGNOSES_11)),]
l<-DATA[(grep(pattern, DATA$DIAGNOSES_12)),]
m<-DATA[(grep(pattern, DATA$DIAGNOSES_13)),]
n<-DATA[(grep(pattern, DATA$DIAGNOSES_14)),]
o<-DATA[(grep(pattern, DATA$DIAGNOSES_15)),]
p<-DATA[(grep(pattern, DATA$DIAGNOSES_16)),]
q<-DATA[(grep(pattern, DATA$DIAGNOSES_17)),]
r<-DATA[(grep(pattern, DATA$DIAGNOSES_18)),]
s<-DATA[(grep(pattern, DATA$DIAGNOSES_19)),]
t<-DATA[(grep(pattern, DATA$DIAGNOSES_20)),]

resp_1<-sqldf('select distinct DIAGNOSES_1 AS RESP_DIAG FROM a')
resp_2<-sqldf('select distinct DIAGNOSES_2 AS RESP_DIAG FROM b')
resp_3<-sqldf('select distinct DIAGNOSES_3 AS RESP_DIAG FROM c')
resp_4<-sqldf('select distinct DIAGNOSES_4 AS RESP_DIAG FROM d')
resp_5<-sqldf('select distinct DIAGNOSES_5 AS RESP_DIAG FROM e')
resp_6<-sqldf('select distinct DIAGNOSES_6 AS RESP_DIAG FROM f')
resp_7<-sqldf('select distinct DIAGNOSES_7 AS RESP_DIAG FROM g')
resp_8<-sqldf('select distinct DIAGNOSES_8 AS RESP_DIAG FROM h')
resp_9<-sqldf('select distinct DIAGNOSES_9 AS RESP_DIAG FROM i')
resp_10<-sqldf('select distinct DIAGNOSES_10 AS RESP_DIAG FROM j')
resp_11<-sqldf('select distinct DIAGNOSES_11 AS RESP_DIAG FROM k')
resp_12<-sqldf('select distinct DIAGNOSES_12 AS RESP_DIAG FROM l')
resp_13<-sqldf('select distinct DIAGNOSES_13 AS RESP_DIAG FROM m')
resp_14<-sqldf('select distinct DIAGNOSES_14 AS RESP_DIAG FROM n')
resp_15<-sqldf('select distinct DIAGNOSES_15 AS RESP_DIAG FROM o')
resp_16<-sqldf('select distinct DIAGNOSES_16 AS RESP_DIAG FROM p')
resp_17<-sqldf('select distinct DIAGNOSES_17 AS RESP_DIAG FROM q')
resp_18<-sqldf('select distinct DIAGNOSES_18 AS RESP_DIAG FROM r')
resp_19<-sqldf('select distinct DIAGNOSES_19 AS RESP_DIAG FROM s')
resp_20<-sqldf('select distinct DIAGNOSES_20 AS RESP_DIAG FROM t')
resp_all<-rbind(resp_1, resp_2, resp_3, resp_4, resp_5, resp_6, resp_7, resp_8, 
                resp_9, resp_10, resp_11, resp_12, resp_13, resp_14, resp_15, 
                resp_16, resp_17, resp_18, resp_19, resp_20)
uniqe_resp<-sqldf('SELECT DISTINCT RESP_DIAG FROM resp_all')
unique_resp<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_SHORT_TITLE, B.LONG_TITLE AS DIAGNOSES_LONG_TITLE FROM uniqe_resp A left join d_icd_diagnoses B on A.RESP_DIAG=B.ICD9_CODE')
unique_resp<-unique_resp[complete.cases(unique_resp),]
# export list of all pulmonary/respiraotry diseases included in the data
write.csv(unique_resp, "all_diagnoses_resp.csv")
DATA<-rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)
rm(a)
rm(b)
rm(c)
rm(d)
rm(e)
rm(f)
rm(g)
rm(h)
rm(i)
rm(j)
rm(k)
rm(l)
rm(m)
rm(n)
rm(o)
rm(p)
rm(q)
rm(r)
rm(s)
rm(t)


nrow(DATA)

dir()
patients<-read.csv("PATIENTS.CSV")
head(patients)

# VERIFY THAT THE SUBJECT_ID IS NOT DUPLICATED IN THE DATA TABLE
sqldf('select SUBJECT_ID, count(distinct (SUBJECT_ID)) FROM DATA GROUP BY SUBJECT_ID HAVING COUNT(DISTINCT(SUBJECT_ID))>1 ')

# add the dob and gender from the patient table
DATA<-sqldf('SELECT A.*, B.GENDER, B.DOB FROM DATA A left join patients B on A.SUBJECT_ID=B.SUBJECT_ID')
head(DATA)

# add the short title based on the icd9 on the diagnosis lines 
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_1_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_1=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_2_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_2=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_3_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_3=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_4_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_4=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_5_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_5=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_6_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_6=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_7_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_7=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_8_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_8=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_9_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_9=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_10_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_10=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_11_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_11=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_12_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_12=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_13_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_13=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_14_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_14=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_15_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_15=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_16_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_16=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_17_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_17=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_18_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_18=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_19_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_19=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_20_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_20=B.ICD9_CODE')

head(DATA)

#reorganize DATA for easier examiniation so that diagnoses titles are beside their icd9 codes and gender and dob are with the demographic DATA
names(DATA)

# see if it is practical to convert the diagnoses columns to a factor variable
table(DATA$DIAGNOSIS)

DATA$DIAGNOSIS<-as.character(DATA$DIAGNOSIS)

## see if it is practical to convert the diagnoses columns to a factor variable
sort(table(as.factor(DATA$DIAGNOSIS)), decreasing=TRUE)

# change all the lesser count diagnoses into "other" category
DATA$DIAGNOSIS<-tolower(DATA$DIAGNOSIS)
str(DATA$DIAGNOSIS)
text<-DATA[, 27]
corp <- Corpus(VectorSource(text))
dtm <- DocumentTermMatrix(corp)

table(as.factor(DATA$DIAGNOSIS[(grepl("pneumonia", DATA$DIAGNOSIS, fixed=TRUE) | grepl("pneum", DATA$DIAGNOSIS, fixed=TRUE))]))

DATA$DIAGNOSIS[(grepl("influ", DATA$DIAGNOSIS, fixed=TRUE) 
)]<-"influenza"

DATA$DIAGNOSIS[(grepl("tuber", DATA$DIAGNOSIS, fixed=TRUE) 
                | grepl("tb", DATA$DIAGNOSIS, fixed=TRUE)
)]<-"tuberculosis"


DATA$DIAGNOSIS[(grepl("pneumonia", DATA$DIAGNOSIS, fixed=TRUE) 
                | grepl("pneum", DATA$DIAGNOSIS, fixed=TRUE))]<-"pnuemonia"


table(as.factor(DATA$DIAGNOSIS[(grep("fail", DATA$DIAGNOSIS))]))
DATA$DIAGNOSIS[(grepl("chron", DATA$DIAGNOSIS, fixed=TRUE) 
                & grepl("resp",DATA$DIAGNOSIS , fixed=TRUE)
                & grepl("fail", DATA$DIAGNOSIS, fixed=TRUE))]<-"chronic respiratory failure"

DATA$DIAGNOSIS[(grepl("emblsm", DATA$DIAGNOSIS, fixed=TRUE) 
                | grepl("embol",DATA$DIAGNOSIS , fixed=TRUE)
)]<-"embolism"

DATA$DIAGNOSIS[(grepl("valve", DATA$DIAGNOSIS, fixed=TRUE) 
)]<-"valve disorder"

DATA$DIAGNOSIS[(grepl("alcohol", DATA$DIAGNOSIS, fixed=TRUE) | grepl("alcoh", DATA$DIAGNOSIS, fixed=TRUE)
)]<-"alcohol"

DATA$DIAGNOSIS[(grepl("acute", DATA$DIAGNOSIS, fixed=TRUE) 
                & grepl("resp",DATA$DIAGNOSIS , fixed=TRUE)
                & grepl("fail", DATA$DIAGNOSIS, fixed=TRUE))]<-"acute respiratory failure"

DATA$DIAGNOSIS[(grepl("urin", DATA$DIAGNOSIS, fixed=TRUE) 
)]<-"urinary"


DATA$DIAGNOSIS[(grepl("postop", DATA$DIAGNOSIS, fixed=TRUE) 
                | grepl("surg",DATA$DIAGNOSIS , fixed=TRUE)
                | grepl("srg",DATA$DIAGNOSIS , fixed=TRUE)
)]<-"postop"

DATA$DIAGNOSIS[(grepl("stomach", DATA$DIAGNOSIS, fixed=TRUE) 
                | grepl("intest",DATA$DIAGNOSIS , fixed=TRUE)
)]<-"gastrointestinal"

DATA$DIAGNOSIS[(grepl("cf", DATA$DIAGNOSIS, fixed=TRUE) 
                | grepl("cystic",DATA$DIAGNOSIS , fixed=TRUE)
)]<-"cystic fibrosis"

DATA$DIAGNOSIS[grepl("aneu",DATA$DIAGNOSIS , fixed=TRUE)
               ]<-"aneurism"

DATA$DIAGNOSIS[grepl("hernia",DATA$DIAGNOSIS , fixed=TRUE)
               ]<-"hernia"

DATA$DIAGNOSIS[(grepl("chr", DATA$DIAGNOSIS, fixed=TRUE) 
                & grepl("obst",DATA$DIAGNOSIS , fixed=TRUE)
)]<-"copd"

DATA$DIAGNOSIS[(grepl("pleur", DATA$DIAGNOSIS, fixed=TRUE) 
)]<-"pleurisy"

DATA$DIAGNOSIS[(grepl("ventr", DATA$DIAGNOSIS, fixed=TRUE) 
)]<-"ventrical"


DATA$DIAGNOSIS[(grep("septicemia", DATA$DIAGNOSIS))]<-"septicemia"

DATA$DIAGNOSIS[(grep("pancrea", DATA$DIAGNOSIS))]<-"pancreas"

DATA$DIAGNOSIS[(grep("asthma", DATA$DIAGNOSIS))]<-"asthma"

DATA$DIAGNOSIS[(grep("melanoma", DATA$DIAGNOSIS))]<-"cancer"

DATA$DIAGNOSIS[(grep("mal", DATA$DIAGNOSIS))]<-"cancer"

DATA$DIAGNOSIS[(grep("pneumonitis", DATA$DIAGNOSIS))]<-"pneumonitis"

DATA$DIAGNOSIS[(grep("hemorr", DATA$DIAGNOSIS))]<-"hemorrhage"

DATA$DIAGNOSIS[(grep("bladder", DATA$DIAGNOSIS))]<-"bladder"

DATA$DIAGNOSIS[(grep("coma", DATA$DIAGNOSIS))]<-"coma"

DATA$DIAGNOSIS[(grepl("kidney", DATA$DIAGNOSIS, fixed=TRUE) & (grepl("kid", DATA$DIAGNOSIS, fixed=TRUE)
))]<-"kidney"

DATA$DIAGNOSIS[(grepl("heart", DATA$DIAGNOSIS, fixed=TRUE) 
                & grepl("fail",DATA$DIAGNOSIS , fixed=TRUE)
)]<-"heart failure"
DATA$DIAGNOSIS[(grepl("hrt", DATA$DIAGNOSIS, fixed=TRUE) 
                & grepl("fail",DATA$DIAGNOSIS , fixed=TRUE)
)]<-"heart failure"

DATA$DIAGNOSIS[(grepl("ami", DATA$DIAGNOSIS, fixed=TRUE) 
)]<-"heart failure"

DATA$DIAGNOSIS[(grepl("cancer", DATA$DIAGNOSIS, fixed=TRUE) 
)]<-"cancer"


DATA$DIAGNOSIS[(grepl("human imm", DATA$DIAGNOSIS, fixed=TRUE) 
                | grepl("hiv", DATA$DIAGNOSIS, fixed=TRUE)
)]<-"hiv"

DATA$DIAGNOSIS[(grepl("tox", DATA$DIAGNOSIS, fixed=TRUE) | grepl("pois", DATA$DIAGNOSIS, fixed=TRUE)
)]<-"toxin/poison"
DATA$DIAGNOSIS[(grepl("liver", DATA$DIAGNOSIS, fixed=TRUE) 
)]<-"liver"
DATA$DIAGNOSIS[(grepl("lymphoma", DATA$DIAGNOSIS, fixed=TRUE) 
                | grepl("leuk", DATA$DIAGNOSIS, fixed=TRUE) 
)]<-"cancer"
DATA$DIAGNOSIS[(grepl("loc", DATA$DIAGNOSIS, fixed=TRUE)
)]<-"loss of consciousness"

DATA$DIAGNOSIS[(grepl("lumb", DATA$DIAGNOSIS, fixed=TRUE) 
                | grepl("lmbr", DATA$DIAGNOSIS, fixed=TRUE)
)]<-"lumbar"

DATA$DIAGNOSIS[(grepl("lun", DATA$DIAGNOSIS, fixed=TRUE) 
                & grepl("contusions",DATA$DIAGNOSIS , fixed=TRUE)
                | grepl("injury",DATA$DIAGNOSIS , fixed=TRUE )
)]<-"lung injury"
DATA$DIAGNOSIS[(grepl("lung", DATA$DIAGNOSIS, fixed=TRUE) & grepl("abs", DATA$DIAGNOSIS, fixed=TRUE)
)]<-"lung abscess"

DATA$DIAGNOSIS[(grepl("esop", DATA$DIAGNOSIS, fixed=TRUE))]<-"esophagus"

DATA$DIAGNOSIS[(grepl("contusion", DATA$DIAGNOSIS, fixed=TRUE) 
                | grepl("inj", DATA$DIAGNOSIS, fixed=TRUE) 
                | grepl("lacer", DATA$DIAGNOSIS, fixed=TRUE) 
                | grepl("wound", DATA$DIAGNOSIS, fixed=TRUE) 
)]<-"non_lung injury"

DATA$DIAGNOSIS[(grepl("absc", DATA$DIAGNOSIS, fixed=TRUE)
)]<-"non_lung abscess"

DATA$DIAGNOSIS[(grepl("obstr", DATA$DIAGNOSIS, fixed=TRUE) 
)]<-"obstruction"

DATA$DIAGNOSIS[(grepl("gastro", DATA$DIAGNOSIS, fixed=TRUE) | grepl("gast", DATA$DIAGNOSIS, fixed=TRUE)
)]<-"gastrointestinal"
DATA$DIAGNOSIS[(grepl("brain", DATA$DIAGNOSIS, fixed=TRUE) | grepl("encephalopathy", DATA$DIAGNOSIS, fixed=TRUE)
)]<-"non_lung injury"

DATA$DIAGNOSIS[(grepl("react", DATA$DIAGNOSIS, fixed=TRUE) | grepl("allergy", DATA$DIAGNOSIS, fixed=TRUE)
)]<-"allergic reaction"

DATA$DIAGNOSIS[(grepl("compression of brain", DATA$DIAGNOSIS, fixed=TRUE) 
)]<-"non_lung injury"

DATA$DIAGNOSIS[(grepl("bronc", DATA$DIAGNOSIS, fixed=TRUE) 
                | grepl("brnch", DATA$DIAGNOSIS, fixed=TRUE)
)]<-"bronchittis"
DATA$DIAGNOSIS[(grepl("crnry", DATA$DIAGNOSIS, fixed=TRUE) 
                | grepl("coronary", DATA$DIAGNOSIS, fixed=TRUE)
                | grepl("chf", DATA$DIAGNOSIS, fixed=TRUE)
)]<-"heart failure"

DATA$DIAGNOSIS[(grepl("spine", DATA$DIAGNOSIS, fixed=TRUE) 
                | grepl("spinal", DATA$DIAGNOSIS, fixed=TRUE)
)]<-"spinal injury/disease"

DATA$DIAGNOSIS[(grep("meth", DATA$DIAGNOSIS))]<-"meth lung"
DATA$DIAGNOSIS[(grep("tracheo", DATA$DIAGNOSIS))]<-"tracheostomy"
DATA$DIAGNOSIS[(grep("subendo", DATA$DIAGNOSIS))]<-"subendo"
DATA$DIAGNOSIS[(grep("fx", DATA$DIAGNOSIS))]<-"broken bone"
DATA$DIAGNOSIS[(grep("staph", DATA$DIAGNOSIS))]<-"staph infection"
DATA$DIAGNOSIS[(grep("bronc", DATA$DIAGNOSIS))]<-"bronchitis"
DATA$DIAGNOSIS[(grep("ulc", DATA$DIAGNOSIS))]<-"ulcer"
DATA$DIAGNOSIS[(grep("atri", DATA$DIAGNOSIS))]<-"atrial"
DATA$DIAGNOSIS[(grep("pulm", DATA$DIAGNOSIS))]<-"pulmonary other"
DATA$DIAGNOSIS[(grep("seps", DATA$DIAGNOSIS))]<-"sepsis"
DATA$DIAGNOSIS[(grep("aort", DATA$DIAGNOSIS))]<-"aorta"
DATA$DIAGNOSIS[(grep("empyema", DATA$DIAGNOSIS))]<-"pleurisy"
DATA$DIAGNOSIS[(grep("asth", DATA$DIAGNOSIS))]<-"asthma"
DATA$DIAGNOSIS[(grep("art", DATA$DIAGNOSIS))]<-"artery"
DATA$DIAGNOSIS[(grep("colon", DATA$DIAGNOSIS))]<-"colon"
DATA$DIAGNOSIS[(grep("hemoth", DATA$DIAGNOSIS))]<-"hemothorax"
DATA$DIAGNOSIS[(grep("spr", DATA$DIAGNOSIS))]<-"sprain"
DATA$DIAGNOSIS[(grep("spleen", DATA$DIAGNOSIS))]<-"spleen"
DATA$DIAGNOSIS[(grep("obesity", DATA$DIAGNOSIS))]<-"obesity"
DATA$DIAGNOSIS[(grep("strep", DATA$DIAGNOSIS))]<-"strep"
DATA$DIAGNOSIS[(grep("hypert", DATA$DIAGNOSIS))]<-"hypertension"
DATA$DIAGNOSIS[(grep("convu", DATA$DIAGNOSIS))]<-"convulsions"
DATA$DIAGNOSIS[(grepl("bact", DATA$DIAGNOSIS) | grepl("inf", DATA$DIAGNOSIS))]<-"bacterial infection"
DATA$DIAGNOSIS[(grepl("pericard", DATA$DIAGNOSIS) | grepl("cardiac", DATA$DIAGNOSIS))]<-"heart failure"
DATA$DIAGNOSIS[(grep("int", DATA$DIAGNOSIS))]<-"gastrointestinal"
DATA$DIAGNOSIS[(grep("hepatic", DATA$DIAGNOSIS))]<-"liver"
DATA$DIAGNOSIS[(grep("keto", DATA$DIAGNOSIS))]<-"diabetes"
DATA$DIAGNOSIS[(grepl("edem", DATA$DIAGNOSIS) | grepl("swell", DATA$DIAGNOSIS))]<-"swelling"
DATA$DIAGNOSIS[(grepl("dev", DATA$DIAGNOSIS) | grepl("graft", DATA$DIAGNOSIS))]<-"device/graft complications"

# seperate out the factor levels with more than count of 150
high_count<-DATA[(DATA$DIAGNOSIS %in% c("septicemia", "pnuemonia", "tuberculosis","acute respiratory failure", "influenza","non_lung abscess","obstruction","bronchittis", "cholangitis", "swelling",
                                        "heart failure","hemorrhage", "subendo", "coma", "gastrointenstinal", "broken bone", 
                                        "chronic respiratory failure","valve disorder", "liver", "toxin/poison", "kidney", 
                                        "meth lung", "embolism", "pancreas", "aneurism", "non_lung injury", "pleurisy", "device/graft complications",
                                        "postop", "cancer", "ventrical", "ulcer", "asthma", "spinal injury/disease", "tracheostomy",
                                        "hiv", "atrial", "ventrical", "ulcer", "asthma", "alcohol", "aorta", "cystic fibrosis",
                                        "urinary", "loss of consciousness", "colon", "bacterial infection", "hemothorax", "diabetes",
                                        "spleen", "lumbar", "obesity", "strep", "hypertension", "bladder","convulsions", "esophagus","blood in stool")), ]

# seperate out the factor levels with more than count of 150
low_count<-DATA[!(DATA$DIAGNOSIS %in% c("septicemia", "pnuemonia", "acute respiratory failure", "tuberculosis", "influenza","non_lung abscess","obstruction","bronchittis", "cholangitis", "swelling",
                                        "heart failure","hemorrhage", "subendo", "coma", "gastrointenstinal", "broken bone", 
                                        "chronic respiratory failure","valve disorder", "liver", "toxin/poison", "kidney", 
                                        "meth lung", "embolism", "pancreas", "aneurism", "non_lung injury", "esophagus","pleurisy", "device/graft complications",
                                        "postop", "cancer", "ventrical", "ulcer", "asthma", "spinal injury/disease", "tracheostomy",
                                        "hiv", "atrial", "ventrical", "ulcer", "asthma", "alcohol", "aorta", "cystic fibrosis",
                                        "urinary", "loss of consciousness", "colon", "bacterial infection", "hemothorax", "diabetes",
                                        "spleen", "lumbar", "obesity", "strep", "hypertension", "bladder","convulsions", "blood in stool")), ]
# change the categories with less than 100 count to "other" category
low_count$DIAGNOSIS<-"other"
# rejoin the two parts of the data
DATA<-rbind(high_count, low_count)
sort(table(as.factor(DATA$DIAGNOSIS)))
DATA$DIAGNOSIS<-as.factor(DATA$DIAGNOSIS)

DATA$DIAGNOSIS<-as.factor(DATA$DIAGNOSIS)

sort(table(as.factor(DATA$DIAGNOSIS)))

# load microevents table
microevents<-read.csv("MICROBIOLOGYEVENTS.csv")
head(microevents)
microevents$ORG_NAME<-as.character(microevents$ORG_NAME)
microevents<-microevents[!microevents$ORG_NAME=="",] # remove rows where there were a negative result
sort(table(as.factor(microevents$ORG_NAME)))
anyDuplicated(microevents$HADM_ID) # check to see if each row has a unique hadm_id 
nrow(microevents[duplicated(microevents$HADM_ID),]) # look at rows with duplicate hadm_id
microevents<-microevents[!duplicated(microevents$HADM_ID),] # remove all duplicated rows 
microevents$ORG_NAME<-as.factor(microevents$ORG_NAME)
DATA<-sqldf("select A.*, B.ORG_NAME FROM DATA A left join microevents B on A.HADM_ID=B.HADM_ID")
names(DATA)


# REDUCE NUMBER OF LEVELS IN FACTOR OF ORG_NAME BY COMBINING LESSER GROUPS TOGETHER AS OTHER
DATA$ORG_NAME<-as.character(DATA$ORG_NAME)
DATA$ORG_NAME[grepl("STAPH", DATA$ORG_NAME)] <- "STAPHYLOCOCCUS"
DATA$ORG_NAME[grepl("STREP", DATA$ORG_NAME)] <- "STREPTOCOCCUS"
DATA$ORG_NAME[grepl("BACILLUS", DATA$ORG_NAME)] <- "BACILLUS"
DATA$ORG_NAME[grepl("PSEUDOMONAS", DATA$ORG_NAME)] <- "PSEUDOMONAS"
DATA$ORG_NAME[grepl("ENTEROCOCCUS", DATA$ORG_NAME)] <- "ENTEROCOCCUS"
DATA$ORG_NAME[grepl("INFLUENZA", DATA$ORG_NAME)] <- "INFLUENZA"
high_count<-DATA[(DATA$ORG_NAME %in% c("YEAST", "PSEUDOMONAS", "STREPTOCOCCUS", "GRAM NEGATIVE ROD(S)", 
                                                "GRAM POSITIVE BACTERIA", "PROTEUS MIRABILIS", "CORYNEBACTERIUM SPECIES (DIPHTHEROIDS",
                                                "KLEBSIELLA OXYTOCA", "ENTEROBACTER AEROGENES", "ENTEROCOCCUS FAECIUM", "ACINETOBACTER BAUMANNII", 
                                                "BACILLUS", "STAPHYLOCOCCUS", "ESCHERICHIA COLI", "ENTEROCOCCUS", "KLEBSIELLA PNEUMONIAE", "INFLUENZA", 
                                                "CLOSTRIDIUM DIFFICILE", "ENTEROBACTER CLOACAE", "SERRATIA MARCESCENS", "CANDIDA ALBICANS, PRESUMPTIVE IDENTIFICATION", 
                                                "PROBABLE ENTEROCOCCUS", "MORAXELLA CATARRHALIS", "ENTEROCOCCUS FAECALIS")), ]

low_count<-DATA[!(DATA$ORG_NAME %in% c("YEAST", "PSEUDOMONAS", "STREPTOCOCCUS", "GRAM NEGATIVE ROD(S)", 
                                               "GRAM POSITIVE BACTERIA", "PROTEUS MIRABILIS", "CORYNEBACTERIUM SPECIES (DIPHTHEROIDS",
                                               "KLEBSIELLA OXYTOCA", "ENTEROBACTER AEROGENES", "ENTEROCOCCUS FAECIUM", "ACINETOBACTER BAUMANNII", 
                                               "BACILLUS", "STAPHYLOCOCCUS", "ESCHERICHIA COLI", "ENTEROCOCCUS", "KLEBSIELLA PNEUMONIAE", "INFLUENZA", 
                                               "CLOSTRIDIUM DIFFICILE", "ENTEROBACTER CLOACAE", "SERRATIA MARCESCENS", "CANDIDA ALBICANS, PRESUMPTIVE IDENTIFICATION", 
                                               "PROBABLE ENTEROCOCCUS", "MORAXELLA CATARRHALIS", "ENTEROCOCCUS FAECALIS")), ]
low_count$ORG_NAME<-"OTHER"
DATA<-rbind(high_count, low_count)
# load the additional variables from postgresql

# get all the urine output for the first 24 hours in the icu for each patient
dir()
uo<-read.csv("urine_output.csv")
head(uo)
names(uo) <- toupper(names(uo))
nrow(uo[duplicated(uo$ICUSTAY_ID),]) # look at rows with duplicate icustay_id
DATA<-sqldf('select A.*, B.URINEOUTPUT AS URINE_OUTPUT from DATA A left join uo B on A.ICUSTAY_ID=B.ICUSTAY_ID')
head(DATA)
sum(is.na(DATA$URINE_OUTPUT))

# add the weight to the DATAset
dir()
weight<-read.csv("weight_first_day.csv")
head(weight)
# CONVERT WEIGHT TO POUNDS 
weight$weight<-(weight$weight * 2.2046226218488)
names(weight) <- toupper(names(weight))
DATA<-sqldf('select A.*, B.WEIGHT from DATA A left join weight B on A.ICUSTAY_ID=B.ICUSTAY_ID')

# add blood gas measure
blood_gas<-read.csv("blood_gas_first_day.csv")
blood_gas_arterial<-read.csv("blood_gas_first_day_arterial.csv")

# remove rows where all the important varialbes are missing
blood_gas_totalco2<-blood_gas[!is.na(blood_gas$totalco2),]
blood_gas_pco2<-blood_gas[!is.na(blood_gas$pco2),]
blood_gas_ph<-blood_gas[!is.na(blood_gas$ph),]
blood_gas_po2<-blood_gas[!is.na(blood_gas$po2),]
blood_gas_glucose<-blood_gas[!is.na(blood_gas$glucose),]
blood_gas<-rbind(blood_gas_totalco2, blood_gas_pco2, blood_gas_ph, blood_gas_po2, blood_gas_glucose)

blood_gas_arterial_totalco2<-blood_gas_arterial[!is.na(blood_gas_arterial$totalco2),]
blood_gas_arterial_pco2<-blood_gas_arterial[!is.na(blood_gas_arterial$pco2),]
blood_gas_arterial_ph<-blood_gas_arterial[!is.na(blood_gas_arterial$ph),]
blood_gas_arterial_po2<-blood_gas_arterial[!is.na(blood_gas_arterial$po2),]
blood_gas_arterial_glucose<-blood_gas_arterial[!is.na(blood_gas_arterial$glucose),]
blood_gas_arterial<-rbind(blood_gas_arterial_totalco2, blood_gas_arterial_pco2, blood_gas_arterial_ph, blood_gas_arterial_po2, blood_gas_arterial_glucose)

blood_gas<-blood_gas[!duplicated(blood_gas$hadm_id),]
blood_gas_arterial<-blood_gas_arterial[!duplicated(blood_gas_arterial$hadm_id),]

names(blood_gas)
names(blood_gas_arterial)
blood_gas<-blood_gas[,c(2,3,9,24,22,25,13)] # subset to include only totalc02, pc02, ph, p02, and glucose
blood_gas_arterial<-blood_gas_arterial[,c(2,3,10,11,17,20,31)] # subset to include only totalc02, pc02, ph, p02, and glucose
names(blood_gas) <- toupper(names(blood_gas))
names(blood_gas_arterial) <- toupper(names(blood_gas_arterial))
#combine the two sets of blood gas measures
blood_gas_all<-sqldf('SELECT A.*, B.HADM_ID, B.ICUSTAY_ID, B.TOTALCO2 AS TOTALCO2_ART,
                     B.PH AS PH_ART, B.PCO2 AS PCO2_ART, B.PO2 AS PO2_ART, B.GLUCOSE AS GLUCOSE_ART
                     FROM blood_gas A join blood_gas_arterial B ON A.HADM_ID=B.HADM_ID')

# replace the null values for the venous blood gas with the arterial blood gas measures
null_totalco2<-blood_gas_all[is.na(blood_gas_all$TOTALCO2),]
not_null_totalco2<-blood_gas_all[!is.na(blood_gas_all$TOTALCO2),]
null_totalco2$TOTALCO2<-null_totalco2$TOTALCO2_ART
blood_gas_all<-rbind(null_totalco2, not_null_totalco2)

null_ph<-blood_gas_all[is.na(blood_gas_all$PH),]
not_null_ph<-blood_gas_all[!is.na(blood_gas_all$PH),]
null_ph$PH<-null_ph$PH_ART
blood_gas_all<-rbind(null_ph, not_null_ph)

null_pco2<-blood_gas_all[is.na(blood_gas_all$PCO2),]
not_null_pco2<-blood_gas_all[!is.na(blood_gas_all$PCO2),]
null_pco2$PCO2<-null_pco2$PCO2_ART
blood_gas_all<-rbind(null_pco2, not_null_pco2)

null_po2<-blood_gas_all[is.na(blood_gas_all$PO2),]
not_null_po2<-blood_gas_all[!is.na(blood_gas_all$PO2),]
null_po2$PO2<-null_po2$PO2_ART
blood_gas_all<-rbind(null_po2, not_null_po2)

null_glucose<-blood_gas_all[is.na(blood_gas_all$GLUCOSE),]
not_null_glucose<-blood_gas_all[!is.na(blood_gas_all$GLUCOSE),]
null_glucose$GLUCOSE<-null_glucose$GLUCOSE_ART
blood_gas_all<-rbind(null_glucose, not_null_glucose)

# remove unwanted variables from the blood gas
blood_gas_all<-blood_gas_all[,c(1:7)]
blood_gas_all<-blood_gas_all[!duplicated(blood_gas_all$ICUSTAY_ID),]

# add blood gas to the DATA set
DATA<-sqldf('select A.*, B.TOTALCO2, B.PH, B.PCO2, B.PO2, B.GLUCOSE FROM DATA A left join blood_gas_all B ON A.ICUSTAY_ID=B.ICUSTAY_ID')

# load rrt (renal replacement therapy) DATA and add it to the DATA set
rrt<-read.csv("rrt_first_day.csv")
names(rrt)<-toupper(names(rrt))
DATA<-sqldf('SELECT A.*, B.RRT FROM DATA A LEFT JOIN rrt B ON A.ICUSTAY_ID=B.ICUSTAY_ID')
DATA$RRT<-as.factor(DATA$RRT)

# add the vitals DATA to the DATA set
vitals<-read.csv("vitals_first_day.csv")
anyDuplicated(vitals$hadm_id)
anyDuplicated(vitals$icustay_id)
vitals<-vitals[, c(2:5, 7,8,10,11, 13,14, 16,17, 19,20, 22, 23, 25, 26)]
names(vitals)<-toupper(names(vitals))
DATA<-sqldf('SELECT A.*, B.* FROM DATA A LEFT JOIN vitals B ON A.ICUSTAY_ID=B.ICUSTAY_ID')
names(DATA)
DATA<-DATA[,-80]
DATA<-DATA[,-81]

# add gcs (glasgow coma score) to DATA set
gcs<-read.csv('gcs_first_day.csv')
head(gcs)
names(gcs)<-toupper(names(gcs))
DATA<-sqldf('SELECT A.*, B.GCSMOTOR, B.GCSVERBAL, B.GCSEYES FROM DATA A LEFT JOIN gcs B ON A.ICUSTAY_ID=B.ICUSTAY_ID')

# add ventilator durations
dir()
vent<-read.csv("ventdurations.csv")
head(vent)
names(vent)<-toupper(names(vent))
anyDuplicated(vent$ICUSTAY_ID)
nrow(vent[duplicated(vent$ICUSTAY_ID),])
vent<-vent[!duplicated(vent$ICUSTAY_ID),]
DATA<-sqldf('SELECT A.*, B.DURATION_HOURS AS VENT_DURATION FROM DATA A LEFT JOIN vent B ON A.ICUSTAY_ID=B.ICUSTAY_ID')

# ADD SAPS DATA
saps<-read.csv("saps_output.csv")
head(saps)
names(saps)<-toupper(names(saps))
nrow(duplicated(saps$ICUSTAY_ID))
DATA<-sqldf('SELECT A.*, B.SAPS FROM DATA A LEFT JOIN saps B ON A.ICUSTAY_ID=B.ICUSTAY_ID')

# ADD SAPS II DATA
sapsii<-read.csv("sapsii_output.csv")
head(sapsii)
names(sapsii)<-toupper(names(sapsii))
DATA<-sqldf('SELECT A.*, B.SAPSII, B.SAPSII_PROB FROM DATA A LEFT JOIN sapsii B ON A.ICUSTAY_ID=B.ICUSTAY_ID')

# ADD SOFA DATA
SOFA<-read.csv("sofa.csv")
head(SOFA)
names(SOFA)<-toupper(names(SOFA))
DATA<-sqldf('SELECT A.*, B.SOFA, B.RESPIRATION AS RESP_SOFA FROM DATA A LEFT JOIN SOFA B ON A.ICUSTAY_ID=B.ICUSTAY_ID')

# ADD LODS DATA
LODS<-read.csv("lods.csv")
head(LODS)
names(LODS)<-toupper(names(LODS))
DATA<-sqldf('SELECT A.*, B.LODS, B.PULMONARY AS LODS_PULM FROM DATA A LEFT JOIN LODS B ON A.ICUSTAY_ID=B.ICUSTAY_ID')

# ADD OASIS DATA
OASIS<-read.csv("oasis_output.csv")
head(OASIS)
names(OASIS)<-toupper(names(OASIS))
anyDuplicated(OASIS$ICUSTAY_ID)
DATA<-sqldf('SELECT A.*, B.OASIS, B.OASIS_PROB, B.ICUSTAY_AGE_GROUP FROM DATA A LEFT JOIN OASIS B ON A.ICUSTAY_ID=B.ICUSTAY_ID')
table(as.factor(DATA$ICUSTAY_AGE_GROUP))
DATA<-DATA[(DATA$ICUSTAY_AGE_GROUP=="adult"),] # subset to include only adult patients
DATA<-DATA[,-107]

# add apache data
apsiii<-read.csv("apsii.csv")
DATA<-sqldf("SELECT A.*, B.apsiii AS APSIII, B.apsiii_prob AS APSIII_PROB FROM DATA A LEFT JOIN apsiii B ON A.ICUSTAY_ID=B.icustay_id")
DATA$APSIII<-as.integer(DATA$APSIII)
DATA$APSIII_PROB<-as.numeric(DATA$APSIII_PROB)

# CREATE LENGTH OF STAY VARIABLE
head(DATA)
DATA$LOS <- as.Date(as.character(DATA$DISCHTIME), format="%Y-%m-%d %H:%M:%S")-
  as.Date(as.character(DATA$ADMITTIME), format="%Y-%m-%d %H:%M:%S")
head(DATA)

head(DATA)
callout<-read.csv("CALLOUT.csv")
head(callout)

caregivers<-read.csv("CAREGIVERS.csv")

table(as.factor(caregivers$DESCRIPTION))

# try to fill in the blanks for the missing description in caregivers table
missing<-caregivers[caregivers$DESCRIPTION=="",]
notmissing<-caregivers[!caregivers$DESCRIPTION=="",]
head(missing)
head(notmissing)
table(as.factor(missing$LABEL))
missing$LABEL<-tolower(missing$LABEL)
table(as.factor(missing$LABEL))
missing$LABEL[grepl("stud", missing$LABEL, ignore.case=FALSE)] <- "student"
missing$LABEL[grepl("stu", missing$LABEL, ignore.case=FALSE)] <- "student"
missing$LABEL[grepl("ms", missing$LABEL, ignore.case=FALSE)] <- "doctor"
missing$LABEL[grepl("di", missing$LABEL, ignore.case=FALSE)] <- "dietician"
missing$LABEL[grepl("res", missing$LABEL, ignore.case=FALSE)] <- "resident"
missing$LABEL[grepl("dr", missing$LABEL, ignore.case=FALSE)] <- "doctor"
missing$LABEL[grepl("cow", missing$LABEL, ignore.case=FALSE)] <- "nursing assistant"
missing$LABEL[grepl("rn", missing$LABEL, ignore.case=FALSE)] <- "nurse"
missing$LABEL[grepl("md", missing$LABEL, ignore.case=FALSE)] <- "doctor"
missing$LABEL[grepl("co-wk", missing$LABEL, ignore.case=FALSE)] <- "nursing assistant"
missing$LABEL[grepl("adm", missing$LABEL, ignore.case=FALSE)] <- "administrator"
missing$LABEL[grepl("int", missing$LABEL, ignore.case=FALSE)] <- "student"
missing$LABEL[grepl("st", missing$LABEL, ignore.case=FALSE)] <- "student"
missing$LABEL[grepl("pst", missing$LABEL, ignore.case=FALSE)] <- "student"
missing$LABEL[grepl("ph", missing$LABEL, ignore.case=FALSE)] <- "pharmacist"
missing$LABEL[grepl("med. s", missing$LABEL, ignore.case=FALSE)] <- "student"
missing$LABEL[grepl("lpn", missing$LABEL, ignore.case=FALSE)] <- "nurse"
missing$LABEL[grepl("int", missing$LABEL, ignore.case=FALSE)] <- "student"
missing$LABEL[grepl("rt", missing$LABEL, ignore.case=FALSE)] <- "respiratory therapist"
missing$LABEL[grepl("pa", missing$LABEL, ignore.case=FALSE)] <- "physician assistant"
missing$LABEL[grepl("int", missing$LABEL, ignore.case=FALSE)] <- "student"
missing$LABEL[grepl("np", missing$LABEL, ignore.case=FALSE)] <- "nurse practitioner"
missing$LABEL[grepl("int", missing$LABEL, ignore.case=FALSE)] <- "student"
missing$LABEL[grepl("sn", missing$LABEL, ignore.case=FALSE)] <- "student"
missing$LABEL[grepl("uc", missing$LABEL, ignore.case=FALSE)] <- "uco"

df<-rbind(missing, notmissing)


df$LABEL<-as.character(df$LABEL)
df$DESCRIPTION<-as.character(df$DESCRIPTION)
df$DESCRIPTION[df$LABEL=="doctor"]<-"Attending"
df$DESCRIPTION[df$LABEL=="student"]<-"Student"
df$DESCRIPTION[df$LABEL=="dietician"]<-"Dietitian"
df$DESCRIPTION[df$LABEL=="coord"]<-"Case Manager"
df$DESCRIPTION[df$LABEL=="nurse"]<-"RN"
df$DESCRIPTION[df$LABEL=="nurse practitioner"]<-"Resident/Fellow/PA/NP"
df$DESCRIPTION[df$LABEL=="pharmacist"]<-"Pharmacist"
df$DESCRIPTION[df$LABEL=="physician assistant"]<-"Resident/Fellow/PA/NP"
df$DESCRIPTION[df$LABEL=="ra"]<-"Research Assistant"
df$DESCRIPTION[df$LABEL=="resident"]<-"Resident/Fellow/PA/NP"
df$DESCRIPTION[df$LABEL=="respiratory therapist"]<-"Respiratory"
nrow(df[df$DESCRIPTION=="",]) # reduced number of missing in description from 2411 to 220
df$LABEL<-as.factor(df$LABEL)
df$DESCRIPTION<-as.factor(df$DESCRIPTION)


# create age variable

DATA$admit_year <- as.POSIXct(DATA$ADMITTIME)
DATA$admit_year<-format(DATA$admit_year, "%Y")
DATA$admit_year<-as.integer(DATA$admit_year)

DATA$dob_year <- as.POSIXct(DATA$DOB)
DATA$dob_year<-format(DATA$dob_year, "%Y")
DATA$dob_year<-as.integer(DATA$dob_year)

DATA$AGE <- DATA$admit_year-DATA$dob_year

# drop unused levels and rearrange variables so that age is with demographic DATA
DATA$dob_year<-NULL
DATA$admit_year<-NULL
droplevels(DATA)

names(DATA)
# reorganize the variables or easier viewing
DATA<-DATA[, c(3:27,29,51,30, 52, 49, 50,      
              71:106, 29, 51, 30, 52, 31,53, 
              32,54, 33,55, 34,56, 35,57, 36,
              58, 37,59, 38,60, 39,61, 40, 62, 
              41, 63, 42, 64, 43, 65, 44, 66, 
              45, 67, 46, 68, 47, 69, 48, 70)]
summary(DATA)
sort(sapply(X = DATA, FUN = function(x) sum(is.na(x))))
# drop variables with more than 20,000 missing values
DATA$GLUCOSE<-NULL
DATA$DIAGNOSES_20<-NULL
DATA$DIAGNOSES_20_TITLE<-NULL
DATA$DIAGNOSES_19<-NULL
DATA$DIAGNOSES_19_TITLE<-NULL
DATA$DIAGNOSES_18<-NULL
DATA$DIAGNOSES_18_TITLE<-NULL
DATA$DIAGNOSES_17<-NULL
DATA$DIAGNOSES_17_TITLE<-NULL
DATA$DIAGNOSES_16<-NULL
DATA$DIAGNOSES_16_TITLE<-NULL
DATA$DIAGNOSES_15<-NULL
DATA$DIAGNOSES_18_TITLE<-NULL
DATA$DIAGNOSES_14<-NULL
DATA$DIAGNOSES_14_TITLE<-NULL

droplevels(DATA)

summary(DATA)
# see if it is practical to convert the diagnoses columns to a factor variable
sort(table(as.factor(DATA$DIAGNOSES_1_TITLE)), decreasing=TRUE)

# change all the lesser count diagnoses into "other" category
DATA$DIAGNOSES_1_TITLE<-tolower(DATA$DIAGNOSES_1_TITLE)
str(DATA$DIAGNOSES_1_TITLE)
text<-DATA[, 27]
corp <- Corpus(VectorSource(text))
dtm <- DocumentTermMatrix(corp)

table(as.factor(DATA$DIAGNOSES_1_TITLE[(grepl("pneumonia", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) | grepl("pneum", DATA$DIAGNOSES_1_TITLE, fixed=TRUE))]))

DATA$DIAGNOSES_1_TITLE[(grepl("influ", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
)]<-"influenza"

DATA$DIAGNOSES_1_TITLE[(grepl("tuber", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                        | grepl("tb", DATA$DIAGNOSES_1_TITLE, fixed=TRUE)
)]<-"tuberculosis"


DATA$DIAGNOSES_1_TITLE[(grepl("pneumonia", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                        | grepl("pneum", DATA$DIAGNOSES_1_TITLE, fixed=TRUE))]<-"pnuemonia"


table(as.factor(DATA$DIAGNOSES_1_TITLE[(grep("fail", DATA$DIAGNOSES_1_TITLE))]))
DATA$DIAGNOSES_1_TITLE[(grepl("chron", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                        & grepl("resp",DATA$DIAGNOSES_1_TITLE , fixed=TRUE)
                        & grepl("fail", DATA$DIAGNOSES_1_TITLE, fixed=TRUE))]<-"chronic respiratory failure"

DATA$DIAGNOSES_1_TITLE[(grepl("emblsm", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                        | grepl("embol",DATA$DIAGNOSES_1_TITLE , fixed=TRUE)
                        )]<-"embolism"

DATA$DIAGNOSES_1_TITLE[(grepl("valve", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                        )]<-"valve disorder"

DATA$DIAGNOSES_1_TITLE[(grepl("alcohol", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) | grepl("alcoh", DATA$DIAGNOSES_1_TITLE, fixed=TRUE)
)]<-"alcohol"

DATA$DIAGNOSES_1_TITLE[(grepl("acute", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                        & grepl("resp",DATA$DIAGNOSES_1_TITLE , fixed=TRUE)
                        & grepl("fail", DATA$DIAGNOSES_1_TITLE, fixed=TRUE))]<-"acute respiratory failure"

DATA$DIAGNOSES_1_TITLE[(grepl("urin", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                        )]<-"urinary"


DATA$DIAGNOSES_1_TITLE[(grepl("postop", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                        | grepl("surg",DATA$DIAGNOSES_1_TITLE , fixed=TRUE)
                        | grepl("srg",DATA$DIAGNOSES_1_TITLE , fixed=TRUE)
)]<-"postop"

DATA$DIAGNOSES_1_TITLE[(grepl("stomach", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                        | grepl("intest",DATA$DIAGNOSES_1_TITLE , fixed=TRUE)
                        )]<-"gastrointestinal"

DATA$DIAGNOSES_1_TITLE[(grepl("cf", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                        | grepl("cystic",DATA$DIAGNOSES_1_TITLE , fixed=TRUE)
                        )]<-"cystic fibrosis"

DATA$DIAGNOSES_1_TITLE[grepl("aneu",DATA$DIAGNOSES_1_TITLE , fixed=TRUE)
                        ]<-"aneurism"

DATA$DIAGNOSES_1_TITLE[grepl("hernia",DATA$DIAGNOSES_1_TITLE , fixed=TRUE)
                       ]<-"hernia"

DATA$DIAGNOSES_1_TITLE[(grepl("chr", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                        & grepl("obst",DATA$DIAGNOSES_1_TITLE , fixed=TRUE)
                        )]<-"copd"

DATA$DIAGNOSES_1_TITLE[(grepl("pleur", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                        )]<-"pleurisy"

DATA$DIAGNOSES_1_TITLE[(grepl("ventr", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
)]<-"ventrical"


DATA$DIAGNOSES_1_TITLE[(grep("septicemia", DATA$DIAGNOSES_1_TITLE))]<-"septicemia"

DATA$DIAGNOSES_1_TITLE[(grep("pancrea", DATA$DIAGNOSES_1_TITLE))]<-"pancreas"

DATA$DIAGNOSES_1_TITLE[(grep("asthma", DATA$DIAGNOSES_1_TITLE))]<-"asthma"

DATA$DIAGNOSES_1_TITLE[(grep("melanoma", DATA$DIAGNOSES_1_TITLE))]<-"cancer"

DATA$DIAGNOSES_1_TITLE[(grep("mal", DATA$DIAGNOSES_1_TITLE))]<-"cancer"

DATA$DIAGNOSES_1_TITLE[(grep("pneumonitis", DATA$DIAGNOSES_1_TITLE))]<-"pneumonitis"

DATA$DIAGNOSES_1_TITLE[(grep("hemorr", DATA$DIAGNOSES_1_TITLE))]<-"hemorrhage"

DATA$DIAGNOSES_1_TITLE[(grep("bladder", DATA$DIAGNOSES_1_TITLE))]<-"bladder"

DATA$DIAGNOSES_1_TITLE[(grep("coma", DATA$DIAGNOSES_1_TITLE))]<-"coma"

DATA$DIAGNOSES_1_TITLE[(grepl("kidney", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) & (grepl("kid", DATA$DIAGNOSES_1_TITLE, fixed=TRUE)
                        ))]<-"kidney"

DATA$DIAGNOSES_1_TITLE[(grepl("heart", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                        & grepl("fail",DATA$DIAGNOSES_1_TITLE , fixed=TRUE)
                        )]<-"heart failure"
DATA$DIAGNOSES_1_TITLE[(grepl("hrt", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                        & grepl("fail",DATA$DIAGNOSES_1_TITLE , fixed=TRUE)
                        )]<-"heart failure"

DATA$DIAGNOSES_1_TITLE[(grepl("ami", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                      )]<-"heart failure"

DATA$DIAGNOSES_1_TITLE[(grepl("cancer", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                        )]<-"cancer"


DATA$DIAGNOSES_1_TITLE[(grepl("human imm", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                        | grepl("hiv", DATA$DIAGNOSES_1_TITLE, fixed=TRUE)
                        )]<-"hiv"

DATA$DIAGNOSES_1_TITLE[(grepl("tox", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) | grepl("pois", DATA$DIAGNOSES_1_TITLE, fixed=TRUE)
                      )]<-"toxin/poison"
DATA$DIAGNOSES_1_TITLE[(grepl("liver", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                        )]<-"liver"
DATA$DIAGNOSES_1_TITLE[(grepl("lymphoma", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                        | grepl("leuk", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                      )]<-"cancer"
DATA$DIAGNOSES_1_TITLE[(grepl("loc", DATA$DIAGNOSES_1_TITLE, fixed=TRUE)
)]<-"loss of consciousness"

DATA$DIAGNOSES_1_TITLE[(grepl("lumb", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                        | grepl("lmbr", DATA$DIAGNOSES_1_TITLE, fixed=TRUE)
                          )]<-"lumbar"

DATA$DIAGNOSES_1_TITLE[(grepl("lun", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                        & grepl("contusions",DATA$DIAGNOSES_1_TITLE , fixed=TRUE)
                        | grepl("injury",DATA$DIAGNOSES_1_TITLE , fixed=TRUE )
                        )]<-"lung injury"
DATA$DIAGNOSES_1_TITLE[(grepl("lung", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) & grepl("abs", DATA$DIAGNOSES_1_TITLE, fixed=TRUE)
)]<-"lung abscess"

DATA$DIAGNOSES_1_TITLE[(grepl("esop", DATA$DIAGNOSES_1_TITLE, fixed=TRUE))]<-"esophagus"

DATA$DIAGNOSES_1_TITLE[(grepl("contusion", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                        | grepl("inj", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                        | grepl("lacer", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                        | grepl("wound", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                        )]<-"non_lung injury"

DATA$DIAGNOSES_1_TITLE[(grepl("absc", DATA$DIAGNOSES_1_TITLE, fixed=TRUE)
)]<-"non_lung abscess"

DATA$DIAGNOSES_1_TITLE[(grepl("obstr", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
)]<-"obstruction"

DATA$DIAGNOSES_1_TITLE[(grepl("gastro", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) | grepl("gast", DATA$DIAGNOSES_1_TITLE, fixed=TRUE)
                      )]<-"gastrointestinal"
DATA$DIAGNOSES_1_TITLE[(grepl("brain", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) | grepl("encephalopathy", DATA$DIAGNOSES_1_TITLE, fixed=TRUE)
)]<-"non_lung injury"

DATA$DIAGNOSES_1_TITLE[(grepl("react", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) | grepl("allergy", DATA$DIAGNOSES_1_TITLE, fixed=TRUE)
)]<-"allergic reaction"

DATA$DIAGNOSES_1_TITLE[(grepl("compression of brain", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
)]<-"non_lung injury"

DATA$DIAGNOSES_1_TITLE[(grepl("bronc", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                        | grepl("brnch", DATA$DIAGNOSES_1_TITLE, fixed=TRUE)
                        )]<-"bronchittis"
DATA$DIAGNOSES_1_TITLE[(grepl("crnry", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                        | grepl("coronary", DATA$DIAGNOSES_1_TITLE, fixed=TRUE)
                        | grepl("chf", DATA$DIAGNOSES_1_TITLE, fixed=TRUE)
)]<-"heart failure"

DATA$DIAGNOSES_1_TITLE[(grepl("spine", DATA$DIAGNOSES_1_TITLE, fixed=TRUE) 
                        | grepl("spinal", DATA$DIAGNOSES_1_TITLE, fixed=TRUE)
)]<-"spinal injury/disease"

DATA$DIAGNOSES_1_TITLE[(grep("meth", DATA$DIAGNOSES_1_TITLE))]<-"meth lung"
DATA$DIAGNOSES_1_TITLE[(grep("tracheo", DATA$DIAGNOSES_1_TITLE))]<-"tracheostomy"
DATA$DIAGNOSES_1_TITLE[(grep("subendo", DATA$DIAGNOSES_1_TITLE))]<-"subendo"
DATA$DIAGNOSES_1_TITLE[(grep("fx", DATA$DIAGNOSES_1_TITLE))]<-"broken bone"
DATA$DIAGNOSES_1_TITLE[(grep("staph", DATA$DIAGNOSES_1_TITLE))]<-"staph infection"
DATA$DIAGNOSES_1_TITLE[(grep("bronc", DATA$DIAGNOSES_1_TITLE))]<-"bronchitis"
DATA$DIAGNOSES_1_TITLE[(grep("ulc", DATA$DIAGNOSES_1_TITLE))]<-"ulcer"
DATA$DIAGNOSES_1_TITLE[(grep("atri", DATA$DIAGNOSES_1_TITLE))]<-"atrial"
DATA$DIAGNOSES_1_TITLE[(grep("pulm", DATA$DIAGNOSES_1_TITLE))]<-"pulmonary other"
DATA$DIAGNOSES_1_TITLE[(grep("seps", DATA$DIAGNOSES_1_TITLE))]<-"sepsis"
DATA$DIAGNOSES_1_TITLE[(grep("aort", DATA$DIAGNOSES_1_TITLE))]<-"aorta"
DATA$DIAGNOSES_1_TITLE[(grep("empyema", DATA$DIAGNOSES_1_TITLE))]<-"pleurisy"
DATA$DIAGNOSES_1_TITLE[(grep("asth", DATA$DIAGNOSES_1_TITLE))]<-"asthma"
DATA$DIAGNOSES_1_TITLE[(grep("art", DATA$DIAGNOSES_1_TITLE))]<-"artery"
DATA$DIAGNOSES_1_TITLE[(grep("colon", DATA$DIAGNOSES_1_TITLE))]<-"colon"
DATA$DIAGNOSES_1_TITLE[(grep("hemoth", DATA$DIAGNOSES_1_TITLE))]<-"hemothorax"
DATA$DIAGNOSES_1_TITLE[(grep("spr", DATA$DIAGNOSES_1_TITLE))]<-"sprain"
DATA$DIAGNOSES_1_TITLE[(grep("spleen", DATA$DIAGNOSES_1_TITLE))]<-"spleen"
DATA$DIAGNOSES_1_TITLE[(grep("obesity", DATA$DIAGNOSES_1_TITLE))]<-"obesity"
DATA$DIAGNOSES_1_TITLE[(grep("strep", DATA$DIAGNOSES_1_TITLE))]<-"strep"
DATA$DIAGNOSES_1_TITLE[(grep("hypert", DATA$DIAGNOSES_1_TITLE))]<-"hypertension"
DATA$DIAGNOSES_1_TITLE[(grep("convu", DATA$DIAGNOSES_1_TITLE))]<-"convulsions"
DATA$DIAGNOSES_1_TITLE[(grepl("bact", DATA$DIAGNOSES_1_TITLE) | grepl("inf", DATA$DIAGNOSES_1_TITLE))]<-"bacterial infection"
DATA$DIAGNOSES_1_TITLE[(grepl("pericard", DATA$DIAGNOSES_1_TITLE) | grepl("cardiac", DATA$DIAGNOSES_1_TITLE))]<-"heart failure"
DATA$DIAGNOSES_1_TITLE[(grep("int", DATA$DIAGNOSES_1_TITLE))]<-"gastrointestinal"
DATA$DIAGNOSES_1_TITLE[(grep("hepatic", DATA$DIAGNOSES_1_TITLE))]<-"liver"
DATA$DIAGNOSES_1_TITLE[(grep("keto", DATA$DIAGNOSES_1_TITLE))]<-"diabetes"
DATA$DIAGNOSES_1_TITLE[(grepl("edem", DATA$DIAGNOSES_1_TITLE) | grepl("swell", DATA$DIAGNOSES_1_TITLE))]<-"swelling"
DATA$DIAGNOSES_1_TITLE[(grepl("dev", DATA$DIAGNOSES_1_TITLE) | grepl("graft", DATA$DIAGNOSES_1_TITLE))]<-"device/graft complications"

# seperate out the factor levels with more than count of 150
high_count<-DATA[(DATA$DIAGNOSES_1_TITLE %in% c("septicemia", "pnuemonia", "tuberculosis","acute respiratory failure", "influenza","non_lung abscess","obstruction","bronchittis", "cholangitis", "swelling",
                                               "heart failure","hemorrhage", "subendo", "coma", "gastrointenstinal", "broken bone", 
                                               "chronic respiratory failure","valve disorder", "liver", "toxin/poison", "kidney", 
                                               "meth lung", "embolism", "pancreas", "aneurism", "non_lung injury", "pleurisy", "device/graft complications",
                                               "postop", "cancer", "ventrical", "ulcer", "asthma", "spinal injury/disease", "tracheostomy",
                                               "hiv", "atrial", "ventrical", "ulcer", "asthma", "alcohol", "aorta", "cystic fibrosis",
                                               "urinary", "loss of consciousness", "colon", "bacterial infection", "hemothorax", "diabetes",
                                               "spleen", "lumbar", "obesity", "strep", "hypertension", "bladder","convulsions", "esophagus","blood in stool")), ]

# seperate out the factor levels with more than count of 150
low_count<-DATA[!(DATA$DIAGNOSES_1_TITLE %in% c("septicemia", "pnuemonia", "acute respiratory failure", "tuberculosis", "influenza","non_lung abscess","obstruction","bronchittis", "cholangitis", "swelling",
                                                "heart failure","hemorrhage", "subendo", "coma", "gastrointenstinal", "broken bone", 
                                                "chronic respiratory failure","valve disorder", "liver", "toxin/poison", "kidney", 
                                                "meth lung", "embolism", "pancreas", "aneurism", "non_lung injury", "esophagus","pleurisy", "device/graft complications",
                                                "postop", "cancer", "ventrical", "ulcer", "asthma", "spinal injury/disease", "tracheostomy",
                                                "hiv", "atrial", "ventrical", "ulcer", "asthma", "alcohol", "aorta", "cystic fibrosis",
                                                "urinary", "loss of consciousness", "colon", "bacterial infection", "hemothorax", "diabetes",
                                                "spleen", "lumbar", "obesity", "strep", "hypertension", "bladder","convulsions", "blood in stool")), ]
# change the categories with less than 100 count to "other" category
low_count$DIAGNOSES_1_TITLE<-"other"
# rejoin the two parts of the data
DATA<-rbind(high_count, low_count)
sort(table(as.factor(DATA$DIAGNOSES_1_TITLE)))
DATA$DIAGNOSES_1_TITLE<-as.factor(DATA$DIAGNOSES_1_TITLE)



# see if it is practical to convert the diagnoses columns to a factor variable
sort(table(as.factor(DATA$DIAGNOSES_2_TITLE)), decreasing=TRUE)

if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
# change all the lesser count diagnoses into "other" category
DATA$DIAGNOSES_2_TITLE<-tolower(DATA$DIAGNOSES_2_TITLE)
str(DATA$DIAGNOSES_2_TITLE)
text<-DATA[, 27]
corp <- Corpus(VectorSource(text))
dtm <- DocumentTermMatrix(corp)

DATA$DIAGNOSES_2_TITLE[(grepl("influ", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
)]<-"influenza"

table(as.factor(DATA$DIAGNOSES_2_TITLE[(grepl("pneumonia", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) | grepl("pneum", DATA$DIAGNOSES_2_TITLE, fixed=TRUE))]))
DATA$DIAGNOSES_2_TITLE[(grepl("pneumonia", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
                        | grepl("pneum", DATA$DIAGNOSES_2_TITLE, fixed=TRUE))]<-"pnuemonia"

DATA$DIAGNOSES_2_TITLE[(grepl("tuber", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
                        | grepl("tb", DATA$DIAGNOSES_2_TITLE, fixed=TRUE)
)]<-"tuberculosis"

DATA$DIAGNOSES_2_TITLE[(grepl("brain", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) | grepl("encephalopathy", DATA$DIAGNOSES_2_TITLE, fixed=TRUE)
)]<-"non_lung injury"

table(as.factor(DATA$DIAGNOSES_2_TITLE[(grep("fail", DATA$DIAGNOSES_2_TITLE))]))
DATA$DIAGNOSES_2_TITLE[(grepl("ch", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
                        & grepl("resp",DATA$DIAGNOSES_2_TITLE , fixed=TRUE)
                        & grepl("fail", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
                        | grepl("flr", DATA$DIAGNOSES_2_TITLE, fixed=TRUE))]<-"chronic respiratory failure"

DATA$DIAGNOSES_2_TITLE[(grepl("emblsm", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
                        | grepl("embol",DATA$DIAGNOSES_2_TITLE , fixed=TRUE)
)]<-"embolism"

DATA$DIAGNOSES_2_TITLE[(grepl("valve", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
)]<-"valve disorder"

DATA$DIAGNOSES_2_TITLE[(grepl("lung", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) & grepl("abs", DATA$DIAGNOSES_2_TITLE, fixed=TRUE)
)]<-"lung abscess"

DATA$DIAGNOSES_2_TITLE[(grepl("obstr", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
)]<-"obstruction"

DATA$DIAGNOSES_2_TITLE[(grepl("absc", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
)]<-"non_lung abscess"

DATA$DIAGNOSES_2_TITLE[(grepl("esop", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
)]<-"esophagus"

DATA$DIAGNOSES_2_TITLE[(grepl("kidny", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) | grepl("renal", DATA$DIAGNOSES_2_TITLE, fixed=TRUE)
)]<-"kidney"

DATA$DIAGNOSES_2_TITLE[(grepl("alcohol", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) | grepl("alcoh", DATA$DIAGNOSES_2_TITLE, fixed=TRUE)
)]<-"alcohol"

DATA$DIAGNOSES_2_TITLE[(grepl("ac", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
                        & grepl("resp",DATA$DIAGNOSES_2_TITLE , fixed=TRUE)
                        & grepl("fail", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
                        | grepl("flr", DATA$DIAGNOSES_2_TITLE, fixed=TRUE))]<-"acute respiratory failure"

DATA$DIAGNOSES_2_TITLE[(grepl("urin", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
)]<-"urinary"


DATA$DIAGNOSES_2_TITLE[(grepl("postop", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
                        | grepl("surg",DATA$DIAGNOSES_2_TITLE , fixed=TRUE)
                        | grepl("srg",DATA$DIAGNOSES_2_TITLE , fixed=TRUE)
)]<-"postop"

DATA$DIAGNOSES_2_TITLE[(grepl("stomach", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
                        | grepl("intest",DATA$DIAGNOSES_2_TITLE , fixed=TRUE)
)]<-"gastrointestinal"

DATA$DIAGNOSES_2_TITLE[(grepl("cf", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
                        | grepl("cystic",DATA$DIAGNOSES_2_TITLE , fixed=TRUE)
)]<-"cystic fibrosis"

DATA$DIAGNOSES_2_TITLE[grepl("aneu",DATA$DIAGNOSES_2_TITLE , fixed=TRUE)
                       ]<-"aneurism"

DATA$DIAGNOSES_2_TITLE[grepl("hernia",DATA$DIAGNOSES_2_TITLE , fixed=TRUE)
                       ]<-"hernia"

DATA$DIAGNOSES_2_TITLE[(grepl("chr", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
                        & grepl("obst",DATA$DIAGNOSES_2_TITLE , fixed=TRUE)
)]<-"copd"

DATA$DIAGNOSES_2_TITLE[(grepl("react", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) | grepl("allergy", DATA$DIAGNOSES_2_TITLE, fixed=TRUE)
)]<-"allergic reaction"


DATA$DIAGNOSES_2_TITLE[(grepl("pleur", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
)]<-"pleurisy"

DATA$DIAGNOSES_2_TITLE[(grepl("ventr", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
)]<-"ventrical"


DATA$DIAGNOSES_2_TITLE[(grep("septicemia", DATA$DIAGNOSES_2_TITLE))]<-"septicemia"

DATA$DIAGNOSES_2_TITLE[(grep("pancrea", DATA$DIAGNOSES_2_TITLE))]<-"pancreas"

DATA$DIAGNOSES_2_TITLE[(grep("asthma", DATA$DIAGNOSES_2_TITLE))]<-"asthma"

DATA$DIAGNOSES_2_TITLE[(grep("melanoma", DATA$DIAGNOSES_2_TITLE))]<-"cancer"

DATA$DIAGNOSES_2_TITLE[(grep("mal", DATA$DIAGNOSES_2_TITLE))]<-"cancer"

DATA$DIAGNOSES_2_TITLE[(grep("pneumonitis", DATA$DIAGNOSES_2_TITLE))]<-"pneumonitis"

DATA$DIAGNOSES_2_TITLE[(grep("hemorr", DATA$DIAGNOSES_2_TITLE))]<-"hemorrhage"

DATA$DIAGNOSES_2_TITLE[(grep("bladder", DATA$DIAGNOSES_2_TITLE))]<-"bladder"

DATA$DIAGNOSES_2_TITLE[(grep("coma", DATA$DIAGNOSES_2_TITLE))]<-"coma"

DATA$DIAGNOSES_2_TITLE[(grepl("kidney", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) & (grepl("kid", DATA$DIAGNOSES_2_TITLE, fixed=TRUE)
))]<-"kidney"

DATA$DIAGNOSES_2_TITLE[(grepl("heart", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
                        & grepl("fail",DATA$DIAGNOSES_2_TITLE , fixed=TRUE)
)]<-"heart failure"
DATA$DIAGNOSES_2_TITLE[(grepl("hrt", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
                        & grepl("fail",DATA$DIAGNOSES_2_TITLE , fixed=TRUE)
)]<-"heart failure"

DATA$DIAGNOSES_2_TITLE[(grepl("ami", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) | grepl("cardio", DATA$DIAGNOSES_2_TITLE, fixed=TRUE)
)]<-"heart failure"

DATA$DIAGNOSES_2_TITLE[(grepl("cancer", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
)]<-"cancer"


DATA$DIAGNOSES_2_TITLE[(grepl("human imm", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
                        | grepl("hiv", DATA$DIAGNOSES_2_TITLE, fixed=TRUE)
)]<-"hiv"

DATA$DIAGNOSES_2_TITLE[(grepl("tox", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) | grepl("pois", DATA$DIAGNOSES_2_TITLE, fixed=TRUE)
)]<-"toxin/poison"
DATA$DIAGNOSES_2_TITLE[(grepl("liver", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
)]<-"liver"
DATA$DIAGNOSES_2_TITLE[(grepl("lymphoma", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
                        | grepl("leuk", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
)]<-"cancer"
DATA$DIAGNOSES_2_TITLE[(grepl("loc", DATA$DIAGNOSES_2_TITLE, fixed=TRUE)
)]<-"loss of consciousness"

DATA$DIAGNOSES_2_TITLE[(grepl("lumb", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
                        | grepl("lmbr", DATA$DIAGNOSES_2_TITLE, fixed=TRUE)
)]<-"lumbar"



DATA$DIAGNOSES_2_TITLE[(grepl("lun", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
                        & grepl("contusions",DATA$DIAGNOSES_2_TITLE , fixed=TRUE)
                        | grepl("injury",DATA$DIAGNOSES_2_TITLE , fixed=TRUE )
)]<-"lung injury"

DATA$DIAGNOSES_2_TITLE[(grepl("contusion", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
                        | grepl("inj", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
                        | grepl("lacer", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
                        | grepl("wound", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
)]<-"non_lung injury"

DATA$DIAGNOSES_2_TITLE[(grepl("gastro", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) | grepl("gast", DATA$DIAGNOSES_2_TITLE, fixed=TRUE)
)]<-"gastrointestinal"

DATA$DIAGNOSES_2_TITLE[(grepl("react", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) | grepl("allergy", DATA$DIAGNOSES_2_TITLE, fixed=TRUE)
)]<-"allergic reaction"

DATA$DIAGNOSES_2_TITLE[(grepl("bronc", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
                        | grepl("brnch", DATA$DIAGNOSES_2_TITLE, fixed=TRUE)
)]<-"bronchittis"
DATA$DIAGNOSES_2_TITLE[(grepl("crnry", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
                        | grepl("coronary", DATA$DIAGNOSES_2_TITLE, fixed=TRUE)
                        | grepl("chf", DATA$DIAGNOSES_2_TITLE, fixed=TRUE)
)]<-"heart failure"

DATA$DIAGNOSES_2_TITLE[(grepl("spine", DATA$DIAGNOSES_2_TITLE, fixed=TRUE) 
                        | grepl("spinal", DATA$DIAGNOSES_2_TITLE, fixed=TRUE)
)]<-"spinal injury/disease"

DATA$DIAGNOSES_2_TITLE[(grep("meth", DATA$DIAGNOSES_2_TITLE))]<-"meth lung"
DATA$DIAGNOSES_2_TITLE[(grep("tracheo", DATA$DIAGNOSES_2_TITLE))]<-"tracheostomy"
DATA$DIAGNOSES_2_TITLE[(grep("subendo", DATA$DIAGNOSES_2_TITLE))]<-"subendo"
DATA$DIAGNOSES_2_TITLE[(grep("fx", DATA$DIAGNOSES_2_TITLE))]<-"broken bone"
DATA$DIAGNOSES_2_TITLE[(grep("staph", DATA$DIAGNOSES_2_TITLE))]<-"staph infection"
DATA$DIAGNOSES_2_TITLE[(grep("bronc", DATA$DIAGNOSES_2_TITLE))]<-"bronchitis"
DATA$DIAGNOSES_2_TITLE[(grep("ulc", DATA$DIAGNOSES_2_TITLE))]<-"ulcer"
DATA$DIAGNOSES_2_TITLE[(grep("atri", DATA$DIAGNOSES_2_TITLE))]<-"atrial"
DATA$DIAGNOSES_2_TITLE[(grep("pulm", DATA$DIAGNOSES_2_TITLE))]<-"pulmonary other"
DATA$DIAGNOSES_2_TITLE[(grep("seps", DATA$DIAGNOSES_2_TITLE))]<-"sepsis"
DATA$DIAGNOSES_2_TITLE[(grep("aort", DATA$DIAGNOSES_2_TITLE))]<-"aorta"
DATA$DIAGNOSES_2_TITLE[(grep("empyema", DATA$DIAGNOSES_2_TITLE))]<-"pleurisy"
DATA$DIAGNOSES_2_TITLE[(grep("asth", DATA$DIAGNOSES_2_TITLE))]<-"asthma"
DATA$DIAGNOSES_2_TITLE[(grep("art", DATA$DIAGNOSES_2_TITLE))]<-"artery"
DATA$DIAGNOSES_2_TITLE[(grep("colon", DATA$DIAGNOSES_2_TITLE))]<-"colon"
DATA$DIAGNOSES_2_TITLE[(grep("hemoth", DATA$DIAGNOSES_2_TITLE))]<-"hemothorax"
DATA$DIAGNOSES_2_TITLE[(grep("spr", DATA$DIAGNOSES_2_TITLE))]<-"sprain"
DATA$DIAGNOSES_2_TITLE[(grep("spleen", DATA$DIAGNOSES_2_TITLE))]<-"spleen"
DATA$DIAGNOSES_2_TITLE[(grep("obesity", DATA$DIAGNOSES_2_TITLE))]<-"obesity"
DATA$DIAGNOSES_2_TITLE[(grep("strep", DATA$DIAGNOSES_2_TITLE))]<-"strep"
DATA$DIAGNOSES_2_TITLE[(grep("hypert", DATA$DIAGNOSES_2_TITLE))]<-"hypertension"
DATA$DIAGNOSES_2_TITLE[(grep("convu", DATA$DIAGNOSES_2_TITLE))]<-"convulsions"
DATA$DIAGNOSES_2_TITLE[(grepl("bact", DATA$DIAGNOSES_2_TITLE) | grepl("inf", DATA$DIAGNOSES_2_TITLE))]<-"bacterial infection"
DATA$DIAGNOSES_2_TITLE[(grepl("pericard", DATA$DIAGNOSES_2_TITLE) | grepl("cardiac", DATA$DIAGNOSES_2_TITLE))]<-"heart failure"
DATA$DIAGNOSES_2_TITLE[(grep("int", DATA$DIAGNOSES_2_TITLE))]<-"gastrointestinal"
DATA$DIAGNOSES_2_TITLE[(grep("hepatic", DATA$DIAGNOSES_2_TITLE))]<-"liver"
DATA$DIAGNOSES_2_TITLE[(grep("keto", DATA$DIAGNOSES_2_TITLE))]<-"diabetes"
DATA$DIAGNOSES_2_TITLE[(grepl("edem", DATA$DIAGNOSES_2_TITLE) | grepl("swell", DATA$DIAGNOSES_2_TITLE))]<-"swelling"
DATA$DIAGNOSES_2_TITLE[(grepl("dev", DATA$DIAGNOSES_2_TITLE) | grepl("graft", DATA$DIAGNOSES_2_TITLE))]<-"device/graft complications"

# seperate out the factor levels with more than count of 150
high_count<-DATA[(DATA$DIAGNOSES_2_TITLE %in% c("septicemia", "pnuemonia", "non_lung abscess","obstruction","acute respiratory failure", "esophagus","bronchittis", "cholangitis", "swelling",
                                                "heart failure","hemorrhage", "subendo", "coma", "gastrointenstinal", "broken bone", 
                                                "chronic respiratory failure","tuberculosis","valve disorder", "liver", "toxin/poison", "kidney", 
                                                "meth lung", "embolism", "pancreas", "aneurism", "non_lung injury", "pleurisy", "device/graft complications",
                                                "postop", "cancer", "ventrical", "ulcer", "asthma", "spinal injury/disease", "tracheostomy",
                                                "hiv", "atrial", "ventrical", "ulcer", "asthma", "alcohol", "aorta", "cystic fibrosis",
                                                "urinary", "loss of consciousness", "colon", "bacterial infection", "hemothorax", "diabetes",
                                                "spleen", "lumbar", "obesity", "strep", "hypertension", "influenza","bladder","convulsions", "blood in stool")), ]

# seperate out the factor levels with more than count of 150
low_count<-DATA[!(DATA$DIAGNOSES_2_TITLE %in% c("septicemia", "pnuemonia", "tuberculosis", "acute respiratory failure", "influenza","non_lung abscess","obstruction", "esophagus","bronchittis", "cholangitis", "swelling",
                                                "heart failure","hemorrhage", "subendo", "coma", "gastrointenstinal", "broken bone", 
                                                "chronic respiratory failure","valve disorder", "liver", "toxin/poison", "kidney", 
                                                "meth lung", "embolism", "pancreas", "aneurism", "non_lung injury", "pleurisy", "device/graft complications",
                                                "postop", "cancer", "ventrical", "ulcer", "asthma", "spinal injury/disease", "tracheostomy",
                                                "hiv", "atrial", "ventrical", "ulcer", "asthma", "alcohol", "aorta", "cystic fibrosis",
                                                "urinary", "loss of consciousness", "colon", "bacterial infection", "hemothorax", "diabetes",
                                                "spleen", "lumbar", "obesity", "strep", "hypertension", "bladder","convulsions", "blood in stool")), ]
# change the categories with less than 100 count to "other" category
low_count$DIAGNOSES_2_TITLE<-"other"
# rejoin the two parts of the data
DATA<-rbind(high_count, low_count)
sort(table(as.factor(DATA$DIAGNOSES_2_TITLE)))
DATA$DIAGNOSES_2_TITLE<-as.factor(DATA$DIAGNOSES_2_TITLE)

# see if it is practical to convert the diagnoses columns to a factor variable
sort(table(as.factor(DATA$DIAGNOSES_3_TITLE)), decreasing=TRUE)

if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
# change all the lesser count diagnoses into "other" category
DATA$DIAGNOSES_3_TITLE<-tolower(DATA$DIAGNOSES_3_TITLE)
str(DATA$DIAGNOSES_3_TITLE)
text<-DATA[, 27]
corp <- Corpus(VectorSource(text))
dtm <- DocumentTermMatrix(corp)

DATA$DIAGNOSES_3_TITLE[(grepl("influ", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
)]<-"influenza"

table(as.factor(DATA$DIAGNOSES_3_TITLE[(grepl("pneumonia", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) | grepl("pneum", DATA$DIAGNOSES_3_TITLE, fixed=TRUE))]))
DATA$DIAGNOSES_3_TITLE[(grepl("pneumonia", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
                        | grepl("pneum", DATA$DIAGNOSES_3_TITLE, fixed=TRUE))]<-"pnuemonia"

DATA$DIAGNOSES_3_TITLE[(grepl("tuber", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
                        | grepl("tb", DATA$DIAGNOSES_3_TITLE, fixed=TRUE)
)]<-"tuberculosis"

DATA$DIAGNOSES_3_TITLE[(grepl("brain", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) | grepl("encephalopathy", DATA$DIAGNOSES_3_TITLE, fixed=TRUE)
)]<-"non_lung injury"

table(as.factor(DATA$DIAGNOSES_3_TITLE[(grep("fail", DATA$DIAGNOSES_3_TITLE))]))
DATA$DIAGNOSES_3_TITLE[(grepl("ch", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
                        & grepl("resp",DATA$DIAGNOSES_3_TITLE , fixed=TRUE)
                        & grepl("fail", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
                        | grepl("flr", DATA$DIAGNOSES_3_TITLE, fixed=TRUE))]<-"chronic respiratory failure"

DATA$DIAGNOSES_3_TITLE[(grepl("emblsm", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
                        | grepl("embol",DATA$DIAGNOSES_3_TITLE , fixed=TRUE)
)]<-"embolism"

DATA$DIAGNOSES_3_TITLE[(grepl("valve", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
)]<-"valve disorder"

DATA$DIAGNOSES_3_TITLE[(grepl("lung", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) & grepl("abs", DATA$DIAGNOSES_3_TITLE, fixed=TRUE)
)]<-"lung abscess"

DATA$DIAGNOSES_3_TITLE[(grepl("obstr", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
)]<-"obstruction"

DATA$DIAGNOSES_3_TITLE[(grepl("absc", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
)]<-"non_lung abscess"

DATA$DIAGNOSES_3_TITLE[(grepl("esop", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
)]<-"esophagus"

DATA$DIAGNOSES_3_TITLE[(grepl("kidny", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) | grepl("renal", DATA$DIAGNOSES_3_TITLE, fixed=TRUE)
)]<-"kidney"

DATA$DIAGNOSES_3_TITLE[(grepl("alcohol", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) | grepl("alcoh", DATA$DIAGNOSES_3_TITLE, fixed=TRUE)
)]<-"alcohol"

DATA$DIAGNOSES_3_TITLE[(grepl("ac", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
                        & grepl("resp",DATA$DIAGNOSES_3_TITLE , fixed=TRUE)
                        & grepl("fail", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
                        | grepl("flr", DATA$DIAGNOSES_3_TITLE, fixed=TRUE))]<-"acute respiratory failure"

DATA$DIAGNOSES_3_TITLE[(grepl("urin", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
)]<-"urinary"


DATA$DIAGNOSES_3_TITLE[(grepl("postop", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
                        | grepl("surg",DATA$DIAGNOSES_3_TITLE , fixed=TRUE)
                        | grepl("srg",DATA$DIAGNOSES_3_TITLE , fixed=TRUE)
)]<-"postop"

DATA$DIAGNOSES_3_TITLE[(grepl("stomach", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
                        | grepl("intest",DATA$DIAGNOSES_3_TITLE , fixed=TRUE)
)]<-"gastrointestinal"

DATA$DIAGNOSES_3_TITLE[(grepl("cf", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
                        | grepl("cystic",DATA$DIAGNOSES_3_TITLE , fixed=TRUE)
)]<-"cystic fibrosis"

DATA$DIAGNOSES_3_TITLE[grepl("aneu",DATA$DIAGNOSES_3_TITLE , fixed=TRUE)
                       ]<-"aneurism"

DATA$DIAGNOSES_3_TITLE[grepl("hernia",DATA$DIAGNOSES_3_TITLE , fixed=TRUE)
                       ]<-"hernia"

DATA$DIAGNOSES_3_TITLE[(grepl("chr", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
                        & grepl("obst",DATA$DIAGNOSES_3_TITLE , fixed=TRUE)
)]<-"copd"

DATA$DIAGNOSES_3_TITLE[(grepl("react", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) | grepl("allergy", DATA$DIAGNOSES_3_TITLE, fixed=TRUE)
)]<-"allergic reaction"


DATA$DIAGNOSES_3_TITLE[(grepl("pleur", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
)]<-"pleurisy"

DATA$DIAGNOSES_3_TITLE[(grepl("ventr", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
)]<-"ventrical"


DATA$DIAGNOSES_3_TITLE[(grep("septicemia", DATA$DIAGNOSES_3_TITLE))]<-"septicemia"

DATA$DIAGNOSES_3_TITLE[(grep("pancrea", DATA$DIAGNOSES_3_TITLE))]<-"pancreas"

DATA$DIAGNOSES_3_TITLE[(grep("asthma", DATA$DIAGNOSES_3_TITLE))]<-"asthma"

DATA$DIAGNOSES_3_TITLE[(grep("melanoma", DATA$DIAGNOSES_3_TITLE))]<-"cancer"

DATA$DIAGNOSES_3_TITLE[(grep("mal", DATA$DIAGNOSES_3_TITLE))]<-"cancer"

DATA$DIAGNOSES_3_TITLE[(grep("pneumonitis", DATA$DIAGNOSES_3_TITLE))]<-"pneumonitis"

DATA$DIAGNOSES_3_TITLE[(grep("hemorr", DATA$DIAGNOSES_3_TITLE))]<-"hemorrhage"

DATA$DIAGNOSES_3_TITLE[(grep("bladder", DATA$DIAGNOSES_3_TITLE))]<-"bladder"

DATA$DIAGNOSES_3_TITLE[(grep("coma", DATA$DIAGNOSES_3_TITLE))]<-"coma"

DATA$DIAGNOSES_3_TITLE[(grepl("kidney", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) & (grepl("kid", DATA$DIAGNOSES_3_TITLE, fixed=TRUE)
))]<-"kidney"

DATA$DIAGNOSES_3_TITLE[(grepl("heart", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
                        & grepl("fail",DATA$DIAGNOSES_3_TITLE , fixed=TRUE)
)]<-"heart failure"
DATA$DIAGNOSES_3_TITLE[(grepl("hrt", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
                        & grepl("fail",DATA$DIAGNOSES_3_TITLE , fixed=TRUE)
)]<-"heart failure"

DATA$DIAGNOSES_3_TITLE[(grepl("ami", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) | grepl("cardio", DATA$DIAGNOSES_3_TITLE, fixed=TRUE)
)]<-"heart failure"

DATA$DIAGNOSES_3_TITLE[(grepl("cancer", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
)]<-"cancer"


DATA$DIAGNOSES_3_TITLE[(grepl("human imm", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
                        | grepl("hiv", DATA$DIAGNOSES_3_TITLE, fixed=TRUE)
)]<-"hiv"

DATA$DIAGNOSES_3_TITLE[(grepl("tox", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) | grepl("pois", DATA$DIAGNOSES_3_TITLE, fixed=TRUE)
)]<-"toxin/poison"
DATA$DIAGNOSES_3_TITLE[(grepl("liver", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
)]<-"liver"
DATA$DIAGNOSES_3_TITLE[(grepl("lymphoma", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
                        | grepl("leuk", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
)]<-"cancer"
DATA$DIAGNOSES_3_TITLE[(grepl("loc", DATA$DIAGNOSES_3_TITLE, fixed=TRUE)
)]<-"loss of consciousness"

DATA$DIAGNOSES_3_TITLE[(grepl("lumb", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
                        | grepl("lmbr", DATA$DIAGNOSES_3_TITLE, fixed=TRUE)
)]<-"lumbar"



DATA$DIAGNOSES_3_TITLE[(grepl("lun", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
                        & grepl("contusions",DATA$DIAGNOSES_3_TITLE , fixed=TRUE)
                        | grepl("injury",DATA$DIAGNOSES_3_TITLE , fixed=TRUE )
)]<-"lung injury"

DATA$DIAGNOSES_3_TITLE[(grepl("contusion", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
                        | grepl("inj", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
                        | grepl("lacer", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
                        | grepl("wound", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
)]<-"non_lung injury"

DATA$DIAGNOSES_3_TITLE[(grepl("gastro", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) | grepl("gast", DATA$DIAGNOSES_3_TITLE, fixed=TRUE)
)]<-"gastrointestinal"

DATA$DIAGNOSES_3_TITLE[(grepl("react", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) | grepl("allergy", DATA$DIAGNOSES_3_TITLE, fixed=TRUE)
)]<-"allergic reaction"

DATA$DIAGNOSES_3_TITLE[(grepl("bronc", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
                        | grepl("brnch", DATA$DIAGNOSES_3_TITLE, fixed=TRUE)
)]<-"bronchittis"
DATA$DIAGNOSES_3_TITLE[(grepl("crnry", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
                        | grepl("coronary", DATA$DIAGNOSES_3_TITLE, fixed=TRUE)
                        | grepl("chf", DATA$DIAGNOSES_3_TITLE, fixed=TRUE)
)]<-"heart failure"

DATA$DIAGNOSES_3_TITLE[(grepl("spine", DATA$DIAGNOSES_3_TITLE, fixed=TRUE) 
                        | grepl("spinal", DATA$DIAGNOSES_3_TITLE, fixed=TRUE)
)]<-"spinal injury/disease"

DATA$DIAGNOSES_3_TITLE[(grep("meth", DATA$DIAGNOSES_3_TITLE))]<-"meth lung"
DATA$DIAGNOSES_3_TITLE[(grep("tracheo", DATA$DIAGNOSES_3_TITLE))]<-"tracheostomy"
DATA$DIAGNOSES_3_TITLE[(grep("subendo", DATA$DIAGNOSES_3_TITLE))]<-"subendo"
DATA$DIAGNOSES_3_TITLE[(grep("fx", DATA$DIAGNOSES_3_TITLE))]<-"broken bone"
DATA$DIAGNOSES_3_TITLE[(grep("staph", DATA$DIAGNOSES_3_TITLE))]<-"staph infection"
DATA$DIAGNOSES_3_TITLE[(grep("bronc", DATA$DIAGNOSES_3_TITLE))]<-"bronchitis"
DATA$DIAGNOSES_3_TITLE[(grep("ulc", DATA$DIAGNOSES_3_TITLE))]<-"ulcer"
DATA$DIAGNOSES_3_TITLE[(grep("atri", DATA$DIAGNOSES_3_TITLE))]<-"atrial"
DATA$DIAGNOSES_3_TITLE[(grep("pulm", DATA$DIAGNOSES_3_TITLE))]<-"pulmonary other"
DATA$DIAGNOSES_3_TITLE[(grep("seps", DATA$DIAGNOSES_3_TITLE))]<-"sepsis"
DATA$DIAGNOSES_3_TITLE[(grep("aort", DATA$DIAGNOSES_3_TITLE))]<-"aorta"
DATA$DIAGNOSES_3_TITLE[(grep("empyema", DATA$DIAGNOSES_3_TITLE))]<-"pleurisy"
DATA$DIAGNOSES_3_TITLE[(grep("asth", DATA$DIAGNOSES_3_TITLE))]<-"asthma"
DATA$DIAGNOSES_3_TITLE[(grep("art", DATA$DIAGNOSES_3_TITLE))]<-"artery"
DATA$DIAGNOSES_3_TITLE[(grep("colon", DATA$DIAGNOSES_3_TITLE))]<-"colon"
DATA$DIAGNOSES_3_TITLE[(grep("hemoth", DATA$DIAGNOSES_3_TITLE))]<-"hemothorax"
DATA$DIAGNOSES_3_TITLE[(grep("spr", DATA$DIAGNOSES_3_TITLE))]<-"sprain"
DATA$DIAGNOSES_3_TITLE[(grep("spleen", DATA$DIAGNOSES_3_TITLE))]<-"spleen"
DATA$DIAGNOSES_3_TITLE[(grep("obesity", DATA$DIAGNOSES_3_TITLE))]<-"obesity"
DATA$DIAGNOSES_3_TITLE[(grep("strep", DATA$DIAGNOSES_3_TITLE))]<-"strep"
DATA$DIAGNOSES_3_TITLE[(grep("hypert", DATA$DIAGNOSES_3_TITLE))]<-"hypertension"
DATA$DIAGNOSES_3_TITLE[(grep("convu", DATA$DIAGNOSES_3_TITLE))]<-"convulsions"
DATA$DIAGNOSES_3_TITLE[(grepl("bact", DATA$DIAGNOSES_3_TITLE) | grepl("inf", DATA$DIAGNOSES_3_TITLE))]<-"bacterial infection"
DATA$DIAGNOSES_3_TITLE[(grepl("pericard", DATA$DIAGNOSES_3_TITLE) | grepl("cardiac", DATA$DIAGNOSES_3_TITLE))]<-"heart failure"
DATA$DIAGNOSES_3_TITLE[(grep("int", DATA$DIAGNOSES_3_TITLE))]<-"gastrointestinal"
DATA$DIAGNOSES_3_TITLE[(grep("hepatic", DATA$DIAGNOSES_3_TITLE))]<-"liver"
DATA$DIAGNOSES_3_TITLE[(grep("keto", DATA$DIAGNOSES_3_TITLE))]<-"diabetes"
DATA$DIAGNOSES_3_TITLE[(grepl("edem", DATA$DIAGNOSES_3_TITLE) | grepl("swell", DATA$DIAGNOSES_3_TITLE))]<-"swelling"
DATA$DIAGNOSES_3_TITLE[(grepl("dev", DATA$DIAGNOSES_3_TITLE) | grepl("graft", DATA$DIAGNOSES_3_TITLE))]<-"device/graft complications"

# seperate out the factor levels with more than count of 150
high_count<-DATA[(DATA$DIAGNOSES_3_TITLE %in% c("septicemia", "pnuemonia", "non_lung abscess","obstruction","acute respiratory failure", "esophagus","bronchittis", "cholangitis", "swelling",
                                                "heart failure","hemorrhage", "subendo", "coma", "gastrointenstinal", "broken bone", 
                                                "chronic respiratory failure","tuberculosis","valve disorder", "liver", "toxin/poison", "kidney", 
                                                "meth lung", "embolism", "pancreas", "aneurism", "non_lung injury", "pleurisy", "device/graft complications",
                                                "postop", "cancer", "ventrical", "ulcer", "asthma", "spinal injury/disease", "tracheostomy",
                                                "hiv", "atrial", "ventrical", "ulcer", "asthma", "alcohol", "aorta", "cystic fibrosis",
                                                "urinary", "loss of consciousness", "colon", "bacterial infection", "hemothorax", "diabetes",
                                                "spleen", "lumbar", "obesity", "strep", "hypertension", "influenza","bladder","convulsions", "blood in stool")), ]

# seperate out the factor levels with more than count of 150
low_count<-DATA[!(DATA$DIAGNOSES_3_TITLE %in% c("septicemia", "pnuemonia", "tuberculosis", "acute respiratory failure", "influenza","non_lung abscess","obstruction", "esophagus","bronchittis", "cholangitis", "swelling",
                                                "heart failure","hemorrhage", "subendo", "coma", "gastrointenstinal", "broken bone", 
                                                "chronic respiratory failure","valve disorder", "liver", "toxin/poison", "kidney", 
                                                "meth lung", "embolism", "pancreas", "aneurism", "non_lung injury", "pleurisy", "device/graft complications",
                                                "postop", "cancer", "ventrical", "ulcer", "asthma", "spinal injury/disease", "tracheostomy",
                                                "hiv", "atrial", "ventrical", "ulcer", "asthma", "alcohol", "aorta", "cystic fibrosis",
                                                "urinary", "loss of consciousness", "colon", "bacterial infection", "hemothorax", "diabetes",
                                                "spleen", "lumbar", "obesity", "strep", "hypertension", "bladder","convulsions", "blood in stool")), ]
# change the categories with less than 100 count to "other" category
low_count$DIAGNOSES_3_TITLE<-"other"
# rejoin the two parts of the data
DATA<-rbind(high_count, low_count)
sort(table(as.factor(DATA$DIAGNOSES_3_TITLE)))
DATA$DIAGNOSES_3_TITLE<-as.factor(DATA$DIAGNOSES_3_TITLE)

# see if it is practical to convert the diagnoses columns to a factor variable
sort(table(as.factor(DATA$DIAGNOSES_4_TITLE)), decreasing=TRUE)

if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
# change all the lesser count diagnoses into "other" category
DATA$DIAGNOSES_4_TITLE<-tolower(DATA$DIAGNOSES_4_TITLE)
str(DATA$DIAGNOSES_4_TITLE)
text<-DATA[, 27]
corp <- Corpus(VectorSource(text))
dtm <- DocumentTermMatrix(corp)

DATA$DIAGNOSES_4_TITLE[(grepl("influ", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
)]<-"influenza"

table(as.factor(DATA$DIAGNOSES_4_TITLE[(grepl("pneumonia", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) | grepl("pneum", DATA$DIAGNOSES_4_TITLE, fixed=TRUE))]))
DATA$DIAGNOSES_4_TITLE[(grepl("pneumonia", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
                        | grepl("pneum", DATA$DIAGNOSES_4_TITLE, fixed=TRUE))]<-"pnuemonia"

DATA$DIAGNOSES_4_TITLE[(grepl("tuber", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
                        | grepl("tb", DATA$DIAGNOSES_4_TITLE, fixed=TRUE)
)]<-"tuberculosis"

DATA$DIAGNOSES_4_TITLE[(grepl("brain", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) | grepl("encephalopathy", DATA$DIAGNOSES_4_TITLE, fixed=TRUE)
)]<-"non_lung injury"

table(as.factor(DATA$DIAGNOSES_4_TITLE[(grep("fail", DATA$DIAGNOSES_4_TITLE))]))
DATA$DIAGNOSES_4_TITLE[(grepl("ch", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
                        & grepl("resp",DATA$DIAGNOSES_4_TITLE , fixed=TRUE)
                        & grepl("fail", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
                        | grepl("flr", DATA$DIAGNOSES_4_TITLE, fixed=TRUE))]<-"chronic respiratory failure"

DATA$DIAGNOSES_4_TITLE[(grepl("emblsm", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
                        | grepl("embol",DATA$DIAGNOSES_4_TITLE , fixed=TRUE)
)]<-"embolism"

DATA$DIAGNOSES_4_TITLE[(grepl("valve", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
)]<-"valve disorder"

DATA$DIAGNOSES_4_TITLE[(grepl("lung", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) & grepl("abs", DATA$DIAGNOSES_4_TITLE, fixed=TRUE)
)]<-"lung abscess"

DATA$DIAGNOSES_4_TITLE[(grepl("obstr", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
)]<-"obstruction"

DATA$DIAGNOSES_4_TITLE[(grepl("absc", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
)]<-"non_lung abscess"

DATA$DIAGNOSES_4_TITLE[(grepl("esop", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
)]<-"esophagus"

DATA$DIAGNOSES_4_TITLE[(grepl("kidny", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) | grepl("renal", DATA$DIAGNOSES_4_TITLE, fixed=TRUE)
)]<-"kidney"

DATA$DIAGNOSES_4_TITLE[(grepl("alcohol", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) | grepl("alcoh", DATA$DIAGNOSES_4_TITLE, fixed=TRUE)
)]<-"alcohol"

DATA$DIAGNOSES_4_TITLE[(grepl("ac", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
                        & grepl("resp",DATA$DIAGNOSES_4_TITLE , fixed=TRUE)
                        & grepl("fail", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
                        | grepl("flr", DATA$DIAGNOSES_4_TITLE, fixed=TRUE))]<-"acute respiratory failure"

DATA$DIAGNOSES_4_TITLE[(grepl("urin", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
)]<-"urinary"


DATA$DIAGNOSES_4_TITLE[(grepl("postop", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
                        | grepl("surg",DATA$DIAGNOSES_4_TITLE , fixed=TRUE)
                        | grepl("srg",DATA$DIAGNOSES_4_TITLE , fixed=TRUE)
)]<-"postop"

DATA$DIAGNOSES_4_TITLE[(grepl("stomach", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
                        | grepl("intest",DATA$DIAGNOSES_4_TITLE , fixed=TRUE)
)]<-"gastrointestinal"

DATA$DIAGNOSES_4_TITLE[(grepl("cf", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
                        | grepl("cystic",DATA$DIAGNOSES_4_TITLE , fixed=TRUE)
)]<-"cystic fibrosis"

DATA$DIAGNOSES_4_TITLE[grepl("aneu",DATA$DIAGNOSES_4_TITLE , fixed=TRUE)
                       ]<-"aneurism"

DATA$DIAGNOSES_4_TITLE[grepl("hernia",DATA$DIAGNOSES_4_TITLE , fixed=TRUE)
                       ]<-"hernia"

DATA$DIAGNOSES_4_TITLE[(grepl("chr", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
                        & grepl("obst",DATA$DIAGNOSES_4_TITLE , fixed=TRUE)
)]<-"copd"

DATA$DIAGNOSES_4_TITLE[(grepl("react", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) | grepl("allergy", DATA$DIAGNOSES_4_TITLE, fixed=TRUE)
)]<-"allergic reaction"


DATA$DIAGNOSES_4_TITLE[(grepl("pleur", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
)]<-"pleurisy"

DATA$DIAGNOSES_4_TITLE[(grepl("ventr", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
)]<-"ventrical"


DATA$DIAGNOSES_4_TITLE[(grep("septicemia", DATA$DIAGNOSES_4_TITLE))]<-"septicemia"

DATA$DIAGNOSES_4_TITLE[(grep("pancrea", DATA$DIAGNOSES_4_TITLE))]<-"pancreas"

DATA$DIAGNOSES_4_TITLE[(grep("asthma", DATA$DIAGNOSES_4_TITLE))]<-"asthma"

DATA$DIAGNOSES_4_TITLE[(grep("melanoma", DATA$DIAGNOSES_4_TITLE))]<-"cancer"

DATA$DIAGNOSES_4_TITLE[(grep("mal", DATA$DIAGNOSES_4_TITLE))]<-"cancer"

DATA$DIAGNOSES_4_TITLE[(grep("pneumonitis", DATA$DIAGNOSES_4_TITLE))]<-"pneumonitis"

DATA$DIAGNOSES_4_TITLE[(grep("hemorr", DATA$DIAGNOSES_4_TITLE))]<-"hemorrhage"

DATA$DIAGNOSES_4_TITLE[(grep("bladder", DATA$DIAGNOSES_4_TITLE))]<-"bladder"

DATA$DIAGNOSES_4_TITLE[(grep("coma", DATA$DIAGNOSES_4_TITLE))]<-"coma"

DATA$DIAGNOSES_4_TITLE[(grepl("kidney", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) & (grepl("kid", DATA$DIAGNOSES_4_TITLE, fixed=TRUE)
))]<-"kidney"

DATA$DIAGNOSES_4_TITLE[(grepl("heart", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
                        & grepl("fail",DATA$DIAGNOSES_4_TITLE , fixed=TRUE)
)]<-"heart failure"
DATA$DIAGNOSES_4_TITLE[(grepl("hrt", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
                        & grepl("fail",DATA$DIAGNOSES_4_TITLE , fixed=TRUE)
)]<-"heart failure"

DATA$DIAGNOSES_4_TITLE[(grepl("ami", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) | grepl("cardio", DATA$DIAGNOSES_4_TITLE, fixed=TRUE)
)]<-"heart failure"

DATA$DIAGNOSES_4_TITLE[(grepl("cancer", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
)]<-"cancer"


DATA$DIAGNOSES_4_TITLE[(grepl("human imm", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
                        | grepl("hiv", DATA$DIAGNOSES_4_TITLE, fixed=TRUE)
)]<-"hiv"

DATA$DIAGNOSES_4_TITLE[(grepl("tox", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) | grepl("pois", DATA$DIAGNOSES_4_TITLE, fixed=TRUE)
)]<-"toxin/poison"
DATA$DIAGNOSES_4_TITLE[(grepl("liver", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
)]<-"liver"
DATA$DIAGNOSES_4_TITLE[(grepl("lymphoma", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
                        | grepl("leuk", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
)]<-"cancer"
DATA$DIAGNOSES_4_TITLE[(grepl("loc", DATA$DIAGNOSES_4_TITLE, fixed=TRUE)
)]<-"loss of consciousness"

DATA$DIAGNOSES_4_TITLE[(grepl("lumb", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
                        | grepl("lmbr", DATA$DIAGNOSES_4_TITLE, fixed=TRUE)
)]<-"lumbar"



DATA$DIAGNOSES_4_TITLE[(grepl("lun", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
                        & grepl("contusions",DATA$DIAGNOSES_4_TITLE , fixed=TRUE)
                        | grepl("injury",DATA$DIAGNOSES_4_TITLE , fixed=TRUE )
)]<-"lung injury"

DATA$DIAGNOSES_4_TITLE[(grepl("contusion", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
                        | grepl("inj", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
                        | grepl("lacer", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
                        | grepl("wound", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
)]<-"non_lung injury"

DATA$DIAGNOSES_4_TITLE[(grepl("gastro", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) | grepl("gast", DATA$DIAGNOSES_4_TITLE, fixed=TRUE)
)]<-"gastrointestinal"

DATA$DIAGNOSES_4_TITLE[(grepl("react", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) | grepl("allergy", DATA$DIAGNOSES_4_TITLE, fixed=TRUE)
)]<-"allergic reaction"

DATA$DIAGNOSES_4_TITLE[(grepl("bronc", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
                        | grepl("brnch", DATA$DIAGNOSES_4_TITLE, fixed=TRUE)
)]<-"bronchittis"
DATA$DIAGNOSES_4_TITLE[(grepl("crnry", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
                        | grepl("coronary", DATA$DIAGNOSES_4_TITLE, fixed=TRUE)
                        | grepl("chf", DATA$DIAGNOSES_4_TITLE, fixed=TRUE)
)]<-"heart failure"

DATA$DIAGNOSES_4_TITLE[(grepl("spine", DATA$DIAGNOSES_4_TITLE, fixed=TRUE) 
                        | grepl("spinal", DATA$DIAGNOSES_4_TITLE, fixed=TRUE)
)]<-"spinal injury/disease"

DATA$DIAGNOSES_4_TITLE[(grep("meth", DATA$DIAGNOSES_4_TITLE))]<-"meth lung"
DATA$DIAGNOSES_4_TITLE[(grep("tracheo", DATA$DIAGNOSES_4_TITLE))]<-"tracheostomy"
DATA$DIAGNOSES_4_TITLE[(grep("subendo", DATA$DIAGNOSES_4_TITLE))]<-"subendo"
DATA$DIAGNOSES_4_TITLE[(grep("fx", DATA$DIAGNOSES_4_TITLE))]<-"broken bone"
DATA$DIAGNOSES_4_TITLE[(grep("staph", DATA$DIAGNOSES_4_TITLE))]<-"staph infection"
DATA$DIAGNOSES_4_TITLE[(grep("bronc", DATA$DIAGNOSES_4_TITLE))]<-"bronchitis"
DATA$DIAGNOSES_4_TITLE[(grep("ulc", DATA$DIAGNOSES_4_TITLE))]<-"ulcer"
DATA$DIAGNOSES_4_TITLE[(grep("atri", DATA$DIAGNOSES_4_TITLE))]<-"atrial"
DATA$DIAGNOSES_4_TITLE[(grep("pulm", DATA$DIAGNOSES_4_TITLE))]<-"pulmonary other"
DATA$DIAGNOSES_4_TITLE[(grep("seps", DATA$DIAGNOSES_4_TITLE))]<-"sepsis"
DATA$DIAGNOSES_4_TITLE[(grep("aort", DATA$DIAGNOSES_4_TITLE))]<-"aorta"
DATA$DIAGNOSES_4_TITLE[(grep("empyema", DATA$DIAGNOSES_4_TITLE))]<-"pleurisy"
DATA$DIAGNOSES_4_TITLE[(grep("asth", DATA$DIAGNOSES_4_TITLE))]<-"asthma"
DATA$DIAGNOSES_4_TITLE[(grep("art", DATA$DIAGNOSES_4_TITLE))]<-"artery"
DATA$DIAGNOSES_4_TITLE[(grep("colon", DATA$DIAGNOSES_4_TITLE))]<-"colon"
DATA$DIAGNOSES_4_TITLE[(grep("hemoth", DATA$DIAGNOSES_4_TITLE))]<-"hemothorax"
DATA$DIAGNOSES_4_TITLE[(grep("spr", DATA$DIAGNOSES_4_TITLE))]<-"sprain"
DATA$DIAGNOSES_4_TITLE[(grep("spleen", DATA$DIAGNOSES_4_TITLE))]<-"spleen"
DATA$DIAGNOSES_4_TITLE[(grep("obesity", DATA$DIAGNOSES_4_TITLE))]<-"obesity"
DATA$DIAGNOSES_4_TITLE[(grep("strep", DATA$DIAGNOSES_4_TITLE))]<-"strep"
DATA$DIAGNOSES_4_TITLE[(grep("hypert", DATA$DIAGNOSES_4_TITLE))]<-"hypertension"
DATA$DIAGNOSES_4_TITLE[(grep("convu", DATA$DIAGNOSES_4_TITLE))]<-"convulsions"
DATA$DIAGNOSES_4_TITLE[(grepl("bact", DATA$DIAGNOSES_4_TITLE) | grepl("inf", DATA$DIAGNOSES_4_TITLE))]<-"bacterial infection"
DATA$DIAGNOSES_4_TITLE[(grepl("pericard", DATA$DIAGNOSES_4_TITLE) | grepl("cardiac", DATA$DIAGNOSES_4_TITLE))]<-"heart failure"
DATA$DIAGNOSES_4_TITLE[(grep("int", DATA$DIAGNOSES_4_TITLE))]<-"gastrointestinal"
DATA$DIAGNOSES_4_TITLE[(grep("hepatic", DATA$DIAGNOSES_4_TITLE))]<-"liver"
DATA$DIAGNOSES_4_TITLE[(grep("keto", DATA$DIAGNOSES_4_TITLE))]<-"diabetes"
DATA$DIAGNOSES_4_TITLE[(grepl("edem", DATA$DIAGNOSES_4_TITLE) | grepl("swell", DATA$DIAGNOSES_4_TITLE))]<-"swelling"
DATA$DIAGNOSES_4_TITLE[(grepl("dev", DATA$DIAGNOSES_4_TITLE) | grepl("graft", DATA$DIAGNOSES_4_TITLE))]<-"device/graft complications"

# seperate out the factor levels with more than count of 150
high_count<-DATA[(DATA$DIAGNOSES_4_TITLE %in% c("septicemia", "pnuemonia", "non_lung abscess","obstruction","acute respiratory failure", "esophagus","bronchittis", "cholangitis", "swelling",
                                                "heart failure","hemorrhage", "subendo", "coma", "gastrointenstinal", "broken bone", 
                                                "chronic respiratory failure","tuberculosis","valve disorder", "liver", "toxin/poison", "kidney", 
                                                "meth lung", "embolism", "pancreas", "aneurism", "non_lung injury", "pleurisy", "device/graft complications",
                                                "postop", "cancer", "ventrical", "ulcer", "asthma", "spinal injury/disease", "tracheostomy",
                                                "hiv", "atrial", "ventrical", "ulcer", "asthma", "alcohol", "aorta", "cystic fibrosis",
                                                "urinary", "loss of consciousness", "colon", "bacterial infection", "hemothorax", "diabetes",
                                                "spleen", "lumbar", "obesity", "strep", "hypertension", "influenza","bladder","convulsions", "blood in stool")), ]

# seperate out the factor levels with more than count of 150
low_count<-DATA[!(DATA$DIAGNOSES_4_TITLE %in% c("septicemia", "pnuemonia", "tuberculosis", "acute respiratory failure", "influenza","non_lung abscess","obstruction", "esophagus","bronchittis", "cholangitis", "swelling",
                                                "heart failure","hemorrhage", "subendo", "coma", "gastrointenstinal", "broken bone", 
                                                "chronic respiratory failure","valve disorder", "liver", "toxin/poison", "kidney", 
                                                "meth lung", "embolism", "pancreas", "aneurism", "non_lung injury", "pleurisy", "device/graft complications",
                                                "postop", "cancer", "ventrical", "ulcer", "asthma", "spinal injury/disease", "tracheostomy",
                                                "hiv", "atrial", "ventrical", "ulcer", "asthma", "alcohol", "aorta", "cystic fibrosis",
                                                "urinary", "loss of consciousness", "colon", "bacterial infection", "hemothorax", "diabetes",
                                                "spleen", "lumbar", "obesity", "strep", "hypertension", "bladder","convulsions", "blood in stool")), ]
# change the categories with less than 100 count to "other" category
low_count$DIAGNOSES_4_TITLE<-"other"
# rejoin the two parts of the data
DATA<-rbind(high_count, low_count)
sort(table(as.factor(DATA$DIAGNOSES_4_TITLE)))
DATA$DIAGNOSES_4_TITLE<-as.factor(DATA$DIAGNOSES_4_TITLE)

# see if it is practical to convert the diagnoses columns to a factor variable
sort(table(as.factor(DATA$DIAGNOSES_5_TITLE)), decreasing=TRUE)

if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
# change all the lesser count diagnoses into "other" category
DATA$DIAGNOSES_5_TITLE<-tolower(DATA$DIAGNOSES_5_TITLE)
str(DATA$DIAGNOSES_5_TITLE)
text<-DATA[, 27]
corp <- Corpus(VectorSource(text))
dtm <- DocumentTermMatrix(corp)

DATA$DIAGNOSES_5_TITLE[(grepl("influ", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
)]<-"influenza"

table(as.factor(DATA$DIAGNOSES_5_TITLE[(grepl("pneumonia", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) | grepl("pneum", DATA$DIAGNOSES_5_TITLE, fixed=TRUE))]))
DATA$DIAGNOSES_5_TITLE[(grepl("pneumonia", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
                        | grepl("pneum", DATA$DIAGNOSES_5_TITLE, fixed=TRUE))]<-"pnuemonia"

DATA$DIAGNOSES_5_TITLE[(grepl("tuber", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
                        | grepl("tb", DATA$DIAGNOSES_5_TITLE, fixed=TRUE)
)]<-"tuberculosis"

DATA$DIAGNOSES_5_TITLE[(grepl("brain", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) | grepl("encephalopathy", DATA$DIAGNOSES_5_TITLE, fixed=TRUE)
)]<-"non_lung injury"

table(as.factor(DATA$DIAGNOSES_5_TITLE[(grep("fail", DATA$DIAGNOSES_5_TITLE))]))
DATA$DIAGNOSES_5_TITLE[(grepl("ch", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
                        & grepl("resp",DATA$DIAGNOSES_5_TITLE , fixed=TRUE)
                        & grepl("fail", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
                        | grepl("flr", DATA$DIAGNOSES_5_TITLE, fixed=TRUE))]<-"chronic respiratory failure"

DATA$DIAGNOSES_5_TITLE[(grepl("emblsm", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
                        | grepl("embol",DATA$DIAGNOSES_5_TITLE , fixed=TRUE)
)]<-"embolism"

DATA$DIAGNOSES_5_TITLE[(grepl("valve", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
)]<-"valve disorder"

DATA$DIAGNOSES_5_TITLE[(grepl("lung", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) & grepl("abs", DATA$DIAGNOSES_5_TITLE, fixed=TRUE)
)]<-"lung abscess"

DATA$DIAGNOSES_5_TITLE[(grepl("obstr", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
)]<-"obstruction"

DATA$DIAGNOSES_5_TITLE[(grepl("absc", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
)]<-"non_lung abscess"

DATA$DIAGNOSES_5_TITLE[(grepl("esop", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
)]<-"esophagus"

DATA$DIAGNOSES_5_TITLE[(grepl("kidny", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) | grepl("renal", DATA$DIAGNOSES_5_TITLE, fixed=TRUE)
)]<-"kidney"

DATA$DIAGNOSES_5_TITLE[(grepl("alcohol", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) | grepl("alcoh", DATA$DIAGNOSES_5_TITLE, fixed=TRUE)
)]<-"alcohol"

DATA$DIAGNOSES_5_TITLE[(grepl("ac", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
                        & grepl("resp",DATA$DIAGNOSES_5_TITLE , fixed=TRUE)
                        & grepl("fail", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
                        | grepl("flr", DATA$DIAGNOSES_5_TITLE, fixed=TRUE))]<-"acute respiratory failure"

DATA$DIAGNOSES_5_TITLE[(grepl("urin", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
)]<-"urinary"


DATA$DIAGNOSES_5_TITLE[(grepl("postop", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
                        | grepl("surg",DATA$DIAGNOSES_5_TITLE , fixed=TRUE)
                        | grepl("srg",DATA$DIAGNOSES_5_TITLE , fixed=TRUE)
)]<-"postop"

DATA$DIAGNOSES_5_TITLE[(grepl("stomach", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
                        | grepl("intest",DATA$DIAGNOSES_5_TITLE , fixed=TRUE)
)]<-"gastrointestinal"

DATA$DIAGNOSES_5_TITLE[(grepl("cf", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
                        | grepl("cystic",DATA$DIAGNOSES_5_TITLE , fixed=TRUE)
)]<-"cystic fibrosis"

DATA$DIAGNOSES_5_TITLE[grepl("aneu",DATA$DIAGNOSES_5_TITLE , fixed=TRUE)
                       ]<-"aneurism"

DATA$DIAGNOSES_5_TITLE[grepl("hernia",DATA$DIAGNOSES_5_TITLE , fixed=TRUE)
                       ]<-"hernia"

DATA$DIAGNOSES_5_TITLE[(grepl("chr", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
                        & grepl("obst",DATA$DIAGNOSES_5_TITLE , fixed=TRUE)
)]<-"copd"

DATA$DIAGNOSES_5_TITLE[(grepl("react", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) | grepl("allergy", DATA$DIAGNOSES_5_TITLE, fixed=TRUE)
)]<-"allergic reaction"


DATA$DIAGNOSES_5_TITLE[(grepl("pleur", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
)]<-"pleurisy"

DATA$DIAGNOSES_5_TITLE[(grepl("ventr", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
)]<-"ventrical"


DATA$DIAGNOSES_5_TITLE[(grep("septicemia", DATA$DIAGNOSES_5_TITLE))]<-"septicemia"

DATA$DIAGNOSES_5_TITLE[(grep("pancrea", DATA$DIAGNOSES_5_TITLE))]<-"pancreas"

DATA$DIAGNOSES_5_TITLE[(grep("asthma", DATA$DIAGNOSES_5_TITLE))]<-"asthma"

DATA$DIAGNOSES_5_TITLE[(grep("melanoma", DATA$DIAGNOSES_5_TITLE))]<-"cancer"

DATA$DIAGNOSES_5_TITLE[(grep("mal", DATA$DIAGNOSES_5_TITLE))]<-"cancer"

DATA$DIAGNOSES_5_TITLE[(grep("pneumonitis", DATA$DIAGNOSES_5_TITLE))]<-"pneumonitis"

DATA$DIAGNOSES_5_TITLE[(grep("hemorr", DATA$DIAGNOSES_5_TITLE))]<-"hemorrhage"

DATA$DIAGNOSES_5_TITLE[(grep("bladder", DATA$DIAGNOSES_5_TITLE))]<-"bladder"

DATA$DIAGNOSES_5_TITLE[(grep("coma", DATA$DIAGNOSES_5_TITLE))]<-"coma"

DATA$DIAGNOSES_5_TITLE[(grepl("kidney", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) & (grepl("kid", DATA$DIAGNOSES_5_TITLE, fixed=TRUE)
))]<-"kidney"

DATA$DIAGNOSES_5_TITLE[(grepl("heart", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
                        & grepl("fail",DATA$DIAGNOSES_5_TITLE , fixed=TRUE)
)]<-"heart failure"
DATA$DIAGNOSES_5_TITLE[(grepl("hrt", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
                        & grepl("fail",DATA$DIAGNOSES_5_TITLE , fixed=TRUE)
)]<-"heart failure"

DATA$DIAGNOSES_5_TITLE[(grepl("ami", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) | grepl("cardio", DATA$DIAGNOSES_5_TITLE, fixed=TRUE)
)]<-"heart failure"

DATA$DIAGNOSES_5_TITLE[(grepl("cancer", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
)]<-"cancer"


DATA$DIAGNOSES_5_TITLE[(grepl("human imm", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
                        | grepl("hiv", DATA$DIAGNOSES_5_TITLE, fixed=TRUE)
)]<-"hiv"

DATA$DIAGNOSES_5_TITLE[(grepl("tox", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) | grepl("pois", DATA$DIAGNOSES_5_TITLE, fixed=TRUE)
)]<-"toxin/poison"
DATA$DIAGNOSES_5_TITLE[(grepl("liver", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
)]<-"liver"
DATA$DIAGNOSES_5_TITLE[(grepl("lymphoma", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
                        | grepl("leuk", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
)]<-"cancer"
DATA$DIAGNOSES_5_TITLE[(grepl("loc", DATA$DIAGNOSES_5_TITLE, fixed=TRUE)
)]<-"loss of consciousness"

DATA$DIAGNOSES_5_TITLE[(grepl("lumb", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
                        | grepl("lmbr", DATA$DIAGNOSES_5_TITLE, fixed=TRUE)
)]<-"lumbar"



DATA$DIAGNOSES_5_TITLE[(grepl("lun", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
                        & grepl("contusions",DATA$DIAGNOSES_5_TITLE , fixed=TRUE)
                        | grepl("injury",DATA$DIAGNOSES_5_TITLE , fixed=TRUE )
)]<-"lung injury"

DATA$DIAGNOSES_5_TITLE[(grepl("contusion", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
                        | grepl("inj", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
                        | grepl("lacer", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
                        | grepl("wound", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
)]<-"non_lung injury"

DATA$DIAGNOSES_5_TITLE[(grepl("gastro", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) | grepl("gast", DATA$DIAGNOSES_5_TITLE, fixed=TRUE)
)]<-"gastrointestinal"

DATA$DIAGNOSES_5_TITLE[(grepl("react", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) | grepl("allergy", DATA$DIAGNOSES_5_TITLE, fixed=TRUE)
)]<-"allergic reaction"

DATA$DIAGNOSES_5_TITLE[(grepl("bronc", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
                        | grepl("brnch", DATA$DIAGNOSES_5_TITLE, fixed=TRUE)
)]<-"bronchittis"
DATA$DIAGNOSES_5_TITLE[(grepl("crnry", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
                        | grepl("coronary", DATA$DIAGNOSES_5_TITLE, fixed=TRUE)
                        | grepl("chf", DATA$DIAGNOSES_5_TITLE, fixed=TRUE)
)]<-"heart failure"

DATA$DIAGNOSES_5_TITLE[(grepl("spine", DATA$DIAGNOSES_5_TITLE, fixed=TRUE) 
                        | grepl("spinal", DATA$DIAGNOSES_5_TITLE, fixed=TRUE)
)]<-"spinal injury/disease"

DATA$DIAGNOSES_5_TITLE[(grep("meth", DATA$DIAGNOSES_5_TITLE))]<-"meth lung"
DATA$DIAGNOSES_5_TITLE[(grep("tracheo", DATA$DIAGNOSES_5_TITLE))]<-"tracheostomy"
DATA$DIAGNOSES_5_TITLE[(grep("subendo", DATA$DIAGNOSES_5_TITLE))]<-"subendo"
DATA$DIAGNOSES_5_TITLE[(grep("fx", DATA$DIAGNOSES_5_TITLE))]<-"broken bone"
DATA$DIAGNOSES_5_TITLE[(grep("staph", DATA$DIAGNOSES_5_TITLE))]<-"staph infection"
DATA$DIAGNOSES_5_TITLE[(grep("bronc", DATA$DIAGNOSES_5_TITLE))]<-"bronchitis"
DATA$DIAGNOSES_5_TITLE[(grep("ulc", DATA$DIAGNOSES_5_TITLE))]<-"ulcer"
DATA$DIAGNOSES_5_TITLE[(grep("atri", DATA$DIAGNOSES_5_TITLE))]<-"atrial"
DATA$DIAGNOSES_5_TITLE[(grep("pulm", DATA$DIAGNOSES_5_TITLE))]<-"pulmonary other"
DATA$DIAGNOSES_5_TITLE[(grep("seps", DATA$DIAGNOSES_5_TITLE))]<-"sepsis"
DATA$DIAGNOSES_5_TITLE[(grep("aort", DATA$DIAGNOSES_5_TITLE))]<-"aorta"
DATA$DIAGNOSES_5_TITLE[(grep("empyema", DATA$DIAGNOSES_5_TITLE))]<-"pleurisy"
DATA$DIAGNOSES_5_TITLE[(grep("asth", DATA$DIAGNOSES_5_TITLE))]<-"asthma"
DATA$DIAGNOSES_5_TITLE[(grep("art", DATA$DIAGNOSES_5_TITLE))]<-"artery"
DATA$DIAGNOSES_5_TITLE[(grep("colon", DATA$DIAGNOSES_5_TITLE))]<-"colon"
DATA$DIAGNOSES_5_TITLE[(grep("hemoth", DATA$DIAGNOSES_5_TITLE))]<-"hemothorax"
DATA$DIAGNOSES_5_TITLE[(grep("spr", DATA$DIAGNOSES_5_TITLE))]<-"sprain"
DATA$DIAGNOSES_5_TITLE[(grep("spleen", DATA$DIAGNOSES_5_TITLE))]<-"spleen"
DATA$DIAGNOSES_5_TITLE[(grep("obesity", DATA$DIAGNOSES_5_TITLE))]<-"obesity"
DATA$DIAGNOSES_5_TITLE[(grep("strep", DATA$DIAGNOSES_5_TITLE))]<-"strep"
DATA$DIAGNOSES_5_TITLE[(grep("hypert", DATA$DIAGNOSES_5_TITLE))]<-"hypertension"
DATA$DIAGNOSES_5_TITLE[(grep("convu", DATA$DIAGNOSES_5_TITLE))]<-"convulsions"
DATA$DIAGNOSES_5_TITLE[(grepl("bact", DATA$DIAGNOSES_5_TITLE) | grepl("inf", DATA$DIAGNOSES_5_TITLE))]<-"bacterial infection"
DATA$DIAGNOSES_5_TITLE[(grepl("pericard", DATA$DIAGNOSES_5_TITLE) | grepl("cardiac", DATA$DIAGNOSES_5_TITLE))]<-"heart failure"
DATA$DIAGNOSES_5_TITLE[(grep("int", DATA$DIAGNOSES_5_TITLE))]<-"gastrointestinal"
DATA$DIAGNOSES_5_TITLE[(grep("hepatic", DATA$DIAGNOSES_5_TITLE))]<-"liver"
DATA$DIAGNOSES_5_TITLE[(grep("keto", DATA$DIAGNOSES_5_TITLE))]<-"diabetes"
DATA$DIAGNOSES_5_TITLE[(grepl("edem", DATA$DIAGNOSES_5_TITLE) | grepl("swell", DATA$DIAGNOSES_5_TITLE))]<-"swelling"
DATA$DIAGNOSES_5_TITLE[(grepl("dev", DATA$DIAGNOSES_5_TITLE) | grepl("graft", DATA$DIAGNOSES_5_TITLE))]<-"device/graft complications"

# seperate out the factor levels with more than count of 150
high_count<-DATA[(DATA$DIAGNOSES_5_TITLE %in% c("septicemia", "pnuemonia", "non_lung abscess","obstruction","acute respiratory failure", "esophagus","bronchittis", "cholangitis", "swelling",
                                                "heart failure","hemorrhage", "subendo", "coma", "gastrointenstinal", "broken bone", 
                                                "chronic respiratory failure","tuberculosis","valve disorder", "liver", "toxin/poison", "kidney", 
                                                "meth lung", "embolism", "pancreas", "aneurism", "non_lung injury", "pleurisy", "device/graft complications",
                                                "postop", "cancer", "ventrical", "ulcer", "asthma", "spinal injury/disease", "tracheostomy",
                                                "hiv", "atrial", "ventrical", "ulcer", "asthma", "alcohol", "aorta", "cystic fibrosis",
                                                "urinary", "loss of consciousness", "colon", "bacterial infection", "hemothorax", "diabetes",
                                                "spleen", "lumbar", "obesity", "strep", "hypertension", "influenza","bladder","convulsions", "blood in stool")), ]

# seperate out the factor levels with more than count of 150
low_count<-DATA[!(DATA$DIAGNOSES_5_TITLE %in% c("septicemia", "pnuemonia", "tuberculosis", "acute respiratory failure", "influenza","non_lung abscess","obstruction", "esophagus","bronchittis", "cholangitis", "swelling",
                                                "heart failure","hemorrhage", "subendo", "coma", "gastrointenstinal", "broken bone", 
                                                "chronic respiratory failure","valve disorder", "liver", "toxin/poison", "kidney", 
                                                "meth lung", "embolism", "pancreas", "aneurism", "non_lung injury", "pleurisy", "device/graft complications",
                                                "postop", "cancer", "ventrical", "ulcer", "asthma", "spinal injury/disease", "tracheostomy",
                                                "hiv", "atrial", "ventrical", "ulcer", "asthma", "alcohol", "aorta", "cystic fibrosis",
                                                "urinary", "loss of consciousness", "colon", "bacterial infection", "hemothorax", "diabetes",
                                                "spleen", "lumbar", "obesity", "strep", "hypertension", "bladder","convulsions", "blood in stool")), ]
# change the categories with less than 100 count to "other" category
low_count$DIAGNOSES_5_TITLE<-"other"
# rejoin the two parts of the data
DATA<-rbind(high_count, low_count)
sort(table(as.factor(DATA$DIAGNOSES_5_TITLE)))
DATA$DIAGNOSES_5_TITLE<-as.factor(DATA$DIAGNOSES_5_TITLE)


str(DATA)
table(as.factor(DATA$FIRST_CAREUNIT))
table(as.factor(DATA$FIRST_WARDID))
DATA$FIRST_WARDID<-as.factor(DATA$FIRST_WARDID)
DATA$LAST_WARDID<-as.factor(DATA$LAST_WARDID)

# REMOVE ALL ROWS WHERE THE PATIENT IS UNDER THE AGE OF 18
DATA<-DATA[DATA$ICUSTAY_AGE_GROUP=="adult",]

# REMOVE UNWANTED VARIALBES
DATA$DIAGNOSES_1.1<-NULL
DATA$DIAGNOSES_1_TITLE.1<-NULL
DATA$DIAGNOSES_2.1<-NULL
DATA$DIAGNOSES_2_TITLE.1<-NULL
DATA$INTIME<-NULL
DATA$DOB<-NULL
DATA$OUTTIME<-NULL
DATA$ADMITTIME<-NULL
DATA$DISCHTIME<-NULL
DATA$DEATHTIME<-NULL
DATA$EDREGTIME<-NULL
DATA$EDOUTTIME<-NULL
DATA$HAS_CHARTEVENTS_DATA<-NULL
DATA$DIAGNOSES_1<-NULL
DATA$DIAGNOSES_2<-NULL
DATA$DIAGNOSES_3<-NULL
DATA$DIAGNOSES_4<-NULL
DATA$DIAGNOSES_5<-NULL
DATA$HADM_ID.1<-NULL
DATA$ICUSTAY_ID.1<-NULL
DATA$ICUSTAY_AGE_GROUP<-NULL
DATA$Dob<-NULL
droplevels(DATA)

DATA$ORG_NAME<-as.factor(DATA$ORG_NAME)

# reduce number of levels of languages
sort(table(as.factor(DATA$LANGUAGE)), decreasing=TRUE)
languages<-DATA[(DATA$LANGUAGE==" "),]<-"NA"
DATA[!(DATA$LANGUAGE %in% c("ENGL", "SPAN", "RUSS", 
                                     "PTUN", "CANT", "PORT", 
                                     "CAPE", "HAIT", "ITAL", 
                                     "GREEK","MAND", "PERS", 
                                     "VIET", "CAMB", "POLI", 
                                     "ARAB")),]<-"OTHER"
summary(DATA$DIAGNOSIS)



DATA$ORG_NAME<-as.factor(DATA$ORG_NAME)
summary(DATA$ORG_NAME)

summary(DATA$LOS)

DATA$HOSPITAL_EXPIRE_FLAG<-as.factor(DATA$HOSPITAL_EXPIRE_FLAG)
DATA$LOS<-as.numeric(DATA$LOS)
DATA$URINE_OUTPUT<-as.numeric(DATA$URINE_OUTPUT)
DATA$WEIGHT<-as.numeric(DATA$WEIGHT)
DATA$TOTALCO2<-as.numeric(DATA$TOTALCO2)
DATA$PH<-as.numeric(DATA$PH)
DATA$PCO2<-as.numeric(DATA$PCO2)
DATA$PO2<-as.numeric(DATA$PO2)
DATA$RRT<-as.factor(DATA$RRT)
DATA$HEARTRATE_MAX<-as.numeric(DATA$HEARTRATE_MAX)
DATA$HEARTRATE_MIN<-as.numeric(DATA$HEARTRATE_MIN)
DATA$SYSBP_MAX<-as.numeric(DATA$SYSBP_MAX)
DATA$SYSBP_MIN<-as.numeric(DATA$SYSBP_MIN)
DATA$DIASBP_MIN<-as.numeric(DATA$DIASBP_MIN)
DATA$DIASBP_MAX<-as.numeric(DATA$DIASBP_MAX)
DATA$MEANBP_MAX<-as.numeric(DATA$MEANBP_MAX)
DATA$MEANBP_MIN<-as.numeric(DATA$MEANBP_MIN)
DATA$RESPRATE_MIN<-as.numeric(DATA$RESPRATE_MIN)
DATA$RESPRATE_MAX<-as.numeric(DATA$RESPRATE_MAX)
DATA$TEMPC_MAX<-as.numeric(DATA$TEMPC_MAX)
DATA$TEMPC_MIN<-as.numeric(DATA$TEMPC_MIN)
DATA$SPO2_MIN<-as.numeric(DATA$SPO2_MIN)
DATA$SPO2_MAX<-as.numeric(DATA$SPO2_MAX)
DATA$GLUCOSE_MAX<-as.numeric(DATA$GLUCOSE_MAX)
DATA$GLUCOSE_MIN<-as.numeric(DATA$GLUCOSE_MIN)
DATA$GCSMOTOR<-as.numeric(DATA$GCSMOTOR)
DATA$GCSVERBAL<-as.numeric(DATA$GCSVERBAL)
DATA$GCSEYES<-as.numeric(DATA$GCSEYES)
DATA$VENT_DURATION<-as.numeric(DATA$VENT_DURATION)
DATA$SAPS<-as.numeric(DATA$SAPS)
DATA$SAPSII<-as.numeric(DATA$SAPSII)
DATA$OASIS<-as.numeric(DATA$OASIS)
DATA$OASIS_PROB<-as.numeric(DATA$OASIS_PROB)
DATA$SAPSII_PROB<-as.numeric(DATA$SAPSII_PROB)
DATA$SOFA<-as.numeric(DATA$SOFA)
DATA$AGE<-as.integer(DATA$AGE)
DATA$RESP_SOFA<-as.numeric(DATA$RESP_SOFA)
DATA$ICUSTAY_ID<-as.integer(DATA$ICUSTAY_ID)

DATA$VENT_DURATION[is.na(DATA$VENT_DURATION)]<-"0"
DATA$VENT_DURATION<-as.numeric(DATA$VENT_DURATION)

DATA$DBSOURCE<-NULL
DATA$ROW_ID<-NULL
DATA$SUBJECT_ID<-NULL
DATA$HADM_ID<-NULL
DATA$RESP_SOFA<-NULL
DATA$SAPS<-NULL

DATA<-DATA[!is.na(DATA$ICUSTAY_ID),]
DATA$APSIII<-as.integer(DATA$APSIII)
DATA$APSIII_PROB<-as.numeric(DATA$APSIII_PROB)
DATA$ORG_NAME<-as.character(DATA$ORG_NAME)
#DATA$ORG_NAME[is.na(DATA$ORG_NAME)]<-"UNKNOWN"
DATA$ORG_NAME<-as.factor(DATA$ORG_NAME)
DATA$GCS<-DATA$GCSEYES+DATA$GCSMOTOR+DATA$GCSVERBAL
apply(is.na(DATA),2,sum)
DATA<-DATA[!is.na(DATA$PCO2),]
DATA<-DATA[!is.na(DATA$WEIGHT),]
summary(DATA)
table(as.factor(DATA$ETHNICITY))
DATA$ETHNICITY<-as.character(DATA$ETHNICITY)
DATA$ETHNICITY[(grepl("ASIAN", DATA$ETHNICITY))]<-"ASIAN"
DATA$ETHNICITY[(grepl("HISPANIC", DATA$ETHNICITY))]<-"HISPANIC"
DATA$ETHNICITY[(grepl("WHITE", DATA$ETHNICITY))]<-"WHITE"
DATA$ETHNICITY[(grepl("UNABLE TO OBTAIN", DATA$ETHNICITY) |grepl("UNKNOWN", DATA$ETHNICITY)|grepl("OTHER", DATA$ETHNICITY) |grepl("DECLINED TO ANSWER", DATA$ETHNICITY))]<-"UNKNOWN"
DATA$ETHNICITY[(grepl("AMERICAN INDIAN", DATA$ETHNICITY))]<-"AMERICAN INDIAN/ALASKA NATIVE"
DATA$ETHNICITY[(grepl("BLACK", DATA$ETHNICITY))]<-"BLACK"
DATA$ETHNICITY<-as.factor(DATA$ETHNICITY)
DATA$LODS<-as.integer(DATA$LODS)
DATA$LODS_PULM<-as.factor(DATA$LODS_PULM)

nrow(DATA)
str(DATA)
summary(DATA)
apply(is.na(DATA),2,sum)
names(DATA)
DATA<-DATA[,c(1:6, 8:11, 20, 35:71)]




write.csv(DATA, "DATA.csv")


# CHECK ACCURACY OF SAPSII 
DATA$SAPSII_PRED<-DATA$SAPSII_PROB
DATA$SAPSII_PRED[DATA$SAPSII_PRED>0.5]<-1
DATA$SAPSII_PRED[DATA$SAPSII_PRED<0.5]<-0
summary(as.factor(DATA$SAPSII_PRED))
DATA$SAPSII_PRED<-as.factor(DATA$SAPSII_PRED)
DATA$HOSPITAL_EXPIRE_FLAG<-droplevels(DATA$HOSPITAL_EXPIRE_FLAG)
summary(DATA$HOSPITAL_EXPIRE_FLAG)
# confusion matrix
table(DATA$HOSPITAL_EXPIRE_FLAG, DATA$SAPSII_PRED)
tp<-1434 #BOTTOM RIGHT
fn<-1446 #BOTTOM LEFT
tn<-12903 #TOP LEFT
fp<-2308 #TOP RIGHT
acc<-(tp+tn)/(tn+fp+fn+tp) # overall accuracy of the model
acc
tpr<-tp/(fn+tp) # when the patient actually died how often did it predict correctly
tpr
spec<-tn/(tn+fp) # when the patient lived how often did it predict correctly
spec
missclass<-(fp+fn)/(tp+fn+tn+fp) # overall how often is it wrong?
missclass
precision<-tp/(tp+fp) # when it predicts yes, how often is it correct?
precision

# CHECK ACCURACY OF OASIS 
DATA$OASIS_PRED<-DATA$OASIS_PROB
DATA$OASIS_PRED[DATA$OASIS_PRED>0.5]<-1
DATA$OASIS_PRED[DATA$OASIS_PRED<0.5]<-0
summary(as.factor(DATA$OASIS_PRED))
DATA$OASIS_PRED<-as.factor(DATA$OASIS_PRED)

# confusion matrix
table(DATA$HOSPITAL_EXPIRE_FLAG, DATA$OASIS_PRED)
tp<-662 #BOTTOM RIGHT
fn<-2218 #BOTTOM LEFT
tn<-14429 #TOP LEFT
fp<-782 #TOP RIGHT
acc<-(tp+tn)/(tn+fp+fn+tp) # overall accuracy of the model
acc
tpr<-tp/(fn+tp) # when the patient actually died how often did it predict correctly
tpr
spec<-tn/(tn+fp) # when the patient lived how often did it predict correctly
spec
missclass<-(fp+fn)/(tp+fn+tn+fp) # overall how often is it wrong?
missclass
precision<-tp/(tp+fp) # when it predicts yes, how often is it correct?
precision

# CHECK ACCURACY OF SOFA 
DATA$SOFA_PRED<-DATA$SOFA
DATA$SOFA_PRED<-as.character(DATA$SOFA)
DATA$SOFA_PRED[DATA$SOFA_PRED %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") ] <- "0"
DATA$SOFA_PRED[!DATA$SOFA_PRED %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11") ] <- "1"
DATA$SOFA_PRED<-as.factor(DATA$SOFA_PRED)
summary(as.factor(DATA$SOFA_PRED))
summary(DATA$HOSPITAL_EXPIRE_FLAG)
# confusion matrix
table(DATA$HOSPITAL_EXPIRE_FLAG, DATA$SOFA_PRED)
tp<-546 #BOTTOM RIGHT
fn<-2334 #BOTTOM LEFT
tn<-14726 #TOP LEFT
fp<-485 #TOP RIGHT
acc<-(tp+tn)/(tn+fp+fn+tp) # overall accuracy of the model
acc
tpr<-tp/(fn+tp) # when the patient actually died how often did it predict correctly
tpr
spec<-tn/(tn+fp) # when the patient lived how often did it predict correctly
spec
missclass<-(fp+fn)/(tp+fn+tn+fp) # overall how often is it wrong?
missclass
precision<-tp/(tp+fp) # when it predicts yes, how often is it correct?
precision

# CHECK ACCURACY OF APSIII_PROB 
DATA$APSIII_PRED<-DATA$APSIII_PROB
DATA$APSIII_PRED<-as.character(DATA$APSIII_PROB)
DATA$APSIII_PRED[DATA$APSIII_PRED>0.5]<-1
DATA$APSIII_PRED[DATA$APSIII_PRED<0.5]<-0
DATA$APSIII_PRED<-as.factor(DATA$APSIII_PRED)
summary(as.factor(DATA$APSIII_PRED))
summary(DATA$HOSPITAL_EXPIRE_FLAG)
# confusion matrix
table(DATA$HOSPITAL_EXPIRE_FLAG, DATA$APSIII_PRED)
tp<-435 #BOTTOM RIGHT
fn<-2445 #BOTTOM LEFT
tn<-14893 #TOP LEFT
fp<-318 #TOP RIGHT
acc<-(tp+tn)/(tn+fp+fn+tp) # overall accuracy of the model
acc
tpr<-tp/(fn+tp) # when the patient actually died how often did it predict correctly
tpr
spec<-tn/(tn+fp) # when the patient lived how often did it predict correctly
spec
missclass<-(fp+fn)/(tp+fn+tn+fp) # overall how often is it wrong?
missclass
precision<-tp/(tp+fp) # when it predicts yes, how often is it correct?
precision

# accuracies of LODS
DATA$LODS_PRED<-DATA$LODS
DATA$LODS_PRED<-as.character(DATA$LODS_PRED)
DATA$LODS_PRED[DATA$LODS_PRED %in% c("8", "9", "10", "11", "12", "14", "15", "16", "17", "18", "19", "20")]<-1
DATA$LODS_PRED[DATA$LODS_PRED %in% c("0", "1", "2", "3", "4", "5", "6", "7")]<-0
DATA$LODS_PRED<-as.factor(DATA$LODS_PRED)
summary(as.factor(DATA$LODS_PRED))
summary(DATA$HOSPITAL_EXPIRE_FLAG)
# confusion matrix
table(DATA$HOSPITAL_EXPIRE_FLAG, DATA$LODS_PRED)
tp<-76 #BOTTOM RIGHT
fn<-2804 #BOTTOM LEFT
tn<-15128 #TOP LEFT
fp<-83 #TOP RIGHT
acc<-(tp+tn)/(tn+fp+fn+tp) # overall accuracy of the model
acc
tpr<-tp/(fn+tp) # when the patient actually died how often did it predict correctly
tpr
spec<-tn/(tn+fp) # when the patient lived how often did it predict correctly
spec
missclass<-(fp+fn)/(tp+fn+tn+fp) # overall how often is it wrong?
missclass
precision<-tp/(tp+fp) # when it predicts yes, how often is it correct?
precision

# accuracies of logistic regression
tp<-699 #BOTTOM RIGHT
fn<-2271 #BOTTOM LEFT
tn<-28907 #TOP LEFT
fp<-317 #TOP RIGHT
acc<-(tp+tn)/(tn+fp+fn+tp) # overall accuracy of the model
acc
tpr<-tp/(fn+tp) # when the patient actually died how often did it predict correctly
tpr
spec<-tn/(tn+fp) # when the patient lived how often did it predict correctly
spec
missclass<-(fp+fn)/(tp+fn+tn+fp) # overall how often is it wrong?
missclass
precision<-tp/(tp+fp) # when it predicts yes, how often is it correct?
precision


# see how unbalanced the target variable is 
summary(DATA$HOSPITAL_EXPIRE_FLAG)
prop.table(table(DATA$HOSPITAL_EXPIRE_FLAG))

#***********************************************PREP DATA FOR MODELING************************************
# SUBSET TO ONLY INCLUDE THE VARIABLES THAT ARE USEUFL FOR MODELING
DATA2<-DATA[,c("ADMISSION_TYPE","ADMISSION_LOCATION", "INSURANCE", "LANGUAGE", 
              "MARITAL_STATUS", "ETHNICITY", "DIAGNOSIS", "HOSPITAL_EXPIRE_FLAG", 
              "GENDER", "ORG_NAME", "URINE_OUTPUT", "WEIGHT", "TOTALCO2", "PH", 
              "PCO2", "PO2", "RRT", "HEARTRATE_MIN", "HEARTRATE_MAX", "SYSBP_MIN", 
              "SYSBP_MAX","DIASBP_MIN", "DIASBP_MAX", "RESPRATE_MAX", "TEMPC_MAX", 
              "SPO2_MIN", "SPO2_MAX", "GLUCOSE_MIN", "GLUCOSE_MAX", "GCSMOTOR", 
              "GCSEYES", "GCSVERBAL", "VENT_DURATION", "SAPSII", "SOFA", "LODS", 
              "LODS_PULM", "OASIS", "APSIII", "LOS","AGE") ]

#**********HANDLE MISSING VALUES*************
nrow(DATA2)
apply(is.na(DATA2),2,sum)
DATA2<-DATA2[complete.cases(DATA2),]

#**********HANDLE OUTLIERS*******************
plot(DATA2$URINE_OUTPUT)
# REMOVE OUTLIERS FROM URINE_OUTPUT
DATA2$URINE_OUTPUT[DATA2$URINE_OUTPUT>3000]<-mean(DATA2$URINE_OUTPUT)
DATA2$URINE_OUTPUT[DATA2$URINE_OUTPUT<0]<-mean(DATA2$URINE_OUTPUT)

plot(DATA2$WEIGHT)
summary(DATA2$WEIGHT)
DATA2$WEIGHT[DATA2$WEIGHT>500 | DATA2$WEIGHT<70]<-mean(DATA2$WEIGHT)

plot(DATA2$TOTALCO2)
summary(DATA2$TOTALCO2)
DATA2$TOTALCO2[DATA2$TOTALCO2>55 | DATA2$TOTALCO2<5]<-mean(DATA2$TOTALCO2)

plot(DATA2$PH)
summary(DATA2$PH)
DATA2$PH[DATA2$PH>7.7 | DATA2$PH<6.9]<-mean(DATA2$PH)

plot(DATA2$PCO2)
summary(DATA2$PCO2)
DATA2$PCO2[DATA2$PCO2>180]<-mean(DATA2$PCO2)

plot(DATA2$PO2)
DATA2$PO2[DATA2$PO2>600]<-mean(DATA2$PO2)

plot(DATA2$HEARTRATE_MIN)
DATA2$HEARTRATE_MIN[DATA2$HEARTRATE_MIN<20]<-mean(DATA2$HEARTRATE_MIN)

plot(DATA2$HEARTRATE_MAX)
DATA2$HEARTRATE_MAX[DATA2$HEARTRATE_MAX>200 | DATA2$HEARTRATE_MAX<50]<-mean(DATA2$HEARTRATE_MAX)

plot(DATA2$SYSBP_MIN)
DATA2$SYSBP_MIN[DATA2$SYSBP_MIN>150 | DATA2$SYSBP_MIN<25]<-mean(DATA2$SYSBP_MIN)

plot(DATA2$SYSBP_MAX)
DATA2$SYSBP_MAX[DATA2$SYSBP_MAX>275 | DATA2$SYSBP_MAX<50]<-mean(DATA2$SYSBP_MAX)

plot(DATA2$DIASBP_MIN)
DATA2$DIASBP_MIN[DATA2$DIASBP_MIN>85 | DATA2$DIASBP_MIN<15]<-mean(DATA2$DIASBP_MIN)

plot(DATA2$DIASBP_MIN)
DATA2$DIASBP_MIN[DATA2$DIASBP_MIN>85 | DATA2$DIASBP_MIN<15]<-mean(DATA2$DIASBP_MIN)

plot(DATA2$DIASBP_MAX)
DATA2$DIASBP_MAX[DATA2$DIASBP_MAX>215 | DATA2$DIASBP_MAX<30]<-mean(DATA2$DIASBP_MAX)

plot(DATA2$MEANBP_MIN)
plot(DATA2$MEANBP_MAX)
plot(DATA2$RESPRATE_MAX)

plot(DATA2$TEMPC_MAX)
DATA2$TEMPC_MAX[DATA2$TEMPC_MAX>42 | DATA2$TEMPC_MAX<33]<-mean(DATA2$TEMPC_MAX)

plot(DATA2$SPO2_MIN)
plot(DATA2$SPO2_MAX)
DATA2$SPO2_MAX[DATA2$SPO2_MAX<85]<-mean(DATA2$SPO2_MAX)

plot(DATA2$GLUCOSE_MIN)
DATA2$GLUCOSE_MIN[DATA2$GLUCOSE_MIN>350]<-mean(DATA2$GLUCOSE_MIN)

plot(DATA2$GLUCOSE_MAX)
DATA2$GLUCOSE_MAX[DATA2$GLUCOSE_MAX>800]<-mean(DATA2$GLUCOSE_MAX)

plot(DATA2$VENT_DURATION)
DATA2$VENT_DURATION[DATA2$VENT_DURATION>1200]<-mean(DATA2$VENT_DURATION)

plot(DATA2$LOS)
DATA2$LOS[DATA2$LOS>150]<-mean(DATA2$LOS)

plot(DATA2$AGE)
nrow(DATA2[DATA2$AGE>200,])
DATA2<-DATA2[DATA2$AGE<150,]
nrow(DATA2)

#*******CONVERT ALL TO NUMERIC DATA TYPE*************
DATA2<-droplevels(DATA2)
DATA2$LODS_PULM<-as.integer(DATA2$LODS_PULM)
DATA2$RRT<-as.integer(DATA2$RRT)
str(DATA2)
names(DATA2$ADMISSION_TYPE)
DATA2$ADMISSION_TYPE <- revalue(DATA2$ADMISSION_TYPE, c("ELECTIVE"="1", "EMERGENCY"="2", "URGENT"="3"))
DATA2$ADMISSION_TYPE<-as.integer(DATA2$ADMISSION_TYPE)

DATA2$ADMISSION_LOCATION<-as.factor(DATA2$ADMISSION_LOCATION)
table(as.factor(DATA2$ADMISSION_LOCATION))
DATA2$ADMISSION_LOCATION <- revalue(DATA2$ADMISSION_LOCATION, c("CLINIC REFERRAL/PREMATURE"="1","EMERGENCY ROOM ADMIT"="2", "PHYS REFERRAL/NORMAL DELI"="3", 
                                                                "TRANSFER FROM HOSP/EXTRAM"="4", "TRANSFER FROM OTHER HEALT"="5","TRANSFER FROM SKILLED NUR"="6", 
                                                                "TRSF WITHIN THIS FACILITY"="7"))
DATA2$ADMISSION_LOCATION<-as.integer(DATA2$ADMISSION_LOCATION)

table(as.factor(DATA2$INSURANCE))
DATA2$INSURANCE <- revalue(DATA2$INSURANCE, c("Government"="1","Medicaid"="2", "Medicare"="3", 
                                              "Private"="4", "Self Pay"="5"))
DATA2$INSURANCE<-as.integer(DATA2$INSURANCE)

table(as.factor(DATA2$LANGUAGE))
DATA2$LANGUAGE <- revalue(DATA2$LANGUAGE, c("ARAB"="1","CAMB"="2", "CANT"="3", 
                                            "CAPE"="4", "ENGL"="5","HAIT"="6", 
                                            "ITAL"="7", "MAND"="8", "PERS"="9", 
                                            "POLI"="10", "PORT"="11", "PTUN"="12",
                                            "RUSS"="13", "SPAN"="14", "VIET"="15"))
DATA2$LANGUAGE<-as.integer(DATA2$LANGUAGE)

table(as.factor(DATA2$MARITAL_STATUS))
DATA2$MARITAL_STATUS[DATA2$MARITAL_STATUS==""]<-"UNKNOWN (DEFAULT)"
DATA2$MARITAL_STATUS<-droplevels(DATA2$MARITAL_STATUS)
DATA2$MARITAL_STATUS <- revalue(DATA2$MARITAL_STATUS, c("UNKNOWN (DEFAULT)"="0","DIVORCED"="1", "LIFE PARTNER"="2", 
                                                        "MARRIED"="3", "SEPARATED"="4","SINGLE"="5", 
                                                        "WIDOWED"="6"))
DATA2$MARITAL_STATUS<-as.integer(DATA2$MARITAL_STATUS)

table(as.factor(DATA2$ETHNICITY))
DATA2$ETHNICITY <- revalue(DATA2$ETHNICITY, c("AMERICAN INDIAN/ALASKA NATIVE"="1","ASIAN"="2", "BLACK"="3", 
                                              "HISPANIC"="4", "MIDDLE EASTERN"="5","MULTI RACE ETHNICITY"="6", 
                                              "PORTUGUESE"="7", "SOUTH AMERICAN"="8", "UNKNOWN"="0", "WHITE"="9"))
DATA2$ETHNICITY<-as.integer(DATA2$ETHNICITY)

table(as.factor(DATA2$GENDER))
DATA2$GENDER <- revalue(DATA2$GENDER, c("M"="1", "F"="2"))
DATA2$GENDER<-as.integer(DATA2$GENDER)

table(as.factor(DATA2$DIAGNOSIS))
DATA2$DIAGNOSIS <- revalue(DATA2$DIAGNOSIS, c("acute respiratory failure"="1","alcohol"="2", "aneurism"="3", 
                                              "aorta"="4", "asthma"="5","atrial"="6", 
                                              "bacterial infection"="7", "bladder"="8", "broken bone"="9", 
                                              "cancer"="10", "cholangitis"="11", "chronic respiratory failure"="12",
                                              "colon"="13", "coma"="14", "cystic fibrosis"="15", "device/graft complications"="16", 
                                              "diabetes"="17","embolism"="18", "esophagus"="19", "heart failure"="20", "hemorrhage"="21", 
                                              "hemothorax"="22", "hiv"="23", "hypertension"="24", "kidney"="25", "liver"="26", "loss of consciousness"="27", 
                                              "lumbar"="28", "meth lung"="29", "non_lung abscess"="30", "non_lung injury"="31", "obesity"="32", "obstruction"="33",
                                              "other"="34", "pancreas"="35", "pleurisy"="36", "pnuemonia"="37", "postop"="38", "spinal injury/disease"="39", 
                                              "subendo"="40", "swelling"="41", "toxin/poison"="42", "tracheostomy"="43", "tuberculosis"="44", "ulcer"="45",
                                              "urinary"="46", "valve disorder"="47", "ventrical"="48"))
DATA2$DIAGNOSIS<-as.integer(DATA2$DIAGNOSIS)

table(as.factor(DATA2$ORG_NAME))
DATA2$ORG_NAME <- revalue(DATA2$ORG_NAME, c("ACINETOBACTER BAUMANNII"="1","BACILLUS"="2", "CANDIDA ALBICANS, PRESUMPTIVE IDENTIFICATION"="3", 
                                            "CLOSTRIDIUM DIFFICILE"="4", "ENTEROBACTER AEROGENES"="5","ENTEROBACTER CLOACAE"="6", 
                                            "ENTEROCOCCUS"="7", "ESCHERICHIA COLI"="8", "GRAM NEGATIVE ROD(S)"="9", "GRAM POSITIVE BACTERIA"="10",
                                            "INFLUENZA"="11", "KLEBSIELLA OXYTOCA"="12", "KLEBSIELLA PNEUMONIAE"="13", "MORAXELLA CATARRHALIS"="14",
                                            "OTHER"="15", "PROTEUS MIRABILIS"="16", "PSEUDOMONAS"="17", "SERRATIA MARCESCENS"="18",
                                            "STAPHYLOCOCCUS"="19", "STREPTOCOCCUS"="20", "YEAST"="21"))
DATA2$ORG_NAME<-as.integer(DATA2$ORG_NAME)
DATA2$HOSPITAL_EXPIRE_FLAG<-as.integer(DATA2$HOSPITAL_EXPIRE_FLAG)

M <- cor(DATA2) # get correlations

library('corrplot') #package corrplot
corrplot(M, method = "circle") #plot matrix

#*******HOUSEKEEPING************
rm(apsiii)
rm(blood_gas)
rm(blood_gas_all)
rm(blood_gas_arterial)
rm(blood_gas_arterial_glucose)
rm(blood_gas_arterial_pco2)
rm(blood_gas_arterial_ph)
rm(blood_gas_arterial_po2)
rm(blood_gas_glucose)
rm(blood_gas_pco2)
rm(blood_gas_arterial_totalco2)
rm(blood_gas_ph)
rm(blood_gas_po2)
rm(blood_gas_totalco2)
rm(callout)
rm(caregivers)
rm(d_icd_diagnoses)
rm(diagnoses_icd)
rm(gcs)
rm(high_count)
rm(icustays)
rm(LODS)
rm(low_count)
rm(microevents)
rm(df)
rm(not_null_glucose)
rm(not_null_pco2)
rm(not_null_ph)
rm(not_null_po2)
rm(not_null_totalco2)
rm(notmissing)
rm(missing)
rm(null_glucose)
rm(null_pco2)
rm(null_ph)
rm(null_po2)
rm(null_totalco2)
rm(patients)
rm(OASIS)
rm(resp_1)
rm(resp_2)
rm(resp_3)
rm(resp_4)
rm(resp_5)
rm(resp_6)
rm(resp_7)
rm(resp_8)
rm(resp_9)
rm(resp_10)
rm(resp_11)
rm(resp_12)
rm(resp_13)
rm(resp_14)
rm(resp_15)
rm(resp_16)
rm(resp_17)
rm(resp_18)
rm(resp_19)
rm(resp_20)
rm(resp_all)
rm(rrt)
rm(saps)
rm(sapsii)
rm(SOFA)
rm(unique_resp)
rm(uniqe_resp)
rm(uo)
rm(vent)
rm(vitals)
rm(weight)
rm(corp)
rm(dtm)
rm(text)

#**********SMOTE BALANCED DATA SET**************
data<-DATA2
data$target<-data$HOSPITAL_EXPIRE_FLAG #change target name for easier coding
data$HOSPITAL_EXPIRE_FLAG<-NULL # remove original target variable
print(prop.table(table(data$target)))

#create a data partition of 70% and 30% of data
set.seed(1234)
splitIndex <- createDataPartition(data$target, p = .70,
                                  list = FALSE,
                                  times = 1)
# create training data set (70%) and test data set (30%)
training <- data[ splitIndex,]
test <- data[-splitIndex,]

# check that probabilities are the same in the new data sets as original
prop.table(table(training$target))
table(training$target)
prop.table(table(test$target))

#install.packages("DMwR")
library(DMwR)
#Use smote to create synthetic instances of minority target class 
training <- SMOTE(target ~ ., training, perc.over = 300)
table(training$target)
prop.table(table(training$target))

# take sample of 20% for pilot models
pilot=training[sample(nrow(training),replace=F,size=0.20*nrow(training)),]

