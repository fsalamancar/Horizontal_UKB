library(tidyr)
library(dplyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)
library(data.table)
library(lubridate)
library(ggplot2)
library(scales)
library(ggrepel)
library(lme4)
library(caret)
library(splines)
library(visreg)
library(ggpubr)

# install package detectseparation
#install.packages('car')
#install.packages('ggeffects')
library(brglm2)
library(car)
library(detectseparation)
##
## Functions
## 
points_plot <- function(dat,plotname,psize=1,txsize=1){
    diagnoses <- dat %>%mutate(CD=ymd(CD),UC=ymd(UC),IBD=ymd(IBD),TM1=ymd(TM1),TM2=ymd(TM2),TM3=ymd(TM3),TM4=ymd(TM4))%>%
    select(eid,CD,UC,IBD,TM1,TM2,TM3,TM4) %>%
    pivot_longer(
    cols = -eid,
    names_to = "Event_Type",
    values_to = "Date"
    ) 
    # if date = 1900-01-01,1901-01-01,1902-02-02,1903-03-03,1909-09-09,2037-07-07 then NA
    #diagnoses <- diagnoses%>%mutate(Date=ifelse(Date%in%ymd('1900-01-01','1901-01-01','1902-02-02','1903-03-03','1909-09-09','2037-07-07'),NA,Date))
    na_dates <- as.Date(c('1900-01-01','1901-01-01','1902-02-02','1903-03-03','1909-09-09','2037-07-07'))
    diagnoses$Date[diagnoses$Date %in% na_dates] <- NA
    diagnoses <- diagnoses%>%mutate(Disease = case_when(
        Event_Type == "CD" ~ "Crohn's Disease",
        Event_Type == "UC" ~ "Ulcerative Colitis",
        Event_Type == "IBD" ~ "Inflammatory Bowel Disease",
        Event_Type == "TM1" ~ "Assessment1",
        Event_Type == "TM2" ~ "Assessment2",
        Event_Type == "TM3" ~ "Assessment3",
        Event_Type == "TM4" ~ "Assessment4",
        TRUE ~ NA
    ),Event = case_when(
        Event_Type == "CD" ~ "Disease",
        Event_Type == "UC" ~ "Disease",
        Event_Type == "IBD" ~ "Disease",
        Event_Type == "TM1" ~ "Assessment",
        Event_Type == "TM2" ~ "Assessment",
        Event_Type == "TM3" ~ "Assessment",
        Event_Type == "TM4" ~ "Assessment",
        TRUE ~ NA
    ) )

    # Calculate timeline ranges for horizontal lines
    timeline_ranges <- diagnoses %>%
    group_by(eid) %>%
    summarize(
        start = min(Date, na.rm = TRUE),
        end = max(Date, na.rm = TRUE)
    )

    ggplot(diagnoses, aes(x = as.Date(Date), y = factor(eid))) +
    # Plot samples as points
    # Add vertical lines for each event
    geom_line(aes(group = eid), linewidth=0.2, color='grey70') +
    geom_point(
        data = filter(diagnoses, Event_Type == "CD"),
        shape = 21, size = psize, fill = "#00fbff", alpha = 0.7
    )+
    geom_point(
        data = filter(diagnoses, Event_Type == "UC"),
        shape = 21, size = psize, fill = "#490e7c", alpha = 0.7
    )+#3c00ff
    geom_point(
        data = filter(diagnoses, Event_Type == "IBD"),
        shape = 21, size = psize, fill = "blue", alpha = 0.7 #"#20b7e9"
    )+#00ff1e#00ff3c
    geom_point(
        data = filter(diagnoses, Event_Type == "TM1"),
        shape=25, size = psize, fill = "#0dff00" ,alpha = 0.7
    )+#eeff00#e1ff00#E1FF00#e1ff00#ddf#fbff00
    geom_point(
        data = filter(diagnoses, Event_Type == "TM2"),
        shape=25, size = psize, fill = "#fbff00" ,alpha = 0.7
    )+
    geom_point(
        data = filter(diagnoses, Event_Type == "TM3"),
        shape=25, size = psize, fill = "#FF9500" ,alpha = 0.7
    )+
    geom_point(
        data = filter(diagnoses, Event_Type == "TM4"),
        shape=25, size = psize, fill = "#ff1900" ,alpha = 0.7
    )+
    geom_text_repel(
        data = diagnoses,
        aes(label = Event_Type),
        direction = "x",
        nudge_x = 0.2,
        nudge_y = 0.5,
        segment.color = NA,
        size = txsize
    ) +
    scale_x_date(
        #limits = c(as.Date("1990-01-01"), as.Date("2025-01-01")),
        breaks = date_breaks("2 years"),
        labels = date_format("%Y"),
        expand = expansion(add = c(50, 50))  # Add padding for labels
    ) +
    # Format axes and theme
    #scale_x_date(date_breaks = "1 year", labels = date_format("%Y")) +
    labs(x = "Date", y = "Individual ID", title = "Disease Diagnoses and Sample Collection Timeline") +
    theme_minimal() +
    theme(
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
    # save as pdf

    ggsave(paste0('/Users/guillermotorres/Library/CloudStorage/Dropbox/DX_Projects/UKB/2024/data2use/',plotname,'.pdf'),width=16,height=45)

}
##

## 
# read olink files 
olinkcharfile <- '/Users/guillermotorres/Library/CloudStorage/Dropbox/DX_Projects/UKB/2024/data2use/olink_chars.tsv'
olinkchar <- fread(olinkcharfile,header = TRUE, stringsAsFactors = FALSE)%>%as_tibble()%>%mutate(pname=Field%>%str_split(';')%>%map_chr(.,1)%>%unique(),.before=ValueType)
olinkfile <- '/Users/guillermotorres/Library/CloudStorage/Dropbox/DX_Projects/UKB/2024/data2use/olink_data.tsv'
olinkdata_raw <- fread(olinkfile,header = TRUE, stringsAsFactors = FALSE)%>%as.data.frame()
data.frame(olinkdata_raw[1:3,1:5])
remove_columns <- colnames(olinkdata_raw)[colnames(olinkdata_raw)%>%duplicated()]#%>%unique()
x <- which(remove_columns%in%colnames(olinkdata_raw))
# filter out duplicated columns
olinkdata <- data.frame(olinkdata_raw)[,-which(colnames(olinkdata_raw)%in%remove_columns)]%>%as_tibble()
olinkdata%>%select('f_183802839_3')
olinkdata%>%select(contains('_183802839_'))
# i want to change the colnames of olinkdata to the names in olinkchar, the column names of olinkdata have a patter: f_FieldID_Instance_Array
# and the information in olinkchar, we have FieldID. I want to change the FieldID to the pname in olinkchar
colnames_relation_df <- tibble(cnames=colnames(olinkdata)[-1])%>%mutate(fids=cnames%>%str_extract('f_\\d+\\_')%>%str_remove_all('f_|_')%>%as.integer(),instance=cnames%>%str_replace('f_\\d+\\_|f_NA\\_',''))%>%
                        left_join(olinkchar%>%select(FieldID,pname),by=c('fids'='FieldID'))%>%mutate(newnames=paste0(pname,'_',instance))
colnames(olinkdata) <- c('eid',colnames_relation_df$newnames)

eids <- olinkdata%>%pull('eid')
## lets filter individuals with olink data from multiple times
##olinkdata%>%select(eid, matches('f_.*_3'))#%>%summarise_all(~sum(!is.na(.)))%>%t()%>%.[,1,drop=T]
##olink_multi_times <- c(olinkdata%>%select(eid, matches('f_.*_0')),olinkdata%>%select(eid, matches('f_.*_1')),olinkdata%>%select(eid, matches('f_.*_2')),olinkdata%>%select(eid, matches('f_.*_3')))
# read phenotypes
#list.files('/Users/guillermotorres/Library/CloudStorage/Dropbox/DX_Projects/UKB/2024/data2use/')
#  Smoking status	20116; Alcohol drinker status	20117 ;; 90:	-3	Prefer not to answer ; 0 Never ; 1 Previous; 2 Current	
pheno_file <- '/Users/guillermotorres/Library/CloudStorage/Dropbox/DX_Projects/UKB/2024/data2use/phenotype_data.tsv'
phenodata_raw <- fread(pheno_file,header = TRUE, stringsAsFactors = FALSE)%>%as_tibble()
phenodata_esential <- phenodata_raw%>%dplyr::select(eid, matches('_20116_|_20117_'))
colnames(phenodata_esential) <- colnames(phenodata_esential)%>%str_replace_all('f_20116','Smoking')%>%str_replace_all('f_20117','Alcohol')%>%str_replace_all('_0$','')
# if -3 then NA for each of the columns except eid
phenodata_esential <- phenodata_esential%>%mutate(across(where(is.numeric),~ifelse(.==-3,NA,.)))


## Genetics - Ethnicity
pcasfile <- '/Users/guillermotorres/Library/CloudStorage/Dropbox/DX_Projects/UKB/2024/data2use/genomics_data.tsv'
pcas_raw <- fread(pcasfile,header = TRUE, stringsAsFactors = FALSE)%>%as_tibble()
colnames(pcas_raw) <- colnames(pcas_raw)%>%str_replace_all('f_22009_0','PC')


# Waist circumference: 48 Hip circumference: 49; Standing height 50; Body mass index (BMI)	23104; Whole body fat mass	23100; Whole body fat-free mass	23101; Whole body water mass	23102
physicalmfile <- '/Users/guillermotorres/Library/CloudStorage/Dropbox/DX_Projects/UKB/2024/data2use/Physical_measures_data.tsv'
phys_raw <- fread(physicalmfile,header = TRUE, stringsAsFactors = FALSE)%>%as_tibble()
phys_esential <- phys_raw%>%dplyr::select(eid, matches('_48_|_49_|_23104_|_23100_|_23101_|_23102_'))%>%mutate(WHR_0=(f_48_0_0/f_49_0_0),WHR_1=(f_48_1_0/f_49_1_0),WHR_2=(f_48_2_0/f_49_2_0),WHR_3=(f_48_3_0/f_49_3_0),.before=f_48_0_0)
colnames(phys_esential) <- colnames(phys_esential)%>%str_replace_all('f_48','Waist')%>%str_replace_all('f_49','Hip')%>%
str_replace_all('f_23104','BMI')%>%str_replace_all('f_23100','wFatMass')%>%str_replace_all('f_23101','wFatFreeMass')%>%str_replace_all('f_23102','wWaterMass')%>%str_replace_all('_0$','')%>%str_replace_all('WHR$','WHR_0')#%>%str_replace_all('WHR','WaistHipRatio')
phys_esential <- phys_esential%>%dplyr::select(-matches('Waist|Hip'))
# Sex	31 (0 female, 1 male); Year of birth	34; Month of birth	52
popcharfile <- '/Users/guillermotorres/Library/CloudStorage/Dropbox/DX_Projects/UKB/2024/data2use/popchar_data.tsv'
popchar_raw <- fread(popcharfile,header = TRUE, stringsAsFactors = FALSE)%>%as_tibble()
popchar_esential <- popchar_raw%>%dplyr::select(eid, matches('_31_|_34_|_52_'))
colnames(popchar_esential) <- colnames(popchar_esential)%>%str_replace_all('f_31','Sex')%>%str_replace_all('f_34','YearOfBirth')%>%str_replace_all('f_52','MonthOfBirth')%>%str_replace_all('_0$','')
# we will get the esential phenotypes for corrections, such as smoking, sex, bmi, whr, age, etc
esentialpheno <- phenodata_esential%>%inner_join(phys_esential,by='eid')%>%inner_join(popchar_esential,by='eid')
##
# read the fist occurrences
fcfile <- '/Users/guillermotorres/Library/CloudStorage/Dropbox/DX_Projects/UKB/2024/data2use/FirstOccurrences_data.tsv'
fcdata_raw <- fread(fcfile,header = TRUE, stringsAsFactors = FALSE)%>%as_tibble()
fcdata_raw[1:3,1:5]
fccharfile <- '/Users/guillermotorres/Library/CloudStorage/Dropbox/DX_Projects/UKB/2024/data2use/FirstOccurrences_chars.tsv'
fochar <- fread(fccharfile,header = TRUE, stringsAsFactors = FALSE)%>%as_tibble()
# get all the dsates from the first occurrences
dates_raw <- lapply(2:ncol(fcdata_raw), function(x) fcdata_raw%>%pull(x)%>%unique()%>%as.character())%>%unlist()%>%unique()
dates_raw0 <- dates_raw[-c(19007,19008,19009,19010,19051)]
dates <- dates_raw0[!is.na(dates_raw0)]%>%ymd()
failed_idx <- which(is.na(dates))
dates_raw0[failed_idx]
dates <- c(dates,dates_raw0[failed_idx]%>%ymd())%>%.[!is.na(.)]%>%sort()
dates
# 
# read recruitment recruitment_data.tsv
recruitmentfile <- '/Users/guillermotorres/Library/CloudStorage/Dropbox/DX_Projects/UKB/2024/data2use/recruitment_data.tsv'
recruitmentdata_raw <- fread(recruitmentfile,header = TRUE, stringsAsFactors = FALSE)%>%as_tibble()
### lets do longitudinal
# lets do a population controls - meaning no individuals with gastric problems

disease_umbrellas <- fochar%>%pull(Path)%>%unique()%>%lapply(.,function(x) x%>%str_split('>')%>%unlist()%>%rev(.)%>%.[1]%>%trimws(.))%>%unlist()
digestive_disorders <- fochar%>%filter(Path%>%str_detect('Digestive system disorders'))%>%filter(ValueType=='Date')#%>%filter(!Field%>%str_detect('K52|K50|K51'))
view(digestive_disorders)
# lets find the from fochar fields with ibd chrons and uc
ibd_fields <- fochar%>%filter(Field%>%str_detect('K52|K50|K51'))%>%filter(ValueType=='Date')
ibd_fields
## filtrate individuals with olink data
olink_recruits <- recruitmentdata_raw%>%filter(eid%in%eids)
fosi <- fcdata_raw%>%filter(eid%in%eids)
# from fosi select the columns eid and those which match partially with the vector of ibd_fields$FieldID
fosid <- fosi%>%select(eid,matches(paste0(ibd_fields$FieldID,collapse='|')))
colnames(fosid) <- colnames(fosid)%>%str_replace_all('f_|_0','')%>%str_replace('131626','CD')%>%str_replace('131628','UC')%>%str_replace('131630','IBD')
# count for each column the non NA values - raw counts; maybe same individual with multiple diseases
fosid%>%summarise_all(~sum(!is.na(.)))%>%t()%>%.[,1,drop=T]
sick <- c(fosid%>%filter(!is.na(CD))%>%pull(eid),fosid%>%filter(!is.na(UC))%>%pull(eid),fosid%>%filter(!is.na(IBD))%>%pull(eid))%>%unique()
## from fosi lets select the population controls that means remove those with digestive disorders
# filtrate first the columns that are related with digestive disorders
ddsid <- fosi%>%select(eid,matches(paste0(digestive_disorders$FieldID,collapse='|')))
# from ddsid select th
# e rows that execpt column eid the rest of columns are NA - which means the individual that has no digestive disorders
popcontrols <- ddsid%>%filter(rowSums(is.na(.))==(ncol(ddsid)-1))
popcontrolsids <- popcontrols%>%pull(eid)
#popcs <- popcontrols%>%select(eid,matches(paste0(ibd_fields$FieldID,collapse='|')))
#colnames(popcs) <- colnames(popcs)%>%str_replace_all('f_|_0','')%>%str_replace('131626','CD')%>%str_replace('131628','UC')%>%str_replace('131630','IBD')
# the time where the measurement was taken is in the recruitment data. We have 4 Dates of attending assessment centre points f_53_0_0   f_53_1_0 f_53_2_0   f_53_3_0 
# and therefore 4 Age informatin, each when attended assessment  f_21003_0_0 f_21003_1_0 f_21003_2_0 f_21003_3_0 
# For corrections we will use the UK Biobank assessment centre f_54_0_0 
# Therefore the new table looks like:
timestamps <- olink_recruits%>%select(eid,f_53_0_0,f_53_1_0,f_53_2_0,f_53_3_0,f_21003_0_0,f_21003_1_0,f_21003_2_0,f_21003_3_0,f_54_0_0)
# rename timestamps like (c('TM1','TM2','TM3','TM4') = c('f_53_0_0','f_53_1_0','f_53_12_0','f_53_3_0'))
colnames(timestamps) <- c('eid','TM1','TM2','TM3','TM4','Age1','Age2','Age3','Age4','AssessmentCentre')
#colnames(olinkdata)%>%str_detect(".+_1&")
# lets merge timestamps and fosid
infodata <- fosid%>%left_join(timestamps,by='eid')


# lets add a new variable Time-Relative-to-Diagnosis
infodata0 <- infodata%>%mutate(CD2T1=as.numeric(ymd(CD)-ymd(TM1)),CD2T2=as.numeric(ymd(CD)-ymd(TM2)),CD2T3=as.numeric(ymd(CD)-ymd(TM3)),CD2T4=as.numeric(ymd(CD)-ymd(TM4)),
                               UC2T1=as.numeric(ymd(UC)-ymd(TM1)),UC2T2=as.numeric(ymd(UC)-ymd(TM2)),UC2T3=as.numeric(ymd(UC)-ymd(TM3)),UC2T4=as.numeric(ymd(UC)-ymd(TM4)),
                               IBD2T1=as.numeric(ymd(IBD)-ymd(TM1)),IBD2T2=as.numeric(ymd(IBD)-ymd(TM2)),IBD2T3=as.numeric(ymd(IBD)-ymd(TM3)),IBD2T4=as.numeric(ymd(IBD)-ymd(TM4)))
#infodata0%>%filter(eid%in%sick)
infodata1_0 <- infodata%>%pivot_longer(c(CD,UC,IBD), names_to = 'Disease',values_to = 'DiagnosedAt')%>%
mutate(DxTM1=as.numeric(ymd(DiagnosedAt)-ymd(TM1)),DxTM2=as.numeric(ymd(DiagnosedAt)-ymd(TM2)),DxTM3=as.numeric(ymd(DiagnosedAt)-ymd(TM3)),DxTM4=as.numeric(ymd(DiagnosedAt)-ymd(TM4)))%>%
mutate(TCategory1=ifelse(DxTM1<0,'Pre-diagnosis','Post-diagnosis'),TCategory2=ifelse(DxTM2<0,'Pre-diagnosis','Post-diagnosis'),TCategory3=ifelse(DxTM3<0,'Pre-diagnosis','Post-diagnosis'),TCategory4=ifelse(DxTM4<0,'Pre-diagnosis','Post-diagnosis'))
#infodata1_0%>%filter(DxTM1<0)
# Now in infodata1 columns: TM1, TM2 TM3 TM4 are one group of information that of time then Age is also another group 
infodata1 <- infodata1_0%>%pivot_longer(
            cols = -c(eid,Disease,DiagnosedAt,AssessmentCentre),
            names_to = c(".value", "Times"),  # Extract variable names & time numbers)
            names_pattern = "([A-Za-z]+)(\\d+)"  # Split into "TM"/"Age"/"DxTM"/"TCategory" and Time (1-4)
            )

# i want to filter those which  Time-Relative-to-Diagnosis is positive
sickdb <- infodata0%>%filter(eid%in%c(sick))%>%filter(CD2T1>0|CD2T2>0|CD2T3>0|CD2T4>0|UC2T1>0|UC2T2>0|UC2T3>0|UC2T4>0|IBD2T1>0|IBD2T2>0|IBD2T3>0|IBD2T4>0)

sickpopcon <- infodata0%>%filter(eid%in%c(sick,popcontrolsids))
pocon <- infodata0%>%filter(eid%in%c(popcontrolsids))

sickpopcon%>%filter(!is.na(CD))
infodata0%>%filter(!is.na(CD))
cds <- infodata0%>%filter(!is.na(CD))

# lets get subset of people that have oly diagnose as CD or UC or IBD
cds_only <- sickpopcon%>%filter(!is.na(CD))%>%filter(is.na(UC) & is.na(IBD))
uc_only <- sickpopcon%>%filter(!is.na(UC))%>%filter(is.na(CD) & is.na(IBD))
ibd_only <- sickpopcon%>%filter(!is.na(IBD))%>%filter(is.na(CD) & is.na(UC))
uc_ibd_only <- sickpopcon%>%filter(!is.na(UC) & !is.na(IBD))%>%filter(is.na(CD))
cd_ibd_only <- sickpopcon%>%filter(!is.na(CD) & !is.na(IBD))%>%filter(is.na(UC))

cds_only%>%filter(is.na(TM2) & is.na(TM3) & is.na(TM4))

cds_popc <- cds_only%>%bind_rows(pocon)
uc_popc <- uc_only%>%bind_rows(pocon)
ibd_popc <- ibd_only%>%bind_rows(pocon)

# point plots
dat <- cds_only
plotname <- 'diagnoses_timeline_CDsOnly'
points_plot(dat,plotname,psize=1.5,txsize=1.5)
#s
### analyisis road map 
# lets transform the data in a simpler way using infodata1 (long version)
disease_only_eids <- c(cds_only$eid,uc_only$eid,ibd_only$eid)
disease_popc <- ibd_only%>%bind_rows(cds_only)%>%bind_rows(uc_only)%>%bind_rows(pocon)
disease_popc_0 <- disease_popc%>%dplyr::select(eid,CD,UC,IBD,TM1,Age1,AssessmentCentre,CD2T1,UC2T1,IBD2T1)

donly <- infodata1%>%filter(eid%in%disease_only_eids)

table(donly%>%filter(Disease=="CD" & Times == 2)%>%pull(TCategory))
table(donly%>%filter(Disease=="UC" & Times == 2)%>%pull(TCategory))
table(donly%>%filter(Disease=="IBD" & Times == 2)%>%pull(TCategory))

t1post <- donly%>%filter(Disease=="IBD" & Times == 1 & TCategory == 'Post-diagnosis')%>%pull(eid)
donly%>%filter(eid%in%t1post & Disease=="IBD" & Times != 1 & TCategory == 'Pre-diagnosis')%>%nrow()

ibd_popc%>%filter(is.na(IBD))

ibd <- disease_popc_0%>%
mutate(Disease=ifelse(!is.na(CD),'CD',ifelse(!is.na(UC),'UC',ifelse(!is.na(IBD),'IBD','Control'))),Time_Category=ifelse(IBD2T1<0,'Pre-diagnosis','Post-diagnosis'))%>%
mutate(Time_Category=ifelse(is.na(Time_Category),'Control',Time_Category))%>%
mutate(Time_Category=factor(Time_Category,levels=c(.$Time_Category%>%unique()%>%sort())))%>%left_join(esentialpheno,by='eid')%>%left_join(pcas_raw%>%dplyr::select(eid,paste0('PC_',1:5)),by='eid')%>%left_join(olinkdata,by='eid')

ibd0 <- ibd%>%dplyr::select(1:12,contains('PC_'),contains('_0'),-WHR_0,-BMI_0,-YearOfBirth_0,-MonthOfBirth_0)%>%mutate(Time_Category=factor(Time_Category,levels=c('Control','Pre-diagnosis','Post-diagnosis')))

# i want to assess the association  expression for each protein (such as A1BG_0) between sick (IBD) of control using a logistic analysis assessing and correcting for smoking, alcohol, wFatMass_0 wFatFreeMass_0 wWaterMass_0, Sex_0

# path for ukb results
ukbpath <- '/Users/guillermotorres/Library/CloudStorage/Dropbox/DX_Projects/miGut/Olinik_2023/ukb/'

ibd0_ready <- ibd0%>%filter(Time_Category%in%c('Control','Post-diagnosis'))%>%mutate(diagnostic=ifelse(Disease!='Control',0,1),.before=2)
ibd0_ready$diagnostic%>%table()
vif(lm(A1BG_0 ~ Age1 + Smoking_0 + Alcohol_0 +wFatMass_0 + wFatFreeMass_0 + wWaterMass_0, data=ibd0_ready))

pasocs <- list()
disease_assoc <- tibble(
  protein  = character(),
  beta = numeric(),
  SD= numeric(),
  pvalue  = numeric()
)
ibd_assoc <- tibble(
  protein  = character(),
  beta = numeric(),
  SD= numeric(),
  pvalue  = numeric()
)
uc_assoc <- tibble(
  protein  = character(),
  beta = numeric(),
  SD= numeric(),
  pvalue  = numeric()
)
cd_assoc <- tibble(
  protein  = character(),
  beta = numeric(),
  SD= numeric(),
  pvalue  = numeric()
)

cd_uc_assoc <- tibble(
  protein  = character(),
  beta = numeric(),
  SD= numeric(),
  pvalue  = numeric()
)
ibd_uc_assoc <- tibble(
  protein  = character(),
  beta = numeric(),
  SD= numeric(),
  pvalue  = numeric()
)
ibd_cd_assoc <- tibble(
  protein  = character(),
  beta = numeric(),
  SD= numeric(),
  pvalue  = numeric()
)
n_proteins <-ncol(ibd0_ready)
pb <- txtProgressBar(min = 24, max = n_proteins, style = 3)  
# lets do a logistic regression for each protein
#ibd0_ready[,-c(1:24)]
for (i in 25:ncol(ibd0_ready)){
    prot <- colnames(ibd0_ready)[i]
    formula_str <- paste0("diagnostic ~",prot, "+ Age1 + PC_1 + PC_2 + PC_3 + PC_4 + PC_5 + Smoking_0 + Alcohol_0 + wFatMass_0 + wFatFreeMass_0 + wWaterMass_0")
    fmla <- as.formula(formula_str)
    # Fit logistic regression
    piglm <- glm(fmla, data = ibd0_ready, family = "binomial")
    stats <- tibble(
        beta = coef(piglm),
        std.error = summary(piglm)$coefficients[, "Std. Error"],
        p.value = summary(piglm)$coefficients[, "Pr(>|z|)"]
    )%>%t()%>%as_tibble()%>%mutate(ids=c('beta','std.error','p.value'),.before=1) 
    colnames(stats) <- c('ids',names(coef(piglm))%>%str_replace_all("\\(|\\)",''))
    pasocs[[i]] <- stats
    stats_prot <- stats%>%pull(contains(prot))
    disease_assoc <- disease_assoc%>%bind_rows(tibble( protein = prot,beta = stats_prot[1], SD = stats_prot[2], pvalue = stats_prot[3]))
    ### IBD only
    formula_str <- paste0("diagnostic ~",prot, "+ Age1 + PC_1 + PC_2 + PC_3 + PC_4 + PC_5 + Smoking_0 + Alcohol_0 + wFatMass_0 + wFatFreeMass_0 + wWaterMass_0")
    fmla <- as.formula(formula_str)
    ibd_glm <- glm(fmla, data = ibd0_ready%>%filter(Disease%in%c('Control','IBD')), family = "binomial")
    stats <- tibble(
        beta = coef(ibd_glm),
        std.error = summary(ibd_glm)$coefficients[, "Std. Error"],
        p.value = summary(ibd_glm)$coefficients[, "Pr(>|z|)"]
    )%>%t()%>%as_tibble()%>%mutate(ids=c('beta','std.error','p.value'),.before=1) 
    colnames(stats) <- c('ids',names(coef(ibd_glm))%>%str_replace_all("\\(|\\)",''))
    stats_prot <- stats%>%pull(contains(prot))
    ibd_assoc <- ibd_assoc%>%bind_rows(tibble( protein = prot,beta = stats_prot[1], SD = stats_prot[2], pvalue = stats_prot[3]))
    ### UC only
    formula_str <- paste0("diagnostic ~",prot, "+ Age1 + PC_1 + PC_2 + PC_3 + PC_4 + PC_5 + Smoking_0 + Alcohol_0 + wFatMass_0 + wFatFreeMass_0 + wWaterMass_0")
    fmla <- as.formula(formula_str)
    uc_glm <- glm(fmla, data = ibd0_ready%>%filter(Disease%in%c('Control','UC')), family = "binomial")
    stats <- tibble(
        beta = coef(uc_glm),
        std.error = summary(uc_glm)$coefficients[, "Std. Error"],
        p.value = summary(uc_glm)$coefficients[, "Pr(>|z|)"]
    )%>%t()%>%as_tibble()%>%mutate(ids=c('beta','std.error','p.value'),.before=1) 
    colnames(stats) <- c('ids',names(coef(uc_glm))%>%str_replace_all("\\(|\\)",''))
    stats_prot <- stats%>%pull(contains(prot))
    uc_assoc <- uc_assoc%>%bind_rows(tibble( protein = prot,beta = stats_prot[1], SD = stats_prot[2], pvalue = stats_prot[3]))
    ### CD only
    formula_str <- paste0("diagnostic ~",prot, "+ Age1 + PC_1 + PC_2 + PC_3 + PC_4 + PC_5 + Smoking_0 + Alcohol_0 + wFatMass_0 + wFatFreeMass_0 + wWaterMass_0")
    fmla <- as.formula(formula_str)
    cd_glm <- glm(fmla, data = ibd0_ready%>%filter(Disease%in%c('Control','CD')), family = "binomial")
    stats <- tibble(
        beta = coef(cd_glm),
        std.error = summary(cd_glm)$coefficients[, "Std. Error"],
        p.value = summary(cd_glm)$coefficients[, "Pr(>|z|)"]
    )%>%t()%>%as_tibble()%>%mutate(ids=c('beta','std.error','p.value'),.before=1)
    colnames(stats) <- c('ids',names(coef(cd_glm))%>%str_replace_all("\\(|\\)",''))
    stats_prot <- stats%>%pull(contains(prot))
    cd_assoc <- cd_assoc%>%bind_rows(tibble( protein = prot,beta = stats_prot[1], SD = stats_prot[2], pvalue = stats_prot[3]))
    # Update the progress bar
    setTxtProgressBar(pb, i)
}

# 1. Calculate lower and upper CI
disease_a <- disease_assoc %>%
    mutate(
        lowerCI = beta - 1.96 * SD,
        upperCI = beta + 1.96 * SD,
        ci_width = upperCI - lowerCI,
        p_adj = p.adjust(pvalue, method = "BH"),
        logp = -log10(p_adj),
        # For illustration, consider p < 0.05 as "significant" 
        # or use adjusted p-values if you've done p.adjust().
        sig_label = ifelse(p_adj < 0.05, "Significant", "Not Significant")
)
ibd_a <- ibd_assoc %>%
    mutate(
        lowerCI = beta - 1.96 * SD,
        upperCI = beta + 1.96 * SD,
        ci_width = upperCI - lowerCI,
        p_adj = p.adjust(pvalue, method = "BH"),
        logp = -log10(p_adj),
        # For illustration, consider p < 0.05 as "significant" 
        # or use adjusted p-values if you've done p.adjust().
        sig_label = ifelse(p_adj < 0.05, "Significant", "Not Significant")
)
uc_a <- uc_assoc %>%
    mutate(
        lowerCI = beta - 1.96 * SD,
        upperCI = beta + 1.96 * SD,
        ci_width = upperCI - lowerCI,
        p_adj = p.adjust(pvalue, method = "BH"),
        logp = -log10(p_adj),
        # For illustration, consider p < 0.05 as "significant" 
        # or use adjusted p-values if you've done p.adjust().
        sig_label = ifelse(p_adj < 0.05, "Significant", "Not Significant")
)
cd_a <- cd_assoc %>%
    mutate(
        lowerCI = beta - 1.96 * SD,
        upperCI = beta + 1.96 * SD,
        ci_width = upperCI - lowerCI,
        p_adj = p.adjust(pvalue, method = "BH"),
        logp = -log10(p_adj),
        # For illustration, consider p < 0.05 as "significant" 
        # or use adjusted p-values if you've done p.adjust().
        sig_label = ifelse(p_adj < 0.05, "Significant", "Not Significant")
)

cd_uc_a <- cd_uc_assoc %>%
    mutate(
        lowerCI = beta - 1.96 * SD,
        upperCI = beta + 1.96 * SD,
        ci_width = upperCI - lowerCI,
        p_adj = p.adjust(pvalue, method = "BH"),
        logp = -log10(p_adj),
        # For illustration, consider p < 0.05 as "significant" 
        # or use adjusted p-values if you've done p.adjust().
        sig_label = ifelse(p_adj < 0.05, "Significant", "Not Significant")
)

ibd_cd_a <- ibd_cd_assoc %>%
    mutate(
        lowerCI = beta - 1.96 * SD,
        upperCI = beta + 1.96 * SD,
        ci_width = upperCI - lowerCI,
        p_adj = p.adjust(pvalue, method = "BH"),
        logp = -log10(p_adj),
        # For illustration, consider p < 0.05 as "significant" 
        # or use adjusted p-values if you've done p.adjust().
        sig_label = ifelse(p_adj < 0.05, "Significant", "Not Significant")
)
ibd_uc_a <- ibd_uc_assoc %>%
    mutate(
        lowerCI = beta - 1.96 * SD,
        upperCI = beta + 1.96 * SD,
        ci_width = upperCI - lowerCI,
        p_adj = p.adjust(pvalue, method = "BH"),
        logp = -log10(p_adj),
        # For illustration, consider p < 0.05 as "significant" 
        # or use adjusted p-values if you've done p.adjust().
        sig_label = ifelse(p_adj < 0.05, "Significant", "Not Significant")
)

# write the tsv table
write_tsv(disease_a,paste0(ukbpath,'ibd_all_proteins_assoc.tsv'))
write_tsv(ibd_a,paste0(ukbpath,'ibd_only_proteins_assoc.tsv'))
write_tsv(uc_a,paste0(ukbpath,'uc_only_proteins_assoc.tsv'))
write_tsv(cd_a,paste0(ukbpath,'cd_only_proteins_assoc.tsv'))
#	1.	A Forest Plot to display effect sizes (betas) with confidence intervals.

hist(disease_a$ci_width)

ggplot(uc_a%>%filter(ci_width < 1), aes(y = reorder(protein, beta), x = beta)) +
  geom_point() +
  geom_errorbar(aes(xmin = lowerCI, xmax = upperCI), width = 0.2) + 
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed") +
  labs(
    x = "Beta (log-odds estimate)",
    y = "Protein",
    title = "Forest Plot of Logistic Regression Results"
  ) +
  theme_bw()

# 2. A Volcano Plot to visualize the relationship between effect size and significance.
ggplot(uc_a%>%filter(ci_width < 1), aes(x = beta, y = logp, color = sig_label)) +
  geom_point() +
  scale_color_manual(values = c("black", "red")) +
  labs(
    x = "Beta (effect size)",
    y = expression(-log[10](pvalue)),
    color = "Significance",
    title = "Volcano Plot"
  ) +
  theme_bw()


ggplot(disease_a%>%filter(ci_width < 1)%>%filter(p_adj < 0.05)%>%filter(abs(beta)>0.5),aes(y = reorder(protein, beta), x = beta)) +
  geom_point() +
  geom_errorbar(aes(xmin = lowerCI, xmax = upperCI), width = 0.2) + 
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed") +
  labs(
    x = "Beta (log-odds estimate)",
    y = "Protein",
    title = "Forest Plot of Logistic Regression Results"
  ) +
  theme_bw()

####
#sigprot <- dat%>%filter(ci_width < 0.5)%>%filter(p_adj < 0.05)%>%filter(abs(beta)>0.5)%>%pull(protein)

## From the significant proteins we will use to evaluate which ones have significant changes over time until diagnosis
# lets filter the data to only include the significant proteins
sigprots <- c(disease_a%>%filter(p_adj < 0.05)%>%pull(protein),
            uc_a%>%filter(p_adj < 0.05)%>%pull(protein),
            cd_a%>%filter(p_adj < 0.05)%>%pull(protein),
            ibd_a%>%filter(p_adj < 0.05)%>%pull(protein))%>%unique()

ibd0_pp <- ibd0%>%mutate(diagnostic=ifelse(Disease!='Control',0,1),.before=2)%>%filter(Disease=='IBD')%>%
                    dplyr::select(1:24,sigprots)

ibd0_pre <- ibd0%>%filter(Time_Category%in%c('Pre-diagnosis'))%>%
                    mutate(diagnostic=ifelse(Disease!='Control',0,1),.before=2)%>%filter(Disease=='IBD')%>%
                    dplyr::select(1:24,sigprots)

ibd0_post <- ibd0%>%filter(Time_Category%in%c('Post-diagnosis'))%>%
                    mutate(diagnostic=ifelse(Disease!='Control',0,1),.before=2)%>%filter(Disease=='IBD')%>%
                    dplyr::select(1:24,sigprots)

ibd_assocs_pre <- list()
ibd_glms_pre <- list()
res_ibd_pre <- tibble(
  protein  = character(),
  beta = numeric(),
  SD= numeric(),
  pvalue  = numeric()
)

ibd_assocs_post <- list()
ibd_glms_post <- list()
res_ibd_post <- tibble(
  protein  = character(),
  beta = numeric(),
  SD= numeric(),
  pvalue  = numeric()
)

n_proteins <-ncol(ibd0_pre)
pb <- txtProgressBar(min = 24, max = n_proteins, style = 3)  
# lets do a logistic regression for each protein
for (i in 25:ncol(ibd0_pre)){
    prot <- colnames(ibd0_pre)[i]
    formula_str <- paste0(prot,"~ IBD2T1 + Age1 + PC_1 + PC_2 + PC_3 + PC_4 + PC_5 + Smoking_0 + Alcohol_0 + wFatMass_0 + wFatFreeMass_0 + wWaterMass_0")
    fmla <- as.formula(formula_str)
    ## Pre
    # Fit regression
    piglm1 <- lm(fmla, data = ibd0_pre)
    stats <- tibble(
        beta = coef(piglm1),
        std.error = summary(piglm1)$coefficients[, "Std. Error"],
        p.value = summary(piglm1)$coefficients[, "Pr(>|t|)"]
    )%>%t()%>%as_tibble()%>%mutate(ids=c('beta','std.error','p.value'),.before=1) 
    colnames(stats) <- c('ids',names(coef(piglm1))%>%str_replace_all("\\(|\\)",''))
    ibd_assocs_pre[[i]] <- stats
    stats_prot <- stats%>%pull(IBD2T1)
    res_ibd_pre <- res_ibd_pre%>%bind_rows(tibble( protein = prot,beta = stats_prot[1], SD = stats_prot[2], pvalue = stats_prot[3]))
    ibd_glms_pre[[i]] <- piglm1

    ### Post
    # Fit regression
    piglm2 <- lm(fmla, data = ibd0_post)
    stats <- tibble(
        beta = coef(piglm2),
        std.error = summary(piglm2)$coefficients[, "Std. Error"],
        p.value = summary(piglm2)$coefficients[, "Pr(>|t|)"]
    )%>%t()%>%as_tibble()%>%mutate(ids=c('beta','std.error','p.value'),.before=1) 
    colnames(stats) <- c('ids',names(coef(piglm2))%>%str_replace_all("\\(|\\)",''))
    ibd_assocs_post[[i]] <- stats
    stats_prot <- stats%>%pull(IBD2T1)
    res_ibd_post <- res_ibd_post%>%bind_rows(tibble( protein = prot,beta = stats_prot[1], SD = stats_prot[2], pvalue = stats_prot[3]))
    ibd_glms_post[[i]] <- piglm2

    ## more than line trend
    # Example with a spline
    #model_spline <- lmer(protein_expr ~ bs(days_since_dx, df = 3) + age + sex + (1 | ID),data = df)
    # Update the progress bar
    setTxtProgressBar(pb, i)
}

res_ibd_post
res_ibd_pre
write_tsv(res_ibd_pre,paste0(ukbpath,'ibd_proteins_assoc_time_pre.tsv'))
write_tsv(res_ibd_post,paste0(ukbpath,'ibd_proteins_assoc_time_post.tsv'))
# 1. Calculate lower and upper CI
ibd_pre <- res_ibd_pre %>%
    mutate(
        lowerCI = beta - 1.96 * SD,
        upperCI = beta + 1.96 * SD,
        ci_width = upperCI - lowerCI,
        p_adj = p.adjust(pvalue, method = "BH"),
        logp = -log10(pvalue),
        # For illustration, consider p < 0.05 as "significant" 
        # or use adjusted p-values if you've done p.adjust().
        sig_label = ifelse(pvalue < 0.01, "Significant", "Not Significant")
)
ibd_post <- res_ibd_post %>%
    mutate(
        lowerCI = beta - 1.96 * SD,
        upperCI = beta + 1.96 * SD,
        ci_width = upperCI - lowerCI,
        p_adj = p.adjust(pvalue, method = "BH"),
        logp = -log10(pvalue),
        # For illustration, consider p < 0.05 as "significant" 
        # or use adjusted p-values if you've done p.adjust().
        sig_label = ifelse(pvalue < 0.01, "Significant", "Not Significant")
)
###
#hist(dat2$ci_width)
# 2. A Volcano Plot to visualize the relationship between effect size and significance.
ggplot(ibd_pre, aes(x = beta, y = logp, color = sig_label)) +
  geom_point() +
  scale_color_manual(values = c("black", "red")) +
  labs(
    x = "Beta (effect size)",
    y = expression(-log[10](pvalue)),
    color = "Significance",
    title = "Volcano Plot IBD - pre"
  ) +
  theme_bw()

ggplot(ibd_post, aes(x = beta, y = logp, color = sig_label)) +
  geom_point() +
  scale_color_manual(values = c("black", "red")) +
  labs(
    x = "Beta (effect size)",
    y = expression(-log[10](pvalue)),
    color = "Significance",
    title = "Volcano Plot IBD - post"
  ) +
  theme_bw()


ggplot(ibd_pre%>%filter(sig_label=='Significant'),aes(y = reorder(protein, beta), x = beta)) +
  geom_point() +
  geom_errorbar(aes(xmin = lowerCI, xmax = upperCI), width = 0.2) + 
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed") +
  labs(
    x = "Beta (log-odds estimate)",
    y = "Protein",
    title = "Forest Plot of Logistic Regression Results"
  ) +
  theme_bw()

###
# arrange the proteins by pvalue
ibd_pre%>%arrange(-desc(pvalue))


sigprot_all <- c(ibd_pre%>%filter(pvalue < 0.01)%>%pull(protein),ibd_post%>%filter(pvalue < 0.01)%>%pull(protein))%>%unique()
sigprot_pre <- c(ibd_pre%>%filter(pvalue < 0.01)%>%pull(protein))
sigprot_post <- c(ibd_post%>%filter(pvalue < 0.01)%>%pull(protein))


names(glms_2)

which(colnames(ibd0_cpp)%in%'CXCL13_0')

#visreg(glms_2[[which(colnames(ibd0_cpp)%in%'CXCL13_0')]], "IBD2T1", partial = TRUE)

visreg(ibd_glms_pre[[which(colnames(ibd0_pre)%in%'CXCL13_0')]], "CXCL13_0", scale="response", partial = TRUE, rug = TRUE, gg = TRUE) +
ggplot2::theme_minimal()

visreg(ibd_glms_post[[which(colnames(ibd0_post)%in%'CXCL13_0')]], "CXCL13_0", scale="response", partial = TRUE, rug = TRUE, gg = TRUE) +
ggplot2::theme_minimal()

library(ggeffects)
# Get effect for "time" while controlling for confounders
pred_data_post <- ggpredict(ibd_glms_post[[which(colnames(ibd0_post)%in%'CXCL13_0')]], terms = c("IBD2T1"))
plot(pred_data_post)

pred_data_pre <- ggpredict(ibd_glms_pre[[which(colnames(ibd0_pre)%in%'CXCL13_0')]], terms = c("IBD2T1"))
plot(pred_data_pre)

###

outliers_controls <- ibd0%>%filter(Time_Category=='Control')%>%mutate(
    mean_val = mean(CCL13_0, na.rm = TRUE),
    sd_val   = sd(CCL13_0, na.rm = TRUE),
    cutoff_lower = mean_val - 5 * sd_val,
    cutoff_upper = mean_val + 5 * sd_val,
    q1 = quantile(CCL13_0, 0.01, na.rm = TRUE),
    q99 = quantile(CCL13_0, 0.99, na.rm = TRUE)
    ,.before=2)%>%filter(!(CCL13_0 >= cutoff_lower & CCL13_0 <= cutoff_upper))%>%pull(eid)

outliers_pre <- ibd0%>%filter(Time_Category=='Pre-diagnosis')%>%mutate(
    mean_val = mean(CCL13_0, na.rm = TRUE),
    sd_val   = sd(CCL13_0, na.rm = TRUE),
    cutoff_lower = mean_val - 5 * sd_val,
    cutoff_upper = mean_val + 5 * sd_val,
    q1 = quantile(CCL13_0, 0.01, na.rm = TRUE),
    q99 = quantile(CCL13_0, 0.99, na.rm = TRUE)
    ,.before=2)%>%filter(!(CCL13_0 >= cutoff_lower & CCL13_0 <= cutoff_upper))%>%pull(eid)

outliers_post <- ibd0%>%filter(Time_Category=='Post-diagnosis')%>%mutate(
    mean_val = mean(CCL13_0, na.rm = TRUE),
    sd_val   = sd(CCL13_0, na.rm = TRUE),
    cutoff_lower = mean_val - 5 * sd_val,
    cutoff_upper = mean_val + 5 * sd_val,
    q1 = quantile(CCL13_0, 0.01, na.rm = TRUE),
    q99 = quantile(CCL13_0, 0.99, na.rm = TRUE)
    ,.before=2)%>%filter(!(CCL13_0 >= cutoff_lower & CCL13_0 <= cutoff_upper))%>%pull(eid)

outliers <- c(outliers_controls,outliers_pre,outliers_post)

ibd$Time_Category%>%levels()

# I want to filter the data to only include the significant proteins

ggplot(ibd0_pp%>%filter(!eid%in%outliers), aes(x = IBD2T1, y = CCL13_0, color = Time_Category)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm") +  # Trends over time
  #geom_hline(data = data, aes(yintercept = mean(Protein1)), linetype = "dashed") +
  labs(x = "Days Relative to Diagnosis")

#plotname <- 'Protein1_CDsOnly_dynamics'
#ggsave(paste0('/Users/guillermotorres/Library/CloudStorage/Dropbox/DX_Projects/UKB/2024/data2use/',plotname,'.pdf'),width=10,height=5)


# boxplot for f_183801_0 by Time_Category - violin with stat summary median 
# i want the labels of the x axis to be ordered as in the data

comp_list <- list(
  c("Pre-diagnosis", "Post-diagnosis"),
  c("Pre-diagnosis", "Control"),
  c("Post-diagnosis", "Control")
)

ggplot(ibd0%>%filter(!eid%in%outliers), aes(x = Time_Category, y = CCL13_0,fill=Time_Category)) +
  geom_violin(alpha=0.5) +
  stat_summary(fun=median,geom='crossbar',show.legend = FALSE)+
  labs(x = "Time Category")+
  theme_minimal()+
    geom_signif(
    comparisons = comp_list,
    map_signif_level = TRUE,   # shows the * symbols automatically
    test = "wilcox.test",      # or "t.test", "anova", etc.
    step_increase = 0.1        # space out the brackets vertically
  )

 # stat_compare_means(
 #   method = "wilcox.test",    # or "t.test"
 #   comparisons = comp_list,   # list of factor pairs to compare
 #   label = "p.signif"         # show asterisks (p.signif) or p-value (p.format)
 # )


plotname <- 'Protein1_CDsOnly_dynamics_violin'
ggsave(paste0('/Users/guillermotorres/Library/CloudStorage/Dropbox/DX_Projects/UKB/2024/data2use/',plotname,'.pdf'),width=10,height=5)



ggplot(ibd0%>%filter(!eid%in%outliers), aes(x = Time_Category, y = CCL13_0)) +
  geom_boxplot() +
  labs(x = "Time Category", y = "Protein1")











##### 2. Overlay controls' mean ± SD as a band  -- not so nice ####

ctrl_mean <- mean(ibd0%>%filter(!eid%in%outliers)%>%filter(Disease == 'Control')%>%.$CCL13_0, na.rm = TRUE)
ctrl_sd   <- sd(ibd0%>%filter(!eid%in%outliers)%>%filter(Disease == 'Control')%>%.$CCL13_0, na.rm = TRUE)


ggplot(ibd0%>%filter(!eid%in%outliers)%>%filter(Disease != 'Control'), aes(x = IBD2T1, y = CCL13_0)) +
  # Case trajectories
  geom_line(aes(group = eid), alpha = 0.3, color = "gray50") +
  
  # Loess curve by Time_Category for the cases
  geom_smooth(aes(color = Time_Category), method = "loess", se = FALSE) +
  # Controls' mean ± SD band as a gray rectangle
  geom_rect(
    data = data.frame(
  xmin = -Inf,
  xmax = 0,  # or pick the minimum day in your cases if you prefer
  ymin = ctrl_mean - ctrl_sd,
  ymax = ctrl_mean + ctrl_sd
),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "gray80", alpha = 0.5,
    inherit.aes = FALSE  # So it doesn't use IBD2T1 from cases_data
  ) +
  
  # Dashed line at the controls' mean
  geom_hline(
    yintercept = ctrl_mean,
    linetype = "dashed",
    color = "black"
  ) +
  
  labs(x = "Days Relative to Diagnosis", y = "CCL13_0") +
  theme_minimal()



ggplot(ibd0%>%filter(!eid%in%outliers)%>%filter(Disease != 'Control'), aes(x = IBD2T1, y = CCL13_0)) +
    # (A) Case trajectories
    geom_line(aes(group = eid), alpha = 0.3, color = "gray50")+
    geom_smooth(aes(color = Time_Category), method = "loess", se = FALSE)+
    geom_hline(
    data = ibd0%>%filter(!eid%in%outliers)%>%filter(Disease == 'Control'),
    aes(yintercept = mean(CCL13_0, na.rm = TRUE)),
    color = "black", linetype = "dashed",
    inherit.aes = FALSE  # so it doesn't use x or color from the main aes
  )+
  # (D) Overlay controls' ± SD as a ribbon
geom_rect(
    data = data.frame(
  xmin = -Inf,
  xmax = 0,  # or pick the minimum day in your cases if you prefer
  ymin = ctrl_mean - ctrl_sd,
  ymax = ctrl_mean + ctrl_sd
),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "gray80", alpha = 0.5,
    inherit.aes = FALSE  # So it doesn't use IBD2T1 from cases_data
  ) 

  geom_ribbon(
    data = ibd0%>%filter(!eid%in%outliers)%>%filter(Disease == 'Control'),
    aes(
      ymin = mean(CCL13_0, na.rm = TRUE) - sd(CCL13_0, na.rm = TRUE),
      ymax = mean(CCL13_0, na.rm = TRUE) + sd(CCL13_0, na.rm = TRUE)
    ),
    fill = "gray80", alpha = 0.5,
    inherit.aes = FALSE  # again, override the main aes
  ) +
labs(x = "Days Relative to Diagnosis", y = "CCL13_0") +
  theme_minimal()

# Plot cases with trajectories
ggplot(ibd0%>%filter(!eid%in%outliers)%>%filter(Disease == 'Control'), aes(x = IBD2T1, y = CCL13_0)) +
  # Case trajectories (pre/post diagnosis)
  geom_line(aes(group = eid), alpha = 0.3, color = "gray50") +
  geom_smooth(aes(color = Time_Category), method = "loess", se = FALSE) +
  # Overlay controls as a horizontal band (mean ± SD)
  geom_hline(
    data = ibd0%>%filter(!eid%in%outliers)%>%filter(Disease == 'Control'),
    aes(yintercept = mean(CCL13_0, na.rm = TRUE)),
    color = "black", linetype = "dashed"
  ) +
  geom_ribbon(
    data = ibd0%>%filter(!eid%in%outliers)%>%filter(Disease == 'Control'),
    aes(ymin = mean(CCL13_0) - sd(CCL13_0), ymax = mean(CCL13_0) + sd(CCL13_0)),
    fill = "gray80", alpha = 0.5
  ) +
  labs(x = "Days Relative to Diagnosis", y = "CCL13_0") +
  theme_minimal()
