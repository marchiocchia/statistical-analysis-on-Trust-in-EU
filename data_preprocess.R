
library(car)

load("~/Google Drive/Tesi/ICPSR_37218/DS0001/37218-0001-Data.rda")

dati<-da37218.0001[,c("SERIALID","COUNTRY","ISOCNTRY","Q1_12","Q1_8","P7IT_R1",
                      "POLINTR","QA8A_1","QA8A_3","QA8A_5","QA8A_12",
                      "QA8A_14","QA17A","D11R1","D8R2","D25","D7R3","D60",
                      "W1","W3","W4","WEX")]

#SERIALID  -->  NUMERO UNICO PROGRESSIVO 
dati$SERIALID<-as.factor(dati$SERIALID)

dati$COUNTRY<-recode(dati$COUNTRY,"'(15) -'=NA;'(42) CH - Switzerland (NOT INCLUDED)'=NA;'(43) IS - Iceland (NOT INCLUDED)'=NA;'(44) LI - Liechtenstein (NOT INCLUDED)'=NA; '(10) GB-NIR Northern Ireland'='(09) GB - Great Britain';'(09) GB-GBN - Great Britain'='(09) GB - Great Britain';'(14) DE-E Germany East'='(14) DE Germany';'(04) DE-W - Germany - West'='(14) DE Germany';'(33) CY-TCC - Cyprus TCC'=NA")

dati$ISOCNTRY<-recode(dati$ISOCNTRY,"'DE-E  '='DE    ';'DE-W  '='DE    ';
                      'GB-GBN'='GB    ';'GB-NIR'='GB    ';'CY-TCC'=NA")

#Q1_12  -->  UNITED KINGDOM
dati$INGHILTERRA<-recode(dati$Q1_12,"'(0) Not mentioned'=0; '(1) Mentioned'=1")
dati<-dati[,-4]

#Q1_8  -->  ITALIA
dati$ITALY<-recode(dati$Q1_8,"'(0) Not mentioned'=0; '(1) Mentioned'=1")
dati<-dati[,-4]

#P7IT_R1  -->  REGION- ITALIA
dati$REGION_IT<-dati$P7IT_R1
dati<-dati[,-4]

#POLINTR-->  POLITICAL INDEX
dati$POL_INDEX<-dati$POLINTR
dati<-dati[,-4]

#QA8A_1-->  TRUST UN ISTITUTION: WRITTEN PRESS --> QUANTO TI FIDI DEI GIORNALI ISTITUZIONALI?
dati$TRUST_GIORNALI<-recode(dati$QA8A_1,"'(1) Tend to trust'=1; '(2) Tend not to trust'=0; '(3) DK'=0")
dati<-dati[,-4]

#QA8A_3-->  TRUST UN ISTITUTION: TELEVISION --> QUANTO TI FIDI DELLE TELEVISIONI ISTITUZIONALI?
dati$TRUST_TV<-recode(dati$QA8A_3,"'(1) Tend to trust'=1; '(2) Tend not to trust'=0; '(3) DK'=0")
dati<-dati[,-4]

#QA8A_5-->  TRUST UN ISTITUTION: ONLINE SOCIAL NETWORK --> QUANTO TI FIDI DEI SOCIAL?
dati$TRUST_SOCIAL<-recode(dati$QA8A_5,"'(1) Tend to trust'=1; '(2) Tend not to trust'=0; '(3) DK'=0")
dati<-dati[,-4]

#QA8A_12-->  TRUST UN ISTITUTION:  NATIONAL GOVERMENT --> QUANTO TI FIDI DEL TUO GOVERNO?
dati$TRUST_GOVERNO<-recode(dati$QA8A_12,"'(1) Tend to trust'=1; '(2) Tend not to trust'=0; '(3) DK'=0")
dati<-dati[,-4]

#QA8A_14-->  TRUST UN ISTITUTION: EUROPEAN UNION --> QUANTO TI FIDI DELL'EUROPA?
#I would like to ask you a question about how much trust you have in certain institutions. For each of the following institutions, please tell me if you tend to trust it or tend not to trust it.

dati$TRUST_EUROPA1<-recode(dati$QA8A_14,"'(1) Tend to trust'='Ho fiducia'; 
                             '(2) Tend not to trust'='Non ho fiducia'; '(3) DK'='Non ho fiducia'")

#TRUST EUROPA RICODIFICATAA
dati$TRUST_EUROPA<-recode(dati$QA8A_14,"'(1) Tend to trust'=1; '(2) Tend not to trust'=0; '(3) DK'=0")
dati<-dati[,-4]
dati$TRUST_EUROPA<-as.numeric(as.character(dati$TRUST_EUROPA))

#QA17A-->  DEMOCRACY SATISFACTION- COUNTRY--> SEI SODDISFATTO DELLA DEMOCRAZIA DEL TUO PAESE??
dati$SODD_DEMOCRAZIA<-recode(dati$QA17A,"'(1) Very satisfied'='1';
                                         '(2) Fairly satisfied'='1';
                                         '(3) Not very satisfied'='0';
                                         '(4) Not at all satisfied'='0'
                                         ;'(5) DK'=NA")
dati<-dati[,-4]
dati$SODD_DEMOCRAZIA<-as.factor(as.character(dati$SODD_DEMOCRAZIA))

#D11R1-->  AGE IN CATEGORIE
dati$CLASSI_ETA<-dati$D11R1
dati<-dati[,-4]

#D8R2-->  ANNI DI ISTRUZIONE IN CLASSI
dati$CLASSI_ISTRUZIONE<-recode(dati$D8R2,"'(1) Up to 15'='LICEO';'(2) 16-19'='LAUREA';'(3) 20+'='MASTER';
                               '(4) Still studying'='STUDENTE';'(5) No full-time education'=NA;
                               '(7) Refusal'=NA;'(8) DK'=NA")
dati<-dati[,-4]

#D25-->   TYPE OF COMMUNITY
dati$COMMUNITY<-dati$D25
dati<-dati[,-4]

#D7R3-->  MARITAL STATUS (RECODED CHILDREN)
dati$FIGLI<-recode(dati$D7R3,"'(1) Single hh without children (9,11,13 in d7)'=0;
                   '(2) Single hh with children (10,12,14 in d7)'=1;
                   '(3) Multiple hh without children (1, 5 in d7)'=0;
                   '(4) Multiple hh with children (2-4, 6-8 in d7)'=1;
                   '(5) Other (SPONT.)'=NA;
                   '(7) Refusal (SPONT.)'=NA")
dati<-dati[,-4]

#D60  -->  DIFFICULTIES PAYING BILLS
dati$DIFFICOLTA_ECONOMICHE<-recode(dati$D60,"'(1) Most of the time'=1; '(2) From time to time'=1;
                                 '(3) Almost never/never'=0; '(7) Refusal (SPONT.)'=NA")
dati<-dati[,-4]

## PESI TOTALI CONSIDERANDO GRAN BRETAGNA E GERMANIA UNITE RISPETTIVAMENTE
dati$Wtot<-ifelse(dati$W3!=0,dati$W3,
                  ifelse(dati$W4!=0, dati$W4,dati$W1))
