library(arm)
library(foreign)
library(gmodels)

##MODELLO LOGISTICO TOTALE (28 STATI MEMBRI)

dati$COUNTRY<-relevel(dati$COUNTRY,"(24) LT - Lithuania")
dati$CLASSI_ETA<-relevel(as.factor(dati$CLASSI_ETA),"(2) 25 - 39 years")
dati$COMMUNITY<-relevel(as.factor(dati$COMMUNITY),"(3) Large town")
dati$CLASSI_ISTRUZIONE<-relevel(as.factor(dati$CLASSI_ISTRUZIONE),"MASTER")

fit_2.1<- glm(TRUST_EUROPA~COUNTRY+COMMUNITY+CLASSI_ETA+TRUST_TV+
                CLASSI_ISTRUZIONE+FIGLI+DIFFICOLTA_ECONOMICHE+
                TRUST_GOVERNO+TRUST_GIORNALI+TRUST_SOCIAL+
                POL_INDEX+SODD_DEMOCRAZIA+TRUST_GOVERNO*COUNTRY
              ,family=binomial(link="logit"), data=dati)
display(fit_2.1)  

round(invlogit(fit_2.1$coefficients[1])*100)

##DIAGNOSTICA DEL MODELLO TOTALE

pred_2.1<- fit_2.1$fitted.values
dati_fit_2.1<-na.omit(dati[,c("TRUST_EUROPA","COMMUNITY","CLASSI_ETA",
                              "CLASSI_ISTRUZIONE","TRUST_TV",
                              "COUNTRY","DIFFICOLTA_ECONOMICHE",
                              "TRUST_GOVERNO","TRUST_GIORNALI","TRUST_SOCIAL",
                              "SODD_DEMOCRAZIA","FIGLI","POL_INDEX","WEX","TRUST_EUROPA1")])
y_2.1<- dati_fit_2.1$TRUST_EUROPA
binnedplot (pred_2.1, y_2.1-pred_2.1, nclass=145, xlab="Prob (avere fiducia) stimata", main=NA,
            ylab="Residui medi", mgp=c(2,.5,0),pch=16, col.pts = 'gray11',cex.axis=0.9, cex.lab=0.9)

##tasso di errore
error.rate.null <- round(mean(round(abs(y_2.1-mean(pred_2.1))))*100)
tax.error <- round(mean((pred_2.1 > 0.5 & y_2.1==0) | (pred_2.1 < 0.5 & y_2.1==1))*100)

#ANALISI DESCRITTIVA E MODELLO SUL SOTTOCAMPIONE RELATIVO AL REGNO UNITO

inghilterra<-subset(dati, ISOCNTRY=="GB    ")
round(prop.table(wtd.table(inghilterra$TRUST_EUROPA1,weights = inghilterra$WEX))*100,2)

##########################################################################################################'
######################## GRAFICO A BARRE (COMMUNITY) RISPETTO A TRUST EUROPA (REGNO UNITO) ###############'
##########################################################################################################'

freq.trust.eu_COMM_rural <- as.vector(round(prop.table(wtd.table(inghilterra$COMMUNITY_GB,
                                                                 inghilterra$TRUST_EUROPA1, 
                                                                 weights = inghilterra$WEX), 
                                                       margin = 2)*100,2))[c(1,4)] 
freq.trust.eu_COMM_small <- as.vector(round(prop.table(wtd.table(inghilterra$COMMUNITY_GB,
                                                                 inghilterra$TRUST_EUROPA1,
                                                                 weights = inghilterra$WEX), 
                                                       margin = 2)*100,2))[c(2,5)]
freq.trust.eu_COMM_big <- as.vector(round(prop.table(wtd.table(inghilterra$COMMUNITY_GB,
                                                               inghilterra$TRUST_EUROPA1,
                                                               weights = inghilterra$WEX), 
                                                     margin = 2)*100,2))[c(3,6)]

df1j_COMM <- data.frame("Rurale" = freq.trust.eu_COMM_rural, 
                        "Piccola città" = freq.trust.eu_COMM_small,
                        "Grande città" = freq.trust.eu_COMM_big, 
                        "livelli" = levels(inghilterra$TRUST_EUROPA1)) 
df2j_COMM <- melt(df1j_COMM, id.vars='livelli') 
df2j_COMM <- df2j_COMM[order(df2j_COMM$variable, df2j_COMM$value, df2j_COMM$livelli),] 

plotCOMM<-ggplot(df2j_COMM, aes(x=livelli, y=value, fill=variable)) + 
  geom_bar(stat='identity', position= 'dodge',colour="black",width=0.8) + 
  labs(fill = "Tipologia di zona:") + xlab("Fiducia UE") + ylab("Percentuale") + 
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  geom_text(aes(label=paste(value, "%"), y=value+2.5), 
            position = position_dodge(0.9), vjust=0.5, size=3.7, fontface='bold')+
  My_Theme+
  scale_fill_manual(values=c("#B3E2CD", "#FDCDAC", "#CBD5E8"))

##########################################################################################################'
################## GRAFICO A BARRE (SODD_DEMOCRAZIA) RISPETTO A TRUST EUROPA (REGNO UNITO) ###############'
##########################################################################################################'

freq.trust.eu_DEMOCRAZ_no <- as.vector(round(prop.table(wtd.table(inghilterra$SODD_DEMOCRAZIA,
                                                                  inghilterra$TRUST_EUROPA1,
                                                                  weights = inghilterra$WEX), 
                                                        margin = 2)*100,2))[c(1,3)] 
freq.trust.eu_DEMOCRAZ_si <- as.vector(round(prop.table(wtd.table(inghilterra$SODD_DEMOCRAZIA,
                                                                  inghilterra$TRUST_EUROPA1,
                                                                  weights = inghilterra$WEX), 
                                                        margin = 2)*100,2))[c(2,4)]

df1j_DEMOCRAZ <- data.frame("SI" = freq.trust.eu_DEMOCRAZ_si, 
                            "NO" = freq.trust.eu_DEMOCRAZ_no, 
                            "livelli" = levels(inghilterra$TRUST_EUROPA1)) 
df2j_DEMOCRAZ <- melt(df1j_DEMOCRAZ, id.vars='livelli') 
df2j_DEMOCRAZ <- df2j_DEMOCRAZ[order(df2j_DEMOCRAZ$variable, df2j_DEMOCRAZ$value, df2j_DEMOCRAZ$livelli),] 
df2j_DEMOCRAZ$livelli <- factor(df2j_DEMOCRAZ$livelli, 
                                levels = df2j_DEMOCRAZ$livelli[df2j_DEMOCRAZ$variable == "NO"])

plotDEMOCRAZ<-ggplot(df2j_DEMOCRAZ, aes(x=livelli, y=value, fill=variable)) + 
  geom_bar(stat='identity', position= 'dodge',colour="black",width=0.8) + 
  labs(fill = "Hai fiducia nella democrazia:") + xlab("Fiducia UE") + ylab("Percentuale") + 
  theme(axis.text.x = element_text(angle=0, vjust=0.6))+
  geom_text(aes(label=paste(value, "%"), y=value+2.5), 
            position = position_dodge(0.9), vjust=0.5, size=3.7, fontface='bold')+
  My_Theme+
  ylim(0, 80)+
  scale_fill_manual(values=c("#BDD7E7","#08519C"))

round(prop.table(table(inghilterra$FIGLI))*100,2)
round(prop.table(table(inghilterra$DIFFICOLTA_ECONOMICHE))*100,2)

##MODELLO LOGISTICO REGNO UNITO 

inghilterra$CLASSI_ETA<-relevel(as.factor(inghilterra$CLASSI_ETA),"(2) 25 - 39 years")
inghilterra$CLASSI_ISTRUZIONE<-relevel(as.factor(inghilterra$CLASSI_ISTRUZIONE),"MASTER")

fitGB<- glm(TRUST_EUROPA~CLASSI_ETA+TRUST_TV+TRUST_GIORNALI+
              TRUST_GOVERNO+TRUST_SOCIAL+POL_INDEX+ CLASSI_ISTRUZIONE
            ,family=binomial(link="logit"), data=inghilterra)
display(fitGB)  

#DIAGNOSTICA INGHILTERRA
predGB<- fitGB$fitted.values
dati_fitGB<-na.omit(inghilterra [,c("TRUST_EUROPA","CLASSI_ETA","POL_INDEX","TRUST_GIORNALI",
                                    "TRUST_TV","TRUST_GOVERNO","TRUST_SOCIAL","CLASSI_ISTRUZIONE")])
yGB<- dati_fitGB$TRUST_EUROPA
binnedplot (predGB, yGB-predGB, nclass=30, xlab="Prob (avere fiducia) stimata", 
            ylab="Residui medi", main=NA, mgp=c(2,.5,0),cex.axis=0.9, cex.lab=0.9)

##tasso di errore
error.rate.null.GB <- mean(round(abs(yGB-mean(predGB))))
tax.error.GB <- mean((predGB > 0.5 & yGB==0) | (predGB < 0.5 & yGB==1))


#ANALISI DESCRITTIVA E MODELLO SUL SOTTOCAMPIONE RELATIVO AL REGNO UNITO

italia<-subset(dati, ISOCNTRY=="IT    ")

round(prop.table(wtd.table(italia$TRUST_EUROPA1,weights = italia$WEX))*100,2)

##########################################################################################################'
########################## GRAFICO A BARRE (FIGLI) RISPETTO A TRUST EUROPA (ITALIA) ######################'
##########################################################################################################'
freq.trust.eu_FIGLI_si <- as.vector(round(prop.table(wtd.table(italia$FIGLI,
                                                               italia$TRUST_EUROPA1, 
                                                               weights = italia$WEX), 
                                                     margin = 2)*100,2))[c(2,4)] 
freq.trust.eu_FIGLI_no <- as.vector(round(prop.table(wtd.table(italia$FIGLI,
                                                               italia$TRUST_EUROPA1,
                                                               weights = italia$WEX), 
                                                     margin = 2)*100,2))[c(1,3)]

df1j_FIGLI <- data.frame("SI" = freq.trust.eu_FIGLI_si, 
                         "NO" = freq.trust.eu_FIGLI_no, 
                         "livelli" = levels(italia$TRUST_EUROPA1)) 
df2j_FIGLI <- melt(df1j_FIGLI, id.vars='livelli') 
df2j_FIGLI <- df2j_FIGLI[order(df2j_FIGLI$variable, df2j_FIGLI$value, df2j_FIGLI$livelli),] 

plotFIGLI<-ggplot(df2j_FIGLI, aes(x=livelli, y=value, fill=variable)) + 
  geom_bar(stat='identity', position= 'dodge',colour="black",width=0.8) + 
  labs(fill = "Avere figli:") + xlab("Fiducia UE") + ylab("Percentuale") + 
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  geom_text(aes(label=paste(value, "%"), y=value+2.5), 
            position = position_dodge(0.9), vjust=0.5, size=3.7, fontface='bold')+
  My_Theme+
  scale_fill_manual(values=c("#D7B5D8","#980043"))



round(prop.table(wtd.table(italia$DIFFICOLTA_ECONOMICHE,
                           italia$TRUST_EUROPA1, weights = italia$WEX), 
                 margin = 1)*100,2)

round(prop.table(wtd.table(italia$DIFFICOLTA_ECONOMICHE,
                           italia$REGION_IT, weights = italia$WEX), 
                 margin = 2)*100,2)

round(prop.table(wtd.table(italia$CLASSI_ISTRUZIONE,
                           weights = italia$WEX))*100,2)

##MODELLO LOGISTICO ITALIA

italia$CLASSI_ETA<-relevel(as.factor(italia$CLASSI_ETA),"(2) 25 - 39 years")
italia$REGION_IT<-relevel(as.factor(italia$REGION_IT),"(1) Nord-Ovest")

fit_IT<- glm(TRUST_EUROPA~REGION_IT+CLASSI_ETA+TRUST_GOVERNO+TRUST_GIORNALI+TRUST_SOCIAL+TRUST_TV+
               SODD_DEMOCRAZIA+POL_INDEX+DIFFICOLTA_ECONOMICHE
             ,family=binomial(link="logit"),data=italia)
display(fit_IT)  


#DIAGNOSTICA ITALIA

pred_IT<- fit_IT$fitted.values
dati_fit_IT<-na.omit(italia[,c("TRUST_EUROPA","REGION_IT","TRUST_GOVERNO","TRUST_GIORNALI",
                               "TRUST_SOCIAL","CLASSI_ETA", "SODD_DEMOCRAZIA","TRUST_TV",
                               "POL_INDEX","DIFFICOLTA_ECONOMICHE")])
y_IT<- dati_fit_IT$TRUST_EUROPA
binnedplot (pred_IT, y_IT-pred_IT, nclass=25, xlab="Prob (avere fiducia ) stimata", 
            ylab="Residui medi", main=NA, mgp=c(2,.5,0), cex.axis=0.9, cex.lab=0.9)

##tasso di errore
error.rate.null.IT <- round(mean(round(abs(y_IT-mean(pred_IT))))*100,2)
error.rate.null.IT
tax.error.IT <- round(mean((pred_IT > 0.5 & y_IT==0) | (pred_IT < 0.5 & y_IT==1))*100,2)
tax.error.IT

