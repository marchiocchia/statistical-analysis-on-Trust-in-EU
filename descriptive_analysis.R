library(ggplot2)
library(RColorBrewer)
library(rworldmap)
library(dplyr)
library(MazamaSpatialUtils)
library(forcats)
library(plotly)
library(questionr)
library(car)
library(gridExtra)


#ANALISI DESCRITTIVA E GRAFICI

round(prop.table(table(dati$TRUST_EUROPA))*100,2) #percentuali campione
round(weighted.mean(dati$TRUST_EUROPA,dati$WEX,na.rm=T)*100,2)  ## percentuali YES popolazione
round(weighted.mean(1-dati$TRUST_EUROPA,dati$WEX,na.rm=T)*100,2)  ## percentuali NO popolazione


#Si definisce un insieme ristretto di variabili su cui svolgere l'analisi descrtittiva, in modo da ottenere risultati meno distroti.

dati_fit_2.1<-na.omit(dati[,c("TRUST_EUROPA","COMMUNITY","CLASSI_ETA",
                              "CLASSI_ISTRUZIONE","TRUST_TV",
                              "COUNTRY","DIFFICOLTA_ECONOMICHE",
                              "TRUST_GOVERNO","TRUST_GIORNALI","TRUST_SOCIAL",
                              "SODD_DEMOCRAZIA","FIGLI","POL_INDEX","WEX","TRUST_EUROPA1")])

round(prop.table(wtd.table(dati_fit_2.1$TRUST_EUROPA, weights = dati_fit_2.1$WEX))*100,2) 
##########################################################################################################
######################### GRAFICO TRUST EUROPA E ISOCOUNTRY ORDINATI #####################################
##########################################################################################################'
#Vettori frequenze di risposta
freq.trust.eu_no <- as.vector(round(prop.table(wtd.table(dati$TRUST_EUROPA,dati$ISOCNTRY,weights = dati$WEX), margin = 2)*100,2))[seq(1,66,2)] 
freq.trust.eu_yes <- as.vector(round(prop.table(wtd.table(dati$TRUST_EUROPA,dati$ISOCNTRY,weights = dati$WEX), margin = 2)*100,2))[seq(2,66,2)]

df1j <- data.frame("yes" = freq.trust.eu_yes, "no" = freq.trust.eu_no, "paesi" = levels(dati$ISOCNTRY)) 
df2j <- melt(df1j, id.vars='paesi') 
df2j <- df2j[order(df2j$variable, df2j$value, df2j$paesi),] 
df2j$paesi <- factor(df2j$paesi, levels = df2j$paesi[df2j$variable == "yes"])

ggplot(df2j, aes(x=paesi, y=value, fill=variable)) + 
  geom_bar(stat='identity', position= 'dodge',colour="black",width=0.8) + 
  labs(fill = "Risposte:") + xlab("Nazione") + ylab("Percentuale") + 
  theme(axis.text.x = element_text(angle=35, vjust=0.6)) + 
  ggtitle("Quanta fiducia hai nell'Unione Europea?") +
  scale_fill_manual(values=c("#00441B","#41AB5D"))

# Si crea un dataframe con la longitudine e la latitudine dei Paesi considerati nell'indagine. Tale dataset viene utilizzato nel grafico con la mappa.

lat<-c(39.250,40.2459,48.5112,42.2507,49.4740,53.4839,52.0526,50.5101,59.2741,38.2466,45.7983,62.8923,
       47.06,52.3952,58.3638,55.4233,51.45,50.0516,46.0303,48.7139,39.1031,35.5120,47.2952,
       56.2956,49.3642,54.5359,44.0710,42.4327,35.1031,44.4814,41.3451,42.2627,41.1939)
long<-c(-8.017,-3.4209,2.2055,12.0634,9.5546,-2.2446,5.0719,4.2055,15.2066,21.7345,24.1255,27.677,
        15.6833,-8.3723,25.59,9.3208,19.28,14.2514,14.3018,21.258,33.2151,14.2611,19.0223,
        25.5126,6.0747,23.5359,15.1431,25.6419,33.2151,20.2754,21.5550,19.1548,19.4908)
name<-c("PT","ES","FR","IT","DE","GB","NL","BE","SE","GR","RO","FI",
        "AT","IE","EE","DK","PL","CZ","SI","SK","TR","MT","HU",
        "LV","LU","LT","HR","BG","CY","RS","MK","ME","AL")

plotcities<-data.frame(lat,long,name)
save(plotcities, file="plotcities.RData")

##########################################################################################################'
########################## GRAFICO TRUST EUROPA SULLA MAPPA DELL'EUROPA ##################################'
##########################################################################################################'

freq.trust.eu_yes <- as.vector(round(prop.table(wtd.table(dati$TRUST_EUROPA,dati$ISOCNTRY,weights = dati$WEX), margin = 2)*100,2))[seq(2,66,2)]
aggregatoPaese<-data.frame("Group.1"=levels(dati$ISOCNTRY),"trust"=freq.trust.eu_yes)

iso_3 <- c()
for (i in aggregatoPaese$Group.1){
  auc <- str_trim(i, "right")  
  print(auc)
  iso_3 <- c(iso_3, iso2ToIso3(auc))
}
aggregatoPaese$iso3 <- iso_3

malMap <- joinCountryData2Map(aggregatoPaese, joinCode = "ISO3",
                              nameJoinColumn = "iso3")
quartz()
mapParams <-mapCountryData(malMap, nameColumnToPlot="trust", catMethod = "quantiles",
                           missingCountryCol = grey(.8), mapRegion = 'Europe',
                           colourPalette=c("#C7E9C0", "#A1D99B", "#74C476" ,"#41AB5D",
                                           "#238B45" ,"#006D2C", "#00441B"),
                           oceanCol="#C6DBEF",  borderCol="black", lwd=0.8, addLegend=FALSE,
                           mapTitle="Quanta fiducia hai nell'Unione Europea? (Percentuale %)")
do.call(addMapLegend, c( mapParams, legendLabels="all", legendWidth=0.5, legendIntervals="data", 
                         legendMar = 2))
points(plotcities$long, plotcities$lat, pch =  16, col = "red")
points(plotcities$long, plotcities$lat, pch =  1, col = "black")
text(plotcities$long, plotcities$lat, plotcities$name, pos = 3,cex=0.6)


##########################################################################################################'
############################################ GRAFICO ENTRATA UE ##########################################'
##########################################################################################################'

aggregatoPaese1<-aggregate(dati, list(dati$ISOCNTRY), mean, na.rm=T)

aggregatoPaese1$ENTRATA<-recode(aggregatoPaese1$Group.1,
                                "'AT    '='1995';
                      'BE    '='1952';
                      'FI    '='1995';
                      'FR    '='1952';
                      'DE    '='1952';
                      'IE    '='1973';
                      'IT    '='1952';
                      'LU    '='1952';
                      'NL    '='1952';
                      'PT    '='1986';
                      'ES    '='1986';
                      'GR    '='1981';
                      'SI    '='2004';
                      'CY    '='2004';
                      'MT    '='2004';
                      'SK    '='2004';
                      'EE    '='2004';
                      'LV    '='2004';
                      'LT    '='2004';
                      'AL    '='No EU';
                      'BG    '='2007';
                      'DK    '='1973';
                      'GB    '='1973';
                      'HR    '='2013';
                      'HU    '='2004';
                      'ME    '='No EU';
                      'MK    '='No EU';
                      'PL    '='2004';
                      'RO    '='2007';
                      'RS    '='No EU';
                      'SE    '='1995';
                      'CZ    '='2004';
                      'TR    '='No EU'")
aggregatoPaese1 <- aggregatoPaese1[, c("Group.1", "ENTRATA","Wtot")]
iso_3 <- c()
for (i in aggregatoPaese1$Group.1){
  auc <- str_trim(i, "right")  
  print(auc)
  iso_3 <- c(iso_3, iso2ToIso3(auc))
}
aggregatoPaese1$iso3 <- iso_3
malMap <- joinCountryData2Map(aggregatoPaese1, joinCode = "ISO3",
                              nameJoinColumn = "iso3")
mapParams <-mapCountryData(malMap, nameColumnToPlot="ENTRATA", catMethod = "categorical",
                           missingCountryCol = grey(.8), mapRegion = 'Europe',
                           colourPalette=c("salmon", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
                                           "#FFFF33", "#A65628", "#F781BF", grey(.8)),
                           oceanCol="#C6DBEF",  borderCol="black", lwd=0.8, addLegend=T,
                           mapTitle="Composizione dell'Unione Europea", )
points(plotcities$long, plotcities$lat, pch =c('€','€','€','€','€',"°",'€','€',"°",'€',"°",'€','€','€','€',"°","°","°",'€','€','€',"°",'€','€','€',"°","°",'€'), col = 'black')
text(plotcities$long, plotcities$lat, plotcities$name, pos = 3,cex=0.6)

#Grafici sulla variabile ooutcome rispetto alle variabili di interesse

My_Theme = theme(
  axis.title.x = element_text(size = 14),
  axis.text.x = element_text(size = 16),
  axis.text.y = element_text(size = 13),
  axis.title.y = element_text(size = 14),
  legend.text = element_text(size = 12),
  legend.title =element_text(size = 12) )


##########################################################################################################'
############################ GRAFICO A BARRE (TRUST_GOVERNO) RISPETTO A TRUST EUROPA #####################'
##########################################################################################################'

Tab_GOV<-round(prop.table(wtd.table(dati_fit_2.1$TRUST_GOVERNO,dati_fit_2.1$TRUST_EUROPA1, 
                                    weights = dati_fit_2.1$WEX),  margin = 2)*100,2)


freq.trust.eu_GOV_no <- as.vector(round(prop.table(wtd.table(dati_fit_2.1$TRUST_GOVERNO,
                                                             dati_fit_2.1$TRUST_EUROPA1, 
                                                             weights = dati_fit_2.1$WEX), 
                                                   margin = 2)*100,2))[c(1,3)] 
freq.trust.eu_GOV_si <- as.vector(round(prop.table(wtd.table(dati_fit_2.1$TRUST_GOVERNO,
                                                             dati_fit_2.1$TRUST_EUROPA1,
                                                             weights = dati_fit_2.1$WEX), 
                                                   margin = 2)*100,2))[c(2,4)]
df1j_GOV <- data.frame("SI" = freq.trust.eu_GOV_si, 
                       "NO" = freq.trust.eu_GOV_no, 
                       "livelli" = levels(dati_fit_2.1$TRUST_EUROPA1)) 
df2j_GOV <- melt(df1j_GOV, id.vars='livelli') 
df2j_GOV <- df2j_GOV[order(df2j_GOV$variable, df2j_GOV$value, df2j_GOV$livelli),] 
df2j_GOV$livelli <- factor(df2j_GOV$livelli, levels = df2j_GOV$livelli[df2j_GOV$variable == "NO"])


plotGOV<-ggplot(df2j_GOV, aes(x=livelli, y=value, fill=variable)) + 
  geom_bar(stat='identity', position= 'dodge',colour="black",width=0.8) + 
  labs(fill = "Hai fiducia\nnel tuo governo:") + xlab("Fiducia UE") + ylab("Percentuale") + 
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  geom_text(aes(label=paste(value, "%"), y=value+2.5), 
            position = position_dodge(0.9), vjust=0.5, size=3.7, fontface='bold')+
  My_Theme+
  scale_fill_manual(values=c("#FDBE85","#A63603"))

##########################################################################################################'
########################## GRAFICO A BARRE (SODD_DEMOCRAZIA) RISPETTO A TRUST EUROPA #####################'
##########################################################################################################'

Tab_DEMO<-round(prop.table(wtd.table(dati_fit_2.1$SODD_DEMOCRAZIA,dati_fit_2.1$TRUST_EUROPA1, 
                                     weights = dati_fit_2.1$WEX),  margin = 2)*100,2)

freq.trust.eu_DEMO_no <- as.vector(round(prop.table(wtd.table(dati_fit_2.1$SODD_DEMOCRAZIA,
                                                              dati_fit_2.1$TRUST_EUROPA1, 
                                                              weights = dati_fit_2.1$WEX), 
                                                    margin = 2)*100,2))[c(1,3)] 
freq.trust.eu_DEMO_si <- as.vector(round(prop.table(wtd.table(dati_fit_2.1$SODD_DEMOCRAZIA,
                                                              dati_fit_2.1$TRUST_EUROPA1,
                                                              weights = dati_fit_2.1$WEX), 
                                                    margin = 2)*100,2))[c(2,4)]

df1j_DEMO <- data.frame("SI" = freq.trust.eu_DEMO_si, 
                        "NO" = freq.trust.eu_DEMO_no, 
                        "livelli" = levels(dati_fit_2.1$TRUST_EUROPA1)) 
df2j_DEMO <- melt(df1j_DEMO, id.vars='livelli') 
df2j_DEMO <- df2j_DEMO[order(df2j_DEMO$variable, df2j_DEMO$value, df2j_DEMO$livelli),] 
df2j_DEMO$livelli <- factor(df2j_DEMO$livelli, levels = df2j_DEMO$livelli[df2j_DEMO$variable == "NO"])

plotDEMO<-ggplot(df2j_DEMO, aes(x=livelli, y=value, fill=variable)) + 
  geom_bar(stat='identity', position= 'dodge',colour="black",width=0.8) + 
  labs(fill = "Sei soddisfatto\ndella democrazia:") + xlab("Fiducia UE") + ylab("Percentuale") + 
  theme(axis.text.x = element_text(angle=0, vjust=0.6))+
  geom_text(aes(label=paste(value, "%"), y=value+2.5), 
            position = position_dodge(0.9), vjust=0.5, size=3.7, fontface='bold')+
  My_Theme+
  ylim(0, 85)+
  scale_fill_manual(values=c("#BDD7E7","#08519C"))



#Tabelle confronto tra variabile autocome e mezzi di comunicazione

Tab_GIORNALI<-round(prop.table(wtd.table(dati_fit_2.1$TRUST_GIORNALI,dati_fit_2.1$TRUST_EUROPA1,
                                         weights = dati_fit_2.1$WEX),  margin = 2)*100,2)
Tab_TV<-round(prop.table(wtd.table(dati_fit_2.1$TRUST_TV,dati_fit_2.1$TRUST_EUROPA1,
                                   weights = dati_fit_2.1$WEX),  margin = 2)*100,2)
Tab_SOCIAL<-round(prop.table(wtd.table(dati_fit_2.1$TRUST_SOCIAL,dati_fit_2.1$TRUST_EUROPA1,
                                       weights = dati_fit_2.1$WEX),  margin = 2)*100,2)
Tab_POL<-round(prop.table(wtd.table(dati_fit_2.1$POL_INDEX,dati_fit_2.1$TRUST_EUROPA1,
                                    weights = dati_fit_2.1$WEX),  margin = 2)*100,2)
