
##################################################################################
### ANALISI 8: Logit sulla probabilità di essere promossi 2018-2019 su 5 dipartimenti
### FILE CSV: "logit-a1"

pro<-file.choose()
logit<-read.csv(pro, head=T,sep=";",dec=",",na.strings="") 
options(max.print = .Machine$integer.max) 
logit
str(logit)
summary(logit)

table(logit$Genere,logit$variazione1)


logit$Genere<-as.factor(logit$Genere)
logit$Grado.di.urbanizzazione<-as.factor(logit$Grado.di.urbanizzazione)
logit$Dipartimento<-as.factor(logit$Dipartimento)
logit$ID<-as.character(logit$ID)

logit$variazione<-(logit$media.p19-logit$media.p18)
logit$variazione1[logit$variazione<0]= 0			# 0=peggiorato, 1=migliorato
logit$variazione1[logit$variazione>=0]= 1			# var dicotomica
logit$variazione1<-as.factor(logit$variazione1)
summary(logit[,c(1,2,4,5,8,9,12,13,15,17)])
#Dalla summary si registra un miglioramento delle performance dei due anni accademici in analisi,
#data la presenza in maggioranza del livello 1 nel fattore variazione1, variabile che misura
#le differenze tra le medie ponderate del 2018-2019 con le medie ponderate del 2019-2020 di ogni singolo studente-


barplot(table(logit$variazione1), xlab="VARIAZIONE", col="blue", ylab="Frequenza")
hist(logit$et?, main="", xlab="ETA'")

par(mfrow=c(2,2))
barplot(table(logit$Genere),xlab="GENERE", ylab="Frequency")
barplot(table(logit$Grado.di.urbanizzazione), xlab="TIPOLOGIA COMUNE", ylab="Frequency")
barplot(table(logit$Dipartimento), xlab="DIPARTIMENTO", ylab="Frequency")

boxplot(logit$et?~logit$variazione1, horizontal=T, xlab="wt", ylab="")

par(mfrow=c(1,2))
spineplot(logit$Genere~logit$variazione1, xlab="VARIAZIONE1", ylab="GENERE") # spineplot
spineplot(logit$variazione1~logit$Dipartimento, xlab="DIPARTIMENTO", ylab="VARIAZIONE") # spineplot
table(logit$Genere,logit$variazione1)
#dai grafici non si registra una variazione diversificata per genere o dipartimento di provenienza.

t.test(logit$media.p18,logit$media.p19, paired=T)
# dal test t rifiuto l'ipotesi nulla H0: d=0, essendo il p-value inferiore a 0.05
# Rifiutando H0, si evidenzia un'effettiva differenza tra le performance universitarie nel periodo in questione
# registrando un miglioramento sulla media delle medie ponderate degli studenti pari a circa 0.4577

t.test(logit$CFU.18,logit$CFU.19, paired=T)
# dal test t rifiuto l'ipotesi nulla H0: d=0, essendo il p-value inferiore a 0.05
# Rifiutando H0, si evince come gli studenti abbiano preferito e sostenuto, superando, esami pi? leggeri dal punto di vista dei CFU
# ma ottenendo risultati migliori in considerazione dell'aumento delle medie ponderate. Ci? vuoldire che anche il numero di esami sostenuti
# risulta essere maggiore nel periodo post-lockdown, nella sessione primaverile-estiva


#LOGIT

glm.proa<-glm(variazione1 ~ ISEE, data=logit, family=binomial)
summary(glm.proa)
# non si registra alcuna influenza, la variabile ISEE non risulta significativa.
summary(glm.proa)$coef

###CONFRONTo LE DEVIANCE SE L'INCLUSIONE DELLA VAR ISEE PRODUCE SENSIBILIT? NELLA DEVIANZA NULLA
round(glm.proa$coefficients,3)
round(confint(glm.proa), 3) #coeff equivalenti a zero

glm.prob<-glm(variazione1 ~ Genere +ISEE, data=logit, family=binomial)
summary(glm.prob)
# leggera influenza positiva della variabile genere
###la RESIDUL DEVIANCE ? PI? PICCOLA, L'AIC ? DIMINUITO
summary(glm.prob)$coef
round(glm.prob$coefficients,3)
round(confint(glm.prob), 3)

glm.proc<-glm(variazione1 ~  Genere +ISEE+et?, data=logit, family=binomial)
summary(glm.proc)
# la variabile età risulta essere molto significativa, influenza positivamente la variazione delle medie
###la RESIDUL DEVIANCE ? PI? PICCOLA, L'AIC ? DIMINUITO
summary(glm.proc)$coef
round(glm.proc$coefficients,3)
round(confint(glm.proc), 3)

glm.prod<-glm(variazione1 ~ Genere +ISEE+et?+Dipartimento, data=logit, family=binomial)
summary(glm.prod)
summary(glm.prod)$coef
round(glm.prod$coefficients,3)
round(confint(glm.prod), 3)

glm.proe<-glm(variazione1 ~Genere +ISEE+et?+Dipartimento+ Grado.di.urbanizzazione, data=logit, family=binomial)
summary(glm.proe)
summary(glm.proe)$coef
round(glm.proe$coefficients,3)
round(confint(glm.proe), 3)

#nei restanti due modelli, le variabili implementate non si registrano come variabili significative nella variazione positiva della variabile dicotomica
### OVVIAMENTE LA RESIDUAL DEVIANCE E L'AIC RISULTANO MAGGIORI
### quindi ? preferibile l'utilizzo dei modelli precedenti
pairs(logit$variazione,logit$Genere)


anova(glm.proa, glm.prob, test="Chisq")     ###IL secondo modello ? leggermente migliore del primo
anova(glm.prob, glm.proc, test="Chisq")	  ###IL terzo modello ? migliore del secondo
anova(glm.proc, glm.prod, test="Chisq")     ###IL quarto modello non ? migliore del terzo
anova(glm.prod, glm.proe, test="Chisq")     ###IL quinto modello non ? migliore del quarto
anova(glm.prob, glm.proc, test="Chisq")     ###IL modello migliore ? il terzo (genere, ISEE ed et?)

glm1<-step(glm.proe,~.^2)
summary(glm1)


# MODELLO LOGIT CON LE UNICHE VARIABILI SIGNIFICATIVE
glm.procd<-glm(variazione1 ~ + Genere +et?, data=logit, family=binomial)
summary(glm.procd)
summary(glm.procd)$coef
round(glm.procd$coefficients,3)
round(confint(glm.procd), 3)  # coeff. diversi da 0

res.proc<- residuals(glm.proc, type = "deviance")
res.proc

qqnorm(res.proc,xlim=max(abs(res.proc)) * c(-1,1),
 ylim=max(abs(res.proc)) * c(-1,1),pch=16, main="")
abline(0,1)



#####
#####Regressione logistica - grafici predicted#####
#####


par(mfrow=c(1,2))
prob <- predict(glm.procd, type = "response")  

plot(logit$et?, prob1, ylim=c(0,1),xlab="Et?",pch=16, 
     ylab=expression(paste(pi, "")),           
     main="M1 - previsioni sulla scala della risposta")
abline(h=0.5,lty=2)                                       ####lty=2 linea tratteggiata
####I PUNTI AL DI SOPRA DELLA RETTA 0.5 HANNO PROBABILIT? ALTA di subire un miglioramento nelle performance univ.
#####PER VALORI ELEVATI DI et? LA PROBABILIT? ? MINORE
 
plot(logit$Genere, prob1, ylim=c(0,1),xlab="Et?",pch=16, 
     ylab=expression(paste(pi, "")),            
     main="M1 - previsioni sulla scala della risposta")
abline(h=0.5,lty=2)                                     
####I PUNTI AL DI SOPRA DELLA RETTA 0.5 HANNO PROBABILIT? ALTA DI SUBIRE UN MIGLIORAMENTO NELLE PERFORMANCE UNIV.
##### SI VERIFICA UNA LEGGERA PROB MAGGIORE PER IL GENERE FEMMINILE RISPETTO AL MASCHILE
##### DI SUBIRE IL MIGLIORAMENTO 


###COSTRUISCO UNA TAB A DOPPIA ENTRATA METTENDO SULLE RIGHE LA PROB ? >0.5
table(ordered(prob>0.5,levels=c("TRUE","FALSE")),ordered(logit$variazione1,levels=c("1","0")))
###IL MODELLO NON D? PREVISIONI NMOLTO AFFIDABILI IN QUANTO 252 CHE HANNO SUBITO UN MIGLIORAMENTO NON AVREBBERO DOVUTO SUBIRILO


