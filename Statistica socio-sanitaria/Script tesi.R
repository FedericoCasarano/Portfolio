### TESI ###    

db=file.choose()      
options(max.print = .Machine$integer.max)           #max righe

database=read.table(db, header=T, sep=";",dec=",",na.strings="")
database1=database[-9219,]
attach(database1)
database1
colnames(database1)<-c("id","2017","2018","2019","TotaleCFU")
str(database1)
summary(database1)



database2<-database1[,c(1,3,4)]   # Immatricolati 2017-2018, CFU ottenuti 2018-2020
database2
names(database2)<-c("Id","X","Y")		#X=2018-2019, Y=2019-2020
summary(database2$X)
summary(database2$Y)
sd(database2$X)
sd(database2$Y)

diff=database2$X-database2$Y
diff
summary(diff)
sd(diff)
boxplot(diff)
qqnorm(diff)
qqline(diff)

diff1<-sample(diff, 5000)
shapiro.test(diff1)              #andamento non normale

t.test(database2$X, database2$Y, paired=TRUE,mu=0) # test t di Student per dati appaiati (test parametrico)
# differenza in media significativa, pari a circa -7 punti in media CFU non ottenuti nel periodo considerato (2020)


wilcox.test(database2$X, database2$Y, paired=TRUE, correct=FALSE) # test di Wilcoxon per dati appaiati (test non parametrico
cor.test(database2$X,database2$Y)


###################################################
### Dipartimenti ###

db1<-file.choose()
dipartimenti<-read.csv(db1, head=T, sep=";",dec=",",na.strings="Null")

colnames(dipartimenti)<-c("Dipartimenti","AA17","AA18","AA19","Totale CFU")
dipartimenti
dipartimenti1<-dipartimenti[-21,c(1,2,3,4,5)]
dipartimenti1


dipartimenti19_20<-dipartimenti1[,c(1,3,4)]
Va1=dipartimenti19_20[,2]
Va2=dipartimenti19_20[,3]
t.test(Va1, Va2, paired=T)        # diff in media

summary(Va1)
summary(Va2)
sd(Va1)
sd(Va2)

diff1=Va1-Va2
diff1						# dalle diff tutte positive si nota un calo in tutti i dipartimenti rispetto ai CFU ottenuti nell'anno2020 

summary(diff1)
sd(diff1)
boxplot(diff1)
qqnorm(diff1)
qqline(diff1)
shapiro.test(diff1)			

dipartimenti
tab1<-dipartimenti1[-21,c(2)]
z1<-tab1/723625.06
pie(z1)

tab2<-dipartimenti1[-21,c(3)]
z2<-tab2/447091.5
z2
pie(z2)

par(mfrow=c(2,2))
pie(z1)
pie(z2)

varperc=z2-z1
varperc			# variazione netta in percentuale degli esami sostenuti e passati, dal 2019 al 2020, sessione estiva
pie(varperc)
				# tale variazione si verifica in ogni dipartimento 
				# 1,8,14,20 dipartimenti con variazione maggiore
				# 2,3,4,10,11,12,13,15,18 con variazione minore
				# Rilevante risulta essere l'andamento del Dipartimento di Lettere e Fac di medicina che, nel 2019, hanno registrato il 31%
				# di CFU rispetto al totale dei dipartimenti, mentre nel 2020 tale percentuale ha subito un incremento del +20%, toccando e superando quota 50% 

dipartimenti[,1]


# dal grafico e dalle percentuali di esami (espressi in CFU) sostenuti per categorie di dipartimenti, si evince come i dipartimenti umanistici presentino valori più elevati
# rispetto i dipartimenti scientifici, probabilmente dovuto alle modalità di erogazione degli esami (maggiormente scritti che orali, con relative complicanze dovute alla didattica online)

###############################################################

db2<-file.choose()
ISEE<-read.csv(db2, head=TRUE,sep=";",dec=",",na.strings="Null" )
ISEE

ISEE$Classi.ISEE<-as.factor(ISEE$Classi.ISEE)
str(ISEE)
ISEE$Classi.ISEE

colnames(ISEE)
ISEE
barplot(ISEE$CFU.Sostenuti~ISEE$Classi.ISEE, xlab="CLASSI ISEE",ylab="CFU SOSTENUTI")	#errore nel grafico

# per classi di ISEE 0-5000 si registra un totale di CFU inferiore rispetto alla media, dovuto probabilmente alla mancanza di fondi per acquistare gli strumenti adatti per effettuare la didattica online.
# un picco viene registrato nelle classi 5000-10000 (classe maggiore) e 10000-15000, dove rientrano molto probabilmente tutti gli individui aventi abbastanza budjet familiare da investire nella didattica online ma non una fascia alta di ISEE per iscriversi ad una Università privata.
# Infatti, l'andamento nelle classi successive risulta essere decrescente.
# Il grafico mostra quindi una asimmetria positiva.


#####################################################################


analisimf<-matrix(c(455446,257007,464236,259389,291403,155688),2,3,dimnames=list(c("M","F"),c("AA17","AA18","AA19")))
rownames(analisimf)
chisq.test(analisimf)			# rifiuto l'ipotesi di

par(mfcol=c(2,2))
barplot(analisimf,beside=F,font=2,cex.axis=1)
abline(h=0)
barplot(analisimf,beside=T,font=2)
abline(h=0)

# dai dati pervale una maggioranza di maschi rispetto alle femmine, sempre in ottica di CFU sostenuti.
# per quanto riguarda una possibile differenza tra i CFU sostenuti nell'anno 2020 (sessione primaverile-estiva) tra sessi non si registrano importanti variazioni rispetto all'anno precedente, oltre che una diminuzione totale dei CFU sostenuti

margin.table(analisimf,1)
margin.table(analisimf,2)
rbind(cbind(analisimf,margin.table(analisimf,1)),c(margin.table(analisimf,2),sum(analisimf)))

barplot(analisimf[1,],main="Maschi",cex.names=2,cex.axis=1,cex.main=1)
abline(h=0)
barplot(analisimf[2,],main="Femmine",cex.names=2,cex.axis=1,cex.main=1)
abline(h=0)

par(mfrow=c(1,3))
barplot(analisimf[,1],main=colnames(analisimf)[1])
abline(h=0)
barplot(analisimf[,2],main=colnames(analisimf)[2])
abline(h=0)
barplot(analisimf[,3],main=colnames(analisimf)[3])
abline(h=0)

rel_marg_col=prop.table(margin.table(analisimf,2))
t_rel_marg_col=matrix(rep(rel_marg_col,dim(analisimf)[1]),byrow=T,ncol=dim(analisimf)[2])
row_pr1=prop.table(analisimf,1)	# per riga
row_pr2=prop.table(analisimf,2)	# per colonna
row_pr1
row_pr2

row_dev1=row_pr1-t_rel_marg_col
round(row_dev1*100,1)

par(mfrow=c(1,2))
for (i in 1:dim(analisimf)[1])
{
barplot(row_dev[i,],ylim=c(min(row_dev),max(row_dev)),main=rownames(analisimf)[i])
abline(h=0)
}
par(mfrow=c(1,1))
mtext("Deviazioni profili riga dal marginale colonna", outer = TRUE,cex=1)
			# barplot della deviazione dei profili riga dal totale colonna

t_rel_marg_row2=prop.table(margin.table(analisimf,1))
col_pr=prop.table(analisimf,2)
round(col_pr*100,2) # ciascuna colonna somma a 100
col_pr[,1]
col_pr[,2]
col_pr[,3]

## costruzione di una tabella/matrice con ciascuna colonna
## uguale al marginale riga
t_rel_marg_row2=matrix(rep(t_rel_marg_row2,dim(analisimf)[2]),
byrow=F,ncol=dim(analisimf)[2])
col_dev=col_pr-t_rel_marg_row2
round(col_dev*100,1)
par(mfrow=c(1,4),oma=c(0,0,2,0))
for (j in 1:dim(analisimf)[2])
{
barplot(col_dev[,j],ylim=c(min(col_dev),max(col_dev)),main=colnames(analisimf)[j])
abline(h=0)
}
## aggiunge testo in alto (oma=c(0,0,2,0) indica la posizione)
mtext("Deviazioni profili colonna dal marginale riga", outer = TRUE,cex=1)

###############################################################################
### Voti medi ###
###################################################################################
## Anno immatricolazione 2017-2018

db4<-file.choose()
votimedi18<-read.table(db4,head=T,sep=";",dec=",",na.strings="")
votimedi18
colnames(votimedi18)
votimedi17$ID<-as.character(votimedi18$ID)
str(votimedi18)
summary(votimedi18)


votimedi18a<-votimedi18[,c(1,2,3)]   # Immatricolati 2018-2019, voto medio 2018-2019 e 2019-2020
votimedi18a
names(votimedi18a)<-c("ID","X","Y")		#X=2018-2019, Y=2019-2020
summary(votimedi18a$X)
summary(votimedi18a$Y)
sd(votimedi18a$X)
sd(votimedi18a$Y)

diff1=votimedi18a$Y-votimedi18a$X
diff1
summary(diff1)
sd(diff1)
boxplot(diff1)
qqnorm(diff1)
qqline(diff1)

diff2<-sample(diff1, 5000)
shapiro.test(diff2)              #andamento non normale

t.test(votimedi18a$Y, votimedi18a$X, paired=TRUE,mu=0)
# dall'analisi sui voti medi si registra una variazione in media positiva di circa 0.615 in media dall'anno accademico 18-19 all'anno in questione (19-20)
# ciò sottolinea che, seppur si è registrato un enorme diminuzione degli esami sostenuti tra i due anni, la variazione della media dei voti risulta positiva, il che afferma un incremento possibile delle prestazioni degli iscritti all'anno accademico 19-20, precisamente nella sessione primaverile-estiva.


db5<-file.choose()
votimedi17<-read.csv(db5, head=T,sep=";",dec=",",na.strings="" )
votimedi17

colnames(votimedi17)
votimedi17$ID<-as.character(votimedi17$ID)
str(votimedi17)
summary(votimedi17)


votimedi17a<-votimedi17[,c(1,3,4)]   # Immatricolati 2017-2018, voto medio 2018-2019 e 2019-2020
votimedi17a
names(votimedi17a)<-c("ID","X","Y")		#X=2018-2019, Y=2019-2020
summary(votimedi17a$X)
summary(votimedi17a$Y)
sd(votimedi17a$X)
sd(votimedi17a$Y)

diff11=votimedi17a$Y-votimedi17a$X
diff11
summary(diff11)
sd(diff11)
boxplot(diff11)
qqnorm(diff11)
qqline(diff11)

diff22<-sample(diff11, 5000)
shapiro.test(diff22)              #andamento non normale

t.test(votimedi17a$Y, votimedi17a$X, paired=TRUE,mu=0)
# differenza sempre positiva ma leggermente inferiore per gli iscritti all'università nell'anno accademico 17-18
# che affrontano, quindi, il terzo anno accademico nel periodo considerato (2020)

