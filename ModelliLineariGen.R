#lettura dataset
data<-read.csv("dataset2.csv",sep=",",dec=".");str(data)#anche le variabili categoriali sono codificate come interi
#escludiamo id
dataset<-data[,-c(1)]
#rendiamo categoriali le variabili
dataset$sex<-factor(dataset$sex,levels=c("0","1"),labels=c("Female","Male"))
dataset$multipleJointPains<-factor(dataset$multipleJointPains,levels=c("0","1"),labels=c("No","Yes "))
dataset$allignmentOfMidFoot<-factor(dataset$allignmentOfMidFoot,levels=c("0","1","2"),labels=c("Normal","highArch","Flat"))
dataset$diabetes<-factor(dataset$diabetes,levels=c("0","1"),labels=c("No","Yes"))
dataset$cyanosis<-factor(dataset$cyanosis,levels=c("0","1"),labels=c("No","Yes"))
dataset$burningSensationInPS<-factor(dataset$burningSensationInPS,levels=c("0","1"),labels=c("No","Yes"))
dataset$Tenderness<-factor(dataset$Tenderness,levels=c("0","1"),labels=c("No","Yes"))
dataset$Status<-factor(dataset$Status,levels=c("0","1"),labels=c("No","Yes"))
dataset$highHealWear<-factor(dataset$highHealWear,levels=c("0","1"),labels=c("No","Yes"))

#rendiamo disponibile ciascuna variabile
attach(dataset)

##analisi preliminare

library(lsr)
library(PropCIs)
library(vcd)
library(tidyverse)
library(plotrix)
library(ggplot2)

str(dataset)

#y-->Status
t<-table(Status)/length(Status);t<-(round(t,digit=2))*100;
cat("tabella dei Malati in %")
t
lbls<-c("Non Malato","Malato");lbls<-paste(lbls,t);lbls<-paste(lbls,"%",sep=" ")
pie3D(t,main="Malati e non malati",labels=lbls,col=rainbow(length(lbls)))

#age
summary(age)
age.class<-cut(age,breaks=c(10,30,50,60)); l<-c("10-30","30-50","50+")
par(mfrow=c(1,2),mar=c(4,4,4,4))
hist(age,col="light blue",main="età degli osservati") #da 10-60 anni
hist(age,col="light green",main="età degli osservati per classi",
breaks=c(10,30,50,60),labels=l)
data2<-dataset %>%
    group_by(Status) %>%
    summarise(
        mean(age, na.rm=T)
    )
par(mar=c(4,4,4,4))
par(mfrow=c(1,2))
barplot(data2$"mean(age, na.rm = T)", col=rainbow(25), las=2,
    names.arg=c("No","Yes"),main="Distribuzione della malattia per età media")

p<-ggplot(data=dataset,aes(x=Status,y=age))+geom_boxplot(outlier.colour="red",outlier.shape=8,outlier.size=4)
p+geom_boxplot(notch=T)+stat_summary(fun.y=mean,geom="point")+
labs(title="Distribuzione dello Status e dell'età congiunta")+
theme(plot.title=element_text(hjust=0.5))

#più anzani hanno + dolori
t<-addmargins(table(age.class,Status));t
#fattore rischio eta'
RR1<-(t[2,2]/t[2,3])/(t[1,2]/t[1,3]);RR1 
RR2<-(t[3,2]/t[3,3])/(t[2,2]/t[2,3]);RR2
chisq.test(t,0.95) #c'è dipendenza

#verifica di collinearità
summary(lm(age~weight)) #R2=0.17 
VIF<-1/(1-0.17) #<10 quindi no collinearità
z<-glm(Status~age,family=binomial(link=logit),data=dataset);summary(z)#significativo
or<-exp(0.1022);or
exp(confint(z))

#sex

t<-table(sex)/length(sex);t<-(round(t,digit=2))*100
lbls<-c("Maschio","Femmina");lbls<-paste(lbls,t);lbls<-paste(lbls,"%",sep=" ")
pie3D(t,main="variabile sex",labels=lbls,col=rainbow(length(lbls)))
m<-cbind(age,weight)
t<-addmargins(table(sex,Status));t
plot(sex,Status,main="Distribuzione congiunta sesso e malattia",col=c("light blue","blue"))
riskscoreci(t[1,2],t[1,3],t[2,2],t[2,3],0.95)
s<-glm(Status~sex,family=binomial(link=logit),data=dataset);summary(s)

#weight
summary(weight)
weight.class<-cut(weight,breaks=c(40,50,60,70,85))

#barplot(t,xlab="peso",ylab="frequenze relative",main="peso distinto per sesso",legend.text=T,col=c("light blue","pink"))
par(mar=c(4,4,4,4))
table(weight)# range da 85-40
hist(weight,main="distribuzione variabile weight",label=T,col="gold")# le freq + alte per modalita' centrali
t<-table(sex,weight)/length(weight)
#% basse perchè frequenze assolute piccole valore
#ci fa pensare a codifica poichè per ogni valore poche frequenze e molti
b<-glm(Status~weight,family=binomial(link=logit),data=dataset);summary(b)
IC<-confint(b);exp(IC)
tt<-ggplot(data=dataset,aes(x=Status,y=weight))+geom_boxplot(outlier.colour="red",outlier.shape=8,outlier.size=4)
tt+geom_boxplot(notch=T)+stat_summary(fun.y=mean,geom="point")+
labs(title="Distribuzione dello Status e del peso congiunta")+
theme(plot.title=element_text(hjust=0.5))


#allignmentOfMidFoot
t<-table(allignmentOfMidFoot);t
barplot(t,main="allineamento Meso-piede",col="orange")
barplot(table(Status,allignmentOfMidFoot),ylab="Status no-yes",main="Distribuzione tra allineamento piede e status")
addmargins(table(allignmentOfMidFoot,Status))
#rispetto a normale
riskscoreci(48,128,28,128,0.95)#di highArch
riskscoreci(56,128,28,128,0.95)#di Flat
d<-glm(Status~allignmentOfMidFoot,family=binomial(link=logit),data=dataset);summary(d)
exp(confint(d))#

#diabetes
t1<-table(diabetes)
t<-addmargins(table(diabetes,Status));t;
RR1<-(t[2,2]/t[2,3])/(t[1,2]/t[1,3]);RR1 #tra i diabeteci la propensione è maggiotr
lbs<-c("Non Malato","Malato")
pie3D(t1,main="Distribuzione diabete",labels=lbs,col=c("pink","#dd00dd"))
plot(table(diabetes,Status),col=c("pink","light blue"))
e<-glm(Status~diabetes,family=binomial(link=logit),data=dataset);summary(e)
exp(confint(e)) #

#cyanosis NON é significativo
t<-table(cyanosis,Status)#indipendenza perfetta
chisq.test(t,0.95);
t1<-addmargins(t)
chisq.test(table(cyanosis,Status),correct=F)
lb<-c("Malati no/cianosi no","Malati no/Cianosi si","Malati si/Cianosi no","Malati si/Cianosi si")
pie3D(t,main="Distribuzione dei malati con cianosi si/ no",labels=lb)
f<-glm(Status~cyanosis,family=binomial(link=logit),data=dataset);summary(f)
exp(confint(f))#indipendenza

#Tenderness NON è significativo
gi=table(Tenderness)
t2<-table(Tenderness,Status);t2#indipendenza perfetta
chisq.test(t,0.95);pie3D(t2,main="Distribuzione della Malattia in base alla durezza dei muscoli")
g<-glm(Status~Tenderness,family=binomial(link=logit),data=dataset);summary(g)
exp(confint(g))#NON SIGNIF, INDIPENDENZA
barplot(gi,col="pink",main="Distribuzione della Tenerezza dei muscoli osservata")

#multipleJointPains NON significativo

table(multipleJointPains); 
ge<-addmargins(table(multipleJointPains,Status));ge
RR<-(ge[2,2]/ge[2,3])/(ge[1,2]/ge[1,3]);RR; riskscoreci(71,192,61,192,0.95)
chisq.test(ge,0.95)#Indipendenza
l<-glm(Status~multipleJointPains,family=binomial(link=logit),data=dataset);summary(l)
exp(confint(l)) #indipendenza
barplot(ge)
#tutte le misure danno la stessa info

#hightHEalWear
barplot(table(highHealWear),horiz=T,main="Distribuzione di chi ha/ non ha capacità di rimarginazione ferite alta",col="light green")
go<-addmargins(table(highHealWear,Status));go
riskscoreci(63,192,69,192,0.95)#indip
m<-glm(Status~highHealWear,family=binomial(link=logit),data=dataset);summary(m)
exp(confint(m))
chisq.test(table(highHealWear,Status))
#non significativo

#BurningSensationInPS
barplot(table(burningSensationInPS),horiz=T,main="Distribuzione di chi ha/non ha bruciore quando è malato")
addmargins(table(burningSensationInPS,Status))
riskscoreci(70,192,62,192,0.95)#indipendenza
n<-glm(Status~burningSensationInPS,family=binomial(link=logit),data=dataset);summary(n)
#non signifcativo

AIC(n,m,l,g,f,e,z,b)# spiega meglio g


#hoursRunInAWeek
t<-table(hoursRunInAWeek)/length(hoursRunInAWeek)
barplot(t,col="light green",main="Ore di corsa in una settimana")
#possibile codifica con 0,1,2,3+
hours.class<-cut(hoursRunInAWeek,breaks=c(0,2,4,12))
t<-table(hoursRunInAWeek,Status);
addmargins(t)
#sulla tabella originale
Or<-(33*48)/(38*66);Or1<-(28*66)/(33*55);Or2<-(30*55)/(28*54)#poi tutti 0
Or#passare da 1 a 0 ore riduce la propens ad avere malattia
x<-c(log(Or)-1.96*((1/33)+(1/66)+(1/48)+(1/38)),log(Or)+1.96*(1/33+1/66+1/48+1/38))
exp(x)#passaree da 0 a 1 h riduce la propensione
Or1;#passare da 2 a 1 aumenta la prop 1%
x1<-c(log(Or1)-1.96*(1/33+1/66+1/55+1/28),log(Or1)+1.96*(1/33+1/66+1/55+1/28))
exp(x1)
Or2#passare da 3 a 2 aumenta la prop 9%
x2<-c(log(Or2)-1.96*(1/33+1/66+1/48+1/38),log(Or2)+1.96*(1/33+1/66+1/48+1/38))
exp(x2)
#se usiamo il rischio relativo e IC di questo
riskscoreci(33,99,38,86,0.95)
riskscoreci(28,83,33,99,0.95)
riskscoreci(30,84,28,83,0.95)
riskscoreci(0,3,30,84,0.95)
#da qui in poi non si può calcolare perchè viene 0 al denominatore tranne per
riskscoreci(0,3,2,5,0.95)
#in ogni IC di rischio relativo è incluso l'1 cioè indipendenza tra hoursRunInWeek e status
barplot(table(Status,hours.class),col=c("light green","dark green"),
main="Distribuzione congiunta HRIAW e malattia")
#prima ricodifica
addmargins(table(hours.class,Status))
riskscoreci(30,87,61,182,0.95) #contiene 1
riskscoreci(3,29,30,87,0.95) #non contiene 1
riskscoreci(3,29,61,182,0.95)#fare da 4 a 12 rispetto a 0-2 è ridotta 

c<-glm(Status~hoursRunInAWeek,family=binomial(link=logit),data=dataset);summary(c)
exp(confint(c)) #risulta essere fattore di protezione
j<-glm(Status~hours.class,family=binomial(link=logit),data=dataset);summary(j)

chisq.test(table(Status,hours.class),0.95)
cramersV(table(Status,hours.class))
chisq.test(table(Status,hoursRunInAWeek),0.95)#non si può usare perchè le celle =0
#portano ad approssimazioni

#proviamo nuova aggregazione
hours.class2<-cut(hoursRunInAWeek,breaks=c(0,2,4,6,8,12))
w<-glm(Status~hours.class2,family=binomial(link=logit),data=dataset);summary(w)
addmargins(table(hours.class2,Status))
riskscoreci(30,87,62,182,0.95)
riskscoreci(1,8,30,87,0.95)
riskscoreci(2,8,1,8,0.95)
riskscoreci(0,13,2,8,0.95)
#tutti gli IC per RR includono 1 
chisq.test(table(Status,hours.class2),0.95)
cramersV(table(Status,hours.class2))
#accetto test di indipendenza se fisso alpha a 0.05 




##relazionii tra variabili
matrix<-cbind(age,weight)
cor(matrix)
chisq.test(table(weight.class,diabetes))#c'è dipendenza
summary(lm(weight~diabetes))
VIF<-(1/(1-0.32))#no collinearità

chisq.test(table(age.class, multipleJointPains))#c'è dipendenza
summary(lm(age~multipleJointPains))# non c'è multicollinearità
#R2 è basso
 weight.class=cut(weight,breaks=c(10,20,30,40,50,60,70,85))
chisq.test(table(weight.class,sex),0.95)
chisq.test(table(age.class, sex))#c'è indipendenza
chisq.test(table(age.class, Tenderness))#c'è indipendenza
table(diabetes,cyanosis)
chisq.test(table(diabetes,cyanosis))#perfetta Indipendenza
chisq.test(table(Tenderness,multipleJointPains))#perfetta Indipendenza
chisq.test(table(highHealWear,multipleJointPains))

##indipendenti Tenderness,MultipleJoinPains,highHealWear,cyanosis,BurningSensation da Status
pseudoR2<-function(mod)
	{1-(deviance(mod)/mod$null.deviance)}

#senza le variabili che risultano essere indipendenti
mod<-glm(Status~age+sex+weight+allignmentOfMidFoot+diabetes+hoursRunInAWeek,family=binomial(link=logit),data=dataset);
summary(mod)

mod0<-glm(Status~+1,family=binomial(link=logit),data=dataset);summary(mod0)

mod1<-glm(Status~age+sex+weight+allignmentOfMidFoot+diabetes+cyanosis+ burningSensationInPS+Tenderness+
multipleJointPains+highHealWear+hoursRunInAWeek,family=binomial(link=logit),data=dataset)
summary(mod1);pseudoR2(mod1)
anova(mod0,mod,mod1,test="Chisq") 
#migliore il secondo mod senza var indip

#non ci sono interazioni significative di hoursRunInAWeek
#*highHealWear
#interazione significativa di *highHealWear

mod1b<-glm(Status~age+sex+weight+allignmentOfMidFoot+diabetes+Tenderness*highHealWear,family=binomial(link=logit),data=dataset)
summary(mod1b);pseudoR2(mod1b);anova(mod1b,test="Chisq")
#tra l'interazione di highHealWear con le altre variabili
#diventa significativo highHealWear e interazione TendernessYes:highHealWearYes
#anche cambiando caetgoria

#altra interazione di highHealWear significativa
mod1t<-glm(Status~age+sex+weight+allignmentOfMidFoot+diabetes+cyanosis*highHealWear,family=binomial(link=logit),data=dataset)
summary(mod1t);pseudoR2(mod1t)


#confronto tra modelli con interazione highHealWear significative (le altre non sono state riportate)
anova(mod,mod1b,test="Chisq")#accetto H0, migliore mod
anova(mod,mod1t,test="Chisq")#rifiuto H0,migliore mod1t
AIC(mod1b,mod1t)#mod1t in termini di AIC
BIC(mod1b,mod1t)#mod1t in termini di BIC

options(contrasts=c("contr.treatment","contr.poly"))

#provato Tenderness in interazione con tutte altre variabili,mai significativa
#solo quella già trovataa nel modello mod1b 

mod2<-glm(Status~age+sex+weight+allignmentOfMidFoot+diabetes+
multipleJointPains*Tenderness,family=binomial(link=logit),data=dataset)
summary(mod2);pseudoR2(mod2)
#togliamo Tenderness

#unica interazione significativa di multipleJoint è con diabetes
mod3<-glm(Status~age+sex+weight+allignmentOfMidFoot+diabetes*multipleJointPains,
family=binomial(link=logit),data=dataset)
summary(mod3)
library(lmtest)
lrtest(mod1,mod3)#migliore mod3
AIC(mod1t,mod3)

#mod3b= mod1t+mod3

mod3b<-glm(Status ~ age + sex + weight + allignmentOfMidFoot + 
    diabetes * multipleJointPains+ cyanosis * highHealWear, family = binomial(link = logit), 
    data = dataset)
summary(mod3b)

anova(mod3,mod3b,test="Chisq") #meglio mod3b se alfa 0.06
anova(mod1t,mod3b,test="Chisq") #meglio mod3b
pseudoR2(mod3b)
#prova burning sensation
mod4<-glm(Status~age+sex+weight+allignmentOfMidFoot*burningSensationInPS+
diabetes*multipleJointPains,family=binomial(link=logit),data=dataset)
summary(mod4)
##confounding su diabetes che diventa negativo 

AIC(mod4,mod3b) #mod3b
pseudoR2(mod4)

#burningSensation provato con tutte variabili interazione
#nessuna significativa, segue altra prova per burning sensation

mod5<-glm(Status~age+sex+weight+allignmentOfMidFoot+Tenderness*burningSensationInPS+
diabetes*multipleJointPains,family=binomial(link=logit),data=dataset)
summary(mod5)
#mod3 migliore perchè l'interazione non è significativa

#confrontiamo mod 4 con mod3b che il modello migliore fino a ora
BIC(mod4,mod3b);AIC(mod4,mod3b) #mod3b migliore
#migliore anche in termini di pseudoR2

###prova -allignment e proviamo interazioni

#prova modello migliore senza allignment
mod7<-glm(formula =  Status~ age+sex+ weight+ highHealWear +diabetes 
+multipleJointPains+cyanosis+diabetes:multipleJointPains + highHealWear:cyanosis, family = binomial(link = logit), data = dataset)
summary(mod7)

anova(mod7,mod3b,test="Chisq") #migliore mod7
pseudoR2(mod7)# poco inferiore a mod7 a quello di mod3b
#ma prima di eliminare allignment

options(contrasts=c("contr.SAS","contr.poly"))
mod3b<-glm(Status ~ age + sex + weight + allignmentOfMidFoot + 
    diabetes * multipleJointPains+ cyanosis * highHealWear, family = binomial(link = logit), 
    data = dataset)
summary(mod3b)
#resta non significativo allignment

options(contrasts=c("contr.treatment","contr.poly")) 

#proviamo recode
 library(dplyr)

dataset<-dataset%>%
	mutate(tidyAllignment=
		recode(allignmentOfMidFoot,
		"Normal"="Normal",
		"highArch"="RiskCondition",
		"Flat"="RiskCondition"))

dataset$tidyAllignment<-factor(dataset$tidyAllignment)
detach(dataset);attach(dataset)

#riscriviamo il mod3b con questa nuova codifica di allignment
mod7b<-glm(formula = Status ~ age + sex + weight + tidyAllignment + 
    diabetes * multipleJointPains + cyanosis * highHealWear, 
    family = binomial(link = logit), data = dataset)
summary(mod7b)

pseudoR2(mod7b)
#nemmeno in questo modo allignment è significativo

#interazioni di allgnment
#provate tutte nemmeno una significativa
mod8<-glm(formula = Status ~ age + sex + weight*allignmentOfMidFoot 
    +diabetes * multipleJointPains + cyanosis * highHealWear, 
    family = binomial(link = logit), data = dataset)
summary(mod8)

#confronto con il modello migliore
anova(mod3b,mod8,test="Chisq") #meglio mod3b
AIC(mod8,mod7b) #mod7b
pseudoR2(mod8);pseudoR2(mod7b)
#tra mod3b e mod7b?
#non sono nested

AIC(mod3b,mod7b,mod);BIC(mod3b,mod7b,mod) #mod7b migliore
anova(mod0,mod7b,test="Chisq") #seguendo criterio parsimonia mod è migliore di mod7b

#modello mod7b > mod3b 
#interazione di diabetes è significativa ma è counfaunders

#mod7b senza diabetes e sua interazione
mod9<-glm(Status ~ age + sex + weight + tidyAllignment + diabetes+
    cyanosis + highHealWear + cyanosis:highHealWear, family = binomial(link = logit), 
    data = dataset);summary(mod9)
#infatti senza quella interazione l'effetto è nuovamente positivo

#come mod9 senza 
mod10<-update(mod9,.~.-tidyAllignment);summary(mod10)
pseudoR2(mod9);pseudoR2(mod10);pseudoR2(mod11)
anova(mod7b,mod10,test="Chisq") #appena superiore a 0.1 accetto H0
anova(mod,mod10,test="Chisq") #mod10

mod11<-update(mod10,.~.-diabetes);summary(mod11)
pseudoR2(mod11)
AIC(mod11,mod)
anova(mod11,mod7b,test="Chisq")#mod11
anova(mod10,mod7b,test="Chisq")#mod10
anova(mod9,mod7b,test="Chisq")#appena migliore mod9 se alpha 0.5
anova(mod11,mod9,test="Chisq")#mod11
anova(mod11,mod10,test="Chisq") #mod11
#confronto con i modelli migliori
AIC(mod11,mod3b,mod1t,mod)#mod11
BIC(mod11,mod3b,mod1t,mod)#mod11

#####quale modello è migliore?-->mod11
#GOF
library(lmtest)
lrtest(mod0,mod11)
AIC(mod1,mod11) #non sono nested
#########

mod12<-glm(formula = Status ~ age + sex + weight, family = binomial(link = logit), data = dataset)
summary(mod12)

anova(mod12,mod11,test="Chisq") #rifiuto

#######analisi grafica########
#analisi grafica per modello mod11

library(vcd)
phat<-predict(mod11,type="response")
logit<-log(phat/(1-phat))
rsp<-rstandard(mod11,type="pearson")
rsd<-rstandard(mod11,type="deviance")
eta<-mod11$linear.predictor

#grafico tra logit e residui

par(mfrow=c(1,2))
plot(logit,rsd,main="pearson",sub="residui standardizzati di pearson",col.sub="blue")
plot(logit,rsp,main="deviance",sub="residui standardizzati di deviance",col.sub="blue")
#la presenza di una curvatura indica che potrebbe esserci
#inadeguatezza della funz link
#manca variabile importante opp necessaria trasformazione delle variabili inserite

##grafico eta e residui non dovrebbe avere particolare andamenti se c'è una buona stima
par(mfrow=c(1,2))
plot(eta,rsp,main="pearson",sub="residui standardizzati di pearson e eta",col.sub="blue")
plot(eta,rsd,main="deviance",sub="residui standardizzati di deviance e eta",col.sub="blue")
#non ha andamento casuale

##grafico residui e variabili
par(mfrow=c(1,2))
plot(rsp,data$age,main="residui vs age")
plot(rsp,data$weight,main="residui vs weight")
par(mfrow=c(3,1))
plot(rsp,data$sex,main="residui vs sex")
plot(rsp,data$highHealWear,main="residui vs highHealWear")
plot(rsp,data$cyanosis,main="residui vs cyanosi")

##cerca outliers

library(tidyverse);library(ggrepel)

i<-c(1:384);lbs=rownames(data);dat<-as.data.frame(cbind(rsp,i))
g1<-ggplot(data=data,aes(i,rsp))+geom_point(colour="blue")+geom_hline(yintercept=-3,col="red")+
geom_hline(yintercept=3,col="red")+labs(title="Grafico dei residui",x="indice di osservazione",y="residui ")+
geom_text(aes(label=lbs),check_overlap=TRUE)+
theme(plot.title=element_text(hjust=0.5),)
g1
###
outliers sono osservazione:
2,1,124,121,150,286,356,311,320
FARE PROVA SENZA QUESTE OSSERVAZIONI PER VEDERE SE MIGLIORA

plot(eta,phat,main="Distribuzione del modello",xlab="valore previsto dal modello (eta)",
ylab="probabilità stimata dal modello",font.main="4",col="blue")

###qualità previsioni
pred <- unique(predict(mod11, type="response")) 
#VPP,VPN,ACCURATEZZA(corretta classificazione),sensibilità,specificità
y<-data$Status
prob<-fitted(mod11)
yprev<-ifelse(prob>0.5,1,0) #soglia arbitraria
tabella<-table(y,yprev);addmargins(tabella)
#confronto tra freq teoriche e freq effettive
VPP<-(tabella[1,1]/(tabella[1,1]+tabella[2,1]))*100;VPP;
cat("86su 100 sono previsti correttamente non malati") 
VPN<-(tabella[2,2]/(tabella[2,2]+tabella[1,2]))*100;VPN
cat("79 su 100 sono previsti correttamente malati") 
ACCUR<-((tabella[1,1]+tabella[2,2])/(length(y)))*100;
ACCUR;cat("84 su 100 sono previsti correttamente") 
sens<-tabella[2,2]/(tabella[1,2]+tabella[2,2])*100;sens #dato che è malato, con che prob è stato predetto
spec<-tabella[1,1]/(tabella[1,1]+tabella[2,1])*100;spec #dato che non è malato, con che prob è predetto non malato
pseudoR2(mod11)
#le previsioni potrebbero essere più accurate
#################################################################ààà
#togliamo gli outliers
dataset3<-dataset[-c(1,2,124,121,150,286,356,311,320),];head(dataset3)

mo1<-glm(Status~age+weight+sex+cyanosis+highHealWear+cyanosis*highHealWear,family = binomial(link = logit), 
    data = dataset3);summary(mo1)

mo<-glm(Status~1,family = binomial(link = logit),data = dataset3);summary(mo)

mo1b<-glm(Status~age+weight+sex,family = binomial(link = logit), 
    data = dataset3);summary(mo1b)


anova(mo,mo1,test="Chisq")
anova(mo1b,mo1,test="Chisq") #migliore mo1b
pseudoR2(mo1);pseudoR2(mo1b)
##per mo1

#plot grafico residui nuovo modello
rsp10<-rstandard(mo1,type="pearson");
l<-c(1:length(dataset3$Status));lbs=l;
dat<-as.data.frame(cbind(rsp10,l))
g3<-ggplot(data=dataset3,aes(l,rsp10))+geom_point(colour="blue")+geom_hline(yintercept=-3,col="red")+
geom_hline(yintercept=3,col="red")+geom_hline(yintercept=4,col="purple")+
labs(title="Grafico dei residui",x="indice di osservazione",y="residui ")+
geom_text(aes(label=lbs),check_overlap=TRUE)+
theme(plot.title=element_text(hjust=0.5))
g3
##OUTLIER evidente 281,100,252

#uso di mo1 per prevedere
prob10<-fitted(mo1)
yprev10<-ifelse(prob10>0.5,1,0) #soglia arbitraria
y10<-dataset3$Status
tabella10<-table(y10,yprev10);addmargins(tabella10)
VPP<-(tabella10[1,1]/(tabella10[1,1]+tabella10[1,2]))*100;VPP;#dato che è previsto no, qual è la prob che non sia malato 
VPN<-(tabella10[2,2]/(tabella10[2,2]+tabella3[1,2]))*100;VPN #dato che è previsto si, qual è la prob che sia malato 
ACCUR<-((tabella10[1,1]+tabella10[2,2])/(length(y)))*100;ACCUR;
sens<-tabella10[2,2]/(tabella10[1,2]+tabella10[2,2])*100;sens #dato che è malato, con che prob è stato predetto
spec<-tabella10[1,1]/(tabella10[1,1]+tabella10[2,1])*100;spec #dato che non è malato, con che prob è predetto non malato


##per mo1b
#plot grafico residui nuovo modello
rsp2<-rstandard(mo1b,type="pearson");
l<-c(1:length(dataset3$Status));lbs=l;
dat<-as.data.frame(cbind(rsp2,l))
g3<-ggplot(data=dataset3,aes(l,rsp2))+geom_point(colour="blue")+geom_hline(yintercept=-3,col="red")+
geom_hline(yintercept=3,col="red")+geom_hline(yintercept=4,col="purple")+
labs(title="Grafico dei residui",x="indice di osservazione",y="residui ")+
geom_text(aes(label=lbs),check_overlap=TRUE)+
theme(plot.title=element_text(hjust=0.5))
g3
##OUTLIER evidente 281,100,252

#uso di mo1b per prevedere
prob3<-fitted(mo1b)
yprev3<-ifelse(prob3>0.5,1,0) #soglia arbitraria
y3<-dataset3$Status
tabella3<-table(y3,yprev3);addmargins(tabella3)
VPP<-(tabella3[1,1]/(tabella3[1,1]+tabella3[1,2]))*100;VPP;#dato che è previsto no, qual è la prob che non sia malato 
VPN<-(tabella3[2,2]/(tabella3[2,2]+tabella3[1,2]))*100;VPN #dato che è previsto si, qual è la prob che sia malato 
ACCUR<-((tabella3[1,1]+tabella3[2,2])/(length(y)))*100;ACCUR;
sens<-tabella3[2,2]/(tabella3[1,2]+tabella3[2,2])*100;sens #dato che è malato, con che prob è stato predetto
spec<-tabella3[1,1]/(tabella3[1,1]+tabella3[2,1])*100;spec #dato che non è malato, con che prob è predetto non malato

plot(rsp2,prob3/(1-prob3),main="residui standardizzati vs logit stimato")

#proviamo a modificare le categorie delle variabili per vedere se cambiano i residui 
#variabile age
dataset3<-dataset3%>%
	mutate(tidyAge=
		recode(dataset3$age,
			"10"="10-30",
			"11"="10-30",
			"12"="10-30",
			"13"="10-30",
			"14"="10-30",
			"15"="10-30",
			"16"="10-30",
			"17"="10-30",
			"18"="10-30",
			"19"="10-30",
			"20"="10-30",
			"21"="10-30",
			"22"="10-30",
			"23"="10-30",
			"24"="10-30",
			"25"="10-30",
			"26"="10-30",
			"27"="10-30",
			"28"="10-30",
			"29"="10-30",
			"30"="10-30",
			"31"="31-45",
			"32"="31-45",
			"33"="31-45",
			"34"="31-45",
			"35"="31-45",
			"36"="31-45",
			"37"="31-45",
			"38"="31-45",
			"39"="31-45",
			"40"="31-45",
			"41"="31-45",
			"42"="31-45",
			"43"="31-45",
			"44"="31-45",
			"45"="31-45",
			"46"="45-60",
			"47"="45-60",
			"48"="45-60",
			"49"="45-60",
			"50"="45-60",
			"51"="45-60",
			"52"="45-60",
			"53"="45-60",
			"54"="45-60",
			"55"="45-60",
			"56"="45-60",
			"57"="45-60",
			"58"="45-60",
			"59"="45-60",
			"60"="45-60",
			)
	)

#recode weight
dataset3<-dataset3%>%
	mutate(tidyWeight=
		recode(dataset3$weight,
			"40"="40-45",
			"41"="40-45",
			"42"="40-45",
			"43"="40-45",
			"44"="40-45",
			"45"="40-45",
			"46"="45-50",
			"47"="45-50",
			"48"="45-50",
			"49"="45-50",
			"50"="45-50",
			"51"="51-55",
			"52"="51-55",
			"53"="51-55",
			"54"="51-55",
			"55"="51-55",
			"56"="56-60",
			"57"="56-60",
			"58"="56-60",
			"59"="56-60",
			"60"="56-60",
			"61"="61-65",
			"62"="61-65",
			"63"="61-65",
			"64"="61-65",
			"65"="61-65",
			"66"="66-70",
			"67"="66-70",
			"68"="66-70",
			"69"="66-70",
			"70"="66-70",
			"71"="71-75",
			"72"="71-75",
			"73"="71-75",
			"74"="71-75",
			"75"="71-75",
			"76"="76-80",
			"77"="76-80",
			"78"="76-80",
			"79"="76-80",
			"80"="76-80",
			"81"="80-85",
			"82"="80-85",
			"83"="80-85",
			"84"="80-85",
			"85"="80-85"
			)
	)
 

dataset3$tidyAge<-factor(dataset3$tidyAge)
dataset3$tidyWeight<-factor(dataset3$tidyWeight)
detach(dataset3)
attach(dataset3)
####variabili codificate
S<-dataset3$Status
table(S,tidyAge)
table(S,tidyWeight)


mo2<-glm(S~tidyAge+sex+tidyWeight+cyanosis+highHealWear+cyanosis*highHealWear-1,family = binomial(link = logit), data = dataset3)
summary(mo2);


phat6<-predict(mo2,type="response")
logit6<-log(phat6/(1-phat6))
rsp6<-rstandard(mo2,type="pearson")

plot(rsp6,logit6)
AIC(mo2,mo1,mo1b)#mo1b
BIC(mo2,mo1,mo1b) #mo1b migliore perche ha meno regressori
pseudoR2(mo2)

#si prova ancora una nuova codifica per weight riducendo ancora di più le classi, fino a farla diventare dicotomica
#si lascia nello script solo la prova significativa
#recode weight
dataset3<-dataset3%>%
	mutate(tidyWeight2=
		recode(dataset3$weight,
			"40"="40-60",
			"41"="40-60",
			"42"="40-60",
			"43"="40-60",
			"44"="40-60",
			"45"="40-60",
			"46"="40-60",
			"47"="40-60",
			"48"="40-60",
			"49"="40-60",
			"50"="40-60",
			"51"="40-60",
			"52"="40-60",
			"53"="40-60",
			"54"="40-60",
			"55"="40-60",
			"56"="40-60",
			"57"="40-60",
			"58"="40-60",
			"59"="40-60",
			"60"="40-60",
			"61"="60+",
			"62"="60+",
			"63"="60+",
			"64"="60+",
			"65"="60+",
			"66"="60+",
			"67"="60+",
			"68"="60+",
			"69"="60+",
			"70"="60+",
			"71"="60+",
			"72"="60+",
			"73"="60+",
			"74"="60+",
			"75"="60+",
			"76"="60+",
			"77"="60+",
			"78"="60+",
			"79"="60+",
			"80"="60+",
			"81"="60+",
			"82"="60+",
			"83"="60+",
			"84"="60+",
			"85"="60+"
			)
	)
dataset3$tidyAge<-factor(dataset3$tidyAge)
dataset3$tidyWeight2<-factor(dataset3$tidyWeight2)
detach(dataset3)
attach(dataset3)


mo3<-glm(S~tidyAge+sex+tidyWeight2,family = binomial(link = logit), data = dataset3)
summary(mo3)


mo4<-update(mo3,.~.+hoursRunInAWeek);summary(mo4)

anova(mo3,mo4,test="Chisq")#migliore mo4 secondo test anova
BIC(mo1b,mo1,mo2,mo3,mo4)
AIC(mo1b,mo1,mo2,mo3,mo4)
#in entrambi i casi migliore mo1b che ha meno parametri di tutti

phat7<-predict(mo3,type="response")
logit7<-log(phat7/(1-phat7))
rsp7<-rstandard(mo3,type="pearson")

plot(rsp7,logit7)

prob5<-fitted(mo3)
yprev5<-ifelse(prob4>0.5,1,0) #soglia arbitraria
y5<-dataset3$Status
tabella5<-table(y5,yprev5);addmargins(tabella5)
VPP<-(tabella5[1,1]/(tabella5[1,1]+tabella5[1,2]))*100;VPP;
VPN<-(tabella5[2,2]/(tabella5[2,2]+tabella5[1,2]))*100;VPN
ACCUR<-((tabella5[1,1]+tabella5[2,2])/(length(y)))*100;ACCUR
sens<-tabella5[2,2]/(tabella5[1,2]+tabella5[2,2])*100;sens #dato che è malato, con che prob è stato predetto
spec<-tabella5[1,1]/(tabella5[1,1]+tabella5[2,1])*100;spec #dato che non è malato, con che prob è predetto non malato


phat8<-predict(mo4,type="response")
logit8<-log(phat8/(1-phat8))
rsp8<-rstandard(mo4,type="pearson")

prob6<-fitted(mo4)
yprev6<-ifelse(prob6>0.5,1,0) #soglia arbitraria
y6<-dataset3$Status
tabella6<-table(y6,yprev6);addmargins(tabella6)
VPP<-(tabella6[1,1]/(tabella6[1,1]+tabella6[1,2]))*100;VPP;
VPN<-(tabella6[2,2]/(tabella6[2,2]+tabella6[1,2]))*100;VPN
ACCUR<-((tabella6[1,1]+tabella6[2,2])/(length(y)))*100;ACCUR
sens<-tabella6[2,2]/(tabella6[1,2]+tabella6[2,2])*100;sens #dato che è malato, con che prob è stato predetto
spec<-tabella6[1,1]/(tabella[1,1]+tabella6[2,1])*100;spec #dato che non è malato, con che prob è predetto non malato

plot(rsp8,logit8)

##############################

xEffettoMax<-function(mod,j){
	return (-mod$coef[1]/mod$coef[j])
}

		#effetto Max su prob
#per age
xEffettoMax(mo1b,2)
#per weight
xEffettoMax(mo1b,3)

####calcolo dei intervalli fiduciari per i beta

IC<-confint(mo1b,parm=2:4,level=0.95) #intervalli di confidenza per i beta stimati
exp(IC)

##stima di probabilità con valori medi

logLik(mo1b)
logLik(mo3)
###########stima prob con valori inseriti####


stimaProb<-function(mod,age_value,weight_value,sex_value){
	return (exp(mod$coef[1]+mod$coef[2]*age_value+mod$coef[3]*weight_value+mod$coef[4]*sex_value)/
	(1+exp(mod$coef[1]+mod$coef[2]*age_value+mod$coef[3]*weight_value+mod$coef[4]*sex_value)))
	
}


###
x1<-mean(age);x2<-mean(weight)
stimaProb(mo1b,x1,x2,1)
#gli uomini anziani dovrebbero avere la prob minore
stimaProb(mo1b,70,50,1)#uomo
stimaProb(mo1b,70,50,0)#donna


#curva di ROC
library(pROC)

par(mfrow=c(1,2))
pred<-mo1b$fitted
y<-dataset3$Status
roc(y,pred,plot=T) #valore previsione=0.9236 (molto accurata)


pred2<-mod11$fitted
y2<-dataset$Status
roc(y2,pred2,plot=T)#0.88 < 0.92

#intervalli confidenza per probabilità
#se new.data=NULL calcola linear predictor

prev<-predict(mo1b,se.fit=T)
L<-c(pred$fit-1.96*pred$se)
U<-c(pred$fit+1.96*pred$se)
cbind(exp(L)/(1+exp(L)),exp(U)/(1+exp(U)))




