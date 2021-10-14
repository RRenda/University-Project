###########################################################################
###########################################################################
###                                                                     ###
###           PROGETTO FINALE CORSO DI CUSTOMER SATISFACTION            ###
###                                                                     ###
###                           RETE Bayesiana                            ###
###                                                                     ###
###              STUDENTI MICIELI OTTAVIO e RENDA ROSSELLA              ###
###                                                                     ###
###########################################################################
###########################################################################


#package usati

library(tidyverse)
library(bnlearn)
library(readstata13)
#per verificare dati mancanti eventuali
library(naniar)
#per i grafici
library(Rgraphviz)
library(bnviewer)
library(Rmpfr)
library(ggplot2)
#per le query
library(gRain)


#directory
setwd("C:/Users/rosse/Desktop/progetto RN e RB")

#lettura dati
data<-read.dta13("ESS9IT.dta",convert.factors=T)
#non ci sono missing
miss_var_summary(data)
############################Grafici usati solo nelle slides###########################
#variabile target
dt<-as.data.frame(data)
ggplot(dt) +
   aes(x = hmsacld) +
   geom_bar(fill = "#112446") +
   labs(x = "livelli di accordo con le adozioni da parte di persone LGBT+", 
        y = "Frequenza assoluta", title = "Variabile Target: hmsacld") +
   theme_minimal() +
   theme(plot.title = element_text(face = "bold.italic", hjust = 0.5))

#altre variabili
ggplot(dt) +
   aes(x = prtvtcit) +
   geom_bar(fill = "#081358") +
   labs(x = "Partito politico votato alle ultime elezioni", y = "Frequenza assoluta", 
        title = "Variabile: prtvtci ") +
   theme_minimal()+
   theme(plot.title = element_text(face = "bold.italic", hjust = 0.5))


ggplot(dt) +
   aes(x = freehms) +
   geom_bar(fill = "#081358") +
   labs(x = "Livelli di accordo riguardo alla libertà di vivere la vita per persone LGBT", y = "Frequenza assoluta", 
        title = "Variabile: freehms ") +
   theme_minimal()+
   theme(plot.title = element_text(face = "bold.italic", hjust = 0.5))



ggplot(dt) +
   aes(x = imbgeco) +
   geom_bar(fill = "#081358") +
   labs(x = "Gli immigrati sono un Bene o un Male per l'economia?", y = "Frequenza assoluta", 
        title = "Variabile: imbego ") +
   theme_minimal()+
   theme(plot.title = element_text(face = "bold.italic", hjust = 0.5))

ggplot(dt) +
   aes(x = rlgdgr) +
   geom_bar(fill = "#081358") +
   labs(x = "Grado di religiosità", y = "Frequenza assoluta", 
        title = "Variabile: rlgdgr ") +
   theme_minimal()+
   theme(plot.title = element_text(face = "bold.italic", hjust = 0.5))



ggplot(dt) +
   aes(x = dscrgrp) +
   geom_bar(fill = "#081358") +
   labs(x = "Appartieni a un gruppo discriminato? ", y = "Frequenza assoluta", 
        title = "Variabile: dscrgrp ") +
   theme_minimal()+
   theme(plot.title = element_text(face = "bold.italic", hjust = 0.5))

ggplot(dt) +
   aes(x = brncntr) +
   geom_bar(fill = "#081358") +
   labs(x = "Nato in Italia? ", y = "Frequenza assoluta", 
        title = "Variabile: brncntr ") +
   theme_minimal()+
   theme(plot.title = element_text(face = "bold.italic", hjust = 0.5))

ggplot(dt) +
   aes(x = evmar) +
   geom_bar(fill = "#081358") +
   labs(x = "Sposato/a ? ", y = "Frequenza assoluta", 
        title = "Variabile: evmar ") +
   theme_minimal()+
   theme(plot.title = element_text(face = "bold.italic", hjust = 0.5))

ggplot(dt) +
   aes(x = bthcld) +
   geom_bar(fill = "#081358") +
   labs(x = "Hai figli? ", y = "Frequenza assoluta", 
        title = "Variabile: bthcld ") +
   theme_minimal()+
   theme(plot.title = element_text(face = "bold.italic", hjust = 0.5))

ggplot(dt) +
   aes(x = gndr) +
   geom_bar(fill = "#081358") +
   labs(x = "Genere", y = "Frequenza assoluta", 
        title = "Variabile: gndr ") +
   theme_minimal()+
   theme(plot.title = element_text(face = "bold.italic", hjust = 0.5))


ggplot(dt) +
   aes(x = agea) +
   geom_bar(fill = "#081358") +
   labs(x = "eta' in classi", y = "Frequenza assoluta", 
        title = "Variabile: agea ") +
   theme_minimal()+
   theme(plot.title = element_text(face = "bold.italic", hjust = 0.5))


ggplot(dt) +
   aes(x = edlveit) +
   geom_bar(fill = "#081358") +
   labs(x = "Titolo di studio", y = "Frequenza assoluta", 
        title = "Variabile: edlveit ") +
   theme_minimal()+
   theme(plot.title = element_text(face = "bold.italic", hjust = 0.5))

ggplot(dt) +
   aes(x = edlvfeit) +
   geom_bar(fill = "#081358") +
   labs(x = "Titolo di studio del padre", y = "Frequenza assoluta", 
        title = "Variabile: edlvfeit ") +
   theme_minimal()+
   theme(plot.title = element_text(face = "bold.italic", hjust = 0.5))


ggplot(dt) +
   aes(x = edlvmeit) +
   geom_bar(fill = "#081358") +
   labs(x = "Titolo di studio della madre", y = "Frequenza assoluta", 
        title = "Variabile: edlvmeit ") +
   theme_minimal()+
   theme(plot.title = element_text(face = "bold.italic", hjust = 0.5))

ggplot(dt) +
   aes(x = hmsfmlsh) +
   geom_bar(fill = "#081358") +
   labs(x = "Si prova vergogna se qualche familiare appartiene alla LGBT+", y = "Frequenza assoluta", 
        title = "Variabile: hmsfmlsh ") +
   theme_minimal()+
   theme(plot.title = element_text(face = "bold.italic", hjust = 0.5))

###########################GRAFO VUOTO####################################################
nodi<-names(data)
g<-empty.graph(nodi);g
hg<-list(nodes=c("hmsacld"),col = "#008C95", textCol = "#008C95")
graphviz.plot(g,layout ="fdp", main="Grafo vuoto" ,highlight = hg)

# è necessario addestrare il modello di rete bayesiana
#si sceglie di usare score based algorithms

blacklist<-data.frame(expand.grid("hmsacld",colnames(data)[-c(4)]))

################# Hill-Climbing e  Individuare grafico iniziale#######################

bn<-hc(data, score="aic", blacklist = blacklist)

#prima rappresentazione grafica
graphviz.plot(bn,layout = "fdp",shape="rectangle", highlight = hg, 
              main="Grafo ottenuto da HC Network function: AIC")

##seconda rappresentazione grafica
strength.viewer(bn,
                bf.strength(bn,data=data),
                bayesianNetwork.layout = "layout_with_gem",
                node.shape= "circle",
                node.size = 40,
                bayesianNetwork.title = "Rete ottenuta con Hill-Climbing funzione score:AIC",
                node.font = list(color = "black", face="Arial"))
modelstring(bn)

#2 modello
bn2<-hc(data, score="bic",blacklist = blacklist)
graphviz.plot(bn2,layout = "fdp",shape="rectangle", highlight = hg)
strength.viewer(bn2,
                bf.strength(bn2,data=data),
                bayesianNetwork.layout = "layout_with_gem",
                node.shape= "circle",
                node.size = 40,
                bayesianNetwork.title = "Rete ottenuta con Hill-Climbing funzione score:BIC",
                node.font = list(color = "black", face="Arial"))
modelstring(bn2)


#3 modello
bn3<-hc(data, score="bde",blacklist = blacklist)
graphviz.plot(bn3,layout="fdp",shape="rectangle", highlight = hg)
strength.viewer(bn3,
                bf.strength(bn3,data=data),
                bayesianNetwork.layout = "layout_with_gem",
                node.shape= "circle",
                node.size = 40,
                bayesianNetwork.title = "Rete ottenuta con Hill-Climbing funzione score:BDE",
                node.font = list(color = "black", face="Arial"))

modelstring(bn3)
# Confronto
score(bn,data=data,type="aic")
score(bn2,data=data,type="bic")
score(bn3,data=data,type="bde")
rb<-bn2 #criterio bic è quello che penalizza di + per evitare overfitting
############################################################################
#rb non è la rete finale, anzi si effettua un ragionamento sulle variabili di dominio

# Poniamo che l'età e il genere non dipende da nessun altro carattere,radice
no_agea<-names(data[-12])
no_gndr<-names(data[-11])
grid1<-data.frame(expand.grid(no_agea,"agea"))
grid2<-data.frame(expand.grid(no_gndr,"gndr"))
grid_black<-rbind(blacklist,grid1,grid2) #voglio agea e gender radice
rm(no_agea,no_gndr,grid1,grid2)
rb1<-hc(data, score="bic", blacklist=grid_black)

hg<-list(nodes=c("agea","gndr"),col="#008C95",textCol= "#008C95")
graphviz.plot(rb1,shape="rectangle",
              highlight=hg, main="Rete addestrata",sub="con blacklist aggiornata per agea e gndr")

arc.strength(rb1, data=data, criterion="mi")
#il test nn suggerisce alcun cambiamento


score(rb, data, type="bic")
score(rb1, data, type="bic")


# Vediamo se davvero il genere non influisce su niente, oltre alla religiosità
attach(data)

IndTestvsTutte<-function(pos){
   no_var<-names(data[-pos])
   var<-names(data[pos])
   result<-matrix(ncol=3)
   for(i in no_var){
      test<-ci.test(var,i,test="x2",data=data)
      result<-rbind(result,c(test$data.name,test$statistic,test$p.value))
   }
   result<-result[-1,] #toglie NA creati quando si crea matrix
   result<-(as.data.frame(result) %>% arrange(V3))
   names(result)[names(result) == 'V3'] <- "pvalue"
   names(result)[names(result) == 'V1'] <- "Variabili"
   names(result)[names(result) == 'x2'] <- "Statistica"
   
   return (result)
}

no_gndr<-IndTestvsTutte(11);no_gndr
#emerge dal p-value che c'è forte relazione tra gndr e "edlveit","hmsfmlsh","dscrgrp","rlgdgr"
#introduco white list e si rieffettua il learning con questi nuovi vincoli

nomi_var<-c("rlgdgr","edlveit","hmsfmlsh","dscrgrp")
grid_white<-data.frame(expand.grid("gndr",nomi_var))
rb2<-hc(data, score="bic", blacklist=grid_black, whitelist=grid_white)
hg<-list(nodes=c("gndr","rlgdgr","edlveit","hmsfmlsh","dscrgrp"),
         arcs=c("gndr","rlgdgr","gndr","edlveit","gndr","hmsfmlsh","gndr","dscrgrp"),
         col="#008C95", textCol="#008C95")

graphviz.plot(rb2,shape="rectangle",highlight=hg, main="Rete addestrata",sub="con whitelist aggiornata per gli archi uscenti da gndr")
arc.strength(rb2, data=data, criterion="mi") #non da suggerimenti
arc.strength(rb2, data=data, criterion="x2")
score(rb1, data,type="bic")
score(rb2, data,type="bic")

#A priori so che Essere italiano non è condizionato da altre, quindi blacklist
names<-names(data[,-8])
grid_black<-rbind(grid_black,expand.grid(names,"brncntr"))
#definiamo brncntr come altra radice
rb3<-hc(data, score="bic", blacklist=grid_black, whitelist=grid_white)
hg<-list(nodes=c("brncntr"),
         col="#008C95", textCol="#008C95")
graphviz.plot(rb3,shape="rectangle",hg,main="Rete addestrata",sub="con blacklist aggiornata per brncntr")
##############################################################################
#rb3 RETE STIMATA CON HC da cui partiranno gli aggiustamenti dato che è una rete
#i cui archi rappresentano perlopiù dipendenze plausibili

#Livello di educazione genitori su livello di educazione soggetto
table(edlveit,edlvfeit)
chisq.test(edlveit,edlvfeit)
chisq.test(edlveit,edlvmeit)
score(rb3,data,type="bic")

#inserire livello istruzione madre influisce su livello istruzione figlio
rb4<-set.arc(rb3,"edlvmeit","edlveit")
score(rb4,data,type="bic")

#inserire livello istruzione padre influisce su livello istruzione figlio
rb4.1<-set.arc(rb4,"edlvfeit","edlveit")
score(rb4.1,data)

#elimina arco tra età e livello istruzione del padre
rb4.2<-drop.arc(rb4.1,"agea","edlvfeit")
score(rb4.2,data,type="bic")



#livello istruzione influisce sull'opinione adozioni 
ci.test("edlveit","hmsacld",test="x2",data=data)

#a partire da rb4, costruisco nuovo modello
rb5<-set.arc(rb4,from="edlveit",to="hmsacld")
graphviz.plot(rb5,shape="rectangle",main ="")

#inverti arco matrimonio-religione
rb5.1<-reverse.arc(rb5,"evmar","rlgdgr")

#elimina arco
rb5.2<-drop.arc(rb5.1,"agea","edlvfeit")

score(rb4.2,data,type="bic") 
score(rb5.2,data,type="bic")

arc.strength(rb5,data,criterion = "mi");
arc.strength(rb5.1,data,criterion = "mi");
arc.strength(rb5.2,data,criterion = "mi");
#nessuna info

#si sceglie rb5.2 perchè include tutte le relazioni logiche ed IL BIC(rb5.2)> BIC(rb54.2)

graphviz.compare(rb3,rb5.2,shape="rectangle",layout = "fdp",
                 main = c("Modello iniziale","Modello finale"),
                 diff.args = list(tp.lwd=2,tp.col = "#008C95",fn.lwd=2,
                                  fn.col = "#f78575", fp.col="#f78575"))

coord <- par("usr")
legend(x = coord[2]*0.7659, y = coord[4],pch = 19,
       legend=c("arco tp", "arco fn","arco fp"),
       col=c("#008C95", "#f78575","#f78575"), 
       lty=c(1,2,1), cex=0.8 )


##############################RAPPRESENTAZIONE GRAFICA FINALE#####################
strength.viewer(rb5.2,
                bf.strength(rb5.2,data=data),
                bayesianNetwork.layout = "layout_with_gem",
                node.shape= "circle",
                node.size = 40,
                bayesianNetwork.title = "Modello finale",
                node.font = list(color = "black", face="Arial"))

modelstring(rb5.2)
directed.arcs(rb5.2)
unlist(compare(rb3, rb5.2))
hamming(rb3,rb5.2) #distanza di hamming-(come edit distance per stringhe )
#operazioni da fare per passare da una rete all'altra


#####################STIME DEI PARAMETRI###########
bnmle <- bn.fit(rb5.2, data =data , method = "mle")

bnbayes<- bn.fit(rb5.2, data =data , method = "bayes", iss=5)

####################INFERENZA ESATTA#############

#d-separazione, mi chiedo se:
dsep(rb5.2, "hmsacld","agea")
dsep(rb5.2, "gndr","brncntr")
dsep(rb5.2, "gndr","brncntr","dscrgrp")


######################quesiti inferenziali#########################

bn<-compile(as.grain(bnbayes))
bn$isCompiled


#1) Il partito votato alle ultime elezioni influenza il livello di accordo all'omogenitorialità?
dsep(rb5.2, "prtvtcit","hmsacld")
ci.test("hmsacld","prtvtcit",test="x2",data=data)

querygrain(bn, nodes = c("hmsacld","prtvtcit"), type="conditional")
querygrain(bn, nodes="hmsacld", type= "marginal")
#c'è della differenza-chi vota lega e FI è + in disaccordo rispetto a chi vota PD e 5stelle

#2)Se si ha un livello di istruzione basso , si crede che gli Immigrati siano il male (mentalità chiusa) come cambia 
#il livello di accordo sulla genitorialità delle persone LGBT+
dsep(rb5.2, "edlveit","hmsacld")
dsep(rb5.2, "imbgeco","hmsacld")

bn_mc<-setEvidence(bn,nodes = c("edlveit","imbgeco"),states = c("Licenza elementare","Male"),propagate = T)
querygrain(bn_mc, nodes="hmsacld", type="marginal")
querygrain(bn, nodes="hmsacld", type="marginal")


#3)	 come cambia la distribuzione di probabilità " del provare vergogna per componenti familiari che appartengono
#alla comunità LGBT+ " al variare del genere ?
dsep(rb5.2,"hmsfmlsh","gndr")# non sono d-sep xk edlveit nn è noto
querygrain(bn,nodes=c("hmsfmlsh","gndr"),type="conditional")
querygrain(bn,nodes=c("hmsfmlsh"),type="marginal")


#4)	Fissato il livello di età come varia 
#la distribuzione congiunta tra la variabile target "hmsacld" e "imbgeco"?

bn_ed<-setEvidence(bn,nodes = c("agea"),states = c(">65 anni "),propagate = T)
querygrain(bn_ed, nodes=c("hmsacld","imbgeco"), type="joint")
querygrain(bn, nodes=c("hmsacld","imbgeco"), type="joint")

###########ALTRE QUERY DA POTER FARE AVENDO TALE DAG E CPT###########
#ALTRO  quesito sul dominio che potrebbero essere di interesse non riportato
#per es. evidenza  di religione, influenzano sposato e eta
bn_rel1<-setEvidence(bn, nodes="rlgdgr",states = c("Molto religioso"))
bn_rel2<-setEvidence(bn, nodes="rlgdgr",states = c("Poco religioso"))
querygrain(bn_rel1, nodes=c("evmar"),type="marginal")
querygrain(bn_rel2, nodes=c("evmar"),type="marginal")
querygrain(bn, nodes=c("evmar"),type="marginal")

querygrain(bn_rel1, nodes=c("evmar","agea"),type="joint")
querygrain(bn_rel2, nodes=c("evmar","agea"),type="joint")
#ipotesi: tra 25-40 nn ti sposi e convivi

#stima di probabilità di eventi a cui posso essere interessata
bnbayes$evmar
bnbayes$hmsacld

ci.test("prtvtcit" ,"evmar","agea",data,test="x2")

#FINE




