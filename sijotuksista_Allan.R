#Simulointi blogipostausta "Keskivertosijoittaja ei saa keskivertotuottoa" Harhala-blogiin 15.9.2016
#Tehnyt Allan Seuri, Alex Tabarrokin pohjalta: http://marginalrevolution.com/marginalrevolution/2014/07/average-stock-market-returns-arent-average.html
#Innoittajana Martin Paaden blogikirjoitus: http://www.nordnetblogi.fi/lapselle-saastaminen-kannattaa/30/08/2016/
#Simuloidaan N kappaletta vastasyntyneitä, joille annetaan tietty alkupääoma. Tarkastellaan loppupääoman jakaumaa.
#Joona Rauhamäki edit, sijoitetaan tasaisesti joka kausi tavoitesummaan pääsemiseksi
setwd("C:/Users/Allan/Documents/blogi/2016/stockReturns")
#Otetaan siemenluvuksi sana "EINSTEIN" käyttämällä TeachingDemos-paketin char2seed-funktiota.
#Siemenluku on 4503855.
install.packages("TeachingDemos")
require(TeachingDemos)
char2seed("EINSTEIN",set=T)
#Asetetaan alkupääoma keskituoton ja säästämishorisontin funktiona.
endCapital<-1000000
T<-67
mu<-0.066

#Joonan edit, muutetaan start capital. Tyhmästi: startCapital <- 867, alla formaalimmin
startCapital <- endCapital/(((1+mu)^(T)-1)/mu)
startCapital

#Tämä on se noin 13800, jonka Paasi esittää.
#startCapital<-endCapital/(1+mu)^T

#Simuloidaan 100 000 havaintoa.
N<-100000
#Asetetaan keskihajonta 20 %:iin (kuten Tabarrok tekee)
sd=0.2
#Koska matriisi alkaa alkuarvosta, matriisin koko on T+1.
T<-T+1
#Alustetaan matriisi, sarakkeissa yksilöt ja riveillä aika
returnMatrix<-matrix(data=NA,nrow=T,ncol=N)
#Ensimmäiselle riville hetken nolla arvo eli alkupääoma.
returnMatrix[1,]<-startCapital
#Simuloidaan.
for(j in 2:T){
    returnMatrix[j,]<-(returnMatrix[j-1,]*(1+rnorm(N,mean=mu,sd=sd))+startCapital)
}



#Tarkastellaan keskiarvoa, mediaani ja niiden suhdetta.
mean(returnMatrix[T,])
median(returnMatrix[T,])
min(returnMatrix[T,])
max(returnMatrix[T,])
mean(returnMatrix[T,])/median(returnMatrix[T,])
#Kuinka moni saavuttaa keskituoton, entä kuinka moni jää alle alkupääoman?
1-sum(returnMatrix[T,]<endCapital)/N
sum(returnMatrix[T,]<startCapital)/N
#Näin suuri osuus jää alle 100 000:n
sum(returnMatrix[T,]<100000)/N



