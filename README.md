# Datos-Rproject-
Tesis pregrado

library(ggplot2)
#Proporción de individuos de murciélagos del Bosque basimontano, durante la estación seca del 2014 y húmeda del 2015, del Santuario Nacional Tabaconas Namballe
datap<-data.frame(Estación=factor(c("seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda")),Especies=factor(c("Cb","Cb","Cp","Cp","Ag","Ag","Ap","Ap","Eh","Eh","Pi","Pi","Pn","Pn","Sb","Sb","Se","Se","So","So","Sl","Sl","Sm","Sm","Vm","Vm","Vt","Vt"),levels=c("Cb","Cp","Ag","Ap","Eh","Pi","Pn","Sb","Se","So","Sl","Sm","Vm","Vt")),Individuos=c(3,2,11,15,3,0,0,0,0,0,0,1,0,10,1,0,8,5,22,28,28,10,1,0,0,0,2,0))
datap
ggplot(data=datap, aes(x=Especies, y=Individuos, fill=Estación))+ geom_bar(stat="identity", position=position_fill())+ ggtitle("Bosque Basimontano (1600 msnm)")+theme(plot.title=element_text(lineheight = .8, face="bold", size=15)) + theme(axis.title.y=element_text(size=10)) + theme(axis.title.x=element_text(size=10)) + theme(legend.title=element_text(size=10))+ theme(legend.text=element_text(size=10)) 

#Proporción de individuos de murciélagos del Bosque montano, durante la estación seca del 2014 y húmeda del 2015, del Santuario Nacional Tabaconas Namballe
datas<-data.frame(Estación=factor(c("seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda")),Especies=factor(c("Cb","Cb","Cp","Cp","Ag","Ag","Ap","Ap","Eh","Eh","Pi","Pi","Pn","Pn","Sb","Sb","Se","Se","So","So","Sl","Sl","Sm","Sm","Vm","Vm","Vt","Vt"),levels=c("Cb","Cp","Ag","Ap","Eh","Pi","Pn","Sb","Se","So","Sl","Sm","Vm","Vt")),Individuos=c(6,0,0,3,4,0,1,1,0,10,0,5,6,1,0,0,24,57,9,20,0,0,0,0,1,0,0,0))
ggplot(data=datas, aes(x=Especies, y=Individuos, fill=Estación)) + geom_bar(stat="identity", position=position_fill()) + ggtitle("Bosque Montano (2000 msnm)")+theme(plot.title=element_text(lineheight = .8, face="bold", size=15)) + theme(axis.title.y=element_text(size=10)) + theme(axis.title.x=element_text(size=10)) + theme(legend.title=element_text(size=10))+ theme(legend.text=element_text(size=10))

#Curva de acumulación de especies de plantas estimadas en heces de murciélagos presentes en el bosque basimontano
bbc<-c(16,6,7,4,4,3,1,3,3,2,2,2,1)
sd(bbc)
bbc1<-iNEXT(bbc, q=0, datatype = "incidence_freq")
bbc1
bbc2<-ggiNEXT(bbc1, type = 1, facet.var="none", color.var="order", grey= TRUE)+ labs(x = "Número de individuos", y = "Número de especies", size=18)+ theme(axis.title.x=element_text(colour="black", size=22),axis.title.y=element_text(colour="black", size=22)) + ylim(0,20) + xlim(0,18)
bbc2
#Curva de acumulación de especies de plantas estimadas n heces de murciélagos presentes en el Bosque montano
bmc<-c(16,5,1,5,2,3,1,3,2,2,5,1,4,3,4,3)
sd(bmc)
bmc1<-iNEXT(bmc, q=0, datatype = "incidence_freq")
bmc1
bmc2<-ggiNEXT(bmc1, type = 1, facet.var="none", color.var="order", grey= TRUE)+labs(x = "Número de individuos", y = "Número de especies", size=18)+ theme(axis.title.x=element_text(colour="black", size=22),axis.title.y=element_text(colour="black", size=22)) + ylim(0,20) + xlim(0,18)
bmc2

#Daieta de urciélagos en el Bosque basimontano, durante la estación seca del 2014 del Santuario Nacional Tabaconas Namballe.
dat1<-read.csv("pvsmbbsec3.csv",TRUE,",")
head(dat1)
tail(dat1)
library(ggplot2)
ggplot(data=dat1,aes(x=Especies.de.murciélagos,y=Nro.de.muestras.fecales,fill=Familias.de.plantas))+theme_bw()+geom_bar(stat="identity",position=position_stack())+ggtitle("Bosque Basimontano-Estación seca")+theme(plot.title=element_text(lineheight= .8, face="bold",size=25))+theme(axis.title.y=element_text(size=20))+theme(axis.title.x=element_text(size=20))+theme(legend.title=element_text(size=20))+theme(legend.text=element_text(size=15))+theme(axis.text.x=element_text(size=15),axis.text.y=element_text(size=15))+ylim(0,20)+geom_text(aes(label=Nro.de.muestras.fecales),size=5,hjust=0.5,vjust=1,position="stack")

#Dieta de murciélagos en el Bosque basimontano, durante la estación húmeda del Santuario Nacional Tabaconas-Namballe.
dat2<-read.csv("pvsmbbhum3.csv",TRUE,",")
head(dat2)
tail(dat2)
library(ggplot2)
ggplot(data=dat2,aes(x=Especies.de.murciélagos,y=Nro.de.muestras.fecales,fill=Familias.de.plantas))+theme_bw()+geom_bar(stat="identity",position=position_stack())+ggtitle("Bosque Basimontano-Estación húmeda")+theme(plot.title=element_text(lineheight= .8, face="bold",size=25))+theme(axis.title.y=element_text(size=20))+theme(axis.title.x=element_text(size=20))+theme(legend.title=element_text(size=20))+theme(legend.text=element_text(size=15))+theme(axis.text.x=element_text(size=15),axis.text.y=element_text(size=15))+ylim(0,20)+geom_text(aes(label=Nro.de.muestras.fecales),size=5,hjust=0.5,vjust=1,position="stack")


#Dieta de murciélagos en el Bosque montano, durante la estación seca del 2014 del Santuario Nacional Tabaconas Namballe.
dat3<-read.csv("pvsmbmsec3.csv",TRUE,",")
head(dat3)
tail(dat3)
library(ggplot2)
ggplot(data=dat3,aes(x=Especies.de.murciélagos,y=Nro.de.muestras.fecales,fill=Familias.de.plantas))+theme_bw()+geom_bar(stat="identity",position=position_stack())+ggtitle("Bosque Montano-Estación seca")+theme(plot.title=element_text(lineheight= .8, face="bold",size=25))+theme(axis.title.y=element_text(size=20))+theme(axis.title.x=element_text(size=20))+theme(legend.title=element_text(size=20))+theme(legend.text=element_text(size=15))+theme(axis.text.x=element_text(size=15),axis.text.y=element_text(size=15))+ylim(0,20)+geom_text(aes(label=Nro.de.muestras.fecales),size=5,hjust=0.5,vjust=1,position="stack")

#Dieta de murciélagos en el Bosque montano, durante la estación húmeda del 2015 del Santuario Nacional Tabaconas Namballe.
dat4<-read.csv("pvsmbmhum3.csv",TRUE,",")
head(dat4)
tail(dat4)
library(ggplot2)
ggplot(data=dat4,aes(x=Especies.de.murciélagos,y=Nro.de.muestras.fecales,fill=Familias.de.plantas))+theme_bw()+geom_bar(stat="identity",position=position_stack())+ggtitle("Bosque Montano-Estación húmeda")+theme(plot.title=element_text(lineheight= .8, face="bold",size=25))+theme(axis.title.y=element_text(size=20))+theme(axis.title.x=element_text(size=20))+theme(legend.title=element_text(size=20))+theme(legend.text=element_text(size=15))+theme(axis.text.x=element_text(size=15),axis.text.y=element_text(size=15))+ylim(0,20)+geom_text(aes(label=Nro.de.muestras.fecales),size=5,hjust=0.5,vjust=1,position="stack")

#Curva de acumulación de especies de plantas estimadas en cuadrantes instaladas dentro del bosque basimontano del SNTN
bbp<-c(10,5,3,2,4,5,1,1,1,2,1,2,1,1,1,3,1,2,1,1)
sd(bbp)
bbp1<-iNEXT(bbp, q=0, datatype = "incidence_freq")
bbp1
bbp2<-ggiNEXT(bbp1, type = 1, facet.var="none", color.var="order", grey= TRUE)+ labs(x = "Número de cuadrantes", y = "Número de especies", size=18)+ theme(axis.title.x=element_text(colour="black", size=22),axis.title.y=element_text(colour="black", size=22)) + ylim(0,65) + xlim(0,20)
bbp2
#Curva de acumulación de plantas estimadas en cuadrantes instaladas dentro del bosque montano del SNTN
bmp<-c(12,1,1,5,1,2,1,2,3,1,1,2,2,3,1,1,3,1,2,1,1,1,2,1,1,1,2,1,3,1,1,3,1,1,1,3)
sd(bmp)
bmp1<-iNEXT(bmp, q=0, datatype = "incidence_freq")
bmp1
bmp2<-ggiNEXT(bmp1, type = 1, facet.var="none", color.var="order", grey= TRUE)+labs(x = "Número de cuadrantes", y = "Número de especies", size=18)+ theme(axis.title.x=element_text(colour="black", size=22),axis.title.y=element_text(colour="black", size=22))+ ylim(0,65)+ xlim(0,20)
bmp2

#Disponibilidad de frutos de familia de plantas durante la estación seca del 2014 y húmeda del 2015, del Santuario Nacional Tabaconas Namballe.
B<-data.frame(Estación=factor(c("seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda")),Familias=factor(c("Ar","Ar","Ce","Ce","Com","Com","Cle","Cle","Clu","Clu","Cy","Cy","Er","Er","Erit","Erit","Ges","Ges","Mar","Mar","Mel","Mel","Mor","Mor","Mox","Mox","Myr","Myr","Pip","Pip", "Pol","Pol", "Rub", "Rub", "Sol", "Sol", "Ste", "Ste", "Urt","Urt")), levels=c("Ar","Ce","Com","Cle","Clu","Cy","Er","Erit","Ges","Mar","Mel","Mor","Mox","Myr","Pip","Pol","Rub","Sol","Ste","Urt"),         Biomasa=c(23,174,0,0,0,12,1,0,4,11,1,3,10,4,0,0,0,1,0,0,0,0,0,106,3,0,1,1,20,106,0,16,215,0,14,1,3,3,0,75))
B
ggplot(data=B,aes(x=Familias, y= Biomasa, fill=Estación))+ geom_bar(stat="identity",position=position_fill())+ggtitle("Disponibilidad de frutos en cuadrantes")+theme(plot.title=element_text(lineheight = .8, face="bold", size=15))+theme(axis.title.y=element_text(size=12))+theme(axis.title.x=element_text(size=12))+theme(legend.title=element_text(size=12))+theme(legend.text=element_text(size=12))
library(ggplot2)
#Indice de Disponibilidad de familias de plantas del Bosque basimontano, durante la estación seca del 2014 y húmeda del 2015, del Santuario Nacional Tabaconas Namballe
datap<-data.frame(Estacion=factor(c("seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda")),Familias=factor(c("Ar","Ar","Ce","Ce","Co","Co","Erx","Erx","Ges","Ges","Mar","Mar","Pip","Pip","Pol","Pol","Ply","Ply","Ru","Ru","Sol","Sol","Urt","Urt"),levels=c("Ar","Ce","Co","Erx","Ges","Mar","Pip","Pol","Ply","Ru","Sol","Urt")),IndicedeDisponibilidad=c(23,21,4,4,5,10,1,1,1,1,0,0,8,3,1,4,2,2,18,7,1,1,1,4))
datap
ggplot(data=datap,aes(x=Familias, y= IndicedeDisponibilidad, fill=Estacion))+ geom_bar(stat="identity",position=position_fill())+ggtitle("i")+theme(plot.title=element_text(lineheight = .8,face="bold",size=25))+theme(axis.title.y=element_text(size=12))+theme(axis.title.x=element_text(size=12))+theme(legend.title=element_text(size=12))+theme(legend.text=element_text(size=12))
#Indice de Disponibilidad de de familias de plantas del Bosque montano, durante la estación seca del 2014 y húmeda del 2015, del Santuario Nacional Tabaconas Namballe
datap<-data.frame(Estacion=factor(c("seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda","seca","húmeda")),Familias=factor(c("Ac","Ac","Ar","Ar","Cle","Cle","Clu","Clu","Cy","Cy","Er","Er","Ges","Ges","Mel","Mel","Mp","Mp","My","My","Pip","Pip","Ru","Ru","Sol","Sol","St","St","Urt","Urt"),levels=c("Ac","Ar","Cle","Clu","Cy","Er","Ges","Mel","Mp","My","Pip","Ru","Sol","St","Urt")),IndicedeDisponibilidad=c(0,0,3,3,14,6,19,12,2,2,8,6,1,3,3,5,9,11,5,1,12,8,9,29,4,5,9,6,3,3))
datap
ggplot(data=datap,aes(x=Familias, y= IndicedeDisponibilidad, fill=Estacion))+ geom_bar(stat="identity",position=position_fill())+ggtitle("i")+theme(plot.title=element_text(lineheight = .8, face="bold", size=30))+theme(axis.title.y=element_text(size=12))+theme(axis.title.x=element_text(size=12))+theme(legend.title=element_text(size=12))+theme(legend.text=element_text(size=12))

#Indice de disponibilidad de frutos en dos bosques del Santuario Nacional Tabaconas Namballe
a = c(22.92,0,0,0,0,0,0,0,0,0,20.12,0,213,0,0,0)
b = c(174,0,0,0,12.24,0,0,0,105.6,0,0,16.32,0,0.88,0,74.88)
c = c(0.13,1.25,4.4,1.39,0,10.17,0,2.77,0,1,0,0,1.68,14.35,3.06,0)
d = c(0.11,0,11.45,2.77,0,3.64,0.72,0,0,0.15,106.37,0,0,0.36,3.2,0)
dati = list(g1=a, g2=b, g3=c, g4=d)
kruskal.test(dati)
#localidad pueblo libre y chichilapa
a = c(196.92,0,0,0,12.24,0,0,0,105.6,0,20.12,16.32,213,0.88,0,74.88)
b = c(0.24,1.25,15.85,4.16,0,13.81,0.72,2.77,0,1.15,106.37,0,1.68,14.71,6.26,0)
dat=list(g1=a,g2=b)
kruskal.test(dat)
#Estación seca y húmeda
a=c(23.05,1.25,4.4,1.39,0,10.17,0,0,2.77,0,1,20.12,0,214.68,14.35,3.06,0)
b=c(174.11,0,11.45,2.77,12.24,3.64,0,0.72,0,105.6,0.15,106.37,16.32,0,1.24,3.2,74.88)
dat=list(g1=a,g2=b)
kruskal.test(dat)
#Indice de disponibilidad
a=c(23,18,8,5,4,0,0,0,0,0,0,0,0,2,1,1,1,0,1,0,1,0,0,0,0)
b=c(21,7,0,10,4,0,0,0,1,0,0,3,4,2,0,1,0,0,1,0,4,0,0,0,0)
c=c(3,0,0,0,0,19,14,2,0,9,1,0,0,0,0,3,0,4,4,9,3,0,12,9,5)
d=c(3,0,0,0,0,12,6,3,0,6,3,29,0,0,0,5,0,0,5,6,3,0,8,11,1)
dat=list(g1=a,g2=b,g3=c,g4=d)
kruskal.test(dat)

#Comparación de ID de biomasa de frutos con la incidencia de plantas en heces de murciélagos del SNTN durante la estación seca
ID<-c(0.05,0.02,0.14,0.1,0.02,0,0,0.04,0,0,0.02,0.02,0,0.11,0.11,0.07,0,0,0.08,0.03,0.09,0.07)
barplot(ID,
  xlab="Plantas en cuadrantes",
        ylab="Indice de Disponibilidad",
        ylim=c(0,0.3),  names.arg=c("Ara", "Cel", "Cle", "Clu", "Cy", "Cov", "Com", "Eri", "Eup", "Fla", "Ges", "Mel", "Mor", "Mph", "Myr", "Pip", "Pol", "Poy", "Rub", "Sol", "Ste", "Urt"),
        col="skyblue")
par(new=TRUE) 
mur<-c(0.08,0.00,0.00,0.13,0.10,0.00,0.00,0.04,0.00,0.02,0.00,0.00,0.13,0.08,0.00,0.04,0.00,0.00,0.00,0.29,0.00,0.08)
plot(mur,
     axes=FALSE,
     ann=FALSE,
     type="b", lwd=2,col="red",
     ylim=c(0,0.3),
     ylab="semillas"
     ,xlab="")
axis(4,labels=TRUE)

#Comparación de ID de biomasa de frutos con la incidencia de plantas en heces de murciélagos del SNTN durante la estación húmeda
ID<-c(0.03,0,0.09,0.09,0.01,0,0.03,0.03,0.05,0,0.05,0.02,0,0.07,0.07,0.06,0.07,0.03,0,0.03,0.07,0.07)
barplot(ID,
        xlab="Plantas en cuadrantes",
        ylab="Indice de Disponibilidad",
        ylim=c(0,0.3),  names.arg=c("Ara", "Cel", "Cle", "Clu", "Cy", "Cov", "Com", "Eri", "Eup", "Fla", "Ges", "Mel", "Mor", "Mph", "Myr", "Pip", "Pol", "Poy", "Rub", "Sol", "Ste", "Urt"),
        col="skyblue")
par(new=TRUE) 
mur<-c(0.08,0.00,0.00,0.11,0.00,0.00,0.00,0.06,0.00,0.00,0.00,0.00,0.11,0.03,0.00,0.14,0.00,0.00,0.22,0.06,0.00,0.19)
plot(mur,
     axes=FALSE,
     ann=FALSE,
     type="b", lwd=2, col="red",
     ylim=c(0,0.3),
     ylab="semillas"
     ,xlab="")
axis(4,labels=TRUE)
legend("top",legend=c("plantas en murciélagos"),text.col="red", col=c("red"),lty=2,cex=1,bty="n")

#Representación del modelo nulo, analizando la superposición media de todos los posibles pares de especies de murciélagos durante la estación seca del Santuario Nacional Tabaconas Namballe
esnf<-read.csv("esnf.csv",TRUE,",")
head(esnf)
esnf1<-niche_null_model(speciesData=esnf,algo="ra3",metric="pianka",suppressProg=TRUE,nReps=1000)
summary(esnf1)
plot(esnf1)

#Representación del modelo nulo, analizando la superposición media de todos los posibles pares de especies de murciélagos durante la estación húmeda del Santuario Nacional Tabaconas Namballe
ehnf<-read.csv("ehnf.csv",TRUE,",")
head(ehnf)
ehnf1<-niche_null_model(speciesData=ehnf,algo="ra3",metric="pianka",suppressProg=TRUE,nReps=1000)
summary(ehnf1)
plot(ehnf1)
