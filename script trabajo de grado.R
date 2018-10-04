#Anteproyecto
#Kevin García - 1533173
#Alejandro Vargas - 1525953

#Simulación viveros
X1<-matrix(0,nrow = 24,ncol = 6)
X2<-matrix(0,nrow = 24,ncol = 6)
X3<-matrix(0,nrow = 24,ncol = 6)
X4<-matrix(0,nrow = 24,ncol = 6)
X5<-matrix(0,nrow = 24,ncol = 6)
X6<-matrix(0,nrow = 24,ncol = 6)
X7<-matrix(0,nrow = 24,ncol = 6)
X8<-matrix(0,nrow = 24,ncol = 6)

Vivero<-matrix(c(X1,X2,X3,X4,X5,X6),nrow = 48,ncol = 24)
#funcion infectar lotes 
remplazardatos=function(b,datos){
  N=length(datos[1,])*length(datos[,1]) #Total de plantas en la cama o en el lote
  a=floor(N*b)  #Se aproxima al entero por debajo
  mustcol1=sample(c(1:length(datos[1,])),a,replace = T)
  mustrow1=sample(c(1:length(datos[,1])),a,replace = T)
  matr=datos
  for(i in 1:a){
    matr[mustrow1[i],mustcol1[i]]=1
  }
  
  return(matr)  
}

X1inf<-remplazardatos(0.001,X1) #Cama 1 infectada con el 50%
viveroinf<-remplazardatos(0.01,Vivero) #Vivero con el 1% de infectados

sample(X1inf,5) #MAS para la cama 1 infectada
#genero mil mustras
vectorsuma=c()
for (i in 1:1000) {
  vectorsuma=c(vectorsuma,sum(sample(X1inf,5)))
  
}
X11()
hist(vectorsuma)

x11()
plot(table(vectorsuma),type = "h")
#MSIS para la cama 1 infectada
lv<-c("l","r","u","d")   #Lados de la cama l:izquierdo, r:derecho, u:arriba, d:Abajo
muestralado<-sample(lv,1)
sample(1:length(X1[,1]),1)  
salto<-floor(length(X1)/20)
aumentos<-c(130)
vectorsuma2=c()
for (i in 1:1000) {
  muestrasis=c()
  coorsaltos=c(sample(matriposiciones,1))
  for (j in 1:20) {
    coorsaltos=c(coorsaltos,coorsaltos[j-1]+salto)
    if(coorsaltos[j]>144){
      coorsaltos[j]=coorsaltos[j]-144
    }
  
  }
  muestrasis=X1inf[coorsaltos]
  vectorsuma2=c(vectorsuma2,sum(muestrasis))
}
X11()
hist(vectorsuma2,xlim = c(3,11))

x11()
plot(table(vectorsuma2),type = "h")

muestrasis=X1inf[coorsaltos]

matriposiciones=matrix(1:144,24,6)
vectorcoor=c(matriposiciones[,1],matriposiciones[,6],matriposiciones[24,2:4],matriposiciones[1,2:4])
sample(vectorcoor,1)
pbinom(7,14,0.5)