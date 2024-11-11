# Cargar el archivo 
# Distribuccion de frecuencias 
tabla.bachata<-table(musica$`[Bachata]`) 
tabla.bachata

fr.bachata<-tabla.bachata/101

round(fr.bachata,2)
12+10+25+31+23

#Representacíon de los datos 
hist(musica$`[Bachata]`)

library(ggplot2)

bachata=as.data.frame(musica)

#GRAFICA 
a=ggplot(data = bachata)+aes(x=`[Bachata]`)+geom_histogram(binwidth = 0.5,col="White",fill="#ee6b6e")+
  ggtitle("Valoraciones Musica Bachata")+xlab("Tipo de musica")+ylab("frecuencia")+
  theme(plot.title = element_text(hjust = 0.5,size = 20,face = "bold"))+theme(panel.background = element_rect(fill = "#c2e6fd"))

#Box plot
b=ggplot(data = bachata)+aes(x=`[Bachata]`,y="")+geom_boxplot(col="#ee6b6e",fill="#c2e6fd")+
  geom_jitter()+ggtitle("Valoraciones musica bachata")+xlab("Tipo de musica")+ylab("")

c=ggplot(data = bachata)+aes(x=`[Bachata]`,y=`Edad (en )`)+geom_point()

d=ggplot(data = bachata)+aes(x=`[Bachata]`)+geom_bar(col="black",fill="#ee6b6e")

library(cowplot)
plot_grid(a,b,c,d,ncol=2,nrow=2)


# Media de tendencia central 
#  Media aritmetica 

media.bacha<-round(mean(na.omit(musica$`[Bachata]`)),2)

#mediana

n=length(na.omit(musica$`[Bachata]`))

n

101/2



mediana<-median(na.omit(musica$`[Bachata]`))
mediana 

moda<-table(na.omit(musica$`[Bachata]`))

moda
var.bac<-round(var(na.omit(bachata$`[Bachata]`)))
des.bac<-round(sd(na.omit(bachata$`[Bachata]`)))
cv<-round((des.bac/media.bacha)*100,2)

library(ggplot2)
ggplot(data = bachata)+aes(x=`[Bachata]`)+
   geom_histogram(binwidth = 0.5,fill="white")+
   geom_vline(xintercept=c(media.bacha),size=1,linetype="dashed",col="red")+
  geom_vline(xintercept=c(mediana),size=1,linetype="dashed",col="green")+
  geom_vline(xintercept=c(4),size=1,linetype="dashed",col="blue")+
  theme(panel.background = element_rect(fill = "brown"))


library(moments)

sime<- skewness(na.omit(bachata$`[Bachata]`))
kurtosis<- kurtosis(na.omit(bachata$`[Bachata]`))

x= rnorm(101,mean=3.43,sd=1)
y=dnorm(x,mean = 3.43,sd=1)
df=data.frame(x,y)
ggplot(data = df)+aes(x=x, y=y)+
  geom_line(col="black",size=2)+geom_area(fill="blue")+
  geom_vline(xintercept = c(3.45))+
  ggtitle("valoraciones musica bachata")+xlab("BACHATA")+ylab("")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))+
  theme(panel.background = element_rect(fill = "skyblue"))


# Cálculo de percentiles
percentiles_bachata <- quantile(na.omit(musica$`[Bachata]`), probs = seq(0, 1, 0.01))

# Mostrar los percentiles específicos 
percentiles_especificos <- quantile(na.omit(musica$`[Bachata]`), probs = c(0.25, 0.50, 0.75))

# Ver el resultado
percentiles_bachata
percentiles_especificos


#cruce de variables
edad.1<-numeric()
edad.1[musica$`Edad (en )`>=10 & musica$`Edad (en )`<20]<-1
edad.1[musica$`Edad (en )`>=20 & musica$`Edad (en )`<35]<-2
edad.1[musica$`Edad (en )`>=35 & musica$`Edad (en )`<45]<-3
edad.1[musica$`Edad (en )`>=45 ]<-4
tab<-table(edad.1, musica$`[Bachata]`)

rownames(tab)<-c("10 y 20 años","20 y 30 años", "35 y 45 años","45 o más")
addmargins(tab,1)
addmargins(tab,2)
ggplot(data=bachata)+aes(x=edad.1, y=bachata$`[Bachata]`, colour = Genero)+
  geom_point(size=5)+
  ggtitle("valoraciones musica bachata")+
  xlab("edad")+ylab("bachata")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))+
  theme(panel.background = element_rect(fill = "brown"))