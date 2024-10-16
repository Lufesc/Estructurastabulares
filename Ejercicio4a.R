# ===========================
# Estructuras tabulares   
# ===========================
# Instructor: Luis Fernando Escobar
# Universidad Autónoma Gabriel René Moreno
# Business School

# Funciones 
# ============

#remove(x)
#rm(y)
#write.csv(z,"output.csv")

#                 Funciones Matemáticas
#sqrt(x)		Raíz de x
#exp(x)		Exponencial de x
#log(x)		Logaritmo natural de x
#log10(x)	Logaritmo base 10
#sum(x)		Suma de los elementos de x
#prod(x)		Producto de los elementos de x
#sin(x)		Seno
#cos(x)		Coseno
#tan(x)		Tangente
#round(x,n)	Redondea a n dígitos
#cumsum(x)	Calcula las sumas acumuladas (x1,x1+x2,+x1+…+xn)

#                 Funciones Estadísticas
#mean(x)	Media
#sd(x)		Desviación estándar
#var(x)		Varianza
#median(x)	Mediana
#quantiles(x)	Quantiles
#cor(x,y)		Correlación
#max(x)		Valor máximo
#min(x)		Valor mínimo
#range(x)	Retorna el máximo y mínimo
#sort(x)		Ordena los elementos de x
#summary	Resumen de las variables
#choose(n,k)	Combinatoria de n sobre k

# Data Frames
# ===============

df <- data.frame(Nombres = c('Juan','Pedro','Ana','Delfina'),
                 Edad = c(21,46,58,27),
                 EstadoCivil = c('Soltero','Casado','Casado','Soltero'),
                 SecundarioCompleto = c(TRUE, FALSE, TRUE, TRUE))
df

str(df) # Resumen del objeto


# Ejemplo 
# Un inspector de seguridad industrial, obtuvo la siguiente información de  las causas principales por las cuales ocurre un accidente grave en una  empresa. Los datos se muestran a continuación
# ===============================================================================================================================================================================================

Accidente<-c("D", "O", "S", "M", "D", "O", "S", "D", "O", "S", "S", "O", "M", "D", "O", "F", "M", "F", "M", "O", "M", "D", "S", "D", "S", "S", "D", "S", "D", "O", "D", "S", "M", "D", "O", "D", "F", "D", "O", "S")
table(Accidente)
prop.table(table(Accidente))*100

#Creando tabla de frecuencia relativas y acumuladas
tabla_freq <- 
  transform(table(Accidente), Rel_freq=prop.table(Freq)*100, 
            Cum_Freq=cumsum(Freq), Cum_Rel=cumsum(prop.table(Freq))*100)
print(tabla_freq)

barplot(table(Accidente))

barplot(prop.table(table(Accidente)),
        main = "Diagrama de barras de frecuencias relativas\n sobre causas de \"accidentes industriales\"")


# Datos de recursos humanos
# ===============================
install.packages("readr")
library(readr)

Data <- read_csv("Data.csv")
View(Data)
attach(Data)
names(Data)

# analisis de una variable cualitativa nominal 
tabla_freq_cargo <- 
  transform(table(CARGO), Rel_freq=prop.table(Freq)*100, 
            Cum_Freq=cumsum(Freq), Cum_Rel=cumsum(prop.table(Freq)*100))

sink("descrip_tab_cargo.text")
print(tabla_freq_cargo)
sink()

barplot(table(CARGO))
pie(table(CARGO))

# analisis de una variable cuantitativa discreta
table(HIJOS)
tabla_freq_hijos <- 
  transform(table(HIJOS), Rel_freq=prop.table(Freq)*100, 
            Cum_Freq=cumsum(Freq), Cum_Rel=cumsum(prop.table(Freq)*100))

sink("descrip_tab_hijos.text")
print(tabla_freq_hijos)
sink()

barplot(table(HIJOS))

# analisis de una variable cuantitativa continua
table(SUELDO)
range(SUELDO)

#Creamos una lista de límites de clase
límites <- seq(1000,3500,by=500)

#Agrupamos la data en los límites de clase
frecuencia_sueldos <- cut(Data$SUELDO, límites)

tabla_freq_sueldos <- 
  transform(table(frecuencia_sueldos), Rel_freq=prop.table(Freq)*100,
            Cum_Freq=cumsum(Freq), Cum_Rel=cumsum(prop.table(Freq)*100))

sink("descrip_tab_sueldo.text")
print(tabla_freq_sueldos)
sink()

hist(SUELDO)

jpeg("histogramas_sueldos.jpg")
hist(SUELDO,  col="blue",  5, xlab  =  "Sueldo",  ylab  =  "Frecuencia", main=  "Histograma del sueldo de los trabajadores")
graphics.off()


# Distribucion de frecuencias
# ===============================
install.packages("summarytools")
library(summarytools)

tab.cargo <- freq(Data$CARGO)
tab.cargo

write.csv(tab.cargo,"output.csv")
sink("tab.cargo.text")
print(tab.cargo)
sink()

# Graficos circulares 
# ===============================
sexo=factor(c("F","M","F","F","F","M","M","F","F","F","F","F","M","M","M","F","M","M",
              "F","F","F","F","F","M"))

tab.sexo <- freq(sexo)
tab.sexo

# aplicamos colores
pie(table(sexo), 
    main="Distribución por género", # título
    col=c("red","blue"), # damos color a los sectores
    labels=c("Mujer", "Hombre"))
# otra representación con tantos por ciento
piepercent <- round(prop.table(table(sexo))*100,2)
pie(table(sexo), 
    main="Distribución por género", # título
    col=c("red","blue"), # damos color a los sectores
    labels=piepercent)
legend("topright", c("Mujer", "Hombre"), cex=0.8, fill=c(2,4)) # añadimos la leyenda al gráfico




