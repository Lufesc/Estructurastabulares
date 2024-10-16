# ===========================
# Estructuras tabulares   
# ===========================
# Instructor: Luis Fernando Escobar
# Universidad Autónoma Gabriel René Moreno
# Business School

# Carga de paqueterias
library(haven)
library(tidyverse)
library(summarytools)
library(psych)
library(stats)
library(normtest)
library(nortest)
library(moments)

EH2019 = read_sav("EH2019_Persona.sav")
View(EH2019)

# Convirtiendo a factor, para mayor comodidad de manipulación
EH2019_Persona = as_factor(EH2019)
attach(EH2019_Persona)

# Distribución por Area

class(area)
table(area)
tab_area= 
  transform(table(area), freq_rel= prop.table(Freq)*100,
            freq_acum= cumsum(Freq), cum_rel = cumsum(prop.table(Freq)*100))

tab_area 
  
# Gráfico      
piepercent <- round(prop.table(table(area))*100,2)
pie(table(area), 
    main="Distribución por Area", # título
    col=c("red","blue"), # damos color a los sectores
    labels=piepercent)
legend("topright", c("Urbano", "Rural"), cex=0.8, fill=c(2,4))        


# Nivel de educación
table(niv_ed)
tab_niv_ed= 
  transform(table(niv_ed), freq_rel= prop.table(Freq)*100,
            freq_acum= cumsum(Freq), cum_rel = cumsum(prop.table(Freq)*100))

tab_niv_ed

tablaNivEdu = freq(niv_ed)

tablaNivEdu


# Grafico
barplot(table(niv_ed))

educacion= EH2019_Persona$niv_ed
educacion= na.omit(educacion)

prop_edu = prop.table(table(niv_ed)*100)

barplot(prop_edu,  main = "Niveles de Educación Específico",
        xlab = "Nivel educativo", ylab = "Proporción", 
        col = c("royalblue", "grey"))


# Niveles de Educación General
table(niv_ed_g)
tab_niv_ed_g= 
  transform(table(niv_ed_g), freq_rel= prop.table(Freq)*100,
            freq_acum= cumsum(Freq), cum_rel = cumsum(prop.table(Freq)*100))

tab_niv_ed_g

# Gráfico
prop_edu_g = prop.table(table(niv_ed_g)*100)

barplot(prop_edu_g,  main = "Niveles de Educación General",
        xlab = "Nivel educativo", ylab = "Proporción", 
        col = c("royalblue", "red"))



# Numero de Personas por Hogar
table(nro)
tab_numeropers= 
  transform(table(nro), freq_rel= prop.table(Freq)*100,
            freq_acum= cumsum(Freq), cum_rel = cumsum(prop.table(Freq)*100))

tab_numeropers

#Grafico 
prop_nropers = prop.table(table(nro)*100)

barplot(prop_nropers,  main = "Numero de Personas por Hogar",
        xlab = "Numero de personas", ylab = "Proporción", 
        col = c("royalblue"))


# Ingreso del Hogar per Capita
table(yhogpc)
tab_ing_hogpc= 
  transform(table(yhogpc), freq_rel= prop.table(Freq)*100,
            freq_acum= cumsum(Freq), cum_rel = cumsum(prop.table(Freq)*100))

tab_ing_hogpc

# Grafico
hist(x = yhogpc, main = "Histograma de Ingreso del Hogar per Capita", 
     xlab = "Ingreso per capita", ylab = "Frecuencia",
     col = "ivory")


# Subgrupo de variables

EH2019_base <- select(EH2019_Persona, depto, area, nro, s02a_02, s02a_03, niv_ed_g, aestudio, tothrs, ylab, yhog, yhogpc)


# Dispersión

plot(x = EH2019_base$aestudio, y = EH2019_base$ylab, col = c("black", "red"), 
     main = "Ingreso Laboral y Años de Estudio", xlab = "Años de Estudio", ylab = "Ingreso")
legend(x = "topleft", legend = c("Años de Estudio", "Ingreso Laboral"), 
       fill = c("black", "red"))


# Continuación...
plot(x = EH2019_base$aestudio, y = EH2019_base$ylab, col = EH2019_base$s02a_02, 
     main = "Ingreso Laboral y Años de Estudio", xlab = "Años de Estudio", ylab = "Ingreso")
legend(x = "topleft", legend = c("Hombre", "Mujer"), 
       fill = c("black", "red"), title = "Genero")

str(EH2019_base$s02a_02) #el primer nivel siempre esta en color negro, y como hombre es el primer nivel los puntos negros corresponden a los hombres.

# Continuación...
plot(x = EH2019_base$aestudio, y = EH2019_base$ylab, col = EH2019_base$depto, 
     main = "Ingreso Laboral y Años de Estudio", xlab = "Años de Estudio", ylab = "Ingreso")
legend(x = "topleft", legend = c("Chuquisaca", "La Paz", "Cochabamba", "Oruro", "Potosí", "Tarija", "Santa Cruz", "Beni", "Pando"), 
       fill = c(1, 2, 3, 4, 5, 6, 7, 8, 9), title = "Genero", cex=0.6)
summary(EH2019_base$depto) #para ver las etiquetas y ponerlas en legend

