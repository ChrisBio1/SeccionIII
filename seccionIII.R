# NOTA encontré como no disponible la pestaña "indicadores" de la liga DENUE

library(magrittr)
c("dplyr", "tidyr", "stringr", "lubridate","ggplot2") %>% 
  sapply(require, character.only = T)


educacion <- read.csv("Datos/denue_inegi_61_.csv", header = T)


capitalizar <- function(x){
  unlist(
    lapply(strsplit(x, ""), function(x){
    paste0(toupper(x[1]), 
         paste0(tolower(x[-1]), collapse = ""))
    }
  )
)
}

### Secundarias ----
# ¿Cuál es la proporción de escuelas de educación
# secundaria privadas y del sector público para la
# Ciudad de México, el estado de Puebla y Jalisco?

secundarias <- educacion %>%
  select(nombre_act, entidad) %>%
  mutate(entidad = gsub(" \\s+", "", entidad)) %>%
  mutate(filtrar = str_detect(nombre_act, "secundar")) %>%
  filter(filtrar == TRUE) %>%
  filter(entidad %in% c("CIUDAD DE MÉXICO", "JALISCO", "PUEBLA")) %>%
  mutate(tipo = ifelse(str_detect(nombre_act, "privad") == T, "Privado", "Público")) %>%
  group_by(entidad,tipo) %>% 
  tally %>% 
  spread(tipo,n) %>% 
  mutate(total = Privado+Público) %>% 
  gather(tipo, n, -total, -entidad) %>%
  data.frame() %>% 
  mutate(entidad = capitalizar(entidad)) %>%
  mutate(porcentaje = round(n/total*100, 2))



# png("Graficas/fig1a.png", width = 1000, height = 800, res = 150)
secundarias %>% 
  mutate(tipo = ifelse(tipo == "Privado", "2Privado", "1Público")) %>% 
  ggplot(aes(x = "", y = porcentaje, fill = tipo)) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(y = (porcentaje/2) ,
                label = paste(porcentaje, "%", "\n", paste0("(",n,")")), 
                size=5)) +
  coord_polar(theta = "y", start = 0) +
  facet_wrap(~entidad) +
  theme_minimal()+
  theme(axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.text.x= element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.title = element_blank(),
    legend.position="bottom",
    legend.text = element_text(colour = "black"),
    strip.text.x = element_text(size=15),
    plot.title=element_text(size=14, face="bold")) +
  guides(size = F,
         fill = guide_legend(title = "")) +
  ggtitle("Proporción de escuelas de educación secundaria \n privadas y del sector público")
# dev.off()


### Educación superior ----
# ¿Cuáles son los 20 municipios con mayor 
# número de escuelas de educación superior 
# en el país y a cuál estado pertenecen?
  
superior <- educacion %>% 
  select(nombre_act, entidad, municipio) %>%
  mutate(entidad = gsub(" \\s+", "", entidad)) %>%
  mutate(filtrar = str_detect(nombre_act, "superior")) %>%
  filter(filtrar == TRUE) %>% 
  group_by(entidad, municipio) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  data.frame() %>% 
  mutate(x = 1:length(entidad)) %>%
  head(20)

# png("Graficas/fig1b.png", width = 2200, height = 1500, res = 150)
par(bty = "n")
plot(superior$x, superior$n, 
     ylim = c(130, 650),
     xlim = c(1,24),
     type ="n", 
     las = 1,
     xlab = "",
     ylab = "",
     xaxt = "n")
points(superior$x, superior$n, 
       pch = 16, 
       col = "steelblue",
       cex = 2)
points(superior$x, superior$n, 
       type ="l")
text(superior$x, superior$n,
     paste(paste0(superior$municipio, ","),
           capitalizar(superior$entidad),
           paste0("(",superior$n,")")), 
     pos = 4, 
     cex = 1.3,
     srt = 45)
mtext("Número de escuelas", 2, 2.5, cex = 1.5)
mtext("Educación superior en el país", 3, 0, cex = 1.5, font = 2)
# dev.off()




### PIB ------
pib <- read.csv("Datos/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_10475113.csv", 
                header = T, sep = ",")
pob <- read.csv("Datos/API_SP.POP.TOTL_DS2_en_csv_v2_10473719.csv", 
                header = T, sep = ",")

pib2 <- pib %>% 
  select(-Indicator.Name, -Indicator.Code, -X2018, -Country.Name) %>% 
  gather(anio, pib, -Country.Code) %>% 
  mutate(id = paste0(Country.Code, anio)) %>% 
  select(-Country.Code, -anio)


pob2 <- pob %>% 
  select(-Indicator.Name, -Indicator.Code, -X2018) %>% 
  gather(anio, pob, -Country.Name, -Country.Code) %>% 
  mutate(id = paste0(Country.Code, anio))

pib2 %>% dim()
pob2 %>% dim()

general <- merge(pib2, pob2, by = "id") %>%
  mutate(anio = as.numeric(as.character(gsub("[X]", "", anio))))

### PIB MÉXICO 2050 -----
mexico <- general %>% 
  filter(Country.Code == "MEX") %>%
  mutate(pib = pib/1e+11) %>%
  data.frame()

# Dada la forma de la curva en la representación gráfica
# del PIB en función del tiempo opté por ajustar una regresión
# logística y exponencial

modeloEX <- nls( pib ~ SSfpl(anio, a, b, c, d ), mexico)
modelo <- nls( pib ~ SSlogis(anio, a, b, c ), mexico)
anova(modeloEX, modelo)
AIC(modeloEX) 
AIC(modelo)

# Al comparar los valores de AIC encuentro que el mejor modelo
# es el exponencial, con el modelo logístico el valor del PIB 
# para el 2050 fue sobreestimado respecto al resultado del modelo
# exponencial.

x <-seq(min(mexico$anio),max(mexico$anio),1)
y <-predict(modeloEX,list(anio=x))

x2<-seq(1960,2050,1)
y2<-predict(modeloEX,list(anio=x2))

# png("Graficas/fig4a.png", width = 2000, height = 1800, res = 250)
par(bty = "n")
plot(mexico$anio, mexico$pib, 
     xlim = c(1960, 2070), 
     ylim = c(min(mexico$pib), max(y2)+1),
     xlab = "",
     ylab = "", 
     type = "n",
     las = 1)
points(mexico$anio, mexico$pib, pch = 16)
lines(x2, y2, 
      col = "red", 
      lty = 3, 
      lwd = 2)
lines(x, y, 
      col = "steelblue", 
      lwd = 4)
points(max(x2), max(y2), 
       cex = 2, 
       pch = 16, 
       col = "red")
abline(v = 2050, lty = 3)
mtext("Años", 1, 2.5, cex = 1.5)
mtext("PIB US$ (MIles de millones)", 2, 2.5, cex = 1.5)
mtext("PIB en México", 3, 2, cex = 1.5)
text(max(x2), max(y2), 
     paste("$", round(max(y2),2)),
     pos = 4)
# dev.off()


### CHINA SUPERARÁ A EU PIB ----
# general %>% 
#   filter(Country.Name %in% c("China", "United States")) %>% 
#   ggplot(aes(x = anio, y = pib, col = Country.Name)) +
#   geom_point() +
#   geom_line() +
#   # facet_wrap(~Country.Name) +
#   theme_classic()
# 
# cu <- general %>% 
#   filter(Country.Name %in% c("China", "United States")) %>% 
#   mutate(pib = pib/1e+11) %>%
#   data.frame()
# 
# 
# china <- general %>% 
#   filter(Country.Name == "China") %>%
#   mutate(pib = pib/1e+11) %>%
#   data.frame()
# 
# eu <- general %>% 
#   filter(Country.Name =="United States") %>% 
#   mutate(pib = pib/1e+11) %>%
#   data.frame()
# 
# modeloEU <- nls(pib ~ SSfpl(anio, a, b, c, d), eu)
# nls(pib ~ SSlogis(anio, a, b, c), eu)
# 
# xEU<-seq(1960,2017,1)
# yEU<-predict(modeloEU,list(anio=xEU))
# 
# x2EU<-seq(1960,2050,1)
# y2EU<-predict(modeloEU,list(anio=x2EU))
# 
# 
# modeloCH <- nls(pib ~ SSfpl(anio, a, b, c, d), china)
# 
# nls(pib ~ a-b*exp(-c*anio),start=list(a=120,b=2007,c=0.05), china)
# 
# xCH <- seq(1960, 2017, 1)
# yCH <- predict(modeloCH, list(anio = xCH))
# 
# 
# plot(cu$anio, cu$pib, xlim = c(1960, 2050), ylim = c(0,300))
# points(china$anio, china$pib, pch = 16, col = "blue")
# points(eu$anio, eu$pib, pch = 16)
# lines(xEU,yEU, col = "red", lwd = 3)
# lines(x2EU,y2EU, col = "red", lwd = 3, lty = 3)

#


### Población mundial -----
mundo <- general %>% 
  filter(Country.Name == "World")

mundo %>% 
  ggplot(aes(x = anio, y = pob)) +
  geom_point() +
  theme_classic()

# Al observar la forma gráfica observo una tendencia lineal
# sin embargo al ajustar la pendiente predictora los primeros 
# valores se dispersan del origen, por lo que opto por ajustar
# un modelo exponencial

modeloMU <- nls( pob ~ SSfpl(anio, a, b, c, d), mundo)
modeloMU2 <- lm( mundo$pob ~ mundo$anio)

AIC(modeloMU)
AIC(modeloMU2)

# Al comparar los valores de AIC de un modelo lineal y el 
# exponencial encuentro que el modelo exponencial presenta un 
# valor mínimo de AIC por lo que opto por este modelo.

xMU <-seq(min(mundo$anio),max(mundo$anio),1)
yMU <-predict(modeloMU,list(anio=xMU))

x2MU <-seq(1960,2150,1)
y2MU <-predict(modeloMU,list(anio=x2MU))

supera <- data.frame(x2MU, y2MU) %>% 
  filter(y2MU >= 11000000000) %>% 
  .[1,]

# png("Graficas/fig5a.png", width = 2000, height = 1800, res = 250)
par(bty = "n")
plot(mundo$anio, mundo$pob, 
     ylim = c(min(mundo$pob), 5e+09+max(mundo$pob)),
     xlim = c(1960, 2150),
     las = 1,
     ylab = "", 
     pch = 16,
     xlab = "")
abline(h = 11e+09, lty= 3)
lines(x2MU, y2MU, 
      col = "red", lwd = 3, lty = 3)
abline(v = supera[1], lty = 3)
points(supera, col = "red", 
       pch = 16, cex = 2)
text(supera[1], supera[2], 
     paste("Año", supera[1]), pos = 3)
mtext("Años", 1, 2.5)
mtext("Población mundial \n (Miles de millones de personas)", 
      3, 1, cex = 1.5)
# dev.off()

max(mundo$pob)

#

### Top 10 países poblados -----
# plot(general$anio, general$pob)
# 
# 
# paises <- list()
# for(i in levels(general$Country.Name)){
#   paises[[i]] <- data.frame(pob = general$pob[general$Country.Name == i],
#              anio = general$anio[general$Country.Name == i],
#              pais = general$Country.Name[general$Country.Name == i])
# }
# 
# 
# paises %>% head()
# 
# paises2 <- lapply(paises, function(x){
#   unique(data.frame(pobM = max(x[1]), x[3]))
# }) %>%  
#   do.call("rbind", .) %>% 
#   data.frame() # %>%  
#   # arrange(desc(pobM)) %>% 
#   # head(100)
# 
# paises2 %>% 
#   mutate(extraer = str_detect(pais, " \\S+ \\S+")) %>% 
#   filter(extraer != TRUE) %>%
#   arrange(desc(pobM)) %>% 
#   head(20)

#

### Sección 3:Análisis General -----
maxi <- read.csv("Datos/testmaxi.csv", header = T)

maxi %>% head()
  separate(start_at, c("fechaI", "horaI")," ") %>% 
  separate(end_at, c("fechaF", "horaF"), " ") %>%
  mutate(horaI = hours(horaI)) %>% 
  head()


  
