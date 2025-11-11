library(here)
library(summarytools)
library(moments)
library(dplyr)
library(ggplot2)

data <- read.csv(here("DataSet", "realestate_texas.csv"), sep = ",", fileEncoding = "Latin1")
#View(data)
attach(data)

#2.	Calcola Indici di posizione, variabilità e forma per tutte le var. per le quali ha senso farlo
# per le altre crea una distribuzione di frequenza
summary(data)
summary(data[, -c(2,3)])

### Variabile City
#Tabella di distribuzione city
freq_ass_city <- table(city)
freq_rel_city <-  prop.table(freq_ass_city) #oppure freq_ass_city / sum(freq_ass_city)
(distr_freq_city <- cbind(freq_ass_city, freq_rel_city))
#La distribuzione è quadrimodale

# Calcolo l'indice di Gini
gini.index <- function(x){
  ni=table(x)
  fi=ni/length(x)
  fi2= fi^2
  J = length(table(x))
  
  gini = 1-sum(fi2)
  gini.normalizzato = gini/((J-1)/J)
  
  return(gini.normalizzato)
}
gini.index(city)
gini.index(volume)


#Tabella di contingenza year e month
table(year, month)/sum(freq_ass_city)

###########################
descr(data)

#funzione per il calcolo della moda
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
###########################
#sales:
media_sales <- mean(sales)
Std.Dev_sales <- sd(sales)
min_sales <- min(sales)
Q1_sales <- (sort(sales)[60]+sort(sales)[61])/2
mediana_sales <- median(sales)
Q3_sales <- (sort(sales)[180]+sort(sales)[181])/2
max_sales <- max(sales)
IQR_sales <- IQR(sales)
Coeff_variaz_sales <- sd(sales)/mean(sales)*100
Indice_Fisher <- skewness(sales)
Curtosi <- kurtosis(sales) -3
moda_sales <- Mode(sales)
Varianza_sales <- sd(sales)^2
Range_variazione <- max(sales)-min(sales)

#volume:
media_volume <- mean(volume)
Std.Dev_volume <- sd(volume)
min_volume <- min(volume)
Q1_volume <- (sort(volume)[60]+sort(volume)[61])/2
mediana_volume <- median(volume)
Q3_volume <- (sort(volume)[180]+sort(volume)[181])/2
max_volume <- max(volume)
IQR_volume <- IQR(volume)
Coeff_variaz_volume <- sd(volume)/mean(volume)*100
Indice_Fisher <- skewness(volume)
Curtosi <- kurtosis(volume) -3
moda_volume <- Mode(volume)
Varianza_volume <- sd(volume)^2
Range_variazione <- max(volume)-min(volume)

#median_price:
media_median_price <- mean(median_price)
Std.Dev_median_price <- sd(median_price)
min_median_price <- min(median_price)
Q1_median_price <- (sort(median_price)[60]+sort(median_price)[61])/2
mediana_median_price <- median(median_price)
Q3_median_price <- (sort(median_price)[180]+sort(median_price)[181])/2
max_median_price <- max(median_price)
IQR_median_price <- IQR(median_price)
Coeff_variaz_median_price <- sd(median_price)/mean(median_price)*100
Indice_Fisher <- skewness(median_price)
Curtosi <- kurtosis(median_price) -3
moda_median_price <- Mode(median_price)
Varianza_median_price <- sd(median_price)^2
Range_variazione <- max(median_price)-min(median_price)

#listings:
media_listings <- mean(listings)
Std.Dev_listings <- sd(listings)
min_listings <- min(listings)
Q1_listings <- (sort(listings)[60]+sort(listings)[61])/2
mediana_listings <- median(listings)
Q3_listings <- (sort(listings)[180]+sort(listings)[181])/2
max_listings <- max(listings)
IQR_listings <- IQR(listings)
Coeff_variaz_listings <- sd(listings)/mean(listings)*100
Indice_Fisher <- skewness(listings)
Curtosi <- kurtosis(listings) -3
moda_listings <- Mode(listings)
Varianza_listings <- sd(listings)^2
Range_variazione <- max(listings)-min(listings)

#months_inventory:
media_months_inventory <- mean(months_inventory)
Std.Dev_months_inventory <- sd(months_inventory)
min_months_inventory <- min(months_inventory)
Q1_months_inventory <- (sort(months_inventory)[60]+sort(months_inventory)[61])/2
mediana_months_inventory <- median(months_inventory)
Q3_months_inventory <- (sort(months_inventory)[180]+sort(months_inventory)[181])/2
max_months_inventory <- max(months_inventory)
IQR_months_inventory <- IQR(months_inventory)
Coeff_variaz_months_inventory <- sd(months_inventory)/mean(months_inventory)*100
Indice_Fisher <- skewness(months_inventory)
Curtosi <- kurtosis(months_inventory) -3
moda_months_inventory <- Mode(months_inventory)
Varianza_months_inventory <- sd(months_inventory)^2
Range_variazione <- max(months_inventory)-min(months_inventory)

# 4. Dividi una delle variabili quantitative in classi, scegli tu quale e come
# costruisci la distribuzione di frequenze, il grafico a barre corrispondente
# infine calcola l’indice di Gini.
volume_cl <- cut(volume, breaks =seq(from = 5, to=85, by=10))

n = length(volume_cl)
ni <- table(volume_cl) #freq. assolute
fi <- ni/n #freq. relative
Ni <- cumsum(ni) #freq. assolute cumulate
Fi <-  Ni/n #freq. relative cumulate

(distr_freq <- as.data.frame(cbind(ni,fi,Ni,Fi)))

###Grafico a barre
barplot(distr_freq$ni,
        main = "Distribuzione delle classi di Volume",
        xlab = "classi di volume delle vendite (in milioni di $)",
        ylab = "frequenze assolute",
        ylim = c(0,70),
        col = "lightgreen",
        names.arg = rownames(distr_freq))

barplot(distr_freq$fi,
        main = "Distribuzione delle classi di Volume",
        xlab = "classi di volume delle vendite (in milioni di $)",
        ylab = "frequenze relative",
        col = "lightblue",
        names.arg = rownames(distr_freq))

# Per calcolare l'indice di Gini partiamo dalle frequenze relative della variabile volume_cl e le eleviamo al quadrato
fi2 <- fi^2
fi2
sum(fi2)
J = length(table(volume_cl)) #numero di classi, 8 nel nostro caso
indice_gini = 1-sum(fi2) #0.8163
indice_gini.normalizzato = indice_gini/((J-1)/J) #0.932
#L'indice è molto alto, quindi c'è molta eterogeneità nella distribuzione delle variabili
# Guardando la distribuzione in classi non mi torna

# 7. Esiste una colonna col prezzo mediano, creane una che indica invece il prezzo medio, utilizzando le altre var che hai a disposizione
mean_price <- volume/sales*1000000
data <-cbind(data, mean_price)

# oppure:
# data <- data %>% 
#   mutate(mean_price = volume/sales*1000000)

# 8.Prova a creare un’altra colonna che dia un’idea di “efficacia” degli annunci di vendita. Riesci a fare qualche considerazione?
data <- data %>% 
  mutate(efficacy_listing = sales/listings*100)

# 9. Prova a creare dei summary(), o semplicemente media e deviazione standard
# di alcune variabili a tua scelta, condizionatamente alla città, agli anni e ai mesi

summary_city <- data %>%
  group_by(city) %>%
  summarise(media_volume=mean(volume),
            sd_volume=sd(volume),
            curt_volume=kurtosis(volume)-3,
            media_sales=mean(sales),
            sd_sales=sd(sales),
            curt_sales=kurtosis(sales)-3)


summary_year <- data %>%
  group_by(year) %>%
  summarise(media_volume=mean(volume),
            sd_volume=sd(volume),
            curt_volume=kurtosis(volume)-3,
            media_sales=mean(sales),
            sd_sales=sd(sales),
            curt_sales=kurtosis(sales)-3)


summary_month <- data%>%
  group_by(year) %>%
  summarise(media_volume=mean(volume),
            sd_volume=sd(volume),
            curt_volume=kurtosis(volume)-3,
            media_sales=mean(sales),
            sd_sales=sd(sales),
            curt_sales=kurtosis(sales)-3)

summary_city
summary_year
summary_month

library(ggplot2)
# 10.	Utilizza i boxplot per confrontare la distribuzione del prezzo mediano delle case tra le varie città. 
# boxplot(median_price~city)
ggplot(data)+
  geom_boxplot(aes(x = city,
                   y = median_price
                   #,fill = city
                   ),
               outlier.color = "red")+
  theme(legend.position="bottom")

# 11. Utilizza i boxplot o qualche variante per confrontare la distribuzione 
# del valore totale delle vendite tra le varie città ma anche tra i vari anni. 
ggplot(data, aes(x=city, y=volume, fill = factor(year)))+
  geom_boxplot()+
  theme(legend.position = "bottom")

#oppure usando facet wrap
ggplot(data, aes(x=city, y=volume, fill = factor(year)))+
  geom_boxplot()+
  theme(legend.position = "none")+
  facet_wrap(~year, scales='free')

# 12. Usa un grafico a barre sovrapposte per confrontare il totale delle vendite nei vari mesi, sempre considerando le città. 
# Prova a commentare ciò che viene fuori. Già che ci sei prova anche il grafico a barre normalizzato. 
# Consiglio: Stai attento alla differenza tra geom_bar() e geom_col().

#costruzione del grafico a barre sovrapposte
ggplot(data) +
  stat_summary(aes(x = factor(month),
                   y = sales, 
                   fill = city),
               fun = sum,
               geom = "bar",
               position = "stack")+
  labs(title = "Totale delle vendite mensili per città",
       x = "mesi",
       y = "totale vendite(in milioni di $")+
  scale_x_discrete(breaks = seq(0, 12, 1))+
  theme_classic()+
  theme(legend.position = "bottom")
  #+facet_wrap(~year)

# There are two types of bar charts: geom_bar() and geom_col(). 
# geom_bar() makes the height of the bar proportional to the number of cases in each group 
# If you want the heights of the bars to represent values in the data, use geom_col() instead

ggplot(data) +
  geom_col(aes(x = factor(month),
               y = sales, 
               fill = city),
               position = "stack")+
  labs(title = "Totale delle vendite mensili per città",
       x = "mesi",
       y = "totale vendite(in milioni di $)")+
  theme_classic()+
  theme(legend.position = "bottom")

# barplot normalizzato
ggplot(data) +
  geom_col(aes(x = factor(month),
               y = sales, 
               fill = city),
           position = "fill")+
  labs(title = "Totale delle vendite mensili per città",
       x = "mesi",
       y = "totale vendite(in milioni di $)")+
  theme_classic()+
  theme(legend.position = "bottom")


#per considerare anche la variabile year
ggplot(data) +
  geom_col(aes(x = factor(month),
               y = sales, 
               fill = city),
           position = "stack")+
  labs(title = "Totale delle vendite mensili per città dal 2010 al 2014",
       x = "mesi",
       y = "totale vendite(in milioni di $)")+
  theme_classic()+
  theme(legend.position = "bottom")+
  facet_wrap(~year, scales='free')

# aggiungo la funzione di facet_wrap anche su quello normalizzato
ggplot(data) +
  geom_col(aes(x = factor(month),
               y = sales, 
               fill = city),
           position = "fill")+
  labs(title = "Totale delle vendite mensili per città dal 2010 al 2014",
       x = "mesi",
       y = "totale vendite(in milioni di $)")+
  theme_classic()+
  theme(legend.position = "bottom")+
  facet_wrap(~year, scales='free')

# 13. Crea il line chart di una var a tua scelta per fare confronti commentati fra città e periodi storici 
date <- as.Date(paste(year,"-", month, "-01", sep = ""))
data <- cbind(data, date)

ggplot(data, aes(x = date, y = volume, col = city)) +
  geom_line()+
  geom_point()+
  scale_x_date(date_minor_breaks = "1 month",
               date_breaks = "1 years",
               date_labels="%Y")+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(title = "Serie storica del volume totale vendite (in mil $) per il periodo 2010-2014 per la variaible city",
       x = "Periodo",
       y = "totale vendite(in milioni di $)")
