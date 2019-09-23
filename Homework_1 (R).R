library(readxl)
PanTHERIA <- read_excel("PanTHERIA.xlsx", sheet = "Sheet2")
View(PanTHERIA)

colnames(PanTHERIA)

unique(PanTHERIA$Order)


library(tidyverse)
ordenes <- PanTHERIA %>% select(Order, MaxLongevity_m) %>% group_by(Order) %>% summarise_all(mean)

View(ordenes)

ordenes_ordenados <- arrange(ordenes, MaxLongevity_m)

x = ordenes_ordenados$Order
last(x)

y = ordenes_ordenados$MaxLongevity_m
max(y)



mamiferos <- PanTHERIA %>% dplyr::select(Order, AdultBodyMass_g, BasalMetRate_mLO2hr) %>% 
  filter(BasalMetRate_mLO2hr >= 0) %>% group_by(Order) %>% summarise_all(mean)
  names(mamiferos)[2] <- "Masa corporal (g)"
names(mamiferos)[3] <- "Tasa de metabolismo basal (mL(O2)/h)"
View(mamiferos)

mamiferos_ord_por_masac <- arrange(mamiferos, AdultBodyMass_g)
View(mamiferos_ord_por_masac)

library(ggplot2)
library(readxl)
PanTHERIA <- read_excel("PanTHERIA.xlsx", sheet = "Sheet2")
ggplot(mamiferos, aes(x = AdultBodyMass_g, y = BasalMetRate_mLO2hr)) + 
  geom_point(color = "#f9144c", position = "jitter", size = 3) + theme_minimal() +
  scale_x_log10() + scale_y_log10() + geom_text(aes(label = Order), hjust=0.5, vjust=0) +
  xlab("Masa corporal (g)") + ylab("Tasa de metabolismo basal (mL(O2)/h)")


mamiferos_todos <- PanTHERIA %>% dplyr::select(Order, AdultBodyMass_g, BasalMetRate_mLO2hr) %>% 
  filter(BasalMetRate_mLO2hr >= 0) %>% group_by(Order)
View(mamiferos_todos)

ggplot(mamiferos_todos, aes(x = AdultBodyMass_g, y = BasalMetRate_mLO2hr), ) + 
  geom_point(position = "jitter", aes(col=Order)) + scale_x_log10() + scale_y_log10() + 
  theme(legend.position = "right")

ggplot(mamiferos_todos, aes(x = AdultBodyMass_g, y = BasalMetRate_mLO2hr), ) + 
  geom_point(position = "jitter", aes(col=Order)) + scale_x_log10() + scale_y_log10() + 
  theme(legend.position = "right") + facet_wrap(~Order, ncol = 3)

mamiferos2 <- PanTHERIA %>% dplyr::select(Order, AdultBodyMass_g, BasalMetRate_mLO2hr, MaxLongevity_m) %>%
  filter(BasalMetRate_mLO2hr >= 0, MaxLongevity_m >= 0) %>% group_by(Order)
View(mamiferos2)
ggplot(mamiferos2, aes(x = AdultBodyMass_g, y = BasalMetRate_mLO2hr, z = MaxLongevity_m)) +
  geom_tile(aes())
