library(tidyverse)
library(dplyr)
library(readr)
library(readxl)
library(tidyverse)
library(stringr)
library(broom)

Datos_diabetes_1 <- diabetic_data %>% select(race, gender, age, time_in_hospital, num_lab_procedures, num_procedures, num_medications, max_glu_serum)
Datos_diabetes_1 <- Datos_diabetes_1 %>%  rename(Raza = race, Género = gender, Edad = age, Tiempo_en_clínica = time_in_hospital, número_procedimientos_lab = num_lab_procedures, número_procedimientos = num_procedures, número_medicamentos = num_medications, Máximo_glucosa_sérica = max_glu_serum )


## Grafico con muchos colores que no dice nada

Datos_diabetes_1 <- diabetic_data %>% select(race, gender, age, time_in_hospital, num_lab_procedures, num_procedures, num_medications, max_glu_serum)
Datos_diabetes_1 <- Datos_diabetes_1 %>%  rename(Raza = race, Género = gender, Edad = age, Tiempo_en_clínica = time_in_hospital, número_procedimientos_lab = num_lab_procedures, número_procedimientos = num_procedures, número_medicamentos = num_medications, Máximo_glucosa_sérica = max_glu_serum )
Datos_diabetes_1 <- Datos_diabetes_1 %>%  filter(Raza != "other")

ggplot(Datos_diabetes_1, aes( x = Edad, y = Tiempo_en_clínica)) + geom_path(aes(group = Género, color = Raza, lty = Type)) + geom_point( aes(color = Raza))
###




Datos_diabetes_1 <- diabetic_data %>% select(race, gender, age, time_in_hospital, num_lab_procedures, num_procedures, num_medications, max_glu_serum)
Datos_diabetes_1 <- Datos_diabetes_1 %>%  rename(Raza = race, Género = gender, Edad = age, Tiempo_en_clínica = time_in_hospital, número_procedimientos_lab = num_lab_procedures, número_procedimientos = num_procedures, número_medicamentos = num_medications, Máximo_glucosa_sérica = max_glu_serum )
Datos_diabetes_1 <- Datos_diabetes_1 %>%  filter(Raza != "other")

Datos2 <- Datos_diabetes_1 %>% group_by(Raza, Edad) %>%  summarise(Media_Tiempo_en_clínica = mean(Tiempo_en_clínica),  Número_procedimientos_lab = mean(número_procedimientos_lab), Número_procedimientos = mean(número_procedimientos), Número_medicamentos = mean(número_medicamentos))

Datos4 <- Datos2 %>%  group_by(Raza, Edad) %>% summarise(Media_Tiempo_en_clínica= mean(Media_Tiempo_en_clínica), Número_procedimientos_lab = mean(Número_procedimientos_lab), Número_procedimientos = mean(Número_procedimientos), Número_medicamentos = mean(Número_medicamentos))
Datos5 <- Datos4 %>%  group_by(Raza) %>% summarise(Media_Tiempo_en_clínica= mean(Media_Tiempo_en_clínica), Número_procedimientos_lab = mean(Número_procedimientos_lab), Número_procedimientos = mean(Número_procedimientos), Número_medicamentos = mean(Número_medicamentos))
 


names(Datos2)

## Evaluando Modelos que se ajusten a los Datos2

Fit1 <- lm(Media_Tiempo_en_clínica ~ Edad, data = Datos2)
glance(Fit1)
Sum1 <- glance(Fit1)

Fit2 <- lm(Media_Tiempo_en_clínica ~ Edad + Raza, data = Datos2)
glance((Fit2))
Sum2 <- glance(Fit2)


Modelo1 <- glance(Fit1) %>% dplyr::select(r.squared, AIC) %>% mutate(Modelo = "Fit1")
Modelo2 <- glance(Fit2) %>% dplyr::select(r.squared, AIC) %>% mutate(Modelo = "Fit2")
Modelos <- bind_rows(Modelo1, Modelo2) %>% arrange(AIC)

## prediciendo los modelos de acuerdo a los datos generados a partir de los datos propocionados por la base de datos de diabtes

DF <- expand.grid(list(Raza = c("AfricanAmerican", "Asian", "Caucasian", "Hispanic", "Other"), Edad = c("[0-10)", "[10-20)", "[20-30)", "[30-40)", "[40-50)", "[50-60)", "[60-70)", "[70-80)", "[80-90)", "[90-100)")))

view(DF)

predict(Fit1, DF)

DF$Pred <- predict(Fit1, DF, se.fit = TRUE)$fit
DF$SE <- predict(Fit1, DF, se.fit = TRUE)$se.fit


## Graficar las prediccioones

ggplot(DF, aes(x= Edad, y = Pred)) + geom_boxplot(aes(ymin = Pred - SE, ymax = Pred + SE))



