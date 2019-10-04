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


## Evaluando Modelos que se ajusten a los Datos2

Fit1 <- lm(Media_Tiempo_en_clínica ~ Edad, data = Datos2)
glance(Fit1)
Sum1 <- glance(Fit1)

Fit2 <- lm(Media_Tiempo_en_clínica ~ Edad + Raza, data = Datos2)
glance((Fit2))
Sum2 <- glance(Fit2)

## Poniendo el modelo en el grafico

stat_smooth(method = "lm",
            formula = y ~ I(x^2) + x)

