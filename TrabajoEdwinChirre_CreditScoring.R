# Carga de librerías ------------------------------------------------------

library(tidyverse)
library(scales)
library(SmartEDA)
library(janitor)
library(scorecard)
library(tidymodels)
library(performance)
library(openxlsx)
library(stargazer)
library(themis)

library(data.table)
library(dplyr)
library(caret)


rm(list = ls())

# Cargar data -------------------------------------------------------------

df <- as.data.frame(
  read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data"), header=FALSE, sep = ""))


# Formato a la data -------------------------------------------------------


df$V1 <- ifelse(
  df$V1 == "A11", "1. < 0M",
  ifelse(
    df$V1 == "A12", "2. 0a<200M",
    ifelse(
      df$V1 == "A13", "3. >=200M","4. Sin Cuenta"
      
    )
  )
)

head(df)
df$V3 <- ifelse(
  df$V3 == "A30", "1. No tomo cred o todos son pagados",
  ifelse(
    df$V3 == "A31", "2. Reembolsado debidamente",
    ifelse(
      df$V3 == "A32","3. Reembolsado debidamente hasta ahora",
      ifelse(
        df$V3 == "A33","4. Retraso en el pago","5. Cuentra critica"
        
      )
    )
  )
)



df$V4 <- ifelse(
  df$V4 == "A40", "1. Carro nuevo",
  ifelse(
    df$V4 == "A41", "2. Carro usado",
    ifelse(
      df$V4 == "A42", "3. Equipo",
      ifelse(
        df$V4 == "A43", "4. Radio",
        ifelse(
          df$V4 == "A44", "5. Electrodomestico",
          ifelse(
            df$V4 == "A45", "6. Reparaciones",
            ifelse(
              df$V4 == "A46", "7. Educacion",
              ifelse(
                df$V4 == "A47", "8. Vacaciones",
                ifelse(
                  df$V4 == "A48", "9. Reciclaje",
                  ifelse(
                    df$V4 == "A49", "10. Negocio","11. Otros"
                    
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)


df$V6 <- ifelse(
  df$V6 == "A61", "1. < 100M",
  ifelse(
    df$V6 == "A62", "2. 100a<500M",
    ifelse(
      df$V6 == "A63", "3. 500a<1000M",
      ifelse(df$V6 == "A64", "4. >=1000M",  "5. Sin Cuenta"
             
      )
      
    )
  )
)


df$V7 <- ifelse(
  df$V7 == "A71", "1. Desempleado",
  ifelse(
    df$V7 == "A72", "2. < 1anio",
    ifelse(
      df$V7 == "A73", "3. 1a<4anio",
      ifelse(df$V7 == "A74", "4. 4a<7anio",  "5. >=7anio"
             
      )
      
    )
  )
)


df$V9 <- ifelse(
  df$V9 == "A91", "1. Masculino Divorciado/Separado",
  ifelse(
    df$V9 == "A92", "2. Femenino Divorciado/Separado/Casado",
    ifelse(
      df$V9 == "A93", "3. Masculino Soltero",
      ifelse(
        df$V9 == "A94", "4. Masculino Casado/Viudo","5. Femenino Soltero"
        
      )
      
    )
    
  )
)


df$V10 <- ifelse(
  df$V10 == "A101", "1. Ninguno",
  ifelse(
    df$V10 == "A102", "2. Cosolicitante","3. Garante"
    
  )
)

df$V12 <- ifelse(
  df$V12 == "A121", "1. Bienes raices",
  ifelse(
    df$V12 == "A122", "2. Cred hipo/seguro de vida",
    ifelse(
      df$V12 == "A123", "3. Automovil","4. Desconocido/sin propiedad"
      
    )
  )
)


df$V14 <- ifelse(
  df$V14 == "A141", "1. Banco",
  ifelse(
    df$V14 == "A142", "2. Tiendas","3. Ninguno"
    
  )
)


df$V15 <- ifelse(
  df$V15 == "A151", "1. Alquiler",
  ifelse(
    df$V15 == "A152", "2. Propio","3. Gratis"
    
  )
)


df$V17 <- ifelse(
  df$V17 == "A171", "1. Desempleado",
  ifelse(
    df$V17 == "A172", "2. No calificado/Residente",
    ifelse(
      df$V17 == "A173", "3. Empleado calificado","4. Empleado"
      
    )
    
  )
)

df$V19 <- ifelse(
  df$V19 == "A191","1. No","2. Si"
)


df$V20 <- ifelse(
  df$V20 == "A201","1. Si","2. No"
)

df$V21 <- ifelse(
  df$V21 == 1,1,0
)




df <- df %>%
  rename(EstadoCuenta = V1,
         Duracion = V2,
         HistorialCredit = V3,
         Proposito = V4,
         MontoCredito = V5,
         CuentaAhorro = V6,
         TiempoEmpleo = V7,
         TasaCuota = V8,
         GeneroEstadoCivil = V9,
         OtrosDeudas =V10,
         TiempoResidencia = V11,
         Propiedad = V12,
         Edad = V13,
         OtrosPlanesCuota = V14,
         Alojamiento = V15,
         NumCreditos = V16,
         Trabajo = V17,
         NumPersonasAlimentos = V18,
         Flag_Telf = V19,
         Trabajador_extranj = V20,
         default = V21)
df <- df %>% clean_names()

head(df)


# EDA ---------------------------------------------------------------------

# Variable dependiente (PD):  Indicador de impago
df %>% 
  count(default) %>% 
  mutate(pct = n/sum(n))

# Variables explicativas: discretas o continuas - Reporte descriptivo

# Mostar tipos de variables:
df %>% ExpData() %>% View()

# Para revisar si hay missing y otros descriptivos
df %>% ExpData(type = 2) %>% View()
# No hay missing

# Descriptivo de las variables numericas
df %>% ExpNumStat() %>% View()

# Descriptivo bivariado de var numericas vs target
df %>% ExpNumStat(by = "G", gp = "default") %>% View()

# Los clientes malos tienen menos duracion (menos tiempo siendo clientes),
# son mayores y tienen menor monto

# Poder ver los IV's
res1 <- df %>% ExpCatStat(Pclass = 1, Target = "default") #%>% View()
# Estado de cuenta es la variable mas predictiva

nopred <- subset(res1, `Predictive Power` == "Not Predictive")

nopred <- as.character(nopred$Variable)

df <- df[,-which(names(df) %in% nopred)]

#Elimnaremos las variables no predictivas

head(df)



# Graficos bivariados -----------------------------------------------------

# Graficaremos las variables numericas:
# Variables explicativas: discretas o continuas

var_num <- names(df)[which(sapply(df, is.numeric))]
var_num <- var_num[-3]


df %>% 
  ggplot(aes(x = duracion, fill = as.factor(default)))+
  geom_density(alpha = 0.2)+
  theme(legend.position = "bottom")

# Los morosos tienen menos tiempo (duracion)


df %>% 
  ggplot(aes(x = monto_credito, fill = as.factor(default)))+
  geom_density(alpha = 0.2)+
  theme(legend.position = "bottom")


# Los morosos tienen menor monto


# Revisar proporciones:

var_char <- names(df)[which(sapply(df, is.character))]
var_char




for (i in 1:length(var_char)) {
  
  print(paste0("Variable: ",var_char[i]))
  
  print(prop.table(table(df[,which(names(df) %in% c(var_char[i]))],df$default),1))
  
  print("-------------------------------------------")
  
}



# Particion data ----------------------------------------------------------

#set.seed(12345)
datasets <- split_df(df, y = "default",
                     ratios = c(0.7, 0.3), seed = 123)


# Selección de variables (IV basado en woe binning con árboles)
names(datasets$test)

bins_model <- datasets$train %>% 
  woebin(y = "default", method = "tree")


bins_model %>% 
  map_df(function(x) pluck(x,10,1)) %>% 
  pivot_longer(cols = everything(), 
               names_to = "var", values_to = "iv") %>% 
  arrange(desc(iv))

woebin_plot(bins_model)



# Regresion logistica sin woe ---------------------------------------------


datasets_woe <- datasets %>% 
  map(function(x) woebin_ply(x, bins_model, to = "woe") %>% 
        as_tibble() %>% 
        mutate(default = as.numeric(default)))


names(datasets_woe$train)

library(corrplot)
corrplot(cor(datasets_woe$train[,-1]))

#No hay correlacion fuerte

# Definición de fórmulas
fm2 <- as.formula("default ~ estado_cuenta_woe + 
                  duracion_woe + historial_credit_woe +proposito_woe +
                  monto_credito_woe + cuenta_ahorro_woe + propiedad_woe +
                  edad_woe")


# Estimación de modelos logit
lg_model2 <- glm(formula = fm2, family = "binomial", 
                 data = datasets_woe$train)

lg_model2 %>% summary()


# Pruebas sobre el modelo
check_collinearity(lg_model2) #hay poca correlacion

performance_accuracy(lg_model2) #82% accuracy

performance_roc(lg_model2) #83% AUC


# grafico ROC
tibble(predicted = lg_model2$fitted.values, 
       real = as.factor(datasets_woe$train$default)) %>% 
  roc_curve(real, predicted) %>% 
  autoplot()


# Definir el punto de corte optimo (cutoff)
perf_eva(lg_model2$fitted.values, datasets_woe$train$default,
         confusion_matrix = T)

print(paste0("Corte optimo ",0.3521))

# Distribucion de las probabilidades predichas
hist(lg_model1$fitted.values)


pred <- ifelse(lg_model1$fitted.values >= 0.3521, 1, 0)

tibble(predicted = as.factor(pred), 
       real = as.factor(datasets_woe$train$default)) %>% 
  conf_mat(real, predicted) %>% 
  autoplot(type = "heatmap")

table(pred, datasets_woe$train$default)

# Metricas
accuracy <- (68+469)/nrow(datasets_woe$train)
accuracy


library(pROC)
roc_obj <- roc(datasets_woe$train$default, lg_model1$fitted.values)

auc <-roc_obj$auc
#gini

gini <- 2*roc_obj$auc - 1
#gini
gini


precision <- posPredValue(as.factor(ifelse(lg_model1$fitted.values >= 0.3521,1,0)), as.factor(datasets_woe$train$default), positive=1)
#precision


recall <- sensitivity(as.factor(ifelse(lg_model1$fitted.values >= 0.3521,1,0)), as.factor(datasets_woe$train$default), positive=1)
#recall

F1 <- (2 * precision * recall) / (precision + recall)
F1

tabla_resumen <- data.frame(
  modelo = "2. regresion logistica con woe",
  data = 'train',
  auc = auc,
  gini = gini,
  precision = precision,
  recall = recall,
  F1 = F1
)

tabla_resumen_final <- rbind(tabla_resumen_final,tabla_resumen)
tabla_resumen_final


# En test:

# Predicciones
prob_test <- predict(lg_model2, newdata = datasets_woe$test)
pred_test <- ifelse(prob_test >= 0.3521, 1, 0)
table(pred_test, datasets_woe$test$default)


# Metricas
accuracy <- (53+166)/nrow(datasets_woe$test)
accuracy


library(pROC)
roc_obj <- roc(datasets_woe$test$default, prob_test)

auc <-roc_obj$auc
#gini

gini <- 2*roc_obj$auc - 1
#gini
gini


precision <- posPredValue(as.factor(ifelse(prob_test >= 0.3521,1,0)), as.factor(datasets_woe$test$default), positive=1)
#precision


recall <- sensitivity(as.factor(ifelse(prob_test >= 0.3521,1,0)), as.factor(datasets_woe$test$default), positive=1)
#recall

F1 <- (2 * precision * recall) / (precision + recall)
F1


tabla_resumen <- data.frame(
  modelo = "2. regresion logistica con woe",
  data = 'test',
  auc = auc,
  gini = gini,
  precision = precision,
  recall = recall,
  F1 = F1
)

tabla_resumen_final <- rbind(tabla_resumen_final,tabla_resumen)
tabla_resumen_final


# Conclusion --------------------------------------------------------------

#Me quedo con el modelo realizado con woe porque es más estable entre el train y test
# El modelo final tiene 8 variables
# Teniendo como principal variable el estado de la cuenta,
# el proposito del prestamo,  duracion (tiempo siendo cliente)
# El modelo tiene un gini en el entrenamiento de 66 y en el test de 52
# El F1 en train es 84.8 y el test es 81.4. Por lo que es un buen modelo
# aunque aún tiene un poco de sobreajuste



summary(lg_model2)
