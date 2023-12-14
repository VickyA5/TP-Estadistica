
generar_muestra <- function (n, theta){
  
  muestra <- rexp(n, rate = 2) + theta
  muestra <- muestra[muestra > theta]
  
  return(muestra)
}

generar_intervalo_t1 <- function(n, promedio_muestra){
  
  intervalo_t1 <- c(promedio_muestra - (1.959964 / (sqrt(n) * 2))  - 0.5, 
                    promedio_muestra + (1.959964 / (sqrt(n) * 2))  - 0.5)
  
  return(intervalo_t1)
}

generar_intervalo_t2 <- function(n, muestra){
  
  intervalo_t2 <- c(min(muestra) - qexp(0.95 , rate=2*n), min(muestra))
  
  return(intervalo_t2)
}

generar_intervalo_t3 <- function(n, muestra){
  
  intervalo_t3 <- c((sum(muestra) - qgamma(0.975, shape=n, scale=1/2))/n, (sum(muestra) - qgamma(0.025, shape=n, scale=1/2))/n)
  
  return(intervalo_t3)
}

# Guarda la cobertura y longitud
generar_muestra_y_intervalo_pivote <- function(n, theta, muestra, intervalo_t1) {
  
  cobertura_t1 <- as.numeric(intervalo_t1[1] <= theta & theta <= intervalo_t1[2])
  #cat(intervalo_t1[1]," - ",theta," - ",intervalo_t1[2],"\n")
  #cat(intervalo_t1[2]-intervalo_t1[1],"-",cobertura_t1,"\n")
  longitud <- intervalo_t1[2]-intervalo_t1[1]
  return(c(longitud,cobertura_t1))
}

# Configuración de la simulación
#set.seed(123) # Para reproducibilidad
k <- 5000
resultados_cobertura_longitud_t1 <- matrix(NA, nrow = k, ncol = 2)
colnames(resultados_cobertura_longitud_t1) <- c("Longitud_T1", "Cobertura_T1")
matriz_n_10_t_2 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_10_t_2) <- c("Longitud_T1", "Cobertura_T1")
matriz_n_10_t_5 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_10_t_5) <- c("Longitud_T1", "Cobertura_T1")
matriz_n_30_t_2 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_30_t_2) <- c("Longitud_T1", "Cobertura_T1")
matriz_n_30_t_5 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_30_t_5) <- c("Longitud_T1", "Cobertura_T1")
matriz_n_100_t_2 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_100_t_2) <- c("Longitud_T1", "Cobertura_T1")
matriz_n_100_t_5 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_100_t_5) <- c("Longitud_T1", "Cobertura_T1")
matriz_n_1000_t_2 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_1000_t_2) <- c("Longitud_T1", "Cobertura_T1")
matriz_n_1000_t_5 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_1000_t_5) <- c("Longitud_T1", "Cobertura_T1")


# Realizar simulación
for (i in 1:k) {
  for (theta_valor in c(2, 5)) {
    for (n_valor in c(10, 30, 100, 1000)) {
      
      muestra <- generar_muestra(n_valor, theta_valor)
      promedio_muestra <- mean(muestra)
      intervalo_t1 <- generar_intervalo_t1(n_valor,promedio_muestra)
      indices <- c(1,2) 
      #cat("N: ",n_valor," - TITA: ",theta_valor,"\n")
      resultado_t1 <- generar_muestra_y_intervalo_pivote(n_valor, theta_valor, muestra, intervalo_t1)
      resultados_cobertura_longitud_t1[i , indices] <- resultado_t1

      if(n_valor == 10 && theta_valor == 2){
        matriz_n_10_t_2[i,indices] <- resultado_t1
      } else if(n_valor == 10 && theta_valor == 5){
        matriz_n_10_t_5[i,indices] <- resultado_t1
      } else if(n_valor == 30 && theta_valor == 2){
        matriz_n_30_t_2[i,indices] <- resultado_t1
      } else if(n_valor == 30 && theta_valor == 5){
        matriz_n_30_t_5[i,indices] <- resultado_t1
      } else if(n_valor == 100 && theta_valor == 2){
        matriz_n_100_t_2[i,indices] <- resultado_t1
      } else if(n_valor == 100 && theta_valor == 5){
        matriz_n_100_t_5[i,indices] <- resultado_t1
      } else if(n_valor == 1000 && theta_valor == 2){
        matriz_n_1000_t_2[i,indices] <- resultado_t1
      } else if(n_valor == 1000 && theta_valor == 5){
        matriz_n_1000_t_5[i,indices] <- resultado_t1
      }
    }
  }
#  cat("\n")
}


# Resumen de los resultados con T1
resumen_resultados_pivote_t1 <- colMeans(resultados_cobertura_longitud_t1, na.rm = TRUE)
print(resumen_resultados_pivote_t1)


