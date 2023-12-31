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
  longitud <- intervalo_t1[2]-intervalo_t1[1]
  return(c(longitud,cobertura_t1))
}

# Configuración de la simulación
#set.seed(123) # Para reproducibilidad
k <- 5000
##### MATRICES PARA T1 ######
matriz_n_10_t_2_T1 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_10_t_2_T1) <- c("Longitud_T1 (n=10 / tita=2)", "Cobertura_T1 (n=10 / tita=2)")
matriz_n_10_t_5_T1 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_10_t_5_T1) <- c("Longitud_T1 (n=10 / tita=5)", "Cobertura_T1 (n=10 / tita=5)")
matriz_n_30_t_2_T1 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_30_t_2_T1) <- c("Longitud_T1 (n=30 / tita=2)", "Cobertura_T1 (n=30 / tita=2)")
matriz_n_30_t_5_T1 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_30_t_5_T1) <- c("Longitud_T1 (n=30 / tita=5)", "Cobertura_T1 (n=30 / tita=5)")
matriz_n_100_t_2_T1 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_100_t_2_T1) <- c("Longitud_T1 (n=100 / tita=2)", "Cobertura_T1 (n=100 / tita=2)")
matriz_n_100_t_5_T1 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_100_t_5_T1) <- c("Longitud_T1 (n=100 / tita=5)", "Cobertura_T1 (n=100 / tita=5)")
matriz_n_1000_t_2_T1 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_1000_t_2_T1) <- c("Longitud_T1 (n=1000 / tita=2)", "Cobertura_T1 (n=1000 / tita=2)")
matriz_n_1000_t_5_T1 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_1000_t_5_T1) <- c("Longitud_T1 (n=1000 / tita=5)", "Cobertura_T1 (n=1000 / tita=5)")
################################

##### MATRICES PARA T2 ######
matriz_n_10_t_2_T2 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_10_t_2_T2) <- c("Longitud_T2 (n=10 / tita=2)", "Cobertura_T2 (n=10 / tita=2)")
matriz_n_10_t_5_T2 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_10_t_5_T2) <- c("Longitud_T2 (n=10 / tita=5)", "Cobertura_T2 (n=10 / tita=5)")
matriz_n_30_t_2_T2 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_30_t_2_T2) <- c("Longitud_T2 (n=30 / tita=2)", "Cobertura_T2 (n=30 / tita=2)")
matriz_n_30_t_5_T2 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_30_t_5_T2) <- c("Longitud_T2 (n=30 / tita=5)", "Cobertura_T2 (n=30 / tita=5)")
matriz_n_100_t_2_T2 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_100_t_2_T2) <- c("Longitud_T2 (n=100 / tita=2)", "Cobertura_T2 (n=100 / tita=2)")
matriz_n_100_t_5_T2 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_100_t_5_T2) <- c("Longitud_T2 (n=100 / tita=5)", "Cobertura_T2 (n=100 / tita=5)")
matriz_n_1000_t_2_T2 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_1000_t_2_T2) <- c("Longitud_T2 (n=1000 / tita=2)", "Cobertura_T2 (n=1000 / tita=2)")
matriz_n_1000_t_5_T2 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_1000_t_5_T2) <- c("Longitud_T2 (n=1000 / tita=5)", "Cobertura_T2 (n=1000 / tita=5)")
################################

##### MATRICES PARA T3 ######
matriz_n_10_t_2_T3 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_10_t_2_T3) <- c("Longitud_T3 (n=10 / tita=2)", "Cobertura_T3 (n=10 / tita=2)")
matriz_n_10_t_5_T3 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_10_t_5_T3) <- c("Longitud_T3 (n=10 / tita=5)", "Cobertura_T3 (n=10 / tita=5)")
matriz_n_30_t_2_T3 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_30_t_2_T3) <- c("Longitud_T3 (n=30 / tita=2)", "Cobertura_T3 (n=30 / tita=2)")
matriz_n_30_t_5_T3 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_30_t_5_T3) <- c("Longitud_T3 (n=30 / tita=5)", "Cobertura_T3 (n=30 / tita=5)")
matriz_n_100_t_2_T3 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_100_t_2_T3) <- c("Longitud_T3 (n=100 / tita=2)", "Cobertura_T3 (n=100 / tita=2)")
matriz_n_100_t_5_T3 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_100_t_5_T3) <- c("Longitud_T3 (n=100 / tita=5)", "Cobertura_T3 (n=100 / tita=5)")
matriz_n_1000_t_2_T3 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_1000_t_2_T3) <- c("Longitud_T3 (n=1000 / tita=2)", "Cobertura_T3 (n=1000 / tita=2)")
matriz_n_1000_t_5_T3 <- matrix(NA, nrow = k, ncol = 2)
colnames(matriz_n_1000_t_5_T3) <- c("Longitud_T3 (n=1000 / tita=5)", "Cobertura_T3 (n=1000 / tita=5)")
################################

# Realizar simulación
for (i in 1:k) {
  for (theta_valor in c(2, 5)) {
    for (n_valor in c(10, 30, 100, 1000)) {
      
      muestra <- generar_muestra(n_valor, theta_valor)
      promedio_muestra <- mean(muestra)
      intervalo_t1 <- generar_intervalo_t1(n_valor,promedio_muestra)
      intervalo_t2 <- generar_intervalo_t2(n_valor,muestra)
      intervalo_t3 <- generar_intervalo_t3(n_valor,muestra)
      indices <- c(1,2) 
      resultado_t1 <- generar_muestra_y_intervalo_pivote(n_valor, theta_valor, muestra, intervalo_t1)
      resultado_t2 <- generar_muestra_y_intervalo_pivote(n_valor, theta_valor, muestra, intervalo_t2)
      resultado_t3 <- generar_muestra_y_intervalo_pivote(n_valor, theta_valor, muestra, intervalo_t3)
      
      if(n_valor == 10 && theta_valor == 2){
        matriz_n_10_t_2_T1[i,indices] <- resultado_t1
        matriz_n_10_t_2_T2[i,indices] <- resultado_t2
        matriz_n_10_t_2_T3[i,indices] <- resultado_t3
      } else if(n_valor == 10 && theta_valor == 5){
        matriz_n_10_t_5_T1[i,indices] <- resultado_t1
        matriz_n_10_t_5_T2[i,indices] <- resultado_t2
        matriz_n_10_t_5_T3[i,indices] <- resultado_t3
      } else if(n_valor == 30 && theta_valor == 2){
        matriz_n_30_t_2_T1[i,indices] <- resultado_t1
        matriz_n_30_t_2_T2[i,indices] <- resultado_t2
        matriz_n_30_t_2_T3[i,indices] <- resultado_t3
      } else if(n_valor == 30 && theta_valor == 5){
        matriz_n_30_t_5_T1[i,indices] <- resultado_t1
        matriz_n_30_t_5_T2[i,indices] <- resultado_t2
        matriz_n_30_t_5_T3[i,indices] <- resultado_t3
      } else if(n_valor == 100 && theta_valor == 2){
        matriz_n_100_t_2_T1[i,indices] <- resultado_t1
        matriz_n_100_t_2_T2[i,indices] <- resultado_t2
        matriz_n_100_t_2_T3[i,indices] <- resultado_t3
      } else if(n_valor == 100 && theta_valor == 5){
        matriz_n_100_t_5_T1[i,indices] <- resultado_t1
        matriz_n_100_t_5_T2[i,indices] <- resultado_t2
        matriz_n_100_t_5_T3[i,indices] <- resultado_t3
      } else if(n_valor == 1000 && theta_valor == 2){
        matriz_n_1000_t_2_T1[i,indices] <- resultado_t1
        matriz_n_1000_t_2_T2[i,indices] <- resultado_t2
        matriz_n_1000_t_2_T3[i,indices] <- resultado_t3
      } else if(n_valor == 1000 && theta_valor == 5){
        matriz_n_1000_t_5_T1[i,indices] <- resultado_t1
        matriz_n_1000_t_5_T2[i,indices] <- resultado_t2
        matriz_n_1000_t_5_T3[i,indices] <- resultado_t3
      }
    }
  }
}

#######Combinar las matrices por columnas###############
matriz_combinada_T1 <- cbind(matriz_n_10_t_2_T1, matriz_n_10_t_5_T1 ,matriz_n_30_t_2_T1 ,matriz_n_30_t_5_T1 ,matriz_n_100_t_2_T1 ,matriz_n_100_t_5_T1 ,matriz_n_1000_t_2_T1 ,matriz_n_1000_t_5_T1)
matriz_combinada_T2 <- cbind(matriz_n_10_t_2_T2, matriz_n_10_t_5_T2 ,matriz_n_30_t_2_T2 ,matriz_n_30_t_5_T2 ,matriz_n_100_t_2_T2 ,matriz_n_100_t_5_T2 ,matriz_n_1000_t_2_T2 ,matriz_n_1000_t_5_T2)
matriz_combinada_T3 <- cbind(matriz_n_10_t_2_T3, matriz_n_10_t_5_T3 ,matriz_n_30_t_2_T3 ,matriz_n_30_t_5_T3 ,matriz_n_100_t_2_T3 ,matriz_n_100_t_5_T3, matriz_n_1000_t_2_T3 ,matriz_n_1000_t_5_T3)
##############

matriz_definitiva_longitud_T1 <- matrix(NA, nrow = 1, ncol = 8)
matriz_definitiva_longitud_T2 <- matrix(NA, nrow = 1, ncol = 8)
matriz_definitiva_longitud_T3 <- matrix(NA, nrow = 1, ncol = 8)
colnames(matriz_definitiva_longitud_T1) <- c("(n=10 / θ=2)","(n=10 / θ=5)","(n=30 / θ=2)","(n=30 / θ=5)","(n=100 / θ=2)","(n=100 / θ=5)","(n=1000 / θ=2)","(n=1000 / θ=5)")
colnames(matriz_definitiva_longitud_T2) <- c("(n=10 / θ=2)","(n=10 / θ=5)","(n=30 / θ=2)","(n=30 / θ=5)","(n=100 / θ=2)","(n=100 / θ=5)","(n=1000 / θ=2)","(n=1000 / θ=5)")
colnames(matriz_definitiva_longitud_T3) <- c("(n=10 / θ=2)","(n=10 / θ=5)","(n=30 / θ=2)","(n=30 / θ=5)","(n=100 / θ=2)","(n=100 / θ=5)","(n=1000 / θ=2)","(n=1000 / θ=5)")

matriz_definitiva_cobertura_T1 <- matrix(NA, nrow = 1, ncol = 8)
matriz_definitiva_cobertura_T2 <- matrix(NA, nrow = 1, ncol = 8)
matriz_definitiva_cobertura_T3 <- matrix(NA, nrow = 1, ncol = 8)
colnames(matriz_definitiva_cobertura_T1) <- c("(n=10 / θ=2)","(n=10 / θ=5)","(n=30 / θ=2)","(n=30 / θ=5)","(n=100 / θ=2)","(n=100 / θ=5)","(n=1000 / θ=2)","(n=1000 / θ=5)")
colnames(matriz_definitiva_cobertura_T2) <- c("(n=10 / θ=2)","(n=10 / θ=5)","(n=30 / θ=2)","(n=30 / θ=5)","(n=100 / θ=2)","(n=100 / θ=5)","(n=1000 / θ=2)","(n=1000 / θ=5)")
colnames(matriz_definitiva_cobertura_T3) <- c("(n=10 / θ=2)","(n=10 / θ=5)","(n=30 / θ=2)","(n=30 / θ=5)","(n=100 / θ=2)","(n=100 / θ=5)","(n=1000 / θ=2)","(n=1000 / θ=5)")


for (i in 1:8){
  matriz_definitiva_longitud_T1[i] <- c(mean(matriz_combinada_T1[,i*2-1]))
  matriz_definitiva_cobertura_T1[i] <- c(mean(matriz_combinada_T1[,i*2]))
  matriz_definitiva_longitud_T2[i] <- c(mean(matriz_combinada_T2[,i*2-1]))
  matriz_definitiva_cobertura_T2[i] <- c(mean(matriz_combinada_T2[,i*2]))
  matriz_definitiva_longitud_T3[i] <- c(mean(matriz_combinada_T3[,i*2-1]))
  matriz_definitiva_cobertura_T3[i] <- c(mean(matriz_combinada_T3[,i*2]))
  
}
