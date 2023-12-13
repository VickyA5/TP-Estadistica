# k = replicaciones

# Función para generar una muestra y calcular el intervalo de confianza utilizando el método del pivote
generar_muestra_y_intervalo_pivote <- function(n, theta) {
  # Generar muestra
  muestra <- rexp(n, rate = 2) + theta
  muestra <- muestra[muestra > theta]

  # Calcular estadísticas
  media_muestra <- mean(muestra)
  varianza_muestra <- var(muestra)
  
  # Calcular intervalo de confianza utilizando el método del pivote (T1)
  # ACA SERIA EL METODO CON 1 DE LOS 3 PIVOTES
  intervalo_t1 <- c(media_muestra - qnorm(0.975) * sqrt(varianza_muestra / n), 
                    media_muestra + qnorm(0.975) * sqrt(varianza_muestra / n))
  
  cobertura_t1 <- as.numeric(intervalo_t1[1] <= theta & theta <= intervalo_t1[2])
#  cat(intervalo_t1[1]," - ",theta," - ",intervalo_t1[2],"\n")
  
  return(c(intervalo_t1[2]-intervalo_t1[1], cobertura_t1))
}

# Configuración de la simulación
set.seed(123) # Para reproducibilidad
k <- 5000
resultados_pivote <- matrix(NA, nrow = k, ncol = 2)
colnames(resultados_pivote) <- c("Longitud_T1", "Cobertura_T1")

# Realizar simulación
for (i in 1:k) {
  for (n_valor in c(10, 30, 100, 1000)) {
    for (theta_valor in c(2, 5)) {
      indices <- c(1,2) #(i - 1) * 8 + c(1, 2)
      resultados_pivote[i , indices] <- generar_muestra_y_intervalo_pivote(n_valor, theta_valor)
    }
  }
}

# Resumen de los resultados del método del pivote
resumen_resultados_pivote <- colMeans(resultados_pivote, na.rm = TRUE)
print(resumen_resultados_pivote)
