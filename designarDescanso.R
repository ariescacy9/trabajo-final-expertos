

# carga el paquete
library(sets)

# Configurar el universo
sets_options("universe", seq(1, 720, 0.1))

# Definimos las variables del sistema difuso
variables <- set(
  horas_trabajo = fuzzy_partition(varnames = c(poco = 180, regular = 360, mucho = 540),
                                sd = 50.0),
  horas_estudio = fuzzy_partition(varnames = c(poco = 180, regular = 360, mucho = 480), 
                            sd = 40.0),
  horas_deporte = fuzzy_partition(varnames = c(poco = 60, regular = 168,mucho = 240), sd = 25.0),
  
  horas_descanso = fuzzy_partition(varnames = c(poco = 300, normal = 420, mucho = 540),
                          FUN = fuzzy_cone, radius = 100)
)

# Definimos las reglas difusas del sistema
reglas <- set(
  fuzzy_rule(horas_trabajo %is% poco && horas_estudio %is% regular &&
               horas_deporte %is% mucho, horas_descanso %is% mucho),
  fuzzy_rule(horas_trabajo %is% poco && horas_estudio %is% poco &&
               horas_deporte %is% regular, horas_descanso %is% normal),
  fuzzy_rule(horas_trabajo %is% mucho && horas_estudio %is% mucho &&
               horas_deporte %is% poco, horas_descanso %is% mucho),
  fuzzy_rule(horas_trabajo %is% poco && horas_estudio %is% regular &&
               horas_deporte %is% poco, horas_descanso %is% normal),
  fuzzy_rule(horas_trabajo %is% regular || horas_estudio %is% regular ||
               horas_deporte %is% regular, horas_descanso %is% normal),
  fuzzy_rule(horas_trabajo %is% mucho && horas_deporte %is% mucho,
             horas_descanso %is% mucho),
  fuzzy_rule(horas_trabajo %is% poco && horas_estudio %is% poco &&
               horas_deporte %is% poco, horas_descanso %is% poco)
)

# Ahora, construyamos el sistema (el modelo)
relax_expert <- fuzzy_system(variables, reglas)

# Mostramos las variables y las reglas del sistema
print(relax_expert)

# Mostramos el gráfico (trama) del sistema
plot(relax_expert)





ejemplo.1 <- fuzzy_inference(relax_expert, list(horas_trabajo = 240, horas_estudio = 300,
                                          horas_deporte = 60))
# Ahora, defuizamos el ejemplo para transformar los parámetros en un número real
gset_defuzzify(ejemplo.1, "centroid")
# Mostramos el gráfico
plot(ejemplo.1)

ejemplo.2 <- fuzzy_inference(relax_expert, list(horas_trabajo = 490, horas_estudio = 0,
                                          horas_deporte = 240))
gset_defuzzify(ejemplo.2, "centroid")
plot(ejemplo.2)


ejemplo.3 <- fuzzy_inference(relax_expert, list(horas_trabajo = 0, horas_estudio = 546,
                                          horas_deporte = 120))
gset_defuzzify(ejemplo.3, "centroid")
plot(ejemplo.3)
