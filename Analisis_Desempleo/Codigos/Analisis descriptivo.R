################################################################################
####              Analisis descriptivo serie desempleo Colombia.            ####
################################################################################

rm(list = ls())

# 0. Librerias y directorios ----------------------------------------------
library(pacman)
p_load(
  tidyverse,      # manipulación y visualización de datos (dplyr, ggplot2, etc.)
  TSstudio,       # herramientas interactivas para series de tiempo
  readxl,         # importar archivos de Excel
  zoo,            # estructura básica para series de tiempo ordenadas
  xts,            # series temporales indexadas (común en finanzas)
  feasts,         # extracción de características en series de tiempo
  fable,          # modelos de pronóstico para datos tidy
  timetk,         # herramientas tidy para series de tiempo
  tsibble,        # estructura tidy para datos temporales
  lubridate,      # manejo sencillo de fechas y horas
  astsa,          # análisis clásico de series de tiempo
  car,            # diagnóstico y pruebas en regresión
  forecast,       # modelos de pronóstico (ARIMA, ETS, etc.)
  VGAM,           # modelos lineales/generalizados vectoriales
  TSA,            # utilidades adicionales de series de tiempo
  nonlinearTseries, # análisis no lineal de series de tiempo
  tseriesChaos,   # herramientas de caos y dinámica no lineal
  plotly,         # visualizaciones interactivas
  rstanarm,       # regresión bayesiana con Stan (fácil de usar)
  rstan,          # modelado bayesiano completo con Stan
  bayesplot,      # visualización de modelos bayesianos
  sarima,         # modelos ARIMA estacionales
  patchwork,      # combinar múltiples gráficos ggplot
  mgcv,           # modelos aditivos generalizados (GAM)
  broom,           # convertir modelos a data frames tidy
  biwavelet,
  ggtime
)

# Directorios
dir_base   = getwd()
dir_input  = paste0(dir_base, '/Input')
dir_output = paste0(dir_base, '/Output')
# Crear un theme para ggplot, para tener graficos estandarizados
theme_custom = function() {
  theme_bw() +
    theme(
      plot.title = element_text(
        face = 'bold',   
        hjust = 0.5      
      )
    )
}

# 1. Datos ----------------------------------------------------------------
desempleo_raw = read_xlsx(paste0(dir_input, '/Base Series Empleo.xlsx'), sheet = 'Series de datos', skip = 2)
# Limpieza de datos
desempleo = desempleo_raw %>% 
  select(...1, `Tasa de desempleo - Total Nacional`) %>% 
  set_names('Fecha','Tasa_Desempleo') %>% 
  # Dejar la fecha en formato Date
  mutate(Fecha = as.Date(as.numeric(Fecha), origin = '1899-12-30')) %>% 
  filter(!is.na(Fecha))
# Podemos generar diversos objetos de series de tiempo.
# ts
desempleo_ts = ts(desempleo$Tasa_Desempleo, start = c(2001,1), frequency = 12)
# xts (derivado de zoo)
desempleo_xts = xts(desempleo$Tasa_Desempleo, order.by = desempleo$Fecha)
colnames(desempleo_xts) = 'Tasa_Desempleo'
# tsibble
desempleo_tsibble = desempleo %>%
  as_tsibble(index = Fecha) %>% 
  mutate(Fecha = yearmonth(Fecha))

# 2. Analisis descriptivo -------------------------------------------------
# 2.1. Grafica ----
# Primero, se graficara la serie para poder ver posibles patrones
gg_serie = ggplot(desempleo_tsibble, aes(x=as.Date(Fecha), y = Tasa_Desempleo))+
  geom_line() +
  labs(y = 'Tasa de Desempleo (%)', title = 'Tasa de Desempleo Nacional') +
  scale_x_date(date_breaks = '3 years') +
  theme_custom()
gg_serie
# A simple vista, pareciera que la varianza marginal es constante y por tanto no se
# necesitaria una transformacion boxcox. 
# Por otro lado, parece que puede tener una tendencia, particularmente desde el inicio
# de la serie hasta el 2018, y posteriormente a partir del 2020 hasta el presente.
# Claramente tiene un ciclo periodico, en donde los datos de enero son mayores en 
# promedio, lo cual sera analizado mas adelante. 

# 2.2. Boxcox ----
# Utilizaremos la funcion BoxCox.lambda para ver el lambda optimo, y ademas, la funcion 
# boxCox del paquete car nos dara una grafica con posibles lambdas. Debemos recordar que
# si el lambda optimo esta cercano a 1, no es necesaria la transformacion boxcox.
lambda_optim = BoxCox.lambda(desempleo_ts, method ='loglik', lower = -5, upper = 5)
boxCox(desempleo_ts~1) 

# Al parecer se necesita un lambda cercano a -1
desempleo_boxcox = BoxCox(desempleo_ts, lambda = lambda_optim)

# Graficar en plotly a ver que tan diferentes son
tiempo = time(desempleo_ts)
df_plotly = data.frame(
  fecha = tiempo, 
  desempleo = as.numeric(desempleo_ts),
  desempleo_trans = as.numeric(desempleo_boxcox)
) 
plot_ly(df_plotly, x = ~fecha) %>%
  add_lines(y = ~desempleo, name = 'Desempleo', yaxis = 'y1') %>%
  add_lines(y = ~desempleo_trans, name = 'Transformado', yaxis = 'y2') %>%
  layout(title = list(
    text = '</b>Transformación Box-Cox</b>',
    x = 0.5),
    yaxis = list(title = 'Desempleo'),
    yaxis2 = list(
      title = 'Transformado',
      overlaying = 'y',
      side = 'right'
    )
  )
# La transformación Box-Cox no muestra una mejora clara en la estabilización de la varianza. 
# Además, como el valor óptimo de λ es cercano a -1, la serie transformada pierde interpretación 
# económica al aproximarse a una transformación inversa. En consecuencia, se continuará el análisis 
# utilizando la serie original.

# 2.3 Tendencia ----
# Veamos el acf
gg_acf_original = ggAcf(desempleo_ts) +
  labs(title = 'ACF Desempleo') +
  theme_custom()
gg_acf_original
# Tiene un decaimiento muy lento, lo cual puede indicar presencia de tendencia estocastica
# Analisis usando decompose (promedio movil)
plot(decompose(desempleo_ts))
# Presenta tendencia, pero tambien un quiebre estructural en 2020, para el cual se deberian usar 
# metodos mas sofisticados de tendencia.

# Usando loess
gg_loess = desempleo_tsibble %>%
  mutate(loess_03 = smooth_vec(Tasa_Desempleo, span = 0.2),
         loess_05 = smooth_vec(Tasa_Desempleo, span = 0.5),
         loess_07 = smooth_vec(Tasa_Desempleo, span = 0.8)) %>%
  ggplot(aes(as.Date(Fecha))) +
  geom_line(aes(y = Tasa_Desempleo, color = 'Serie original'), alpha = 0.6) +
  geom_line(aes(y = loess_03, color = 'LOESS (span = 0.2)')) +
  geom_line(aes(y = loess_05, color = 'LOESS (span = 0.5)')) +
  geom_line(aes(y = loess_07, color = 'LOESS (span = 0.8)')) +
  labs(title = 'Comparación de suavizamientos LOESS', x = 'Fecha',
       y = 'Tasa de desempleo', color = 'Serie') +
  scale_color_manual(values = c('Serie original' = 'black', 'LOESS (span = 0.2)' = 'blue',
    'LOESS (span = 0.5)' = 'red','LOESS (span = 0.8)' = 'green')) +
  scale_x_date(breaks = '3 years')+
  theme_custom()
gg_loess
# La estimación de la tendencia mediante LOESS evidencia una alta sensibilidad al parámetro de 
# suavizamiento (span). Valores bajos permiten capturar fluctuaciones de corto plazo, incluyendo shocks 
# transitorios como el observado en 2020, mientras que valores altos producen una tendencia más estable.
# Sin embargo, todos los spans muestran una tendencia negativa, es decir de mejoramiento del empleo, a 
# excepcion del choque del 2020, el cual se debe tratar independientemente.

# Otras formas de estimar la tendencia: suavizamiento con kernel gaussiano y STL.
n = nrow(desempleo_tsibble)
x = 1:n
ks_6  = ksmooth(x, desempleo_tsibble$Tasa_Desempleo, kernel = 'normal', bandwidth = 6)
ks_12 = ksmooth(x, desempleo_tsibble$Tasa_Desempleo, kernel = 'normal', bandwidth = 12)
ks_24 = ksmooth(x, desempleo_tsibble$Tasa_Desempleo, kernel = 'normal', bandwidth = 24)

df_ks = data.frame(Fecha = as.Date(desempleo_tsibble$Fecha),ks_6  = ks_6$y,ks_12 = ks_12$y,ks_24 = ks_24$y)

# Gráfico
gg_kernel = desempleo_tsibble %>%
  ggplot(aes(as.Date(Fecha))) +
  geom_line(aes(y = Tasa_Desempleo, color = 'Serie original'), alpha = 0.4) +
  geom_line(data = df_ks, aes(y = ks_6,  color = 'Kernel (6 meses)'), linewidth = 1) +
  geom_line(data = df_ks, aes(y = ks_12, color = 'Kernel (12 meses)'), linewidth = 1) +
  geom_line(data = df_ks, aes(y = ks_24, color = 'Kernel (24 meses)'), linewidth = 1) +
  labs(title = 'Suavizamiento con Kernel Gaussiano',y = 'Tasa de desempleo', x = 'Fecha',
       color = 'Método') +
  scale_color_manual(values = c('Serie original' = 'black', 'Kernel (6 meses)' = 'blue',
                                'Kernel (12 meses)' = 'red','Kernel (24 meses)' = 'green')) +
  scale_x_date(breaks = '3 years') +
  theme_custom()
gg_kernel

# Metodo STL
# Se escogera este metodo para probar eliminacion de tendencia, dado que el metodo 
# robust = T es apto para outliers.
decomp_STL = desempleo_tsibble %>% 
  model(STL(Tasa_Desempleo ~ trend() + season(window = 'periodic'), robust = T))
gg_STL = decomp_STL %>% 
  components() %>% 
  autoplot() +
  theme_custom()
gg_STL

# 2.4. Eliminacion de tendencia ----
# Hay dos alternativas, la primera es restando una tendencia estimada. La segunda es diferenciar la 
# serie. Vamos a realizar ambas alternativas y analizar el acf de los resultados para ver cual es la
# mas apta.

# Primera opcion: restando la tendencia
trend_stl = (decomp_STL %>% components())$trend
df_stl = desempleo_tsibble %>%
  mutate(STL_sin_tendencia = Tasa_Desempleo - trend_stl,
         Tendencia = trend_stl) 

# Segunda opcion: Diferenciación
df_diff = desempleo_tsibble %>%
  mutate(Diff = c(NA, diff(Tasa_Desempleo))) 

df_final = df_stl %>%
  select(Fecha, STL_sin_tendencia) %>% 
  left_join(df_diff %>% select(Fecha, Diff), by = 'Fecha') %>%
  pivot_longer(-Fecha, names_to = 'Serie', values_to = 'Valor') %>%
  filter(!is.na(Valor))

# Grafico comparativo
gg_compare = df_final %>%
  ggplot(aes(x = as.Date(Fecha), y = Valor, color = Serie)) +
  geom_line() +
  labs(title = 'Comparación: Serie sin tendencia (STL) vs Diferenciada', x = 'Fecha',
       y = 'Valor',color = 'Método') +
  scale_x_date(date_breaks = '3 years') +
  scale_color_manual(values = c('STL_sin_tendencia' = 'blue','Diff' = 'red')) +
  theme_custom()
gg_compare
 
# Ambas transformaciones logran eliminar la tendencia de la serie, generando procesos centrados alrededor 
# de cero. No obstante, la diferenciación produce una mayor volatilidad y amplifica los shocks transitorios, 
# mientras que la serie ajustada por STL conserva mejor la estructura cíclica subyacente. Esto sugiere que, 
# para análisis de dinámica de corto plazo, la diferenciación puede ser útil, mientras que STL ofrece una 
# representación más estable de las fluctuaciones alrededor de la tendencia.

# ACF diferenciado
gg_acf_diff = ggAcf(df_diff$Diff) +
  labs(title = 'ACF Desempleo Diferenciada') +
  theme_custom()
gg_acf_diff

# ACF con remocion de tendencia STL
gg_acf_stl = ggAcf(df_stl$STL_sin_tendencia) +
  labs(title = 'ACF Desempleo sin Tendencia') +
  theme_custom()
gg_acf_stl

# Ambas series muestran dos cosas clave: caida rapida, por lo cual no habria presencia de raiz unitaria
# 2. Presentan significancia estadistica en rezago 12 y 24, por lo cual habria presencia de estacionalidad
# y por tanto la serie no es estacionaria. Para el resto del analisis nos quedaremos con la serie sin
# tendencia a traves de STL, ya que preserva de una mejor manera el ciclo y aparte no distorsiona mucho
# la interpretacion economica como si lo hace la diferenciacion.

# Anlizando correlaciones de los rezagos.
astsa::lag1.plot(df_stl$STL_sin_tendencia, 24, corr=F) 
# Se ve que hay correlacion significativa en los rezagos 6, 12, 18 y 24, lo cual indica estacionalidad. Lo cual 
# se analizara en seguida.

# 2.5 Estacionalidad ----
# Analisis grafico
desempleo_sin_trend_xts = xts(df_stl$STL_sin_tendencia, order.by = as.Date(df_stl$Fecha))
ts_heatmap(desempleo_sin_trend_xts, title = 'Heatmap tasa de desempleo diferenciada')
# En enero se ve un aumento del desempleo a lo largo de todos los años. 

# Grafico de media por mes
gg_media_mes = df_stl %>% 
  as_tibble() %>% 
  mutate(Fecha = as.Date(Fecha)) %>% 
  plot_seasonal_diagnostics(.date_var = Fecha, .value = STL_sin_tendencia, .feature_set = 'month.lbl',
                            .geom = 'boxplot', .interactive = F, .title = 'Diagnóstico estacional por mes')
gg_media_mes
# Se confirma la estacionalidad, en enero el promedio de la diferencia en el desempleo es mayor que en el resto de meses

# Tambien se puede graficar la subserie mensual
gg_serie_mes = df_stl %>% 
  ggtime::gg_subseries(STL_sin_tendencia, period = 12) +
  labs(title = 'Serie mensual', y = 'Desempleo sin tendencia (%)')+
  theme_custom()
gg_serie_mes

## Wavelet
wavelet = df_stl %>% 
  transmute(Fecha = as.numeric(Fecha), STL_sin_tendencia) %>% 
  as.matrix() %>% 
  biwavelet::wt()

par(mfrow = c(1,1))
plot(wavelet, type = 'power.corr.norm', main = 'Bias-corrected wavelet power',plot.cb=TRUE)
plot(wavelet, type = 'power.norm', main = 'Biased wavelet power',plot.cb=TRUE)
# No se ve nada claro. Probablemente por el choque en 2020.

# Periodograma 
periodograma = mvspec(df_stl$STL_sin_tendencia, log='no', main = 'Periodograma desempleo sin tendencia')
analisis_period = data.frame(frecuencia = periodograma$freq, spectrum = periodograma$spec) %>% 
  mutate(periodo = 1/frecuencia)
# Obtener los cuatro spectrum mas altos para graficar
maxspec = analisis_period %>% 
  arrange(desc(spectrum)) %>% 
  slice(1:4) %>% 
  pull(frecuencia, periodo)
abline(v= maxspec, lty = 2, col = 'red', lwd = 1)
# Mostrar cuales son los periodos con mayor espectro
cat('Los periodos principales son:', paste(names(maxspec), reduce = 'meses,'))
# Tenemos 3, 4, 6 y 12 meses. Lo cual lleva a entender una estacionalidad anual de 12, pero es normal encontrar
# relevancia tambien en sus armonicos.

### Modelo dummies para estacionalidad
df_stl = df_stl %>% 
  mutate(mes = month(Fecha))
modelo_dummies = lm(STL_sin_tendencia ~ factor(mes)-1, data = df_stl)
summary(modelo_dummies)
# Vemos significancia en las dummies, sobre todo de enero, febrero, octubre, noviembre y diciembre.
# Encontremos la estimacion de la estacionalidad
df_stl = df_stl %>% 
  mutate(estacional = fitted(modelo_dummies),
         sin_estacional = STL_sin_tendencia - estacional)

# Graficar la serie sin tendencia vs la estacionalidad estimada
gg_estacion_dummies = df_stl %>% 
  transmute(Fecha = as.Date(Fecha),'Desempleo_sin_tendencia' = STL_sin_tendencia,estacional) %>% 
  ggplot(aes(x = Fecha)) +
  geom_line(aes(y = Desempleo_sin_tendencia, color = 'Desempleo_sin_tendencia'),alpha = 0.7) +
  geom_line(aes(y = estacional, color = 'estacional')) +
  scale_color_manual(values = c('Desempleo_sin_tendencia' = 'black','estacional' = 'red')) +
  scale_x_date(date_breaks = '3 years') +
  labs(color = 'Serie', title='Componente estacional', y='Desempleo sin tendencia y componente estacional') +
  theme_custom()
gg_estacion_dummies

# Estimar estacionalidad con Fourier. Preguntar de que depende K
df_fourier = df_stl %>%
  transmute(Fecha = as.Date(Fecha),y = STL_sin_tendencia) %>%
  mutate(t = 1:n(),Mes = month(Fecha))

# A mano
df_fourier = df_fourier %>%
  mutate(cos1 = cos(2*pi*1*t/12),sin1 = sin(2*pi*1*t/12),
         cos2 = cos(2*pi*2*t/12),sin2 = sin(2*pi*2*t/12),
         cos3 = cos(2*pi*3*t/12),sin3 = sin(2*pi*3*t/12),
         cos4 = cos(2*pi*4*t/12),sin4 = sin(2*pi*4*t/12))

mod_fourier_4 = lm(y ~ cos1 + sin1 + cos2 + sin2 + cos3 + sin3 + cos4 + sin4,
                   data = df_fourier)
summary(mod_fourier_4)

# Graficar
df_fourier = df_fourier %>%
  mutate(estacional_fourier = fitted(mod_fourier_4),resid_fourier = y - estacional_fourier)

gg_fourier = df_fourier %>%
  ggplot(aes(x = Fecha)) +
  geom_line(aes(y = y, color = 'Serie sin tendencia'), alpha = 0.6) +
  geom_line(aes(y = estacional_fourier, color = 'Estacionalidad Fourier'), linewidth = 1) +
  scale_color_manual(values = c(
    'Serie sin tendencia' = 'black',
    'Estacionalidad Fourier' = 'red')) +
  labs(title = 'Estacionalidad estimada con series de Fourier',x = 'Fecha',y = 'Desempleo',color = 'Serie') +
  theme_custom()
gg_fourier
# Dado que el periodograma muestra un pico principal en la frecuencia anual y armónicos en 6, 4 y 3 meses, 
# la estacionalidad puede modelarse mediante una expansión de Fourier con periodicidad 12 y orden K = 4. Este 
# enfoque permite capturar de forma parsimoniosa la estructura estacional observada en la serie sin tendencia.

## Modelo GAM
modelo_gam = gam(y ~ s(Mes, bs = 'cc', k =12), data = df_fourier)
summary(modelo_gam)
# Agregar los datos ajustados en la misma tabla de fourier
df_fourier = df_fourier %>% 
  mutate(estacional_gam = fitted(modelo_gam),
         resid_gam = resid(modelo_gam))
gg_gam = df_fourier %>%
  ggplot(aes(x = Fecha)) +
  geom_line(aes(y = y, color = 'Serie sin tendencia'), alpha = 0.6) +
  geom_line(aes(y = estacional_gam, color = 'Estacionalidad GAM'), linewidth = 1) +
  scale_color_manual(values = c(
    'Serie sin tendencia' = 'black',
    'Estacionalidad GAM' = 'blue')) +
  labs(title = 'Estacionalidad estimada con modelo GAM',x = 'Fecha',y = 'Desempleo',color = 'Serie') +
  theme_custom()
gg_gam

# Comparacion de los dos modelos
gg_gam_fourier = df_fourier %>%
  ggplot(aes(x = Fecha)) +
  geom_line(aes(y = y, color = 'Serie sin tendencia'), alpha = 0.6) +
  geom_line(aes(y = estacional_gam, color = 'Estacionalidad GAM'), linewidth = 1) +
  geom_line(aes(y = estacional_fourier, color = 'Estacionalidad Fourier'), linewidth = 1)+
  scale_color_manual(values = c('Serie sin tendencia' = 'black', 'Estacionalidad GAM' = 'blue',
                                'Estacionalidad Fourier' = 'red')) +
  labs(title = 'Estacionalidad estimada con modelo GAM y series de Fourier',x = 'Fecha',y = 'Desempleo',color = 'Serie') +
  scale_x_date(breaks = '3 years')+
  theme_custom()
gg_gam_fourier
# Ambos enfoques permiten capturar la estacionalidad de la serie de manera adecuada. Sin embargo, el modelo de Fourier 
# impone una estructura funcional basada en armónicos sinusoidales, lo que resulta en patrones más regulares y pronunciados. 
# En contraste, el modelo GAM ofrece una mayor flexibilidad al estimar la estacionalidad de forma no paramétrica, produciendo 
# ajustes más suaves y adaptados a la forma observada en los datos. Esto sugiere que, si bien Fourier es útil para representar 
# ciclos periódicos bien definidos, el enfoque GAM puede capturar mejor irregularidades en la estructura estacional.

# VEamos los AIC de los tres modelos
AIC(modelo_dummies)
AIC(mod_fourier_4)
AIC(modelo_gam)
# El que minimiza este indice es el modelo de dummies. Sumado a su interpretabilidad, vamos a quedarnos con este modelo, para
# obtener la serie sin estacionalidad, y a investigar sus propiedades.

# 2.5 Serie final ----
# Grafico
gg_resid_dummies = df_stl %>% 
  ggplot(aes(x=as.Date(Fecha), y=sin_estacional)) +
  geom_line() +
  scale_x_date(date_breaks = '3 years') +
  labs(title='Desempleo sin tendencia ni estacionalidad', y='Desempleo (%)', x = 'Fecha') +
  theme_custom()
gg_resid_dummies
# Los residuales presentan comportamiento de ruido blanco, a excepcion del 2020, el cual
# corresponde a un quiebre estructural, y deberia ser modelado por una dummy o algunotro
# metodo que sea adecuado.

# Analizar acf de la serie resultante
gg_acf_final = ggAcf(df_stl$sin_estacional) +
  labs(title = 'ACF Desempleo Final') +
  theme_custom()
gg_pacf_final = ggPacf(df_stl$sin_estacional) +
  labs(title = 'PACF Desempleo Final') +
  theme_custom()
gg_acf_final
gg_pacf_final

# Parece que tiene todavia correlacion con los primeros rezagos, pero ya no presenta raiz unitaria
# ni estacionalidad. El correlograma parcial muestra un único rezago significativo, lo que indica que la 
# dinámica de la serie puede ser adecuadamente capturada por un modelo autoregresivo de orden 1. Este resultado 
# es consistente con el comportamiento observado en la función de autocorrelación, donde la persistencia decae 
# gradualmente, sugiriendo una estructura AR(1).
