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
  biwavelet
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
        face = "bold",   
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
  mutate(Fecha = as.Date(as.numeric(Fecha), origin = "1899-12-30")) %>% 
  filter(!is.na(Fecha))
# Podemos generar diversos objetos de series de tiempo.
# ts
desempleo_ts = ts(desempleo$Tasa_Desempleo, start = c(2001,1), frequency = 12)
# xts (derivado de zoo)
desempleo_xts = xts(desempleo$Tasa_Desempleo, order.by = desempleo$Fecha)
colnames(desempleo_xts) = 'Tasa_Desempleo'
# tsibble
desempleo_tsibble = desempleo %>%
  as_tsibble(index = Fecha)

# 2. Analisis descriptivo -------------------------------------------------
# 2.1. Grafica ----
# Primero, se graficara la serie para poder ver posibles patrones
gg_serie = ggplot(desempleo_tsibble, aes(x=Fecha, y = Tasa_Desempleo))+
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
lambda_optim = BoxCox.lambda(desempleo_ts, method ="loglik", lower = -5, upper = 5)
boxCox(desempleo_ts~1) 

# Al parecer se necesita un lambda cercano a -1 (Preguntar al profesor)
desempleo_boxcox = BoxCox(desempleo_ts, lambda = lambda_optim)

# Graficar en plotly a ver que tan diferentes son
tiempo = time(desempleo_ts)
df_plotly = data.frame(
  fecha = tiempo, 
  desempleo = as.numeric(desempleo_ts),
  desempleo_trans = as.numeric(desempleo_boxcox)
) 
plot_ly(df_plotly, x = ~fecha) %>%
  add_lines(y = ~desempleo, name = "Desempleo", yaxis = "y1") %>%
  add_lines(y = ~desempleo_trans, name = "Transformado", yaxis = "y2") %>%
  
  layout(
    yaxis = list(title = "Desempleo"),
    yaxis2 = list(
      title = "Transformado",
      overlaying = "y",
      side = "right"
    )
  )
# NO parece que cambie mucho la varianza, y el problema de hacer la transformacion
# es que se pierde la interpretacion economica, dado que es parecido a hacer una
# transformacion 1/x. Vamos a proceder con la serie original

# 2.3 Tendencia ----
# Veamos el acf
gg_acf_original = ggAcf(desempleo_ts) +
  labs(title = 'ACF Desempleo') +
  theme_custom()
gg_acf_original
# Tiene un decaimiento muy lento, lo cual puede indicar presencia de tendencia estocastica
# Analisis usando decompose
plot(decompose(desempleo_ts))

# Usando loess
gg_loess = desempleo_tsibble %>%
  mutate(Desempleo_ajus = smooth_vec(Tasa_Desempleo,span = 0.5, degree = 2))%>%
  ggplot(aes(Fecha, Tasa_Desempleo)) +
  geom_line() +
  geom_line(aes(y = Desempleo_ajus), color = "red") +
  theme_custom()
gg_loess

# Eliminar la tendencia usando diferenciacion. Preguntar si es mejor estimando la tendencia y 
# restandola de la serie
desempleo_diff = desempleo_tsibble %>% 
  mutate(Diff_Desempleo = c(NA,diff(Tasa_Desempleo, 1))) %>% 
  filter(!is.na(Diff_Desempleo))

gg_diff = ggplot(desempleo_diff, aes(x=Fecha, y = Diff_Desempleo))+
  geom_line() +
  labs(y = 'Tasa de Desempleo Diff', title = 'Tasa de Desempleo Nacional Diferenciada') +
  scale_x_date(date_breaks = '3 years') +
  theme_custom()
gg_diff

# ACF diferenciado
gg_acf_diff = ggAcf(desempleo_diff$Diff_Desempleo) +
  labs(title = 'ACF Desempleo Diferenciada') +
  theme_custom()

astsa::lag1.plot(desempleo_diff$Diff_Desempleo, 12, corr=F) 
# Tiene decaimiento lento entonces ya muestra signos de estacionariedad. Sin embargo, se ve que hay 
# correlacion significativa en los rezagos 6, 12, 18 y 24, lo cual indica estacionalidad. Lo cual 
# se analizara en seguida.

# 2.4 Estacionalidad ----
desempleo_diff_xts = xts(desempleo_diff$Diff_Desempleo, order.by = desempleo_diff$Fecha)
ts_heatmap(desempleo_diff_xts, title = 'Heatmap tasa de desempleo diferenciada')
# En enero se ve un aumento del desempleo a lo largo de todos los años

# Grafico de media por mes
prueba = desempleo_diff %>% 
  as_tibble() %>% 
  mutate(mes = month(Fecha, label = T)) %>% 
  group_by(mes) %>% 
  summarise(mean = mean(Diff_Desempleo, na.rm = T)) %>% 
  ggplot(aes(x=mes, y= mean, group = 1)) +
  geom_line() +
  theme_custom()
# Se confirma la estacionalidad, en enero el promedio de la diferencia en el desempleo es mayor que en el resto de meses

## Wavelet
wavelet = desempleo_diff %>% 
  transmute(Fecha = as.numeric(Fecha), Diff_Desempleo) %>% 
  as.matrix() %>% 
  biwavelet::wt()

plot(wavelet, type = "power.corr.norm", main = "Bias-corrected wavelet power",plot.cb=TRUE)
plot(wavelet, type = "power.norm", main = "Biased wavelet power",plot.cb=TRUE)
# No se ve nada claro

desempleo_diff %>% 
  plot_seasonal_diagnostics(.date_var = Fecha,.value = Diff_Desempleo,.feature_set = c("month.lbl"),.geom="boxplot")

# Periodograma
mvspec(desempleo_diff$Diff_Desempleo, lag='no')
# Se ve picos en 0.08, 0,16, 0.25, 0.33, 0.5. Se comprueba estacionalidad anual, con ciclos armonicos en
# 6, 4, 3, 2 meses.

# Estimar estacionalidad con Fourier. Preguntar de que depende K
fit  = desempleo_diff %>%
  model(
    fourier_model = TSLM(
      Diff_Desempleo ~ fourier(K = 1)
    )
  )

augment(fit) %>%
  ggplot(aes(x = Fecha)) +
  geom_line(aes(y = Diff_Desempleo, colour = "Real"), alpha = 0.4) +
  geom_line(aes(y = .fitted, colour = "Fourier"), linewidth = 1) +
  scale_color_manual(values = c("Real" = "black", "Fourier" = "red")) +
  labs(
    title = "Componentes estacionales (Fourier)",
    y = "Diferencia Desempleo",
    x = "Fecha"
  ) +
  theme_minimal()

# Diferenciar la estacionalidad
desempleo_diff = desempleo_diff %>% 
  mutate(Desempleo_sin_season = c(rep(NA,12),diff(Diff_Desempleo, lag = 12)))

# 2.5 Serie final ----
# analizar acf de la serie resultante
serie_final = desempleo_diff %>% 
  filter(!is.na(Desempleo_sin_season)) %>% 
  select(Fecha, Desempleo_sin_season)

gg_acf_final = ggAcf(serie_final$Desempleo_sin_season) +
  labs(title = 'ACF Desempleo Final') +
  theme_custom()
# Preguntar ese acf negativo

gg_final = ggplot(serie_final, aes(x = Fecha, y = Desempleo_sin_season)) + 
  geom_line() +
  theme_custom()

