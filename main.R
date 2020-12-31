library(MLTools)
library(fpp2)
library(lmtest)
library(tseries) #contains adf.test function
library(TSA)
library(readxl)
library(Hmisc) # for computing lagged variables

library(caret)
library(kernlab)
library(nnet)
library(NeuralNetTools)
library(NeuralSens)


library(RColorBrewer)


#### CARGA Y EXPLORACIÓN DE DATOS ####

fdata <- readxl::read_excel("./data/DATOS_Bolivia.xlsx")
fdata <- as.data.frame(fdata)
summary(fdata)


## Tratamiento de missing values ----

# Faltan los últimos datos de temperatura
# Se introducirán los datos de los mismos días del año anterior
fechas <- fdata[which(is.na(fdata$REFTEMP)),]["Time"]
fechas <- fechas[,1] # Se transofrma la matriz en un vector


# Se introduce la temperatura del año anterior en las fechas que no tienen temperatura
for(i in 1:length(fechas)){
    # Se obtienen la fecha y temeratura del año anterior
    fecha.last.year = as.Date(seq(fechas[i], length=2, by="-1 years")[2])
    temperature.last.year = fdata[fdata[,"Time"] == fecha.last.year,"REFTEMP"]
    
    # Se introduce sustituyendo así los NA
    fdata[fdata[,"Time"] == fechas[i],"REFTEMP"] = temperature.last.year
}



## Exploratory data analysis -----

# Se crea la variable anual para ver la evolución de demanda
fdata["YEAR"] <- as.numeric(format(fdata["Time"][,1],'%Y'))
ggplot(fdata)+
    geom_point(aes(x=REFTEMP, y=DEMAND, color = YEAR))+
    labs(title="Demanda diaria por años 2014-20", color = "Año")+
    xlab("Temperatura")+
    ylab("Demanda")+
    scale_x_continuous(breaks = round(seq(min(fdata$REFTEMP), max(fdata$REFTEMP), by = 5),1))+
    theme(
        plot.title = element_text(size=23),
        legend.key.size = unit(2, 'cm'),
        legend.title = element_text(color = "black", size = 23),
        legend.text = element_text(color = "black", size = 17))


# Se crea la variable mes para ver la comparación entre los distintos meses
fdata["MONTH"] <- format(as.Date(fdata["Time"][,1]), '%m')
ggplot(fdata)+geom_point(aes(x=REFTEMP, y=DEMAND, color = MONTH))+
    labs(title="Demanda diaria por meses 2014-20", color = "Mes")+
    xlab("Temperatura")+
    ylab("Demanda")+
    scale_color_discrete(labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
    scale_x_continuous(breaks = round(seq(min(fdata$REFTEMP), max(fdata$REFTEMP), by = 5),1))+
    theme(
        plot.title = element_text(size=23),
        legend.key.size = unit(1.2, 'cm'),
        legend.title = element_text(color = "black", size = 23),
        legend.text = element_text(color = "black", size = 17))



# Se compara también la media mensual y diaria para cada uno de los años
means <- aggregate(fdata[, c("DEMAND")], by = list(fdata$YEAR,fdata$MONTH ) , mean)
colnames(means) <- c("YEAR", "MONTH", "AVG.DEMAND")
ggplot(means, aes(x = YEAR, y=AVG.DEMAND, label = MONTH))+
    geom_point(aes(x=YEAR, y=AVG.DEMAND, color =MONTH ))+
    labs(title="Demanda media mensual 2014-20", color = "Mes")+
    xlab("Año")+
    ylab("Demanda media")+
    geom_text(aes(label=MONTH),hjust=-1, vjust=0)+
    scale_color_discrete(labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
    scale_x_continuous(breaks = round(seq(min(fdata$YEAR), max(fdata$YEAR), by = 1),1))+
    theme(
        plot.title = element_text(size=23),
        legend.key.size = unit(1.2, 'cm'),
        legend.title = element_text(color = "black", size = 23),
        legend.text = element_text(color = "black", size = 17))
means[which.min(means$AVG.DEMAND),] # Minimum AVG is April 2020
means[which.max(means$AVG.DEMAND),] # Max AVG is October 2020

means <- aggregate(fdata[, c("DEMAND")], by = list(fdata$YEAR,fdata$WEEKDAY ) , mean)
colnames(means) <- c("YEAR", "WEEKDAY", "AVG.DEMAND")
myPalette <- colorRampPalette(rev(brewer.pal(7, "Accent")))
sc <- scale_colour_gradientn(name="Día", colours = myPalette(7), limits=c(1,7), labels=c("1 L", "2 M", "3 X", "4 J", "5 V", "6 S", "7 D"))
ggplot(means, aes(x = YEAR, y=AVG.DEMAND, label = WEEKDAY))+
    geom_point(aes(x=YEAR, y=AVG.DEMAND, color =WEEKDAY ))+
    geom_text(aes(label=WEEKDAY),hjust=-1, vjust=0)+
    labs(title="Demanda media según día de la semana 2014-20", color = "Día")+
    xlab("Año")+
    ylab("Demanda media")+
    sc+
    scale_x_continuous(breaks = round(seq(min(fdata$YEAR), max(fdata$YEAR), by = 1),1))+
    theme(
        plot.title = element_text(size=23),
        legend.key.size = unit(2, 'cm'),
        legend.title = element_text(color = "black", size = 23),
        legend.text = element_text(color = "black", size = 17))

means[which.min(means$AVG.DEMAND),] # Minimum AVG is monday 2015
means[which.max(means$AVG.DEMAND),] # Max AVG is saturday 2019


# Centrándonos en 2020, se intenta obtener el momento en el cual el covid empieza a afectar
data.2020 <- fdata[  fdata["YEAR"]==2020,]
myPalette <- colorRampPalette(rev(brewer.pal(7, "Accent")))
sc <- scale_colour_gradientn(name="Día", colours = myPalette(7), limits=c(1,7), labels=c("L", "M", "X", "J", "V", "S", "D"))
ggplot(data.2020)+
    geom_point(aes(x=as.Date(Time), y=DEMAND, color = WEEKDAY)) +
    sc + 
    labs(title="Demanda diaria 2020")+
    xlab("Fecha")+
    ylab("Demanda")+
    scale_x_date(date_labels = "%b", date_breaks = "1 month")+
    theme(
        plot.title = element_text(size=23),
        legend.key.size = unit(2, 'cm'),
        legend.title = element_text(color = "black", size = 23),
        legend.text = element_text(color = "black", size = 17))
    


ggplot() + 
    geom_line(data = fdata[  fdata["YEAR"]==2020,], aes(x=as.Date(Time), y = DEMAND), color = "red") +
    geom_line(data = fdata[  fdata["YEAR"]==2019,], aes(x=as.Date(Time), y = DEMAND), color = "blue") +
    geom_line(data = fdata[  fdata["YEAR"]==2018,], aes(x=as.Date(Time), y = DEMAND), color = "light blue") +
    labs(title="Demanda energética 2018-20")+
    xlab('Fecha') +
    ylab('Demanda')+
    scale_x_date(date_labels = "%b", date_breaks = "2 month")+
    theme(
        plot.title = element_text(size=23))
        


ggplot() + 
    geom_line(data = fdata[  fdata["YEAR"]==2020 & fdata["MONTH"]=="03" ,], aes(x=as.Date(Time), y = DEMAND), color = "blue") +
    labs(title="Demanda marzo 2020")+
    xlab('Fecha') +
    ylab('Demanda')+
    theme(
        plot.title = element_text(size=23))

ggplot() + 
    geom_line(data = fdata[  fdata["YEAR"]==2019 & fdata["MONTH"]=="03" ,], aes(x=as.Date(Time), y = DEMAND), color = "blue") +
    labs(title="Demanda marzo 2019")+
    xlab('Fecha') +
    ylab('Demanda')+
    theme(
        plot.title = element_text(size=23))

## Se crean tablas distintas para pre y post COVID en caso de ser necesarias más adelante
fdata.precovid = fdata[fdata[,"Time"]<as.Date("2020-03-16"),] 
fdata.postcovid = fdata[fdata[,"Time"]>=as.Date("2020-03-16"),]

# Se observa la relación de GDP con demanda
normalize<-function(x) {return ((x-min(x))/(max(x)-min(x)))}
ggplot(data = fdata, aes(x = as.Date(Time))) + 
    geom_line(aes(y = normalize(DEMAND), colour = "Demand")) +
    geom_line(aes(y = normalize(GDP2020), colour = "GDP")) +
    labs(title="Relación Demanda ~ GDP (2018-20)", color = "Serie")+
    xlab('Fecha') +
    ylab('Valor normalizado')+
    scale_color_manual(values = c(Demand = 'light blue', GDP = 'black'))+
    scale_x_date(date_labels = "%Y", date_breaks = "1 year")+
    theme(
        plot.title = element_text(size=23),
        legend.key.size = unit(2, 'cm'),
        legend.title = element_text(color = "black", size = 23),
        legend.text = element_text(color = "black", size = 17))

# Se observa la relación de SPECIAL con demanda


means <- aggregate(fdata[, c("DEMAND")], by = list(fdata$YEAR,fdata$SPECIAL ) , mean)
colnames(means) <- c("YEAR", "SPECIAL", "AVG.DEMAND")
ggplot(means, aes(x = YEAR, y=AVG.DEMAND, fill=factor(SPECIAL)))+
    geom_bar(stat="identity",position="dodge")+
    scale_fill_manual("Día Especial", values=c("0"="gray","1"="light blue" ), labels = c("No", "Sí"))+
    labs(title="Demanda media según tipo de día 2014-20")+
    xlab("Año")+
    ylab("Demanda media")+
    scale_x_continuous(breaks = round(seq(min(fdata$YEAR), max(fdata$YEAR), by = 1),1))+
    scale_y_continuous(breaks = round(seq(0, 25000, by = 5000),1))+
    theme(
        plot.title = element_text(size=23),
        legend.key.size = unit(2, 'cm'),
        legend.title = element_text(color = "black", size = 23),
        legend.text = element_text(color = "black", size = 17))




## Creación de tabla de comparación de modelos ----
df.models <- data.frame(Model=character(), Covid.Accounted = logical(),DynReg = logical(), Rolling.Error=double(), stringsAsFactors = FALSE)

#### MODELO ARIMA INICIAL - ajuste sin COVID ####

## Arima sin regresión dinámica ----

# Transformación en serie temporal 

head(fdata)
fdata$Time <- as.Date(fdata$Time)

dia.ano.inicial =  as.numeric(format(fdata$Time[1], "%j"))
fdata_ts <- ts(fdata$DEMAND, start = c(2014, dia.ano.inicial),frequency = 365)


fdata[fdata[,"Time"]==as.Date("2020-03-16"),] # Se busca el índice del 16 de marzo
dia.ano.inicio.covid =  as.numeric(format(fdata$Time[1934], "%j")) # Se busca qué día del año es ese
fdata_ts_no_covid <- ts(fdata$DEMAND, start = c(2014, dia.ano.inicial),end = c(2020, dia.ano.inicio.covid),frequency = 365)


# Functions for plotting a time series
#Plot time series
forecast::autoplot(fdata_ts_no_covid) +
    ggtitle("Serie demanda sin datos COVID") +
    xlab('Fecha') +
    ylab('Demanda')


# Se obtiene la longitud de los datos y se divide en train y test
longitud <- length(fdata_ts_no_covid)
longitud_train <- longitud-7
longitud_test <- 7 
#longitud_train <- longitud*0.8
#longitud_test <- longitud*0.2

fdata_ts_train <- subset(fdata_ts_no_covid,end = longitud-longitud_test)
fdata_ts_val <- subset(fdata_ts_no_covid,start = longitud-longitud_test+1)

length(fdata_ts_train)
length(fdata_ts_val)


y <- fdata_ts_train
autoplot(y)
# Box-Cox transformation
Lambda <- BoxCox.lambda.plot(y,7) # Varianza no sufre cambios importantes con el cambio de nivel medio

z <- BoxCox(y,Lambda)
#autoplot(z)
#z<-y

# Differentiation: if the ACF decreases very slowly -> needs differenciation
ggtsdisplay(z)

# Regular differencing
Bz <- diff(z,differences = 1)
ggtsdisplay(Bz, lag.max=50) 

# Seasonal differencing
B7z <- diff(z,lag = 7, differences = 1)
ggtsdisplay(B7z, lag.max=50) 

# Regular and seasoal differencing
B7Bz <- diff(Bz, lag = 7, differences = 1)
ggtsdisplay(B7Bz,lag.max = 50)
# Me queda uno significativo y un decr exponencial


# Opción 1
# Fit seasonal model with estimated order
arima.fit <- Arima(y,
                   order=c(1,1,2),
                   seasonal = list(order=c(5,1,0), period=7),
                   lambda = Lambda,
                   include.constant = FALSE)
summary(arima.fit) # summary of training errors and estimated coefficients
coeftest(arima.fit) # statistical significance of estimated coefficients
autoplot(arima.fit) # root plot

# Check residuals
CheckResiduals.ICAI(arima.fit, bins = 30, lag = 50)
# If residuals are not white noise, change order of ARMA
ggtsdisplay(residuals(arima.fit),lag.max = 100)
arima.fit.1 <- arima.fit

# Opción 2
# Fit seasonal model with estimated order
arima.fit <- Arima(y,
                   order=c(1,1,2),
                   seasonal = list(order=c(5,1,0), period=7),
                   #lambda = Lambda,
                   include.constant = FALSE)
summary(arima.fit) # summary of training errors and estimated coefficients
coeftest(arima.fit) # statistical significance of estimated coefficients
autoplot(arima.fit) # root plot

# Check residuals
CheckResiduals.ICAI(arima.fit, bins = 30, lag = 50)
# If residuals are not white noise, change order of ARMA
ggtsdisplay(residuals(arima.fit),lag.max = 100)
arima.fit.2 <- arima.fit




# Comparación de ambos modelos

arimaRollingError <- function(fdata_ts_train, fdata_ts_val, arima.fit, returnPredictions = FALSE){
    longitud_test = length(fdata_ts_val)
    total.error = 0
    observation.error = 0
    prediction.results <- list()
    for (i in  1:length(fdata_ts_val)){
        
        # Se actualiza la serie temporal de entrada con el dato verdadero del día siguiente
        fdata_ts_train_updated <- subset(fdata_ts,end = length(fdata_ts)-longitud_test + i -1) # Cuando i = 1, coge length - longtest (predices la meuestra i de fdata_ts_val)
        
        # Con el mismo modelo obtenido anteriormente, de vuelve a ajustar los parámetros con los nuevos datos
        arima.refit <- Arima(y = fdata_ts_train_updated, model = arima.fit)  
        
        # Se calcula la estimación con los nuevos datos pero retrained
        y_est.refit <- forecast(arima.refit, h=1)
        
        
        # Se calcula el sq error 
        observation.error = (as.numeric(y_est.refit$mean)- fdata_ts_val[i])^2
        
        # Y se añade al valor total
        total.error = total.error + observation.error
        
        
        # Se obtienen el mes y año de predicción
        row.name = row.names(data.frame(y_est.refit))
        
        # Se añade la predicción a la lista
        prediction.results[[row.name]] <- y_est.refit
    }
    
    rmse.error = sqrt(total.error/length(fdata_ts_val))
    
    return.value = -1
    if (returnPredictions){
        return.value = prediction.results
    }else{
        return.value = rmse.error
    }
    return(return.value)
    
}


# Como era de esperar, ambos modelos producen resultados similares 
error.1 = arimaRollingError(fdata_ts_train, fdata_ts_val, arima.fit.1, FALSE)
error.2 = arimaRollingError(fdata_ts_train, fdata_ts_val, arima.fit.2, FALSE)

print(error.1)
print(error.2)
df.models[nrow(df.models) + 1,] = list("ARIMA (1,1,2) (5,1,0) [7] (lambda)",FALSE, FALSE, error.1)
df.models[nrow(df.models) + 1,] = list("ARIMA (1,1,2) (5,1,0) [7]",FALSE, FALSE, error.2)


# El modelo elegido se muestra respecto a los valores reales
arima.fit <- arima.fit.2

# Check fitted forecast
autoplot(y, series = "Real", main = "Comparación de la serie con modelo obtenido", ylab = "Demanda" )+
    forecast::autolayer(arima.fit$fitted, series = "Fitted")

# Perform future forecast
y_est <- forecast(arima.fit, h=length(fdata_ts_val))
y_est_cut <- window(y_est, start = c(2019,1))
autoplot(y_est)
y_est$mean





## Arima con regresión dinámica ----
ggplot(fdata)+
    geom_point(aes(x=REFTEMP, y=DEMAND))+
    labs(title="Temperaturas diarias 2014-20", color = "Año")+
    xlab("Temperatura")+
    ylab("Demanda")+
    scale_x_continuous(breaks = round(seq(min(fdata$REFTEMP)-0.2, max(fdata$REFTEMP), by = 2),1))+
    theme(
        plot.title = element_text(size=23))
    

mean(fdata$REFTEMP)
quantile(fdata$REFTEMP, 0.25)
quantile(fdata$REFTEMP, 0.75)
min(fdata$REFTEMP)
max(fdata$REFTEMP)

ggplot(fdata, aes(x=REFTEMP))+ 
    geom_histogram(fill = "light blue")+
    labs(title="Distribución temperatura diaria 2014-20")+
    xlab("Temperatura")+
    ylab("Nº observaciones")+
    theme(
        plot.title = element_text(size=23))


ggplot(fdata, aes(x=DEMAND)) +
    geom_histogram(fill = "light blue")+
    labs(title="Distribución demanda diaria 2014-20")+
    xlab("Demanda")+
    ylab("Nº observaciones")+
    theme(
        plot.title = element_text(size=23))



#Create new time series splitting the temperatures
fdata$TCOLD <- sapply(fdata$REFTEMP,min,17.5) # Coge el mínimo entre la temperatura y 17
fdata$THOT <- sapply(fdata$REFTEMP,max,17.5) # Aplica la función máximo entre temperatura y 17.5
#ggplot(fdata)+geom_point(aes(x=tcold, y=DEMAND))
#ggplot(fdata)+geom_point(aes(x=thot, y=DEMAND))

#Convert to time series object
dia.ano.inicial =  as.numeric(format(fdata$Time[1], "%j"))
fdata_ts <- ts(fdata, start = c(2014, dia.ano.inicial),frequency = 365)
autoplot(fdata_ts, facets = TRUE)

#Create time series 
y <- fdata_ts[,"DEMAND"]
x <- fdata_ts[,c("SPECIAL","TCOLD", "THOT")]
autoplot(cbind(y,x), facets = TRUE)

#Scale
y.trans <- y/10000
x.trans <- x
x.trans[,"TCOLD"] <- x[,"TCOLD"]/10
x.trans[,"THOT"] <- x[,"THOT"]/10
autoplot(cbind(y.trans,x.trans), facets = TRUE)+
    labs(title="Variables para regresión dinámica")+
    xlab("Variables")+
    ylab("Series")+
    theme(
        plot.title = element_text(size=23))


# Separate covid data
y.trans_no_covid <- ts(y.trans, start = c(2014, dia.ano.inicial),end = c(2020, dia.ano.inicio.covid),frequency = 365)
x.trans_no_covid <- ts(x.trans, start = c(2014, dia.ano.inicial),end = c(2020, dia.ano.inicio.covid),frequency = 365)



# Se obtiene la longitud de los datos y se divide en train y test
longitud <- length(y.trans_no_covid)
longitud_train <- longitud-7
longitud_test <- 7

#longitud_train <- longitud*0.8
#longitud_test <- longitud*0.2

y.train <- subset(y.trans_no_covid,end = longitud-longitud_test)
y.val <- subset(y.trans_no_covid,start = longitud-longitud_test+1)


x.train <- subset(x.trans_no_covid,end = longitud-longitud_test)
x.val  <- subset(x.trans_no_covid,end = longitud-longitud_test)


TF.fit <- arima(y.train,
                order=c(1,0,0),
                seasonal = list(order=c(1,1,0),period=7),
                xtransf = x.train,
                transfer = list(c(0,9), c(0,9), c(0,9)), #List with (r,s) orders
                include.mean = TRUE,
                method="ML")
summary(TF.fit) # summary of training errors and estimated coefficients
coeftest(TF.fit) # statistical significance of estimated coefficients
# Check regression error to see the need of differentiation
TF.RegressionError.plot(y.train,x.train,TF.fit,lag.max = 60)
#NOTE: If this regression error is not stationary in variance,boxcox should be applied to input and output series.

# Check numerator coefficients of explanatory variable
TF.Identification.plot(x.train,TF.fit)


# SPECIAL
# Como el primero es significativo, B vale 0
# Como no hay patrón la r vale 0 - se cambia a 1 para ver si ajusta mejor por la corr cruzada. Resulta que 1 no es significativo
# Como empieza a decreccer desde el tercero la s vale 2

# TCOLD
# Como el tercero es significativo, B vale 0
# Como no hay patrón la r vale 0
# Como empieza a decreccer desde el segundo la s vale 1 - esto se ajusta a 0 porque no es significativo

# THOT
# Como el primero es significativo, B vale 0
# Como es patrón expl, la r vale 1
# Como empieza a decreccer desde el primero la s vale 0



#### Fit arima noise with selected
#x_2_lag = Lag(x_2,2)   # b
#xlag <- ts(cbind(x_1, x_2_lag), frequency = 12)
#xlag[is.na(xlag)]=0
# No hay lags necesarios, todas las b=0
xlag = x.train

arima.fit <- arima(y.train,
                   order=c(0,0,2),
                   seasonal = list(order=c(1,1,1),period=7),
                   xtransf = xlag,
                   transfer = list(c(0,2),c(0,0), c(1,0) ), #List with (r,s) orders
                   include.mean = FALSE,
                   method="ML")
summary(arima.fit) # summary of training errors and estimated coefficients
coeftest(arima.fit) # statistical significance of estimated coefficients
# Check residuals
CheckResiduals.ICAI(arima.fit, bins = 30, lag = 50)
# If residuals are not white noise, change order of ARMA
ggtsdisplay(residuals(arima.fit),lag.max = 50)


### Cross correlation residuals - expl. variable
res <- residuals(arima.fit)
res[is.na(res)] <- 0
ccf(y = res, x = x.train[,1]) # Aquí puede haber una relación que no está siendo capturada del todo
ccf(y = res, x = x.train[,2])
ccf(y = res, x = x.train[,3])

# Error de predicción a un paso

dynamicRollingError <- function(fitted.model,x,y, x.train, x.valid, y.train, y.valid, returnPredictions = FALSE){
    
    longitud_tot = length(y.train) + length(y.valid)
    longitud_valid =  length(y.valid)
    
    observation.error = 0
    total.error.squared = 0
    predictions = NULL
    for (i in  1:longitud_valid){
        i = 3
        y_train_updated <- subset(y,end = longitud_tot-longitud_valid + i -1) # Cuando i = 1, coge length - longtest (predices la meuestra i de fdata_ts_val)
        x_train_updated <- subset(x,end = longitud_tot-longitud_valid + i -1) 
        
        val.forecast_h1 <- TF.forecast(y.old = as.matrix(y_train_updated), #past values of the series
                                       x.old = as.matrix(x_train_updated), #Past values of the explanatory variables
                                       x.new = subset(x.valid, start = i, end = i), #New values of the explanatory variables
                                       model = fitted.model, #fitted transfer function model
                                       h=1) #Forecast horizon
        
        observation.error = (val.forecast_h1- y.valid[i])^2
        total.error.squared = total.error.squared + observation.error
        predictions = c(predictions, val.forecast_h1)
        
    }
    
    if (returnPredictions){
        
    }else{
        return(   sqrt(total.error.squared/longitud_valid))   
    }
}
error = dynamicRollingError(arima.fit, x.trans_no_covid, y.trans_no_covid, x.train, x.val, y.train, y.val)
error.unscaled = error * 10000 # se deshace el escalado que se hizo con la y. Como están en la misma unidad esto se puede hacer sin problema
print(error.unscaled)
df.models[nrow(df.models) + 1,] = list("ARIMA (0,0,2) (1,1,1) [7] (0,2)(0,0)(1,0)",FALSE, TRUE, error.unscaled)






## Comparación predicciones COVID con realidad ----

# Separate covid data -- 1935 is the index of the separation between non covid and covid effect
y.trans_no_covid <- ts(y.trans[1:1935], start = c(2014, dia.ano.inicial),frequency = 365)
x.trans_no_covid <- ts(x.trans[1:1935,], start = c(2014, dia.ano.inicial),frequency = 365)

length(y.trans_no_covid)
length(y.trans)

dia.ano.inicio.covid.mas1 =  as.numeric(format(fdata$Time[1935], "%j")) # Se busca qué día del año es ese
y.trans_yes_covid <- ts(y.trans[1936:2138], start = c(2020, dia.ano.inicio.covid.mas1),frequency = 365)
x.trans_yes_covid <- ts(x.trans[1936:2138,], start = c(2020, dia.ano.inicio.covid.mas1),frequency = 365)
yes.covid.dates <- as.Date(fdata[1936:2138,"Time"])

length(y.trans_yes_covid)
length(y.trans_yes_covid) + length(y.trans_no_covid) == length(y.trans)

# Se ajustan el train y test para que correspondan a las etapas cony sin covid
y.train <- y.trans_no_covid
y.val <- y.trans_yes_covid


x.train <- x.trans_no_covid
x.val  <- x.trans_yes_covid


forecast_hcovid <- TF.forecast(y.old = as.matrix(y.train), #past values of the series
                               x.old = as.matrix(x.train), #Past values of the explanatory variables
                               x.new = as.matrix(x.val), #New values of the explanatory variables
                               model = arima.fit, #fitted transfer function model
                               h=length(y.val)) #Forecast horizon


# Comparación de series
df.comparison <- data.frame(Time.2020=yes.covid.dates, True.Value.2020 = as.numeric(y.val)*10000,Forecast.2020 = as.numeric(forecast_hcovid)*10000, stringsAsFactors = FALSE)
ggplot(data = df.comparison, aes(x = as.Date(Time.2020))) + 
    geom_line(aes(y = True.Value.2020, colour = "Real")) +
    geom_line(aes(y = Forecast.2020, colour = "Estimada")) +
    labs(title="2020 | Comparación valor esperado con serie real")+
    xlab('Fecha') +
    ylab('Demanda')+
    scale_color_manual("Series 2020",values = c("Real" = 'black', "Estimada"= 'blue'))+
    scale_x_date(date_labels = "%d %b", date_breaks = "2 weeks")+
    theme(
        plot.title = element_text(size=23),
        legend.key.size = unit(2, 'cm'),
        legend.title = element_text(color = "black", size = 23),
        legend.text = element_text(color = "black", size = 17))


# Comparación de errores en época de covid vs no época de covid

# Se introduce el error de 2020
df.comparison$Error.2020 = df.comparison$Forecast.2020 - df.comparison$True.Value.2020


# Ahora se hace lo mismo para 2019 para poder comparar los errores
# Se calcula el error de 2019  --- 1569 es el índice de 17 marzo de 2019
y.trans.2019.comp <- ts(y.trans[1:1569], start = c(2014, dia.ano.inicial),frequency = 365)
x.trans.2019.comp <- ts(x.trans[1:1569,], start = c(2014, dia.ano.inicial),frequency = 365)


# Start dates for 2020 and 2019
fdata[1935,]
fdata[1569,]


# End dates for 2020 and 2019
fdata[2138,]
fdata[1772,]


length(y.trans.2019.comp)
length(y.trans.2019.comp)

dia.ano.inicio.comparacion.mas1 =  as.numeric(format(fdata$Time[1569], "%j")) # Se busca qué día del año es ese
y.trans_comparison_val <- ts(y.trans[1570:1772], start = c(2019, dia.ano.inicio.comparacion.mas1),frequency = 365)
x.trans_comparison_val <- ts(x.trans[1570:1772,], start = c(2019, dia.ano.inicio.comparacion.mas1),frequency = 365)
comparison_val_dates <- as.Date(fdata[1570:1772,"Time"])


# Se hace el forecast correspondiente a 2019

forecast_hcomparison <- TF.forecast(y.old = as.matrix(y.trans.2019.comp), #past values of the series
                               x.old = as.matrix(x.trans.2019.comp), #Past values of the explanatory variables
                               x.new = as.matrix(x.trans_comparison_val), #New values of the explanatory variables
                               model = arima.fit, #fitted transfer function model
                               h=length(y.trans_comparison_val)) #Forecast horizon


# Se introducen en la tabla
df.comparison$Time.2019 =comparison_val_dates
df.comparison$True.Value.2019 = as.numeric(y.trans_comparison_val)*10000
df.comparison$Forecast.2019 = as.numeric(forecast_hcovid)*10000
df.comparison$Error.2019 = df.comparison$Forecast.2019 - df.comparison$True.Value.2019


ggplot(data = df.comparison, aes(x = as.Date(Time.2019))) + 
    geom_line(aes(y = True.Value.2019, colour = "Real")) +
    geom_line(aes(y = Forecast.2019, colour = "Estimada")) +
    geom_col(aes(y=Error.2019, fill="Error"))+
    labs(title="2019 | Comparación valor esperado con serie real", label = "")+
    xlab('Fecha') +
    ylab('Demanda')+
    scale_fill_manual("Diferencia", values = c("Error"="rosybrown1"))+
    scale_color_manual("Series 2019",values = c("Real" = 'black', "Estimada"= 'blue'))+
    scale_x_date(date_labels = "%d %b", date_breaks = "2 weeks")+
    theme(
        plot.title = element_text(size=23),
        legend.key.size = unit(2, 'cm'),
        legend.title = element_text(color = "black", size = 23),
        legend.text = element_text(color = "black", size = 17))


ggplot(data = df.comparison, aes(x = as.Date(Time.2020))) + 
    geom_line(aes(y = True.Value.2020, colour = "Real")) +
    geom_line(aes(y = Forecast.2020, colour = "Estimada")) +
    geom_col(aes(y=Error.2020, fill="Error"))+
    labs(title="2020 | Comparación valor esperado con serie real", label = "")+
    xlab('Fecha') +
    ylab('Demanda')+
    scale_fill_manual("Diferencia", values = c("Error"="rosybrown1"))+
    scale_color_manual("Series 2020",values = c("Real" = 'black', "Estimada"= 'blue'))+
    scale_x_date(date_labels = "%d %b", date_breaks = "2 weeks")+
    theme(
        plot.title = element_text(size=23),
        legend.key.size = unit(2, 'cm'),
        legend.title = element_text(color = "black", size = 23),
        legend.text = element_text(color = "black", size = 17))



# Para calcular el efecto en demanda
mean.error.2019 = mean(df.comparison$Error.2019)
mean.error.2020 = mean(df.comparison$Error.2020)



which(df.comparison$True.Value.2020==max(df.comparison$True.Value.2020))
df.comparison[192,]

# El 190 ya es error negativo y lo consideramos como recuperación del COVID ya
error.2020.prev.recuperacion = df.comparison$Error.2020[1:189]

error.2020.sup = error.2020.prev.recuperacion
error.2020.inf = error.2020.prev.recuperacion - mean.error.2019

sum.error.2020.sup = sum(error.2020.sup)
sum.error.2020.inf = sum(error.2020.inf)


economic.effect.Bs = c(sum.error.2020.inf,sum.error.2020.sup)*0.91 # 0.91 is mean price per KWh
economic.effect.M.Bs = economic.effect.Bs/(10^6) # Millions




#### MODELO ARIMA RETOCADO - ajuste con COVID ####

## Arima con regresión dinámica ----

#Create new intervention variable
fdata$COVID <- ifelse(fdata$Time >= as.Date("2020-03-16"), 1, 0) # If after march 16 = 1, else  0 

#Convert to time series object
dia.ano.inicial =  as.numeric(format(fdata$Time[1], "%j"))
fdata_ts <- ts(fdata, start = c(2014, dia.ano.inicial),frequency = 365)
autoplot(fdata_ts, facets = TRUE)


#Create time series 
y <- fdata_ts[,"DEMAND"]
x <- fdata_ts[,c("SPECIAL","TCOLD", "THOT", "COVID")]
autoplot(cbind(y,x), facets = TRUE)

#Scale
y.trans <- y/10000
x.trans <- x
x.trans[,"TCOLD"] <- x[,"TCOLD"]/10
x.trans[,"THOT"] <- x[,"THOT"]/10
autoplot(cbind(y.trans,x.trans), facets = TRUE)+
    labs(title="Variables para regresión dinámica")+
    xlab("Variables")+
    ylab("Series")+
    theme(
        plot.title = element_text(size=23))



y.trans_yes_covid <- ts(y.trans, start = c(2014, dia.ano.inicial),frequency = 365)
x.trans_yes_covid <- ts(x.trans, start = c(2014, dia.ano.inicial),frequency = 365)
length(y.trans_yes_covid) 
length(y.trans_no_covid)

# Se obtiene la longitud de los datos y se divide en train y test
longitud <- length(y.trans_yes_covid)
longitud_train <- longitud - 7
longitud_test <- 7
#longitud_train <- longitud*0.95
#longitud_test <- longitud*0.05 # Tiene que tener algunas muestras de covid
#longitud_test < length(y.trans_yes_covid) -length(y.trans_no_covid)

y.train <- subset(y.trans_yes_covid,end = longitud-longitud_test)
y.val <- subset(y.trans_yes_covid,start = longitud-longitud_test+1)

x.train <- subset(x.trans_yes_covid,end = longitud-longitud_test)
x.val  <- subset(x.trans_yes_covid,end = longitud-longitud_test)


TF.fit <- arima(y.train,
                order=c(1,0,0),
                seasonal = list(order=c(1,1,0),period=7),
                xtransf = x.train,
                transfer = list(c(0,9), c(0,9), c(0,9), c(0,9)), #List with (r,s) orders
                include.mean = TRUE,
                method="ML")
summary(TF.fit) # summary of training errors and estimated coefficients
coeftest(TF.fit) # statistical significance of estimated coefficients
# Check regression error to see the need of differentiation
TF.RegressionError.plot(y.train,x.train,TF.fit,lag.max = 60)
#NOTE: If this regression error is not stationary in variance,boxcox should be applied to input and output series.

# Check numerator coefficients of explanatory variable
TF.Identification.plot(x.train,TF.fit)


# SPECIAL
# Como el primero es significativo, B vale 0
# Como no hay patrón la r vale 0
# Como empieza a decreccer desde el tercero la s vale 2

# TCOLD
# Como el primero es significativo, B vale 0
# Como no hay patrón la r vale 0
# Como empieza a decreccer desde el segundo la s vale 1

# THOT
# Como el primero es significativo, B vale 0
# Como es patrón expl, la r vale 1
# Como empieza a decreccer desde el primero la s vale 0

# COVID
# Como el primero es significativo, B vale 7
# Como no hay patrón r vale 0
# Como empieza a decreccer desde el tercero la s vale 2


#### Fit arima noise with selected
xlag = x.train
xlag[,4] <- Lag(x.train[,4],7)
xlag[is.na(xlag[,4]),4]=0


arima.fit <- arima(y.train,
                   order=c(0,0,3),
                   seasonal = list(order=c(0,1,2),period=7),
                   xtransf = xlag,
                   transfer = list(c(0,1),c(0,1), c(0,0), c(0,0) ), #List with (r,s) orders
                   include.mean = FALSE,
                   method="ML")
summary(arima.fit) # summary of training errors and estimated coefficients
coeftest(arima.fit) # statistical significance of estimated coefficients
# Check residuals
# If residuals are not white noise, change order of ARMA
CheckResiduals.ICAI(arima.fit, bins = 30, lag = 50)
ggtsdisplay(residuals(arima.fit),lag.max = 50)


### Cross correlation residuals - expl. variable
res <- residuals(arima.fit)
res[is.na(res)] <- 0
ccf(y = res, x = x.train[,1]) # Aquí puede haber una relación que no está siendo capturada del todo
ccf(y = res, x = x.train[,2])
ccf(y = res, x = x.train[,3])
ccf(y = res, x = x.train[,4])

error = dynamicRollingError(arima.fit, x.trans_yes_covid, y.trans_yes_covid, x.train, x.val, y.train, y.val)
error.unscaled = error * 10000 # se deshace el escalado que se hizo con la y. Como están en la misma unidad esto se puede hacer sin problema
print(error.unscaled)
df.models[nrow(df.models) + 1,] = list("ARIMA (0,0,3) (0,1,2) [7] (0,1)(0,1)(0,0)(0,0)",TRUE, TRUE, error.unscaled)


