##################################################
# CÓDIGO PARA LA ESTIMACIÓN DE GRIN SIN VIAJES   #
# PABLO VELÁZQUEZ                                #
##################################################

# NOTA: El modelo contempla que el usuario introduzca variables de número de usuarios, scooters y viajes para estimar distintos escenarios

library(dplyr)
library(lubridate)
library(data.table)

# Funcion para numero de operaciones en el dia, numero de patines y numero de usuarios
sample_obs <- function() {  cat("Cuantas operaciones deseas simular en este analisis? ")
  num_sample <- readline()
  cat("Cuantos patines hay? ")
  scooters <- readline()
  cat("Cuantos usuarios hay? ")
  usuarios <- readline()
  nfecha <- as.character(readline(prompt = "A partir de que dia quieres simular? (usa formato YYYY/MM/DD):"))
  return(list(num_sample, scooters, usuarios, nfecha))
}


###########################################################################################
# El usuario debe correr e introducir los valores que requiere el código antes de avanzar
num_sample <- sample_obs()
###########################################################################################

# Alisto las variables en los formatos que requiero
operaciones <- as.integer(num_sample[1])
scooters <- as.integer(num_sample[2])
usuarios <- as.integer(num_sample[3])
fecha <- as.POSIXct(paste(num_sample[4], " 07:00:00"))


# Hago una muestra de las operaciones posibles para la table SCOOTER_EVENT
scooter_id_evento <- as.data.frame(sample(1:scooters, operaciones, replace = TRUE))
created_at_evento <- sample(seq(fecha, length.out = 43200, by = '1 min'), operaciones)
user_id_evento <- as.data.frame(sample(1:usuarios, operaciones, replace=TRUE))
#asumo que el numero de patines en calle esta entre -1 y 1 desviacion estandar del numero total de patines
status <- as.data.frame(rnorm(operaciones))
status <- cut(status$`rnorm(operaciones)`, c(-4,-1, 1, 4), labels = c("Cargando", "Patin en calle", "En reparacion"), include.lowest = T)
scooter_event <- cbind(scooter_id_evento, user_id_evento, created_at_evento, status)
colnames(scooter_event) <- c("scooter_id_evento", "user_id_evento", "created_at_evento", "status_evento")
#para determinar a que hora acabo el evento, asumo que termine cuando empieza el siguiente
scooter_event <- scooter_event[order(scooter_id_evento, created_at_evento),]
scooter_event <- scooter_event %>% group_by(scooter_id_evento) %>% mutate(finished_at_evento = lead(created_at_evento))
scooter_event$finished_at_evento <- as.POSIXct(ifelse(is.na(scooter_event$finished_at_evento), 
                                           max(scooter_event$created_at_evento), scooter_event$finished_at_evento), origin="1970-01-01")

# Hago una muestra de las operaciones posibles para la table RIDE
scooter_id <- as.data.frame(sample(1:scooters, operaciones, replace = TRUE))
user_id <- as.data.frame(sample(1:usuarios, operaciones, replace=TRUE))
started_at <- sample(seq(fecha, length.out = 43200, by = '1 min'), operaciones)
finished_at <- started_at + sample(60:3600, operaciones)
start_location <- paste(sample(seq(from=19.31, to=19.5, by=.001), 1000, replace=TRUE), ",",  sample(seq(from=-99.2, to=-99.07, by=.001), 1000, replace=TRUE), sep="")
finished_location <- paste(sample(seq(from=19.31, to=19.5, by=.001), 1000, replace=TRUE), "," ,  sample(seq(from=-99.2, to=-99.07, by=.001), 1000, replace=TRUE), sep="")
ride <- cbind(scooter_id, user_id, started_at, finished_at, start_location, finished_location)
colnames(ride) <- c("scooter_id", "user_id", "started_at", "finished_at", "started_location", " final_location")
ride <- ride[order(scooter_id, started_at),]


# Uno las tablas para determinar cuales patines que estaban en la calle fueron utilizados
rides1 <- data.table(id = ride$scooter_id, start = ride$started_at, end = ride$finished_at)
setkeyv(rides1, colnames(rides1))
events <- as.data.table(scooter_event[scooter_event$status_evento=='Patin en calle', ])
setnames(events, c(1, 3,5), c('id', 'start', 'end'))
setkeyv(events, cols = c('id', 'start', 'end'))
indice <- foverlaps(rides1, events, type='within', which=TRUE, mult='first')
rides1 <- rides1[, match := +(!is.na(indice))] 
rides1 <- rides1[rides1$match==1,][order(start, id), ]


# Comprimo la informacion por fecha para saber cuantos fueron utilizados
num_scooters_day <- setDT(rides1)[, .(count = uniqueN(id)), by = as.Date(start, tz="MST")]
names(num_scooters_day) <- c("day", "num_of_scooters_w_rides")
num_scooters_day$num_of_scooters_wo_rides <- scooters - num_scooters_day$num_of_scooters_w_rides
setcolorder(num_scooters_day, c(1, 3, 2))
print(num_scooters_day)


