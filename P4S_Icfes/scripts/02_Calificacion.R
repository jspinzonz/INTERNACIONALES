################################################################################
# # 02_Calificacion.R
# # R Versions: R version 3.2.3 x64
# #
# # Author(s): Jorge Mario Carrasco Ortiz y Jeison Rodriguez
# #
# # Pisa For Schools (P4S)
# # Calcular IRT 
# #
# # Inputs: Base de datos depurada del proceso 00_depuracion
# #
# # Outputs: Un archivo xlsx y txt con los valores plausibles para los colegios
# #          participantes en P4S
# #
# # File history:
# #   20170831: Creation
# # ToDo: - Creacion de las funciones para calificacion
# #       - Implementar tabla de salida de los proceso de calificacion
################################################################################

################################################################################
# # Definicon de carpetas 
################################################################################
inPath  <- file.path("..","input")
outPath <- file.path("..", "output")
srcPath <- file.path("..","src")
funPath <- "Functions"

################################################################################
# # Cargar Librerias
################################################################################
library(data.table)
library(xlsx)
library(TAM)
library(dplyr)
library(GDAtools)
library(survey)


###############################################################################
# # Cargar funciones para la calificacion
################################################################################
source(file.path(funPath, "scoreFunctions.R"))

################################################################################
# # Insumos del proceso
################################################################################
# # Definir llave
colID <- c("SCHOOL_ID", "STUDENT_ID")

# # parametros internaciones
fileParam <- file.path(inPath, "PBTS_InternationalParameters.xlsx")
interDiff <- xlsx::read.xlsx(fileParam, sheetName = "ALL", 
                             stringsAsFactors = FALSE)

# # Lectura de base depurada 
fileDat <- file.path(inPath, "FieldTrialCleaned.csv")
bdTAM   <- read.csv(fileDat, header = T, sep = ",", dec = ".", fileEncoding  = "latin1")
if (any(duplicated(bdTAM[, colID]))) {
	stop("ERROR..... Existen STUDENT_ID duplicados")
}

# # Ajuste de variables
setnames(bdTAM,  c("X15.ST001.A", "X15.ST004"), c("GRADE", "GENDER"))
bdTAM[, "GRADE"] <- bdTAM[, "GRADE"] - 10
auxCuader <- dichotom(bdTAM$Cuadernillo)
setnames(auxCuader, names(auxCuader), gsub("data\\.", "C", names(auxCuader)))
bdTAM <- cbind(bdTAM, auxCuader)

# # Lectura cuestionario de estudiantes 
filACP <- file.path(inPath, "P4SEst.txt")
filACP <- data.table(read.table(filACP, header = T, sep = "\t", dec = ",", 
	                 fileEncoding = "latin1"))
bdHISEI <- computeHISEI(filACP, colHISEI = c("X15.ST014.A", "X15.ST015.A"), 
	                    codeHISEI = file.path(inPath, "HISEI_Syntax.sps"))
bdTAM <- merge(bdTAM, bdHISEI[, .(SCHOOL_ID, STUDENT_ID, HISEI)], 
	           by = colID, all.x = TRUE)

if (any(is.na(bdTAM[, "HISEI"]))) {
	stop("ERROR..... Existen STUDENT_ID sin HISEI")
}

# # Lectura de N por establecimiento
fileNR <- file.path(inPath, "Conteos_Establecimiento.csv")
nReal  <- data.table(read.table(fileNR, header = T, sep = "\t", dec = ","))

# # Lectura de agregados por calcular
fileAux <- file.path(inPath, "estructuraTablas.xlsx")
armaAgr <- xlsx::read.xlsx(fileAux, sheetName = "Agregados", 
                             stringsAsFactors = FALSE)


################################################################################
# # Insumos del proceso
################################################################################
# # Calculo/lectura de ACP 
filACP <- filACP %>% select(SCHOOL_ID, STUDENT_ID, 
	                 X15.ST002, X09.ST06, X15.ST127.A:X15.ST127.C, 
	                 X15.ST005:X15.ST006.C, X15.ST007:X15.ST019.C, 
	                 X15.ST022, X15.ST011.A:X15.ST063.F, X15.ST129.A:X15.ST113.D,
	                 X15.ST103.A:X06.ST27.D)
resulACP <- pcaStudent(filACP, colID)
bdTAM    <- merge(bdTAM, resulACP, by = colID)

# # Calculo de pesos de reemuestreo
bdIDTAM <- data.table(subset(bdTAM, select = colID))
bdIDTAM[resulACP, on = .(SCHOOL_ID, STUDENT_ID), orderVar := i.PC1] # Agregar primera componente
bdIDTAM[nReal, on = .(SCHOOL_ID), weight := i.N_Total / .N , by = .EACHI]
 
if (bdIDTAM[, any(is.na(weight))]) { # Comprobacion N
	print(bdIDTAM[is.na(weight), ])
	stop("Existe uno o mas SCHOOL_ID que falta en la base 'nReal' ")
}
bdIDTAM <- bdIDTAM[order(SCHOOL_ID, orderVar)]
wrFay   <- rbindlist(lapply(split(bdIDTAM, f = bdIDTAM$SCHOOL_ID), repSchool))

# # Valores plausibles
infoCal <- data.table(nomSalida = c("LECTURA_PFS.txt", "CIENCIAS_PFS.txt", 
                                    "MATEMATICAS_PFS.txt"), 
                      colsItems = c("PR\\d+Q.+", "PS\\d+Q.+", "PM\\d+Q.+"), 
                      conMean   = c(0.4837, 0.4837, 0.4837), 
                      conDesv   = c(1.1002, 1.1002, 1.1002), 
                      consX     = c(0.883, 1, 1))

# # Calificación por prueba
bdTAM[, "C7"] <- NULL
resultPFS <- list()
for (ii in 1:nrow(infoCal)) {
  # # Filtrando ítems de cada prueba
  auxAnchor <- subset(interDiff, item %like% infoCal[ii, colsItems])
  auxTest   <- gsub("_PFS.txt",  "", infoCal[ii, nomSalida])
  resultPFS[[auxTest]] <- try(calificaPrueba(bdTAM, nomSalida = infoCal[ii, nomSalida], 
                                         conMean = infoCal[ii, conMean], 
                                         conDesv = infoCal[ii, conDesv], 
                                         colsItems = infoCal[ii, colsItems], 
                                         consX = infoCal[ii, consX], 
                                         colID = colID, anchorValues = auxAnchor,
                                         colsAuxil = c("^C\\d$", "^PC\\d+$", "HISEI", 
                                                       "GENDER", "GRADE")))
}

################################################################################
# # Construcción de Indices
################################################################################

# # Lectura de calibraciones del indice
interIndic <- xlsx::read.xlsx(fileParam, sheetName = "Indices", 
                              stringsAsFactors = FALSE, encoding = "UTF-8")
escalaWLE <- xlsx::read.xlsx(fileParam, sheetName = "WLE", 
                              stringsAsFactors = FALSE, encoding = "UTF-8")
# # Calculando indices
datIndice <- filACP[, c("SCHOOL_ID", "STUDENT_ID")]
for (ii in 1:nrow(escalaWLE)){
  auxIndex  <- escalaWLE[ii, "Indice"]
  dataIndex <- try(calificaIndice(filACP, nomIndic = auxIndex, anchorValues))
  if (class(dataIndex) != "try-error"){
    datIndice <- merge(datIndice, dataIndex, by = c("SCHOOL_ID", "STUDENT_ID"), 
                     all.x = TRUE)
  }
}


################################################################################
# # Construcción de agregados
################################################################################
# # Variable Repitente
isNOREP  <- filACP[, "X15.ST127.A"] == "1" & filACP[, "X15.ST127.B"] == "1" &
            filACP[, "X15.ST127.C"] == "1" 
filACP[, "REPETIDORES"] <- ifelse(isNOREP, 0, 1)

# # Filtrar variables
varsAux <- lapply(armaAgr[, "auxAgre"], function(x) strsplit(x, "-")[[1]])
varsAux <- unique(sapply(varsAux, function(x) x[length(x)]))
varsAux <- subset(filACP, select = c("STUDENT_ID", intersect(varsAux, 
                  names(filACP))))
infoIndice <- merge(varsAux, dplyr::select(bdTAM, SCHOOL_ID:edadM), by = colID)

# # Lectura de niveles de agregación
infoIndice <- cbind(infoIndice, 
                    REPETIDORES =  round(runif(676, 1, 4)),
                    PERFILLECTOR =  round(runif(676, 1, 4)),
                    PERCENTIL_PAIS = sample(c("P10", "P25", "P75", "P90"), 676, replace = TRUE),
                    SECTOR = sample(c("OFICIAL", "URBANO", "NO OFICIAL", "RURAL"), 676, replace = TRUE))


# # Caculo de agregados
tableResult <- list()
for (ii in (1:nrow(armaAgr))) {
    tableResult[[ii]] <- computeEst(resultPFS, wrFay, infoIndice, 
                                    codAgre = armaAgr[ii, "COD_AGREGADO"],
                                    byVars  = strsplit(armaAgr[ii, "auxAgre"], "-")[[1]], 
                                    byTests = strsplit(armaAgr[ii, "PRUEBA"], "\\|")[[1]],
                                    funAgre = armaAgr[ii, "Funcion"])
}

# # Guardando salida de agregados
tableResult <- rbindlist(tableResult)
fileOut     <- file.path(outPath, "Puntajes", "AgregadosP4S_EJEMPLO.txt")
write.table(tableResult, file = fileOut, 
            quote = FALSE, row.names = FALSE, sep = ";")
