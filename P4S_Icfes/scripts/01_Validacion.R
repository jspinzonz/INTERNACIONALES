################################################################################
# # validacion.R
# # R Versions: 1.0.0
# #
# # Autores: Viviana Beltrán - Jonathan Pinzón
# #
# # Proceso: Análisis de ítem
# # Descripción: Funciones para llevar a cabo los procesos de SAMPLING y ANALISIS NACIONAL
# #              DE ITEM
# #
# # Entradas: Base de datos
#
# # Salidas: Funciones
# #
# # File history:
# #   20170817: Creación
################################################################################
loadLibraries <- TRUE
if(loadLibraries){
# # Librerías
library("TAM")
library("plyr")
library("dplyr")
library("xlsx")
library("stringr")
library("haven")
library("tidyr")
}
################################################################################
# # Parámetros generales
################################################################################
setallParams <- TRUE
if(setallParams){
rutaPrincipal   <- "C:\\Users\\vbeltran\\Google Drive\\pisa_icfes\\"

# # Ruta base de datos depurada
filenameBDItems <- paste0(rutaPrincipal,"input\\FieldTrialCleaned.csv")

# # Ruta base de datos de parámetros internacionales
parametrosInt <- paste0(rutaPrincipal,"input\\PBTS_InternationalParameters.xlsx")

# # Ruta de parámetros internacionales - frecuencias
filenameFreq <- paste0(rutaPrincipal,"input\\frecuencias.xlsx")

# # Número de columnas previas a ítems
nColInfo <- 12  
################################################################################
# # Creando variables necesarias
################################################################################
# # Base de datos depurada
itemsBD <- read.table(filenameBDItems,header=TRUE, sep=",", dec=".")

print('Revisando valores posibles de los ítems en base de depurada: ')
(unique(unlist(itemsBD[,-(1:nColInfo)])))

# # Creando archivo de salida
wb <- createWorkbook()
outputFile <- paste0(rutaPrincipal,"output\\validacion\\analisis.xlsx")
wbS <- createWorkbook()
outputFileSummary <- paste0(rutaPrincipal,"output\\validacion\\summary.xlsx")
}
#Correr sampling y correlación de frecuencias
runSampling    <- TRUE
################################################################################
# # SAMPLING
################################################################################
if(runSampling){
# # Parámetros de sampling
sampling  <- c()
cGradoNan <- 99 # Código nulo para grado
nMinE     <- 35 # Número mínimo de estudiantes

# # Número de estudiantes por colegio
nEstColegio <- ddply(itemsBD,~SCHOOL_ID,summarise,number_of_students=length(unique(STUDENT_ID)))

# # Número de colegios
nColTotales <- nrow(nEstColegio)

# # Contando # de colegios con n < 35 y n > 35 estudiantes
nColMenor   <- nrow(subset(nEstColegio,number_of_students < nMinE))

nColMayor   <- nrow(subset(nEstColegio,number_of_students >= nMinE))

# # Creando tabla #1
sampling           <- rbind(sampling,c(paste0("<", nMinE," students"),nColMenor))
sampling           <- rbind(sampling,c(paste0(">", nMinE," students"),nColMayor))
sampling           <- rbind(sampling,c("Total",nColTotales))
sampling           <- as.data.frame(sampling)
colnames(sampling) <- c("Condición","Nº of schools")

# # Contando número de evaluados por grado y género
nGradoGenero                <- table(itemsBD$X15.ST001.A,itemsBD$X15.ST004)
dimnames(nGradoGenero)[[2]] <- c("F", "M")
nGradoGenero                <- data.frame(nGradoGenero, stringsAsFactors = FALSE)
colnames(nGradoGenero)      <- c("Grado","Genero","Freq")
nGradoGenero                <- nGradoGenero %>% filter(Grado != cGradoNan)  %>% mutate(Porce = Freq/sum(Freq) * 100)

# # Creando tabla #2
nGradoGenero            <- reshape(nGradoGenero, idvar =  "Grado", timevar = "Genero", direction = "wide")
nGradoGenero[,"Total"]  <- nGradoGenero[,"Freq.F"] + nGradoGenero[,"Freq.M"]
nGradoGenero[,"TotalP"] <- nGradoGenero[,"Porce.F"] + nGradoGenero[,"Porce.M"]
nGradoGenero            <- rbind(nGradoGenero,c(NA,colSums(nGradoGenero[, -1])))

# # Guardando tablas en archivo de salida
assign("nameSheet", xlsx::createSheet(wb, sheetName = "sampling"))

# # Tabla 1
addDataFrame(sampling, sheet = get("nameSheet"), startRow = 1,
             startColumn = 1, row.names = FALSE,
             col.names = TRUE)
# # Tabla 2
addDataFrame(nGradoGenero, sheet = get("nameSheet"), startRow = 7,
             startColumn = 1, row.names = FALSE,
             col.names = TRUE)

################################################################################
# # Correlación entre frecuencias de respuestas Nacional - Internacional
################################################################################
frecuenciasI   <- xlsx::read.xlsx(filenameFreq,sheetName = "frecuenciasInter")
frecuenciasNac <- xlsx::read.xlsx(filenameFreq,sheetName = "frecuenciasN")

frecTot        <- merge(x = frecuenciasNac, y = frecuenciasI, by = "item")
frecTot[,-1]   <- sapply(frecTot[,-1],as.numeric)

corrT <- c()

for (ii in c("PM","PR","PS")){
  corr      <- c()
  itemsFreq <- frecTot[grep(ii,frecTot[,"item"]),]
  corr[1]   <- cor(itemsFreq[,"n0"],itemsFreq[,"i0"])
  corr[2]   <- cor(itemsFreq[,"n1"],itemsFreq[,"i1"])
  f         <- filter(itemsFreq, n2!= 0 & i2!= 0)
  corr[3]   <- cor(f[,"n2"],f[,"i2"])
  corrT     <- rbind(corrT, corr)
}
colnames(corrT)  <- c("Corr0","Corr1","Corr2")
row.names(corrT) <- c("Matematicas","Lectura","Ciencias")

# # Guardando frecuencias Internacionales
# # Tabla 3
addDataFrame(corrT, sheet = get("nameSheet"), startRow = 15,
             startColumn = 1, row.names = TRUE,
             col.names = TRUE)
}
################################################################################
# # NATIONAL ITEM ANALYSIS
################################################################################
# # Contador para páginas de archivo de parámetros internacionales en orden:
# #                       Matemáticas(1), Lectura(2), Ciencias(3)

# # Función para ir añadiendo items sospechosos de toda la prueba
addItemsSos <- function (iTot, iSos, label, sublabel, parametro) {
  print(iTot)
  print(iSos)
  print(label)
  print(sublabel)
  print(parametro)
  if(length(iSos) > 0){
    print('OK')
    iSos <- cbind(c(label),unique(iSos),c(sublabel),c(parametro))
    iTot <- rbind(iTot,iSos)  
  }
  return(iTot)
}

jj <- 1
#iTest <- "PR"
for (iTest in c("PM","PR","PS")){
  itemsSospechososTest <- c()
  assign(iTest, xlsx::createSheet(wbS, sheetName = iTest))
  # # Filtrando items de la prueba actual
  items <- itemsBD[, grepl( iTest, names(itemsBD))] 
    
  # # Corriendo análisis inicial - modelo 1PL
  m1    <- tam(items)
  m1fit <- tam.fit(m1)
  # # Guardando análisis
  #write.table(m1$xsi, file = paste0(rutaPrincipal,"output\\validacion\\",iTest,"Estimates.csv"), sep = ",", dec = ".", row.names = TRUE,col.names = TRUE)
  addDataFrame(m1$xsi, sheet = get(iTest), startRow =1,startColumn = 1, row.names = FALSE,
               col.names = TRUE)
  #write.table(m1fit$item, file = paste0(rutaPrincipal,"output\\validacion\\",iTest,"Fit.csv"), sep = ",", dec = ".", row.names = TRUE,col.names = TRUE)
  addDataFrame(m1fit$item, sheet = get(iTest), startRow =1,startColumn = 4, row.names = FALSE,
               col.names = TRUE)
  
  m1WLE           <- tam.wle(m1)
  m1PV            <- tam.pv(m1, nplausible = 5)
  m1Ctt           <- tam.ctt(items, wlescore=m1WLE$theta, pvscores=m1PV$pv, group=NULL , progress=TRUE)
  m1Ctt[, "item"] <- as.character(m1Ctt[, "item"])
  # # Guardando análisis
  #write.table(m1Ctt, file = paste0(rutaPrincipal,"output\\validacion\\",iTest,"Ctt.csv"), sep = ",", dec = ".", row.names = FALSE,col.names = TRUE)
  addDataFrame(m1Ctt, sheet = get(iTest), startRow =1,
               startColumn = 14, row.names = FALSE,
               col.names = TRUE)
  ################################################################################
  # # Punto biserial
  ################################################################################
  # # Criterio #1: Las categorías no correctas (0) deben tener un índice biserial-puntual de discriminación negativo.
  itemsSospechosos     <- subset(m1Ctt,(Categ == "0" & m1Ctt$rpb.WLE > 0))
  itemsSospechososTest <- addItemsSos(itemsSospechososTest, unlist(itemsSospechosos[,"item"]),"Discriminación","DISC(criterio1)",unlist(itemsSospechosos[,"rpb.WLE"]))

  # # Criterio #2: El índice biserial-puntual de discriminación para un ítem de
  # #  crédito parcial(3 categorías ó más) debe ser ordenado, es decir, las categorías puntuadas 0 deben 
  # #  tener una correlación biserial-puntual más baja que las categorías puntuadas como 1, y así sucesivamente.
  
  nCateg <- ddply(m1Ctt, .(item), summarize, nCat = length(item))
  
  # # Lista de ítems de crédito parcial
  listaCP <- subset(nCateg, nCat >= 3)
  listaCP <- listaCP[, "item"]
  
  itemsSospechosos <- NULL
  
  for (ii in listaCP) {
    # # Punto biserial
    pb <- subset(m1Ctt,item == ii)
    pb <- pb[,"rpb.WLE"]
    if (is.unsorted(pb)){
      itemsSospechosos <- c(itemsSospechosos,ii)
    }
  }
  
  itemsSospechososTest <- addItemsSos(itemsSospechososTest,unlist(itemsSospechosos),"Discriminación","DISC(criterio2)","X")
  
    
  # # Criterio #3: Discriminación de la respuesta correcta debe ser mayor > 0.2
  
  # # Items dicotómicos
  dico <- subset(m1Ctt, !(item %in% listaCP))
  dico <- subset(dico, Categ == '1')
  dico <- subset(dico, rpb.WLE <= 0.2)
  dico <- dico[c("item", "rpb.WLE")] 
  
  # # Items credito parcial
  cp <- subset(m1Ctt, item %in% listaCP)
  cp <- subset(cp, Categ == '2')
  cp <- subset(cp, rpb.WLE <= 0.2)
  cp <- cp[c("item", "rpb.WLE")] 
  
  # # Total items con discriminación extraña
  itemsSospechosos     <- rbind(dico, cp)
  itemsSospechososTest <- addItemsSos(itemsSospechososTest,unlist(itemsSospechosos[,"item"]),"Discriminación","DISC(criterio3)",unlist(itemsSospechosos[,"rpb.WLE"]))
  
  ################################################################################
  # # Items extraños basados en diferencias en la dificultad
  ################################################################################
  #Cargando base internacional
  internationalD  <- xlsx::read.xlsx(parametrosInt, sheetIndex = jj)[c("xsi.TAM","item")]
  internationalD[,"item"] <- as.character(internationalD[,"item"])
  
  #Obteniendo dificultad de análisis --> parametros nacionales
  nacD <- (m1$xsi)[c("xsi")]
  nacD[,"item"] <- gsub("_Cat1","",row.names(nacD))
  rownames(nacD) <- NULL
  dim(nacD)
  joinItem                    <- merge(x = nacD, y = internationalD, by = "item")
  dim(joinItem)
  setdiff(unlist(nacD[,"item"]),unlist(internationalD[,"item"]))
  joinItem[,"EstNac"]         <- (joinItem$xsi - mean(joinItem$xsi))/sd(joinItem$xsi)
  joinItem[,"EstInt"]         <- (joinItem$xsi.TAM - mean(joinItem$xsi.TAM))/sd(joinItem$xsi.TAM)
  joinItem[,"Residuales"]     <- abs(joinItem[,"EstNac"] - joinItem[,"EstInt"])
  
  # # Total items con RESIDUAL mayor a 1.96
  joinItem[,"item"] <- gsub("_Cat2","",joinItem[,"item"])
  
  itemsSospechosos       <- subset(joinItem, Residuales > 1.96)
  itemsSospechososTest   <- addItemsSos(itemsSospechososTest,unlist(itemsSospechosos[,"item"]),"Dificultad","DIFICULTAD",unlist(itemsSospechosos[,"xsi"]))
  
  ################################################################################
  # # Items extraños basados en índice MNSQ
  ################################################################################
  mnsq               <- m1fit$itemfit
  mnsq               <- subset(mnsq, Infit < 0.8 |Infit > 1.2 )[c("parameter","Infit")]
  mnsq[,"parameter"] <- paste0(mnsq[,"parameter"]) 
  mnsq[,"parameter"] <- gsub("_Cat.*","",mnsq[,"parameter"])
  mnsq <- mnsq[order(mnsq$Infit,decreasing = TRUE),]
  mnsq <- mnsq[!duplicated(mnsq[,"parameter"]), ]
  itemsSospechososTest <- addItemsSos(itemsSospechososTest,unlist(mnsq[,"parameter"]),"MNSQ","MNSQ",unlist(mnsq[,"Infit"]))
  
  ################################################################################
  # # Funcionamiento diferencial de los ítems basado en "Género"
  ################################################################################
  gender   <- itemsBD[,"X15.ST004"]  ##  1=female, 2=male
  formulaA <- ~item+item:step+item*gender
  facets   <- as.data.frame(gender)
  dif      <- tam.mml.mfr(items, facets= facets , formulaA = formulaA)
  
  diff2    <- capture.output(summary(dif))
  matches  <- grep(paste(":gender(1|2) item:gender",collapse="|"), 
                  diff2, value=TRUE)
  
  matches              <- as.data.frame(matches)
  matches              <- matches %>% separate(matches, c("A", "B", "C", "D","E"),  sep = "\\s+")
  matches              <- matches[,c("B","D","E")]
  matches[,"B"]        <- gsub(":gender(1|2)","",matches[,"B"])
  matches[,c("D","E")] <- apply(matches[,c("D","E")], 2, function(x) abs(as.numeric(x)));
  colnames(matches)    <- c("Item","V1", "V2")
  itemsDiff2           <- subset(matches,  V1 > 0.25)

  
  # ## item estimate vectors for girls and boys first 1-Female 2-Male
  # even_indexes     <- seq(2,dim(items)[2]*2,2)
  # odd_indexes      <- seq(1,dim(items)[2]*2,2)
  # namesitem        <- gsub("-.*","",dif$item$item)
  # Items            <- namesitem[c(odd_indexes)]
  # Female           <- dif$item$xsi.item[c(even_indexes)]
  # Male             <- dif$item$xsi.item[c(odd_indexes)]
  # result           <- data.frame(seq(1:dim(items)[2]),Items,Female,Male,Female-Male)
  # colnames(result) <- c("ID","Item","Female","Male","Diff")
  # 
  # # # Items con diff a reportar 
  # itemsDiff          <- subset(result, abs(Diff) > 0.25)
  # itemsDiff          <- itemsDiff[order(abs(itemsDiff$Diff)), ]
  # itemsDiff[,"Item"] <- paste0(itemsDiff[,"Item"])
  ##################################################################
  # # Analisis adicional --> Porcentaje de respuestas Femenino - Masculino
  # 
  # stati <- NULL
  # for(ii in Items){
  #   itemsBD[, "X"] <- itemsBD[, ii]
  #   a <- ddply(itemsBD, .(X15.ST004, X), summarize, cuantos = length(X15.ST004))
  #   a[, "item"] <- ii
  #   stati <- rbind(stati, a)
  # }
  # stati[, "lab"] <- paste0(stati[, "X15.ST004"], stati[, "X"])
  # stati[, "X15.ST004"] <- NULL 
  # stati[, "X"] <- NULL 
  # stati <- reshape(stati, idvar = c("item"), timevar = c("lab"), direction = "wide")
  # 
  # varInt <- c("cuantos.11", "cuantos.10", "cuantos.21", "cuantos.20")
  # for(vn in varInt) stati[is.na( stati[, vn] ), vn] <- 0
  # stati[, "por1"] <- round(100 * stati[, "cuantos.11"] / (stati[, "cuantos.10"]+ stati[, "cuantos.11"]), 1)
  # stati[, "por2"] <- round(100 * stati[, "cuantos.21"] / (stati[, "cuantos.20"]+ stati[, "cuantos.21"]), 1)
  # stati[, "difpor"] <-  stati[, "por1"] - stati[, "por2"] 
  # 
  # compara <- merge(result, stati[, c("item", "por1", "por2", "difpor")], by.x = "Item", by.y = "item")
  # compara <- compara[order(abs(compara$Diff)), ]
  # compara[, "indDIF"] <- ifelse(abs(compara[, "Diff"]) > 0.25, 1, 0)

  #################################################################
  # # Diff 1
  #itemsSospechososTest <- addItemsSos(itemsSospechososTest,unlist(itemsDiff[,"Item"]),"DIFF","DIFF",unlist(itemsDiff[,"Diff"]))
  
  # # Diff 2
  itemsDiff2 <- itemsDiff2[!duplicated(itemsDiff2[c("Item", "V1")]), ]
  itemsSospechososTest <- addItemsSos(itemsSospechososTest,unlist(itemsDiff2[,"Item"]),"DIFF2","DIFF",unlist(itemsDiff2[,"V1"]))

  colnames(itemsSospechososTest) <- c("Analisis","Item","Subanalisis","Parametro")
  itemsSospechososTest           <- as.data.frame(itemsSospechososTest)
  
  itemsSospechososParams <- itemsSospechososTest[c("Item","Subanalisis","Parametro")]
  itemsSospechososTest   <- as.data.frame(itemsSospechososTest[c("Analisis","Item")])

  #Generando tabla principal de análisis
  iInformacion <- itemsSospechososTest %>% group_by(Item, Analisis) %>%
                                        dplyr::summarise(aler = n())
  iInformacion <- reshape(as.data.frame(iInformacion), idvar = "Item",
                          timevar = "Analisis", direction = "wide")
  iInformacion[, "total"] <- apply(iInformacion[,2:dim(iInformacion)[2]], 1, 
                                  function(x) sum(!is.na(x)) )
  
  
  #Generando tabla de subanalisis junto con parámetros
  iParams <- reshape(as.data.frame(itemsSospechososParams), idvar = "Item",
                          timevar = "Subanalisis", direction = "wide")
  iParams[, "total"] <- apply(iParams[,2:dim(iParams)[2]], 1, 
                                   function(x) sum(!is.na(x)) )
  ################################################################################
  # # Guardando analisis
  ################################################################################
  assign(iTest, xlsx::createSheet(wb, sheetName = iTest))
   
  addDataFrame(iInformacion[order(iInformacion$Item),], sheet = get(iTest), startRow =1,
                startColumn = 1, row.names = FALSE,
                col.names = TRUE)
  
  assign(paste0(iTest,"Parametros"), xlsx::createSheet(wb, sheetName = paste0(iTest,"Parametros")))
  
  addDataFrame(iParams[order(iParams$Item),], sheet = get(paste0(iTest,"Parametros")), startRow =1,
               startColumn = 1, row.names = FALSE,
               col.names = TRUE)
  jj <- jj + 1

}
xlsx::saveWorkbook(wb, file = outputFile)
xlsx::saveWorkbook(wbS, file = outputFileSummary)
