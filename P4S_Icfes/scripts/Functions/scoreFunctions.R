################################################################################
# # scoreFunctions.R
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
options(encoding = "UTF-8")

rowVars <- function (x, na.rm = TRUE) 
{
    sqr = function(x) x * x
    n = rowSums(!is.na(x))
    n[n <= 1] = NA
    return(rowSums(sqr(x - rowMeans(x,na.rm = na.rm)), na.rm = na.rm)/(n - 1))
}

VAR_P4S <- function(x, muReal = x[81]){
  return(sum((x[-81] - muReal) ^ 2) / 20)
}

calRepitente <- function(filACP) {

  filACP <- filACP %>% mutate(X15.ST127.A.2 = ifelse(X15.ST127.A %in% c(7, 9), NA, X15.ST127.A)) %>% 
        mutate(X15.ST127.A.2 = ifelse(X15.ST127.A.2 == 1, 0, X15.ST127.A.2)) %>%
          mutate(X15.ST127.A.2 = ifelse(X15.ST127.A.2 %in% c(2, 3), 1, X15.ST127.A.2))

  filACP <- filACP %>% mutate(X15.ST127.B.2 = ifelse(X15.ST127.B %in% c(7, 9), NA, X15.ST127.B)) %>% 
        mutate(X15.ST127.B.2 = ifelse(X15.ST127.B.2 == 1, 0, X15.ST127.B.2)) %>%
          mutate(X15.ST127.B.2 = ifelse(X15.ST127.B.2 %in% c(2, 3), 1, X15.ST127.B.2))

  filACP <- filACP %>% mutate(X15.ST127.C.2 = ifelse(X15.ST127.C %in% c(7, 9), NA, X15.ST127.C)) %>% 
        mutate(X15.ST127.C.2 = ifelse(X15.ST127.C.2 == 1, 0, X15.ST127.C.2)) %>%
          mutate(X15.ST127.C.2 = ifelse(X15.ST127.C.2 %in% c(2, 3), 1, X15.ST127.C.2))

  filACP[,"suma.127.ABC"] <- rowSums(filACP[, c("X15.ST127.A.2", "X15.ST127.B.2", 
                                                "X15.ST127.C.2")], na.rm = TRUE)
  filACP <- filACP %>% mutate(Repitentes = ifelse(suma.127.ABC >= 1, 1, 0))
  return(filACP)
}

computeEst <- function(resultPFS, wrFay, infoStudent, codAgre, verbose = TRUE,
                       funAgre = "Promedio", byVars = c("SCHOOL_ID"), 
                       byTests = c("LECTURA", "CIENCIAS", "MATEMATICAS")) {
  
  if (funAgre == "ESCS") {
    return(NULL)
  }
  # # Filtrando los resultados por prueba
  if (all(byTests != "NULL")){
    resultPFS <- resultPFS[byTests]
  }
  idAux <- c("SCHOOL_ID", "STUDENT_ID")
  # # Tabla para estandarizar salidas
  tipoAgre <- ifelse("SCHOOL_ID" %in% byVars, "COLEGIO", 
                     ifelse("REGION_ID" %in% byVars, "REGION", "PAIS"))  
  auxAgre  <- paste(byVars, collapse = "-")

  # # Iterando por pruebas
  faltaVars <- byVars[!byVars %in% names(infoStudent)]
  if (length(faltaVars) > 0) {
    warning("Hay algunas variables que faltan en la base (", 
           paste0(faltaVars, collapse = "-"), ")")
    return(NULL)
  }
  infoStudent <- data.table(subset(infoStudent, select = unique(c(idAux, byVars))))
  resultEst  <- list() 

  # # Si se debe calcular un porcentaje
  regxCols <- ifelse(funAgre %in% c("Promedio", "Promedio/ESCS"), "PV\\d", "data\\.")
  varCat   <- byVars[!byVars %in% c("SCHOOL_ID")]
  if (length(varCat) == 0 & funAgre == "Porcentaje"){
    stop("Se debe definir al menos una variable de categorias (Funcion Porcentaje)")
  }

  # # Arreglando entrada de datos
  infoStudent[, CATEGORIA := do.call(paste, c(.SD, list(sep = "-"))), .SDcols = varCat]
  if (funAgre == "Porcentaje") {
    infoStudent <- cbind(infoStudent, dichotom(infoStudent[, CATEGORIA]))
    resultPFS   <- list('CATEGORIA' = infoStudent[, .(SCHOOL_ID, STUDENT_ID)])
    byVars      <- byVars[!byVars %in% varCat]
  }
 
  if (funAgre != "Mediana") {
     for (testX in names(resultPFS)) {
        datTest <- merge(infoStudent, merge(resultPFS[[testX]], wrFay, by = idAux), 
                         by = idAux)
        # # Calculo de media por replica y valor plausible
        namesCols <- grep(regxCols, names(datTest), value = TRUE)
        auxMultiReg <- function(data, byVars) {
          infoWei <- data[, .SD, .SDcols = grep("Weight_", names(data))]
          infoPV  <- data[, .SD, .SDcols = namesCols]
          fullDat <- cbind(infoPV, infoWei, data[, .SD, .SDcols = byVars])
          listOut <- list()
          lapply(names(infoPV), function(itPV){
            lapply(names(infoWei), function(itWGT){
              tempDat <- fullDat[, .SD, .SDcols = c(byVars, itPV, itWGT)]
              modEst <- multilevelPISA(tempDat, itPV, byVars[1], byVars[2], itWGT)
              datOut <- cbind("PV" = itPV, "Replica" = itWGT, modEst)
              listOut[[paste(itPV, itWGT, sep = "-")]] <<- datOut
            })
          })
          datTB <- rbindlist(listOut, fill = TRUE)
          datOut <- dcast.data.table(datTB, Replica + SCHOOL_ID + N ~ PV, value.var = "Intercept")
          return(datOut)
        }
        auxMean <- function(datAux) {
            nrowAux <- nrow(datAux)
            infoWei <- datAux[, .SD, .SDcols = grep("Weight_", names(datAux))]
            infoPV  <- datAux[, .SD, .SDcols = namesCols]
            mea1Rep <- function(z){
              unlist(infoWei[, lapply(.SD, function(y) weighted.mean(z, y))][1,])
            }
            datAux <- infoPV[, lapply(.SD, mea1Rep)]
            #datAux <- sapply(infoPV, mea1Rep)
            cbind('N' = nrowAux, 'Replica' = paste0("Weight_", 1:81), 
                  data.table(datAux))
        }
        # # Calculo del error de estimación
        listLinkSEE <- list('LECTURA' = 8.132, 
                            'CIENCIAS' = 11.188, 
                            'MATEMATICAS' = 10.101)        
        if (funAgre == "Promedio/ESCS") {
          datAux <- datTest[, auxMultiReg(.SD, byVars)]
          initByVar <- byVars
          byVars <- byVars[1]
        } else {
          datAux <- datTest[, auxMean(.SD), by = byVars]
        }
        auxEst <- datAux[Replica == "Weight_81", ]
        auxSEE <- datAux[, lapply(.SD, VAR_P4S), by = byVars, 
                        .SDcols = namesCols]
        if (funAgre == "Porcentaje") {
         auxEst <- melt(auxEst, id = c(byVars, "N", "Replica"), value.name = "ESTIMACION")
         auxSEE <- melt(auxSEE, id = byVars, value.name = "VAR_sam")
         auxEst[, VAR_measure := 0]
         auxSEE[, VAR_link := 0]
         auxEst[, CATEGORIA := gsub("data\\.", "", variable)]
         auxSEE[, CATEGORIA := gsub("data\\.", "", variable)]

        } 
        if (funAgre %in% c("Promedio", "Promedio/ESCS")) {
          auxEst[, ESTIMACION := rowMeans(select(auxEst, starts_with("PV")))]
          auxEst[, VAR_measure := rowVars(select(auxEst, starts_with("PV")))]
          auxSEE[, VAR_sam := rowMeans(select(auxSEE, starts_with("PV")))]
          auxSEE[, VAR_link := listLinkSEE[[testX]]]
          if (length(varCat) > 0 & funAgre != "Promedio/ESCS"){
            auxEst[, CATEGORIA := do.call(paste, c(.SD, list(sep = "-"))), .SDcols = varCat]
            auxSEE[, CATEGORIA := do.call(paste, c(.SD, list(sep = "-"))), .SDcols = varCat]
          } else {
            auxEst[, CATEGORIA := "NULL"]
            auxSEE[, CATEGORIA := "NULL"]
          }
        }
        # # Arreglando salida
        testCol <- ifelse(funAgre %in% c("Promedio", "Promedio/ESCS"), testX, "NULL")
        if (length(byTests) == 1)
          testCol <- byTests
        auxEst  <- auxEst[, cbind('TIPO_AGREGADO' = tipoAgre, 'COD_AGREGADO' = codAgre, 
                                  'PRUEBA' = testCol, .SD), 
                          .SDcols = c(byVars, "CATEGORIA", "N", "ESTIMACION", "VAR_measure")]
        resultEst[[testX]] <- merge(auxEst, auxSEE, by = c(byVars, "CATEGORIA"))
        if (verbose)
           cat("...... Termino agregado (", auxAgre, ") para --", testX, "--\n")
        if(funAgre == "Promedio/ESCS"){
          byVars <- initByVar
        }
     }
  } else {
    resultEst <- lapply(names(resultPFS), function(x) {
                         auxDat <- merge(infoStudent, resultPFS[[x]], by = idAux)
                         resultPFS[[x]][, .('TIPO_AGREGADO' = tipoAgre, 
                                            'COD_AGREGADO' = codAgre,
                                            'PRUEBA' = x, 'N' = .N, 
                                            'ESTIMACION' = median(PV_Mean)
                                            ), by = byVars]})
    resultEst <- lapply(resultEst, function(x) 
                        cbind(x, VAR_measure = 0, VAR_sam = 0,
                        VAR_link = 0))
  }
  resultEst <- rbindlist(resultEst)

  # # Ajustar columnas en el reporte FINAL
  resultEst[, SSE_LIN  := sqrt(1.2 * VAR_measure + VAR_sam + VAR_link)]
  resultEst[, SSE_NLIN := sqrt(1.2 * VAR_measure + VAR_sam)]
  colFix <- c("SCHOOL_ID", "CATEGORIA", "SSE_NLIN", "SSE_LIN")
  colFix <- colFix[!colFix %in% names(resultEst)]
  if (length(colFix) > 0){ 
    resultEst[, (colFix) := "NULL"] 
  }
  resultEst <- resultEst[, .(TIPO_AGREGADO, COD_AGREGADO, CATEGORIA, 
                             SCHOOL_ID, PRUEBA, N, ESTIMACION, SSE_NLIN, 
                             SSE_LIN)]
  return(resultEst)
}

recodeFun <- function(z, recodeTbl, flagVerbose = FALSE, recodeNA = "99"){ 
  auxHISEI <- unname(setNames(recodeTbl$codFin, recodeTbl$codOri)[as.character(z)])
  if (any(is.na(auxHISEI)) & flagVerbose) {
    print("Existen codigos sin recodificacion")
    print(unique(z[is.na(auxHISEI)]))  
  }
  auxHISEI[is.na(auxHISEI)] <- recodeNA
  return(auxHISEI)
}

computeHISEI <- function(dataACP, colHISEI, codeHISEI) {
  dataACP <- data.table(dataACP)
  datHISEI  <- dataACP[, colHISEI, with = FALSE]
  # # Lectura de codigos(recodificacion)
  spsHISEI  <- readLines(codeHISEI, encoding = "UTF-8", warn = FALSE)
  codeHISEI <- unique(grep("^\\(\\d+=\\d+\\)$", spsHISEI, value = TRUE)) 

  # # recodificacion y calculo HISEI
  recodeTbl <- tibble(codOri = gsub("\\((\\d+)=(.+)\\)$", "\\1", codeHISEI), 
                      codFin = gsub("\\((\\d+)=(.+)\\)$", "\\2", codeHISEI))
  datHISEI <- datHISEI[, lapply(.SD, recodeFun, recodeTbl, flagVerbose = TRUE)]
  datHISEI <- datHISEI[, lapply(.SD, as.numeric)]
  datHISEI[, HISEI := do.call(pmax,.SD)]
  datHISEI <- cbind(dataACP[, .(SCHOOL_ID, STUDENT_ID)], 
                    datHISEI)
  return(datHISEI)
}


pcaStudent <- function(dataACP, colID) {
  dataACP <- data.table(dataACP)
  #princomp(#as.formula(paste("~", paste(colPCA, collapse = " + "))), 
  #prcomp(dataACP[, colPCA, with = FALSE], scale = TRUE)
  #corPCA   <- cor(dataACP[, colPCA, with = FALSE], use = "pairwise.complete.obs")

  # # Recodificacion
  specCase  <- c("X09.ST06", "X15.ST022")
  colPCA    <- names(dataACP)[!names(dataACP) %in% colID]
  recodeTbl <- tibble(codOri = c(1:6, 7, 8, 9, "r"), 
                      codFin = c(1:6, NA, NA, NA, NA))
  auxACP <- dataACP[, lapply(.SD, recodeFun, recodeTbl, recodeNA = 0), 
                    .SDcols = colPCA[!colPCA %in% specCase]]
  recodeTblII <- tibble(codOri = c(1:9, 156, 160, 474, 608, 97, 98, 
                                   99, "r", 998, 999), 
                        codFin = c(1:9, 156, 160, 474, 608, NA, NA, 
                                   NA, NA, NA, NA))
  auxACP <- cbind(auxACP, dataACP[, lapply(.SD, recodeFun, recodeTblII, 
                                  recodeNA = 0), .SDcols = specCase])
  # #PCA
  blockPCA <- prcomp(auxACP, scale = TRUE, center = TRUE)
  # # Eigenvalues (Cumulative variances)
  eigDat   <- (blockPCA$sdev)^2
  variance <- cumsum(eigDat*100/sum(eigDat))
  indEig   <- min(which(variance >= 95))
  # # Capturar componentes
  blockPCA <- cbind(dataACP[, .(SCHOOL_ID, STUDENT_ID)], 
                    blockPCA$x[, 1:indEig])
  return(blockPCA)
}


repSchool <- function(x) {
  nSt <- nrow(x)
  x[["pseStrata"]] <- c(unlist(lapply(1:(nSt/2), rep, 2)), 
                            rep(floor(nSt/2), nSt %%2))
  scddes <- svydesign(data = x[1:(nSt - (3 * (nSt %% 2))), ], 
                      prob = ~weight, id = ~STUDENT_ID, 
                      strata = ~pseStrata, nest = TRUE)
  scd2fay <- as.svrepdesign(scddes, type="Fay", fay.rho=0.5, 
                          hadamard.matrix=paley(79))
  if (nSt %% 2 != 0) {
    auxFay <- matrix(c(1.7071, 0.6464, 0.6464, 0.2929, 
                   1.3536, 1.3536), nrow = 3)
    weiDat <- x[(nSt - (3 * (nSt %% 2)) + 1):nSt, weight]
    auxFay <- sapply(2 - paley(79)[, 80], function(x) auxFay[, x] * weiDat)
    auxFay <- rbind(scd2fay$repweights$weights, auxFay)
  } else {
    auxFay <- scd2fay$repweights$weights
  }
  auxFay <- cbind(x[, .(SCHOOL_ID, STUDENT_ID)], auxFay * x[, weight], 
                 "Weight_81" =x[, weight])
  setnames(auxFay, names(auxFay), gsub("V(\\d+)", "Weight_\\1", names(auxFay)))
  return(auxFay)
}


transCal <- function(x, conMean, conDesv, desInt = 100, 
                     meanInt = 500, consX = 1) {
  return (((consX * x - conMean) / conDesv) * 100 + 500)
}


calificaPrueba <- function(bdTAM, nomSalida, conMean, conDesv, colID = "Ã¯..ID",
                           colsItems = "PR\\d+Q.+", anchorValues, consX,
                           colsAuxil = c("^C\\d$", "^FAC\\d+$", "HISEI", 
                                         "GENDER", "GRADE")) {
  bdTAM   <- data.frame(bdTAM)
  auxTest <- gsub("_PFS.txt",  "", nomSalida)
  # # Seleccionar columnas / Modelo Inicial
  colID        <- bdTAM[, colID]                    # id
  datReading   <- select(bdTAM, matches(colsItems)) # items
  datReading   <- datReading[, order(names(datReading))]
  datReading[datReading == 6] <- NA                # Cambiar 6s a NA
  colsAuxil    <- rowSums(sapply(colsAuxil, function(x) names(bdTAM) %like% x)) == 1
  datAuxiliar  <- bdTAM[, colsAuxil]                # Variables Auxiliares
  
  # # Filtrando parametros de ancla
  indValues    <- rowSums(sapply(names(datReading), function(x) anchorValues$item %like% x)) == 1
  if (any(!indValues)) {
    warning("No se tuvieron en cuenta estos Ã?tems: \n", 
            anchorValues$item[!indValues],
            "\nExisten parametros internacionales que no se tienen encuenta (Se excluyen)")
  }
  anchorValues <- anchorValues[indValues, ]
  anchorValues <- anchorValues[order(anchorValues$item), ]

  # # Validando parametros de Ã?tems
  nameAnchor <- unique(gsub("_Cat2", "", anchorValues[, "item"]))
  isErrorA   <- !names(datReading) %in% nameAnchor
  if (any(isErrorA)) {
    warning("No se encontro estos Ã?tems: \n", names(datReading)[isErrorA],
            "\n...El conjunto de anclaje no es igual al conjunto de items (Se excluyen)")
    datReading <- datReading[, names(datReading)[!isErrorA]]
  }

  # # Validando parametros de credito parcial
  posPCM <- which(sapply(datReading, function(x) 2 %in% unique(x)))
  posPCM <- paste0(names(posPCM), "_Cat2")
  if (any(!posPCM %in% anchorValues[, "item"])) {
    colElim <- posPCM[!posPCM %in% anchorValues[, "item"]]
    warning("Estos items son politomicos pero no tienen _Cat2 \n", 
            paste(colElim, sep = "-"), 
            "\n....Se excluyeron estos del conjunto de items (Se excluyen)")
    datReading   <- datReading[, !names(datReading) %in% colElim]
    anchorValues <- subset(anchorValues, !item %in% colElim)
  }

  # # Modelo tam
  anchorValues[, "id"] <- 1:nrow(anchorValues)
  anchorValues[, "tam"] <- anchorValues[, "xsi.TAM"]
  model_R      <- tam(datReading, xsi.fixed = anchorValues[, c("id", "tam")], 
                     Y = datAuxiliar, pid = NULL)
  cat("----> Listo proceso de estimacion >_< \n")
  
  # # Calcular valores plausibles
  readingPVs   <- tam.pv(model_R, nplausible = 5)
  cat("----> Listo proceso de VP >_< \n")
  pbaResult <- data.table(readingPVs$pv)
  
  # # TransformaciÃ³n de los puntajes
  namesPV <- grep("^PV", names(pbaResult), value = T)
  pbaResult <- cbind(colID, pbaResult[,  lapply(.SD, transCal, 
                        conMean = conMean, conDesv = conDesv, consX = consX), 
                        .SDcols = namesPV])
  pbaResult <- data.table(pbaResult)
  pbaResult <- pbaResult[, PV_Mean := apply(.SD, 1, mean), .SDcols = namesPV]
  
  # # Archivo de salida valores plausibles
  setnames(pbaResult, names(pbaResult), gsub("(PV\\d)(.+)", 
           paste("\\1", auxTest, sep = "_"), names(pbaResult)))
  outScoring <- file.path(outPath, "Puntajes")
  dir.create(outScoring, showWarnings = FALSE)
  fileOut <- file.path(outScoring, nomSalida)
  write.table(pbaResult, file = fileOut, sep = ";", dec = ",", 
              row.names = FALSE, col.names = TRUE)
  cat('----> Creacion de salida en el archivo"', fileOut, '" >_< \n', sep = "")
  return(pbaResult)
}

calificaIndice <- function(filACP, nomIndic = "MATHEFF",
                           anchorValues) {
  # Base con los resultados del cuestionario de estudiante
  filACP    <- data.frame(filACP)
  
  # Filtrando parametros de ancla
  itemIndic <- filter(interIndic, Indice == nomIndic)
  if(nrow(itemIndic) == 0) {
    stop(" el índice (", nomIndic ,") NO existe")
  }
    
  if(any(is.na(itemIndic[, "item"])) | any(is.na(itemIndic[, "item"]))) {
    stop(" el índice (", nomIndic ,") no tiene parámetros internacionales")
  }
  colsItems <- unique(substr(itemIndic[, "item"], 1, 8))
  # Filtrando parametros de cambio de escala
  escalaWLE_Ind <- filter(escalaWLE, Indice == nomIndic)
  datIndi   <- select(filACP, matches(colsItems)) # items para construir el índice
  
  # Dirección de medición de los ítems
  direc <- unique(itemIndic[, "direccion"]) 
  
  # Todos los ítems de los índices tienen 4 categorías

  cat("----> Recodificando ítems de forma", direc, ":D \n")
  # Recodificacion 
  if(direc == "INVERSA"){
    recode_inv <- tibble(codOri = c(1:4, 5, 6, 7, 8, 9, "r"), 
                         codFin = c(3:0, NA, NA, NA, NA, NA, NA))    
    datIndi <- apply(datIndi, MARGIN = 2, FUN = recodeFun, 
                     recode_inv, recodeNA = NA)
  }
  
  if(direc == "DIRECTA"){
    recode_dir <- tibble(codOri = c(1:4, 5, 6, 7, 8, 9, "r"), 
                         codFin = c(0:3, NA, NA, NA, NA, NA, NA))
    datIndi <- apply(datIndi, MARGIN = 2, FUN = recodeFun, 
                     recode_dir, recodeNA = NA)
  }
  
  # # Comprobando el numero de respuetas validas
  isValid <- rowSums(!is.na(datIndi)) >= 3
  anchorValues <- itemIndic[, "xsi.TAM"]
  anchorValues <- cbind(1:(length(anchorValues)) , anchorValues)

  if(nomIndic == "DISCLICI") {
  B.fixed    <- as.matrix(xlsx::read.xlsx(fileParam, sheetName = "discrim_DISCLICI",
                      stringsAsFactors = FALSE, encoding = "UTF-8", header = FALSE))

  Indi_model <- tam.mml.2pl(resp = datIndi[isValid, ]  , xsi.fixed = anchorValues,
                             B.fixed = B.fixed, irtmodel = "GPCM")
  Indi_wle   <- tam.mml.wle2(Indi_model)
  valIndex   <- (Indi_wle$theta - escalaWLE_Ind[, "Mean"])/escalaWLE_Ind[, "SD"]
  colID      <- filACP[isValid, c("SCHOOL_ID", "STUDENT_ID")]
  valIndex   <- data.frame(colID, valIndex)
  setnames(valIndex, 'valIndex', nomIndic)
  
  } else {

  # IRT model
  Indi_model <- tam(datIndi[isValid, ], xsi.fixed = anchorValues)
  Indi_wle   <- tam.wle(Indi_model)
  valIndex   <- (Indi_wle$theta - escalaWLE_Ind[, "Mean"])/escalaWLE_Ind[, "SD"]
  colID      <- filACP[isValid, c("SCHOOL_ID", "STUDENT_ID")]
  valIndex   <- data.frame(colID, valIndex)
  setnames(valIndex, 'valIndex', nomIndic)

  }
  return(valIndex)
} 

 
multilevelPISA <- function(dat, pv, lvl, fixed, wgt){
  # Extrae registros con valores faltantes
  naCodes <- c("", "M", "N", "I")
  condNA <- dat[eval(parse(text=fixed)) %in% naCodes, ]
  dat <- dat[!condNA, ]

  dat[ ,"lvl"] <- dat[ , lvl, with = FALSE]
  dat[ ,"wgt"] <- dat[ , wgt, with = FALSE]
  datTemp <- dat %>%
        group_by(lvl) %>%
        summarise(sumWgt = sum(wgt),
                  nbre = n()) %>%
        setnames("lvl", "SCHOOL_ID")

  temp <- merge(dat, datTemp, by = lvl)
  # Ajuste de pesos
  temp[, "std_wgt"] <- (temp[, wgt, with = FALSE] * temp[, nbre])/temp[, sumWgt]

  # temp <- full_join(dat, datTemp, by = lvl)
  form <- formula(paste0(pv, " ~ ", fixed, " + (1|", lvl, ")"))
  fit <- lmer(form, data = temp, weights=std_wgt, REML = FALSE)
  fijos <- as.numeric(fixef(fit))[1]
  randEffect <- ranef(fit, condVar = TRUE)[[1]]
  names(randEffect) <- "Intercept"
  finalEst <- fijos + randEffect
  out <- data.table(datTemp[, "SCHOOL_ID"], "N" = datTemp$nbre, finalEst)
  return(out)
}

readerProfiles <- function(datInt, outPath){
  # Dicotomiza las variables de interes
  idDat <- datInt[, .SD, .SDcols = c("SCHOOL_ID", "STUDENT_ID")]
  varNames <- c("magazine", "comic", "fiction", "nonfic", "news")
  specDicom <- function(x) {
    ifelse(x <= 2, 1, 0)
  }
  dicomDat <- datInt[, lapply(.SD, specDicom), .SDcols = grep("ST25", names(datInt), value= TRUE)]
  names(dicomDat) <- varNames

  # # Recordar y entender
  # condiciones para UNDREM var
  recordar <- datInt[, .SD, .SDcols = grep("ST41", names(datInt), value = TRUE)]
  condUNDREM <- c("(X09.ST41.C > X09.ST41.A)", "(X09.ST41.C > X09.ST41.B)",
                  "(X09.ST41.C > X09.ST41.F)", "(X09.ST41.D > X09.ST41.A)",
                  "(X09.ST41.D > X09.ST41.B)", "(X09.ST41.D > X09.ST41.F)",
                  "(X09.ST41.E > X09.ST41.A)", "(X09.ST41.E > X09.ST41.B)",
                  "(X09.ST41.E > X09.ST41.F)")
  lapply(condUNDREM, function(y){
    recordar[eval(parse(text=y)), paste0("var-", y) := 1, ]
  })
  reScale <- function(z, m, s) {
    stdZ <- (z - mean(z))/sd(z)
    newZ <- (stdZ*(s) + m)
    return(newZ)
  }
  recordar[, undrem := round(reScale(rowSums(.SD, na.rm = TRUE)/9, 0.57, 0.30), 4), .SDcols = grep("var", names(recordar), value= T)]

  # condiciones para METASUM var
  entender = datInt[, .SD, .SDcols = grep("ST42", names(datInt), value = TRUE)]
  condMETASUM <- c("(X09.ST42.D > X09.ST42.A)", "(X09.ST42.D > X09.ST42.B)",
                   "(X09.ST42.D > X09.ST42.C)", "(X09.ST42.E > X09.ST42.A)",
                   "(X09.ST42.E > X09.ST42.B)", "(X09.ST42.E > X09.ST42.C)",
                   "(X09.ST42.A > X09.ST42.B)", "(X09.ST42.C > X09.ST42.B)")
  lapply(condMETASUM, function(y){
    entender[eval(parse(text=y)), paste0("var-", y) := 1, ]
  })
  entender[, metasum := round(reScale(rowSums(.SD, na.rm = TRUE)/8, 0.60, 0.29), 4), .SDcols = grep("var", names(entender), value= T)]

  datOut <- cbind(idDat, dicomDat, recordar[, "undrem"], entender[, "metasum"])
  write.table(datOut, file = file.path(outPath, "perfilLector.dat"), 
            sep = "\t", quote = FALSE, col.names = FALSE, row.names = FALSE)
  return(datOut)
}

# consoInfo <- function() {

# }

# salidaXLSX <- function(){

# }
###################################
# Códigos para el cálculo de ECSC
###################################
                         
computePARED <- function(dataACP, colPARED = c("X15.ST005", "X15.ST006.A", "X15.ST006.B", "X15.ST006.C",
                                                "X15.ST007", "X15.ST008.A", "X15.ST008.B", "X15.ST008.C")){
  dataACP <- data.table(dataACP)
  datPARED  <- dataACP[, colPARED, with = FALSE]

  # # Recodificación
  # variables X15.ST005 y X15.ST007 
  # Cuestionario                | ISCED | Recodificación
  # -------------------------------------------------------             
  # No completo primaria        | None  | 0
  # Básica primaria             | 1     | 1
  # Básica secundaria (6 - 9)   | 2     | 2
  # Educación media (10 - 11)   | 3A    | 4

  recode1 <- tibble(codOri = c(4:1, 5, 6, 7, 8, 9, "r"), 
                         codFin = c(0, 1, 2, 4, NA, NA, NA, NA, NA, NA))
  datIndi <- apply(datPARED[, c("X15.ST005", "X15.ST007")], MARGIN = 2,
                   FUN = recodeFun, recode1, recodeNA = NA)

  recode2 <- tibble(codOri = c(1, 2, 7, 8, 9, "r"), 
                         codFin = c(6, 0, NA, NA, NA, NA))
  datIndi2 <- apply(datPARED[, c("X15.ST006.A", "X15.ST008.A", "X15.ST006.B", "X15.ST008.B")],
                    MARGIN = 2, FUN = recodeFun, recode2, recodeNA = NA)

  recode3 <- tibble(codOri = c(1, 2, 7, 8, 9, "r"), 
                         codFin = c(5, 0, NA, NA, NA, NA))
  datIndi3 <- apply(datPARED[, c("X15.ST006.C", "X15.ST008.C")], MARGIN = 2,
                     FUN = recodeFun, recode3, recodeNA = NA)

  datPARED_recod <- data.frame(cbind(datIndi, datIndi2, datIndi3))
  datPARED_recod[, "HISCED"] <- apply(datPARED_recod, MARGIN = 1,
                               FUN = max, na.rm = TRUE)
  
  colID          <- dataACP[, c("SCHOOL_ID", "STUDENT_ID")]
  datPARED_recod <- data.frame(colID, datPARED_recod)
  datPARED_recod <- merge(datPARED_recod, escalaPARED[, c("HISCED", "PARED")])

  PARED       <- datPARED_recod[, c("SCHOOL_ID", "STUDENT_ID", "PARED")]
  return(PARED)
}


computeHOMEPOS <- function(dataACP, colHOMESPOS = c("X15.ST011", "X15.ST012", "X15.ST013")){

  dataACP    <- data.table(dataACP)

  require(dplyr)
  dat1    <- select(dataACP, starts_with("X15.ST011"))
  recode1 <- tibble(codOri = c(1, 2, 7, 8, 9, "r"), 
                         codFin = c(1, 0, NA, NA, NA, NA))
  datIndi1 <- apply(dat1, MARGIN = 2,
                   FUN = recodeFun, recode1, recodeNA = NA)

  dat2 <- select(dataACP, starts_with("X15.ST012"))
  recode2 <- tibble(codOri = c(1:4, 7, 8, 9, "r"), 
                         codFin = c(0, 1, 2, 3, NA, NA, NA, NA))
  datIndi2 <- apply(dat2, MARGIN = 2,
                   FUN = recodeFun, recode2, recodeNA = NA)

  dat3 <- select(dataACP, starts_with("X15.ST013"))
  recode3 <- tibble(codOri = c(1:6, 7, 8, 9, "r"), 
                         codFin = c(0:5, NA, NA, NA, NA))
  datIndi3 <- apply(dat3, MARGIN = 2,
                   FUN = recodeFun, recode3, recodeNA = NA)

  datHOMEPOS <- cbind(datIndi1, datIndi2, datIndi3)
  
  #Fix difficulty parameters
  anchorValues <- xlsx::read.xlsx("../input/PBTS_InternationalParameters.xlsx",
                                  sheetName = "dif_HOMEPOS", stringsAsFactors = FALSE,
                                   encoding = "UTF-8")
  
  #Fix discrimination parameters
  B.fixed <- as.matrix(xlsx::read.xlsx("../input/PBTS_InternationalParameters.xlsx",
                                  sheetName = "discrim_HOMEPOS", stringsAsFactors = FALSE,
                                   encoding = "UTF-8"))

  # GPCM Model
  model <- tam.mml.2pl(resp = datHOMEPOS, xsi.fixed = anchorValues,
                B.fixed = B.fixed, irtmodel = "GPCM")
  
  HOMEPOSwle <- tam.wle(model)
  colID      <- dataACP[, c("SCHOOL_ID", "STUDENT_ID")]

  HOMEPOS        <- cbind(colID, HOMEPOSwle$theta)
  names(HOMEPOS) <-  c("SCHOOL_ID", "STUDENT_ID", "HOMEPOS")

  return(HOMEPOS)
}

computeECSC <- function(bdTAM, colECSC = c("HISEI", "PARED", "HOMEPOS")){

  colID   <- bdTAM[, c("SCHOOL_ID", "STUDENT_ID")]
  datECSC <- bdTAM[, colECSC] # items para construir el índice

  # AQUI LA REGRESIÓN PARA IMPUTAR !!

  HISEI_  <- (bdTAM[, "HISEI"] - 51.5) / 21.98
  PARED_  <- (bdTAM[, "PARED"] - 13.85) / 3.08

  ESCS    <- (0.81*HISEI_ + 0.78*PARED_ + 0.80*bdTAM[, "HOMEPOS"])/1.915 
  ESCS    <- cbind(colID, ESCS)

  return(ESCS)
}
                         
