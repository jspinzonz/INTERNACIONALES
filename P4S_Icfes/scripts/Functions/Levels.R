################################################################################
# # Levels.R
# # R Versions: R version 3.2.3 x64
# #
# # Author(s): Carlos Reyes
# #
# # Pisa For Schools (P4S)
# # Calcula y asigna Niveles de desempeño
# #
# # Inputs: Base de datos de agregados PISA
# #
# # Outputs: Base de datos de agregados PISA con noveles de desempeño asignados
# #
# # File history:
# #   20171010: Creation
# #
################################################################################

levP4SStu <- function(x, flagMedian = FALSE){
  listCut  <- list('CIENCIAS' = c(0, 334.94, 409.54, 484.14, 558.73, 633.33,
                                  707.93, 10000),
                   'MATEMATICAS' = c(0, 357.77,	420.07,	482.38,	544.68,
                                     606.99, 669.30, 10000),
                   'LECTURA' = c(0, 334.75, 407.47, 480.18, 552.89, 625.61,
                                 698.32, 10000))
  listDatNiv <- list()

  for (test in names(listCut)){
    auxTest <- copy(x[[test]])
    if (flagMedian) {
      auxFun <- apply(select(auxTest, matches("^PV\\d.+")), 1, median)
    } else {
      auxFun <- auxTest[, PV_Mean]
    }
    auxTest[, LevelP4S := cut(auxFun,
                  breaks = listCut[[test]],
                  labels = c("1a", "1b", "2", "3", "4", "5", "6"),
                  include.lowest = FALSE, right = TRUE)]
    listDatNiv[[test]] <- auxTest
  }
  listDatNiv <- dcast.data.table(rbindlist(listDatNiv, idcol = "PRUEBA"),
                            SCHOOL_ID + STUDENT_ID ~ PRUEBA,
                            value.var = "LevelP4S")
  names(listDatNiv)[3:5] <- paste0("NIV_", names(listDatNiv)[3:5])
  return(listDatNiv)
}

lev_P4S <- function(x, test){
  listCut  <- list('CIENCIAS' = c(0, 334.94, 409.54, 484.14, 558.73, 633.33,
                                  707.93, 10000),
                   'MATEMATICAS' = c(0, 357.77,	420.07,	482.38,	544.68,
                                     606.99, 669.30, 10000),
                   'LECTURA' = c(0, 334.75, 407.47, 480.18, 552.89, 625.61,
                                 698.32, 10000))
  auxLevel <- c("1a", "1b", "2", "3", "4", "5", "6")
  auxTest  <- copy(x[[test]])
  for (ii in grep("PV", names(x[[test]]), value = T)) {
    auxTest[, paste0(ii, "_NV") := cut(auxTest[[ii]],
                        breaks = listCut[[test]],
                        labels = auxLevel,
                        include.lowest = FALSE, right = TRUE), with = FALSE]
    auxTest <- cbind(auxTest,
                     dichotom(auxTest[, paste0(ii, "_NV"), with = FALSE]))
    auxTest[, ii := NULL, with = FALSE]
    auxTest[, paste0(ii, "_NV") := NULL, with = FALSE]
  }
  auxTest <- lapply(auxLevel, function(z)
                    cbind(auxTest[, .(SCHOOL_ID, STUDENT_ID)],
                                     select(auxTest, ends_with(z))))
  names(auxTest) <- paste0(test, "_NV", auxLevel)
  return(auxTest)
}

cuarDec<-function(resultPFS, wrFay){

  cuarDecil<- as.data.frame(resultPFS[1])[, 1:2]
  colnames(cuarDecil)<-c("SCHOOL_ID", "STUDENT_ID")

  for( ii in names(resultPFS)){
    temp           <- as.data.frame(resultPFS[ii])
    colnames(temp) <- substr( colnames(temp), nchar(ii)+2 , 100L)
    temp <- merge(temp[, c("SCHOOL_ID", "STUDENT_ID", "PV_Mean")],
                  wrFay[,  .(SCHOOL_ID, STUDENT_ID, Weight_81)],
                  by=c("SCHOOL_ID", "STUDENT_ID") )

    colegios<- unique(temp[,"SCHOOL_ID"])
    tempTodo<-NULL

    for( jj in colegios){
      tempCol<- subset(temp, SCHOOL_ID==jj)

      corte <- wtd.quantile( tempCol[, "PV_Mean"],
                             weights = tempCol[,"Weight_81"],
                             probs = seq(0, 1, 0.25) )
      tempCol[,"CUARTIL"] <- cut(tempCol[, "PV_Mean"],
                                 breaks = corte,
                                 include.lowest = TRUE,
                                 labels= c("C1","C2","C3","C4") )

      corte <- wtd.quantile( tempCol[, "PV_Mean"],
                             weights = tempCol[,"Weight_81"],
                             probs = seq(0, 1, 0.1) )
      tempCol[,"DECIL"] <- cut(tempCol[, "PV_Mean"],
                               breaks = corte,
                               include.lowest = TRUE,
                               labels= c("D1","D2","D3","D4", "D5", "D6", "D7", "D8", "D9", "D10") )
      tempTodo<-rbind(tempTodo , tempCol)
    }
    tempTodo<- tempTodo[, c("SCHOOL_ID", "STUDENT_ID", "CUARTIL", "DECIL")]
    colnames(tempTodo)<- c("SCHOOL_ID", "STUDENT_ID", paste0(c("CUARTIL_", "DECIL_"), substr(ii,1,1) ) )
    cuarDecil<-merge(cuarDecil, tempTodo, by=c("SCHOOL_ID", "STUDENT_ID"))
  }
  return(cuarDecil)
}
