################################################################################
# # Puntuada.R
# # R Versions: 
# #
# # Autores: Jorge Carrasco, Stalyn Guerrero y Cristian Montaño
# #
# # Proceso:
# # Descripción: 
# #
# # Entradas: 
#
# # Salidas: 
################################################################################

# # Librerías
library("tibble")
library("haven")
library("xlsx")
library("readxl")
library("readr")
library("data.table")

# # Rutas
inPath  <- file.path("..", "input")
outPath <- file.path("..", "output")

# # Ruta base de datos original
filenamePlantilla <- file.path(inPath, "School_level_GoldDataset_Codebook.xlsx")

#StdQ_golddataset17_1 <- read_delim(filenamePlantillacsv,";", escape_double = FALSE, trim_ws = TRUE)
plantillaSchool_1 <- read_excel(filenamePlantilla, sheet = "Code")
plantillaSchool_2 <- read_excel(filenamePlantilla, sheet = "Codebook")
colName <- na.omit(plantillaSchool_2[["Variable Name"]])

################################################################################
# # Lectura de agregados y generación de plantilla
################################################################################
#StdQ_golddataset17_1[,"fullid"] <- as.character(StdQ_golddataset17_1[,"fullid"])
#StdQ_golddataset17_1[,"bookid"] <- as.character(StdQ_golddataset17_1[,"bookid"])
load("C:\\Users\\jcarrasco\\Google Drive\\PISA para establecimientos educativos\\output\\resultPFSALL_AGREGADOS.Rdata")
tableResult <- tableResult[SCHOOL_ID != "NULL", ]
# # Reordenar base
valEst  <- dcast(tableResult, SCHOOL_ID ~ PRUEBA + COD_AGREGADO + CATEGORIA, value.var = "ESTIMACION")
valN    <- subset(tableResult, COD_AGREGADO == "EST1" & PRUEBA == "LECTURA", select = c("SCHOOL_ID", "N"))
names(valN) <- c("SCHOOL_ID", "totalpupils")
valSENL <- dcast(tableResult, SCHOOL_ID ~ PRUEBA + COD_AGREGADO + CATEGORIA, value.var = "SSE_NLIN")
names(valSENL)[-1] <- paste0(names(valSENL)[-1], "_se")
conLE   <- c("EST1", "EST2", "EST3", "EST4", "EST5", "EST20", "EST21")
valSE   <- dcast(tableResult[COD_AGREGADO %in% conLE, ], SCHOOL_ID ~ PRUEBA + COD_AGREGADO + CATEGORIA, value.var = "SSE_LIN")
names(valSE)[-1] <- paste0(names(valSE)[-1], "_se_le")
planPS4 <- merge(valEst, merge(valSENL, valSE, by = "SCHOOL_ID"), by = "SCHOOL_ID")
planPS4 <- merge(planPS4, valN, by = "SCHOOL_ID")

# # Eliminar columnas de porcentajes que no se reportan
isElim  <- grep("_(NO|OTHER)(_|$)", names(planPS4))
isElim  <- c(isElim, grep("_EST1(4|5)\\D_C\\d-(NO|OTHER)", names(planPS4)))
isElim  <- c(isElim, grep("_EST1(4|5)\\D_C(2|3).+", names(planPS4)))
isElim  <- c(isElim, grep("_EST1(4|5)\\D_NA.+", names(planPS4)))
isElim  <- c(isElim, grep("EST2_(C(2|3)|NA)", names(planPS4)))
isElim  <- c(isElim, grep("EST2_C(1|4)_se", names(planPS4)))
isElim  <- c(isElim, grep("_EST22_NORMAL", names(planPS4)))
isElim  <- c(isElim, grep("EST34_C(2|3)", names(planPS4)))
planPS4 <- planPS4[, -unique(isElim), with = FALSE]

# grep("EST26", names(planPS4), value = T)
# # Cambiando Nombres
codOri <- c("_?NULL", "LECTURA", "MATEMATICAS", "CIENCIAS", "_EST1(\\D|$)", 
            "(math|sci|read)_EST4_1", "(math|sci|read)_EST4_2", "_EST5", "_EST20_1", "_EST20_2", "_EST20_3", 
            "_EST20_4", "_EST20_5", "_EST20_6", "_NV1a_EST6", "_NV1b_EST6", 
            "_NV2_EST6", "_NV3_EST6", "_NV4_EST6", "_NV5_EST6", "_NV6_EST6", 
            "SCHOOL_ID", "_EST7", "_NV(.+)_EST25", 
            "(DISCLIMA|DISCLIM|DISCLICI|STUDREL|INSTMOT|MATHEFF|INSTSCIE|SCIEEFF)_EST\\d+(_se)?", 
            "read_EST8(\\D)_SI(_se)?", "sci_EST9(\\D)_SI(_se)?", "math_EST10(\\D)_SI(_se)?", 
            "_EST11(\\D)_SI(_se)?", "math_EST12(\\D)_SI(_se)?", "sci_EST13(\\D)_SI(_se)?", 
            "math_EST23(\\D)_SI(_se)?", "sci_EST24(\\D)_SI(_se)?", 
            paste0("_EST19_", 1:6), "(math|sci)_EST1(4|5)(\\D)_C4-SI(_se)?", 
            "(math|sci)_EST1(4|5)(\\D)_C1-SI(_se)?", "(math|sci|read)_EST16A", 
            "(math|sci|read)_EST16C", "_EST22_ENRIESGO", "_EST22_TODOTERRENO", 
            "(math|sci|read)_EST16B", "(math|sci|read)_EST34_C1", "(math|sci|read)_EST34_C4", 
            "_EST17_1", "(math|sci|read)_EST18_1", "(math|sci|read)_EST18_0")
codFin <- c("", "read", "math", "sci", "score\\1", "\\1score_F", "\\1score_M", "score_escs", 
           "score_prof_sw","score_prof_sn","score_prof_shr","score_prof_dw",
           "score_prof_dn","score_prof_dhr", "_bl1", "_l1", "_l2", "_l3", 
           "_l4", "_l5", "_l6", "schoolid", "", "lev_\\1", "\\1\\2", 
           "dis_eng_\\1\\2", "dis_sci_\\1\\2", "dis_math_\\1\\2", 
           "relations_\\1\\2", "mot_math_\\1\\2", "mot_sci_\\1\\2", 
           "eff_math_\\1\\2", "eff_sci_\\1\\2", "prof_sw", "prof_sn", 
           "prof_shr", "prof_dw", "prof_dn", "prof_dhr", "eff_\\1_\\3_top\\4", 
           "eff_\\1_\\3_btn\\4", "btn_per_\\1", "top_per_\\1", "lower", "allrounder", 
           "highlowperf\\1_median", "btn_escs_\\1", "top_escs_\\1", "repeat", "repeat\\1", 
           "nonrepeat\\1")
recodeTbl <- data.frame(codOri, codFin, stringsAsFactors = FALSE)
for (ii in 1:nrow(recodeTbl)) {
  names(planPS4) <- gsub(recodeTbl[ii, "codOri"], recodeTbl[ii, "codFin"], names(planPS4))
}
# # Cambiando letras por numeros en los indices
for (jj in 1:8) {
  if (jj != 6){
    names(planPS4) <- gsub(paste0("_", LETTERS[jj], "(_|$)"), 
                           paste0("_", jj, "\\1"), names(planPS4))
  } else {
    names(planPS4) <- gsub(paste0("(eff_.+_)", LETTERS[jj], "(_|$)"), 
                           paste0("\\1", jj, "\\2"), names(planPS4))    
  }
}  
# # Eliminado 
planPS4 <- data.frame(planPS4)
planPS4[, "CountryCode"]    <- "170"
planPS4[, "Year"]           <- "2017"  
planPS4[, c("schoolyear", "schoolname", "district", "Country2", "pupilcount", 
            "pupilssampled", "schooltype", "schooltype2", "setting")]  <- ""
planPS4[, "SchoolUniqueID"] <- paste0(planPS4[, "CountryCode"], planPS4[, "schoolid"], planPS4[, "Year"])
planPS4[, "Country"]        <- "COL"
planPS4[, "testdate"]       <- "30-07-2017"

planPS4 <- planPS4[, -grep("_EST", names(planPS4))]

idSchol <- c("CountryCode", "schoolid", "Year", "schoolyear", 
             "SchoolUniqueID", "schoolname", "district", "Country", 
             "Country2", "pupilcount", "pupilssampled", "totalpupils", 
             "schooltype", "schooltype2", "testdate", "setting") 
idSchol <- c(idSchol, sort(names(planPS4)[!names(planPS4) %in% idSchol]))
planPS4 <- planPS4[, idSchol]

# # Llenado plantilla 
plantillaSchool_1 <- data.frame(plantillaSchool_1)[0, ]
plantillaSchool_1[1:nrow(planPS4), ] <- NA
for(ii in plantillaSchool_2[["Match_Name"]]){
  if(!is.na(ii)){  
    nii <- subset(plantillaSchool_2  , Match_Name == ii)[["Variable Name"]]
    if(!is.na(nii)){
      plantillaSchool_1[[nii]] <- planPS4[[ii]]
    }
  }
}

# # guardando plantilla
wb         <- createWorkbook()
outputFile <- file.path(outPath, "School_level_GoldDataset_Codebook.xlsx")
sheet1     <-"Code"
assign(sheet1, xlsx::createSheet(wb, sheetName = sheet1))
addDataFrame(plantillaSchool_1, sheet = get(sheet1), startRow =1,startColumn = 1, row.names = FALSE,
             col.names = TRUE)
sheet2<-"Codebook"
plantillaSchool_2 <- data.frame(plantillaSchool_2)
assign(sheet2, xlsx::createSheet(wb, sheetName = sheet2))
addDataFrame(plantillaSchool_2, sheet = get(sheet2), startRow =1,startColumn = 1, row.names = FALSE,
             col.names = TRUE)
xlsx::saveWorkbook(wb, file = outputFile)
