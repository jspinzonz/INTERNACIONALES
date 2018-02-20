################################################################################
# # Sin_puntuar.R
# # R Versions: 
# #
# # Autores: Jonnathan Pinzón
# #
# # Proceso:
# # Descripción: 
# #
# # Entradas: 
#
# # Salidas: 
################################################################################
rm(list=ls(all=TRUE))

################################################################################
# # Define rutas
################################################################################

docPath <- file.path("..", "doc")
inPath  <- file.path("..", "input")
outPath <- file.path("..", "output")

################################################################################
# # Define librerías
################################################################################

library(xlsx)
library(tools)
library(gtools)
library(dplyr)
library("rJava")
library("haven")
library("readxl")

# # LEE BASE PRINCIPAL - P4S_COGNITIVA
metaDat01  <- read.csv(file.path(inPath, "P4SCog.csv"), 
                       header = TRUE, sep =";", 
                       colClasses = "character")

# Generar alerta si hay duplicados!!!!  
metaDat01 <- unique(metaDat01)

#metaDat01[metaDat01[, "STUDENT_ID"] == "00043" & metaDat01[, "SCHOOL_ID"] == "003" , ]

# # coded response items
codResponse <- c("PM5008Q01", "PM5010Q02", "PM5013Q03", "PM5017Q03", "PM5020Q02", 
                 "PM5021Q02", "PM5026Q01", "PM5101Q02", "PM5107Q02", "PM5109Q01", 
                 "PM5109Q02", "PM5110Q01", "PM5110Q02", "PM5124Q01", "PM5124Q02", 
                 "PM5124Q03", "PM5142Q01", "PM5142Q02", "PM5145Q01", "PM5163Q01", 
                 "PM5169Q01", "PM5169Q02", "PM5181Q02", "PM5183Q01", "PM5188Q01", 
                 "PR6002Q04", "PR6004Q01", "PR6004Q05B", "PR6008Q02", "PR6008Q04", 
                 "PR6008Q05", "PR6010Q03", "PR6010Q07", "PR6011Q05", "PR6011Q07", 
                 "PR6013Q03", "PR6013Q08", "PR6014Q07", "PR6015Q03", "PR6015Q04", 
                 "PR6017Q08", "PR6018Q03", "PR6018Q08", "PR6018Q09", "PR6023Q08", 
                 "PR6027Q10", "PS7002Q03", "PS7012Q03", "PS7013Q02", "PS7014Q01", 
                 "PS7014Q03", "PS7017Q04", "PS7020Q04", "PS7201Q01", "PS7208Q02", 
                 "PS7211Q01", "PS7212Q01", "PS7212Q04", "PS7215Q02", "PS7215Q03", 
                 "PS7216Q03", "PS7217Q02", "PS7221Q02", "PS7228Q02", "PS7228Q03")

#P4Sc1[P4Sc1[, "STUDENT_ID"] == "00043" & P4Sc1[, "SCHOOL_ID"] == "003" , ]

pas = gsub("(.+)\\..+", "\\1",names(metaDat01))
pas = unique(pas)
#ii <- pas[39]

prueba <- list()
for(ii in pas){
  nii <- grep(ii, names(metaDat01),value = TRUE)
  if (ii %in% codResponse) {
    nii <- paste0(ii, ".A")
    if (!nii %in% names(metaDat01)){
      warning("No se encuentra la columna ", nii)
      next
    }

  }
  if (length(nii) > 1 ) {
    prueba[[ii]] <-  apply(metaDat01[,nii], 1,
                          function(x) paste(x, collapse = "" )) 
  } else {
   prueba[[ii]]<-metaDat01[,nii]
  }
}
#dim(sin_puntuar)
sin_puntuar <- bind_rows(prueba)

# # Ruta base de datos original
filenamePlantilla <- file.path(inPath, "RAW_Stdcogitem.xlsx")
RAW_Stdcogitem1 <- read_excel(filenamePlantilla, sheet = "Code")
RAW_Stdcogitem2 <- read_excel(filenamePlantilla, sheet = "Codebook")

RAW_Stdcogitem1 <- as.data.frame(RAW_Stdcogitem1) # es necesario?
RAW_Stdcogitem2 <- as.data.frame(RAW_Stdcogitem2)
nombresRAW <- colnames(RAW_Stdcogitem1)
RAW_Stdcogitem1 <- matrix(NA,ncol = ncol(RAW_Stdcogitem1),
                          nrow = nrow(sin_puntuar))
RAW_Stdcogitem1 <- as.data.frame(RAW_Stdcogitem1)
colnames(RAW_Stdcogitem1) <- nombresRAW
ii=RAW_Stdcogitem2[,"Match_Name"][1]
for(ii in RAW_Stdcogitem2[,"Match_Name"]){
  if(!is.na(ii)){  
    nii <- subset(RAW_Stdcogitem2  , Match_Name == ii)[,"Name"]
    if(!is.na(nii)){
      RAW_Stdcogitem1[,nii] <- sin_puntuar[[ii]]
    }
  }
}
#RAW_Stdcogitem1[,"fullid"]<-metaDat01[["IDCUES"]]
#RAW_Stdcogitem1[,"bookid"]<-metaDat01[["FORMCODE"]]
#names(sin_puntuar)
#RAW_Stdcogitem1[,"fullid"] <- as.character(RAW_Stdcogitem1[,"fullid"])
#RAW_Stdcogitem1[,"bookid"] <- as.character(RAW_Stdcogitem1[,"bookid"])
wb <- createWorkbook()
outputFile <- file.path(outPath,"RAW_Stdcogitem1.xlsx")
sheet1<-"Code"
assign(sheet1, xlsx::createSheet(wb, sheetName = sheet1))
addDataFrame(RAW_Stdcogitem1, sheet = get(sheet1), startRow =1,startColumn = 1, row.names = FALSE,
             col.names = TRUE)
sheet2<-"Codebook"
assign(sheet2, xlsx::createSheet(wb, sheetName = sheet2))
addDataFrame(RAW_Stdcogitem2, sheet = get(sheet2), startRow =1,startColumn = 1, row.names = FALSE,
             col.names = TRUE)
outputFile <- file.path(outPath, 'RAW_Stdcogitem1.xlsx')
xlsx::saveWorkbook(wb, file = outputFile)

#RAW_Stdcogitem1[,"PM5008Q01"] 
#sin_puntuar[,"PM5008Q01"]
#metaDat01[,"PM5008Q01"]
#names(metaDat01)

