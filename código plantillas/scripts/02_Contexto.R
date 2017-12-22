################################################################################
# # Puntuada.R
# # R Versions: 
# #
# # Autores: Jonnathan Pinzón y Jorge Carrasco
# #
# # Proceso:
# # Descripción: 
# #
# # Entradas: 
#
# # Salidas: 
################################################################################

# # Librerías

library("rJava")
library("haven")
library("xlsx")
library("readxl")
library("readr")

# # Rutas
inPath  <- file.path("..", "input")
outPath <- file.path("..", "output")

# # Ruta base de datos original
filenamePlantilla <- file.path(inPath, "StdQ_golddataset17.xlsx")
filenamePlantillacsv <- file.path(inPath, "StdQ_golddataset17.csv")
# # Ruta base de datos final
filenameBasePuntuada <- file.path(inPath, "Base_contexto.csv")

#StdQ_golddataset17_1 <- read_delim(filenamePlantillacsv,";", escape_double = FALSE, trim_ws = TRUE)
StdQ_golddataset17_1 <- read_excel(filenamePlantilla, sheet = "Code")
StdQ_golddataset17_2 <- read_excel(filenamePlantilla, sheet = "Codebook")

# # Lectura de info proceso de calificacion
fileScore <- file.path(inPath, "resultPFSALL.Rdata")
load(fileScore)
datOut[, STUDENT_ID := sprintf("%0.5d", STUDENT_ID)]
datOut[, SCHOOL_ID := sprintf("%0.3d", SCHOOL_ID)]

#Base_puntuada <- read_excel(filenameBasePuntuada)
Base_contexto <- read_delim(filenameBasePuntuada, ";", escape_double = FALSE, trim_ws = TRUE)
Base_contexto <- merge(Base_contexto, datOut, by = c("SCHOOL_ID", "STUDENT_ID"))

StdQ_golddataset17_1 <- as.data.frame(StdQ_golddataset17_1) # es necesario?
StdQ_golddataset17_2 <- as.data.frame(StdQ_golddataset17_2)
nombresStdQ <- colnames(StdQ_golddataset17_1)
StdQ_golddataset17_1 <- matrix(NA,ncol = ncol(StdQ_golddataset17_1),
                          nrow = nrow(Base_contexto))
StdQ_golddataset17_1 <- as.data.frame(StdQ_golddataset17_1)
colnames(StdQ_golddataset17_1) <- nombresStdQ
#ii<-StdQ_golddataset17_2[,"Match_Name"][9]
#StdQ_golddataset17_2[,"Match_Name"]%in%nombresStdQ
for(ii in StdQ_golddataset17_2[,"Match_Name"]){
  if(!is.na(ii)){  
    nii <- subset(StdQ_golddataset17_2  , Match_Name == ii)[,"Name"]
    if(!is.na(nii)){
      StdQ_golddataset17_1[,nii] <- Base_contexto[[ii]]
      cat(ii," = ",nii,"\n")
      print(Base_contexto[[ii]])
    }
  }
}



################################################################################
# # Agregando información de sobre pesos y plausibles
################################################################################
#StdQ_golddataset17_1[,"fullid"] <- as.character(StdQ_golddataset17_1[,"fullid"])
#StdQ_golddataset17_1[,"bookid"] <- as.character(StdQ_golddataset17_1[,"bookid"])
wb         <- createWorkbook()
outputFile <- file.path(outPath, "StdQ_golddataset17.xlsx")
sheet1     <-"Code"
assign(sheet1, xlsx::createSheet(wb, sheetName = sheet1))
addDataFrame(StdQ_golddataset17_1, sheet = get(sheet1), startRow =1,startColumn = 1, row.names = FALSE,
             col.names = TRUE)
sheet2<-"Codebook"
assign(sheet2, xlsx::createSheet(wb, sheetName = sheet2))
addDataFrame(StdQ_golddataset17_2, sheet = get(sheet2), startRow =1,startColumn = 1, row.names = FALSE,
             col.names = TRUE)

xlsx::saveWorkbook(wb, file = outputFile)
