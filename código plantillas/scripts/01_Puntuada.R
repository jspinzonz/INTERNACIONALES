################################################################################
# # Puntuada.R
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

# # Librerías

library("rJava")
library("haven")
library("xlsx")
library("readxl")

# # Rutas
docPath <- file.path("..", "doc")
inPath  <- file.path("..", "input")
outPath <- file.path("..", "output")

# # Ruta base de datos original
filenamePlantilla    <- file.path(inPath, "SCORED_Stdcogitem.xlsx")

# # Ruta base de datos final
filenameBasePuntuada <- file.path(inPath, "FieldTrialCleaned.xlsx")

SCORED_Stdcogitem1 <- read_excel(filenamePlantilla, sheet = "Code")
SCORED_Stdcogitem2 <- read_excel(filenamePlantilla, sheet = "Codebook")

Base_puntuada <- read_excel(filenameBasePuntuada)
SCORED_Stdcogitem1 <- as.data.frame(SCORED_Stdcogitem1) # es necesario?
SCORED_Stdcogitem2 <- as.data.frame(SCORED_Stdcogitem2)
nombresRAW <- colnames(SCORED_Stdcogitem1)
SCORED_Stdcogitem1 <- matrix(NA,ncol = ncol(SCORED_Stdcogitem1),
                          nrow = nrow(Base_puntuada))
SCORED_Stdcogitem1 <- as.data.frame(SCORED_Stdcogitem1)
colnames(SCORED_Stdcogitem1) <- nombresRAW

for(ii in SCORED_Stdcogitem2[,"Match_Name"]){
  if(!is.na(ii)){  
    nii <- subset(SCORED_Stdcogitem2  , Match_Name == ii)[,"Name "]
    if(!is.na(nii)){
      SCORED_Stdcogitem1[,nii] <- Base_puntuada[[ii]]
    }
  }
}
#SCORED_Stdcogitem1[,"fullid"] <- as.character(SCORED_Stdcogitem1[,"fullid"])
#SCORED_Stdcogitem1[,"bookid"] <- as.character(SCORED_Stdcogitem1[,"bookid"])
wb <- createWorkbook()
outputFile <- file.path(outPath, "SCORED_Stdcogitem.xlsx")
sheet1<-"Code"
assign(sheet1, xlsx::createSheet(wb, sheetName = sheet1))
addDataFrame(SCORED_Stdcogitem1, sheet = get(sheet1), startRow =1,startColumn = 1, row.names = FALSE,
             col.names = TRUE)
sheet2<-"Codebook"
assign(sheet2, xlsx::createSheet(wb, sheetName = sheet2))
addDataFrame(SCORED_Stdcogitem2, sheet = get(sheet2), startRow =1,startColumn = 1, row.names = FALSE,
             col.names = TRUE)
outputFile <- file.path(outPath, 'SCORED_Stdcogitem.xlsx')
xlsx::saveWorkbook(wb, file = outputFile)
  
