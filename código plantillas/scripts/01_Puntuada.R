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

rutaPrincipal   <- "C:/Users/jpinzon/Documents/JONNATHAN/Internacionales/P4S/Plantillas/codigo/"

# # Ruta base de datos original
filenamePlantilla <- paste0(rutaPrincipal,"input/SCORED_Stdcogitem.xlsx")

# # Ruta base de datos final
filenameBasePuntuada <- paste0(rutaPrincipal,"input/Base_puntuada.xlsx")

RAW_Stdcogitem1 <- read_excel(filenamePlantilla, sheet = "Code")
RAW_Stdcogitem2 <- read_excel(filenamePlantilla, sheet = "Codebook")

Base_puntuada <- read_excel(filenameBasePuntuada)
RAW_Stdcogitem1 <- as.data.frame(RAW_Stdcogitem1) # es necesario?
RAW_Stdcogitem2 <- as.data.frame(RAW_Stdcogitem2)
nombresRAW <- colnames(RAW_Stdcogitem1)
RAW_Stdcogitem1 <- matrix(NA,ncol = ncol(RAW_Stdcogitem1),
                          nrow = nrow(Base_puntuada))
RAW_Stdcogitem1 <- as.data.frame(RAW_Stdcogitem1)
colnames(RAW_Stdcogitem1) <- nombresRAW

for(ii in RAW_Stdcogitem2[,"Match_Name"]){
  if(!is.na(ii)){  
    nii <- subset(RAW_Stdcogitem2  , Match_Name == ii)[,"Name "]
    if(!is.na(nii)){
      RAW_Stdcogitem1[,nii] <- Base_puntuada[[ii]]
    }
  }
}
RAW_Stdcogitem1[,"fullid"] <- as.character(RAW_Stdcogitem1[,"fullid"])
RAW_Stdcogitem1[,"bookid"] <- as.character(RAW_Stdcogitem1[,"bookid"])
wb <- createWorkbook()
outputFile <- paste0(rutaPrincipal,"output/RAW_Stdcogitem.xlsx")
sheet1<-"Code"
assign(sheet1, xlsx::createSheet(wb, sheetName = sheet1))
addDataFrame(RAW_Stdcogitem1, sheet = get(sheet1), startRow =1,startColumn = 1, row.names = FALSE,
             col.names = TRUE)
sheet2<-"Codebook"
assign(sheet2, xlsx::createSheet(wb, sheetName = sheet2))
addDataFrame(RAW_Stdcogitem2, sheet = get(sheet2), startRow =1,startColumn = 1, row.names = FALSE,
             col.names = TRUE)

xlsx::saveWorkbook(wb, file = outputFile)
  
