################################################################################
# # Depuracion.R
# # R Versions: 1.0.0
# #
# # Autores: Viviana Beltr�n - Jonathan Pinz�n
# #
# # Proceso: Depuraci�n bases de datos
# # Descripci�n: Funciones para llevar a cabo los procesos de depuraci�n de bases
# #    de datos - Generaci�n de archivo de frecuencias de respuestas Nacionales e
# #    internacionales
# #
# # Entradas: Bases de datos
#
# # Salidas: Funciones
# #
# # File history:
# #   20170817: Creaci�n
################################################################################
# # Librer�as
library("dplyr")
library("rJava")
library("haven")
library("xlsx")

################################################################################
# # Par�metros generales
################################################################################
rutaPrincipal   <- "C:\\Users\\vbeltran\\Google Drive\\pisa_icfes\\"

# # Par�metros nacionales
# # Ruta base de datos original
filenameBDItems <- paste0(rutaPrincipal,"input\\FieldTrialP4S.csv")

# # Ruta base de datos final
filenameBaseFinal <- paste0(rutaPrincipal,"input\\FieldTrialCleaned.csv")

# # Archivo de salida de frencuencia de respuestas
outputFile      <- paste0(rutaPrincipal,"input\\frecuencias.xlsx")

# # Valores posibles de codificaci�n de �tems
vPosiblesItems  <- c("0","1","2","7","8","9","r")
# # N�mero  de columnas iniciales de informaci�n -- antes de columnas de �tems
nColInfo        <- 12
vNanItems       <- c("7","r")
vCerosItems     <- c("8","9")

# # Valores posibles de grados
vPosiblesGrado  <- c("7","8","9","10","11","99")
# # N�mero de la columna referente al grado
nColGrado       <- 7

# # Valores posibles de g�nero
vPosiblesGenero <- c("1","2","9")
# # N�mero de la columna referente al g�nero
nColGenero      <- 8
vNullGenero     <- "9" # Remover columnas con �ste c�digo

# # Par�metros internacionales
filenameFrecuenciasInt  <- paste0(rutaPrincipal,"input\\Pilot2012_CogTestScoredResponses.sav")
vNanItemsIn     <- c("6","7")
nColInfoI       <- 4
################################################################################
# # Creando archivo de salida --> Frecuencia de respuestas
wb <- createWorkbook()

# # Leyendo base de �tems original
itemsBD <- read.table(filenameBDItems, header=TRUE, sep="|", dec=".")

# # Revisando nombres de items
# # Pendiente --> Cargar archivo con listado de �tems

# # Pendiente --> Revisar qu� hacer con grados menores o no existentes
print("Revisando valores para grados: ")
grado        <- itemsBD[,nColGrado] 
uValuesGrado <- as.vector(unique(unlist(grado)))
print("N�mero de valores diferentes: ")
print(length(setdiff(uValuesGrado,vPosiblesGrado)))

# # Pendiente
# # print("Revisando valores posibles de edad: ")
# # Filtro 1 - Edad (15 a�os 3 meses - 16 a�os 2 meses)

print("Revisando valores posibles de g�nero: ")
genero  <- itemsBD[,nColGenero] 
uGenero <- unique(genero)
print("N�mero de valores diferentes: ")
print(length(setdiff(uGenero,vPosiblesGenero)))
print("Removiendo valores nulos - inv�lidos de g�nero: ")
itemsBD <- subset(itemsBD,X15.ST004 != vNullGenero)

print("Revisando # de colegios: ")
print(length(unique(itemsBD$SCHOOL_ID)))


print("Revisando �nicos pares de colegio - Id de estudiantes")
print(dim(itemsBD))
if(!dim(unique(itemsBD[c("SCHOOL_ID", "STUDENT_ID")]))[1] == dim(itemsBD)[1]){
  print(dim(unique(itemsBD[c("SCHOOL_ID", "STUDENT_ID")])))
  itemsBD <- itemsBD[!duplicated(itemsBD[c("SCHOOL_ID", "STUDENT_ID")]), ]
}
print(dim(itemsBD))

print("Revisando valores para codificaci�n de �tems: ")
items        <-  itemsBD[, -(1:nColInfo)] 

uValuesItems <- as.vector(unique(unlist(items)))
print("N�mero de valores diferentes: ")
print(length(setdiff(uValuesItems,vPosiblesItems)))
print("Imprimiendo conteos originales de base de �tems...")
print(table(as.vector(unlist(items))))

items <- data.frame(lapply(items, as.character), stringsAsFactors = FALSE)
print("Recodificando valores correspondientes a NA")
items <- data.frame(sapply(items, function(x){ 
      x[x %in% vNanItems]   <- NA
      return(x)}), stringsAsFactors = FALSE)

print("Recodificando valores correspondientes a 0")
items <- data.frame(sapply(items, function(x){ 
      x[x %in% vCerosItems] <- "0"
      return(x)}), stringsAsFactors = FALSE)

print("Imprimiendo conteos con valores recodificados...")
print(table(as.vector(unlist(items))))


# # Guardando base depurada de �tems
baseItemsFinal <- cbind(itemsBD[,1:nColInfo],items)
write.table(baseItemsFinal, file = filenameBaseFinal,
            sep = ",", dec = ".", row.names = FALSE, col.names = TRUE)

################################################################################
# # FRECUENCIAS
################################################################################
# # Frecuencias Nacionales
count.0        <- sapply(names(items),FUN=function(x,items){sum(items[,x]==0,na.rm=T)},items)
count.1        <- sapply(names(items),FUN=function(x,items){sum(items[,x]==1,na.rm=T)},items)
count.2        <- sapply(names(items),FUN=function(x,items){sum(items[,x]==2,na.rm=T)},items)
frecuenciasNac <- as.data.frame(cbind(names(items),count.0,count.1,count.2),row.names = FALSE,stringsAsFactors = FALSE)
colnames(frecuenciasNac) <- c("item","n0","n1","n2")

# # Frecuencias Internacionales
baseFrecuencias <- read_sav(filenameFrecuenciasInt)
names(baseFrecuencias) <- sapply(baseFrecuencias, attr, "label")

# # Revisar si es contra la base de USA
#baseFrecuencias <- as.data.frame(subset(baseFrecuencias, Country == 'USA'))
itemsI <- baseFrecuencias[-(1:nColInfoI)] # Se saca la �ltima columna referente a otra informaci�n
itemsI <- itemsI[,names(baseItemsFinal)[-(1:nColInfo)]]
uValuesItems <- as.vector(unique(unlist(itemsI)))
print(length(setdiff(uValuesItems,vPosiblesItems)))

itemsI <- data.frame(lapply(itemsI, as.character), stringsAsFactors=FALSE)
itemsI <- data.frame(sapply(itemsI, function(x){ 
  x[x %in% vNanItemsIn] <- NA
  return(x)}),stringsAsFactors=FALSE)

itemsI <- data.frame(sapply(itemsI, function(x){ 
  x[x %in% vCerosItems] <- "0"
  return(x)}),stringsAsFactors=FALSE)

# # Frecuencias Internacionales
count.0      <- sapply(names(itemsI),FUN=function(x,itemsI){sum(itemsI[,x]==0,na.rm=T)},itemsI)
count.1      <- sapply(names(itemsI),FUN=function(x,itemsI){sum(itemsI[,x]==1,na.rm=T)},itemsI)
count.2      <- sapply(names(itemsI),FUN=function(x,itemsI){sum(itemsI[,x]==2,na.rm=T)},itemsI)
frecuenciasI <- as.data.frame(cbind(names(itemsI),count.0,count.1,count.2),row.names = FALSE,stringsAsFactors = FALSE)
colnames(frecuenciasI) <- c("item","i0","i1","i2")


# # Guardando frecuencias Nacionales
assign("frecuenciasN", xlsx::createSheet(wb, sheetName = "frecuenciasN"))

addDataFrame(frecuenciasNac, sheet = get("frecuenciasN"), startRow =1,
             startColumn = 1, row.names = FALSE,
             col.names = TRUE)

# # Guardando frecuencias Internacionales
assign("frecuenciasInter", xlsx::createSheet(wb, sheetName = "frecuenciasInter"))

addDataFrame(frecuenciasI, sheet = get("frecuenciasInter"), startRow =1,
             startColumn = 1, row.names = FALSE,
             col.names = TRUE)

# # Guardando archivo de frecuencias
xlsx::saveWorkbook(wb, file = outputFile)

