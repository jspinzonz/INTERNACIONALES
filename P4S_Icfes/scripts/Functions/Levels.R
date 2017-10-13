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

lev_P4S <- function(x){
  x[,LevelP4S := cut(x[,PV_Mean], 
                  breaks = c(0, 334.94, 409.54, 484.14, 558.73, 633.33, 
                             707.93, max(x[,PV_Mean])),
                  labels = c("1b", "1a", "2", "3", "4", "5", "6"),
                  include.lowest = FALSE, right = TRUE)]
  return(x)
}

cuarDec<-function(resultPFS, wrFay){
  
  cuarDecil<- as.data.frame(resultPFS[1])[, 1:2]
  colnames(cuarDecil)<-c("SCHOOL_ID", "STUDENT_ID")
  
  for( ii in names(resultPFS)){
    temp           <- as.data.frame(resultPFS[ii])  
    colnames(temp) <- substr( colnames(temp), nchar(ii)+2 , 100L)
    temp <- merge(temp[, c("SCHOOL_ID", "STUDENT_ID", "PV_Mean")], 
                  wrFay[,  c("SCHOOL_ID", "STUDENT_ID", "Weight_81")],
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
