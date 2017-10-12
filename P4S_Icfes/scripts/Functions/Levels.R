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
