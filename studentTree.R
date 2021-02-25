#---Instalaci�n de paquetes
install.packages("dplyr")
install.packages('rpart')
install.packages('rattle')
install.packages('rpart.plot')

#---Importaci�n de datos
mat_class=read.table("RFiles/student/student-mat.csv",sep=";",header=TRUE)

print(nrow(mat_class)) 

#---Limpieza y preprocesamiento de datos
library(dplyr)

mat_data <- filter(mat_class,school == 'GP')
mat_data$alcohol <- round((((5*mat_data$Dalc) + (2*mat_data$Walc))/7),0)  

#se crea la funci�n de asignacion de pasado o no
fun_pass <- function(calif) ifelse(calif >= 10, 1, 0)

mat_data$pass <- fun_pass(mat_data$G3)

mat_data <- select(mat_data,sex,address,Pstatus,Medu,Fedu,failures,schoolsup,famsup,
                   higher,internet,famrel,absences,alcohol,G1,G2,G3,pass)

library(rpart)
library(rattle)
library(rpart.plot)

#---Modelado del arbol de decisi�n
#Pasara o no la materia a partir de calificaciones
arbol_pass_calif <- rpart(                     
  formula = pass ~ G1 + G2,
  data = mat_data,
  method = 'class',
  cp = -1
)

arbol_pass_calif
fancyRpartPlot(arbol_pass_calif) 
rpart.plot(arbol_pass_calif,extra = 4)

printcp(arbol_pass_calif)  #estadisticas de resultados
plotcp(arbol_pass_calif)   #evoluci�n del error a medida que crece el arbol

#podar arbol
arbolPodado <- prune(arbol_pass_calif, cp = arbol_pass_calif$cptable[which.min(arbol_pass_calif$cptable[,"xerror"]),"CP"])
print(arbolPodado)
printcp(arbolPodado)

#Predecir                                           #cambiar a test
pred_arbol_passCalif <- predict(arbol_pass_calif, newdata = mat_data, type = 'class')
table(pred_arbol_passCalif,mat_data$pass)

sum(pred_arbol_passCalif == mat_data$pass) / length(mat_data$pass)*100

#Pasara o no la materia partir de otros datos
arbol_pass_data <- rpart(                     
  formula = pass ~  failures +  internet + absences + alcohol,
  data = mat_data,
  method = 'class'
)

arbol_pass_data
fancyRpartPlot(arbol_pass_data) 


#Piensa en educaci�n preparatoria si o no 
arbol_higher <- rpart(                     
  formula = higher ~ sex + address + Pstatus + Medu + Fedu + schoolsup + 
            famsup + famrel + alcohol + pass,
  data = mat_data,
  method = 'class',
  cp = -1, 
  maxdepth = 3
)

arbol_higher
fancyRpartPlot(arbol_higher) 

#Nivel de alcoholismo
arbol_alcohol <- rpart(                     
  formula = alcohol ~ sex + address + schoolsup + famsup + failures,
  data = mat_data,
  method = 'class',
  cp = -1,
  maxdepth = 3
)

arbol_alcohol
fancyRpartPlot(arbol_alcohol) 

#---Predicciones
#Predecir con el arbol 
pred_arbol_passCalif <- predict(arbol_pass_calif, type = 'class')
pred_arbol_passData <- predict(arbol_pass_data, type = 'class')
pred_arbol_higher <- predict(arbol_higher, type = 'class')
pred_arbol_alcohol <- predict(arbol_alcohol, type = 'class')

  
mat_predictions <- cbind(mat_data, pred_arbol_passCalif)       
mat_predictions <- cbind(mat_predictions, pred_arbol_passData)
mat_predictions <- cbind(mat_predictions, pred_arbol_higher)
mat_predictions <- cbind(mat_predictions, pred_arbol_alcohol)

#---Prediciones ejemplo
predict(arbol_pass_calif, 
        newData = data.frame(G1 = 0, G2 = 3),
        type = 'class' ) 

predict(object = arbol_pass_data, 
        newData = data.frame(failures = 2,  internet = 'no', absences = 0, alcohol = 5),
        type = 'class' ) 

predict(object = arbol_higher, 
        newData = data.frame(sex = 'M', address = 'R', Pstatus = 'A',  Medu = 0, Fedu = 0, schoolsup = 'no', famsup = 'no', famrel = 1, alcohol = 4, pass = 0),
        type = 'class' ) 

predict(object = arbol_alcohol,
        newdata = data.frame(sex = 'M', address = 'R', schoolsup = 'yes', famsup = 'no', failures = 4), 
        type = 'class')

