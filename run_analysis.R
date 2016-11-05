#----------------------------------.............................#

#   * LISTA DE FICHEROS EN EL DIRECTORIO *

init <- Sys.time()

path_train <- "C:/Users/Jose Antonio Rios/Desktop/GIT/train/Inertial Signals/"
path_test  <- "C:/Users/Jose Antonio Rios/Desktop/GIT/test/Inertial Signals/"

files_train  <- list.files(path_train)
files_test   <- list.files(path_test)

#----------------------------------.............................#

#   * CREA LOS NOMBRES DE LAS COLUMNAS *

col_nombre <- vector()

for (i in files_train){
  col_nombre <- c(col_nombre,sub("_train.txt", "", i))
}

#----------------------------------.............................#

#   * AGRUPA COLS FILE TEST *

df_total <-data.frame("Type"= 0)

df_test<-data.frame("Type"=  1)

for (j in files_test){
  file <- j
  fich0 <- paste(path_test,file,sep ="")
  df <- read.csv(fich0, sep="",dec=".",header=FALSE,encoding = "UTF-8")
  
  # Convertir el fichero en un vector
  
  p <- vector()
  col <- dim(df)[2]
 
  for (i in 1:col) {
    p<- append(p,df[,i]) 
  }
  
  df_test<- cbind(df_test,p)

}

names(df_test)<- c("Type",col_nombre)

#----------------------------------.............................#

#   * AGRUPA COLS FILE TRAIN *


df_train<-data.frame("Type"= 2)

for (j in files_train){
  file <- j
  fich0 <- paste(path_train,file,sep ="")
  df <- read.csv(fich0, sep="",dec=".",header=FALSE,encoding = "UTF-8")
  
  # Convertir el fichero en un vector
  
  p <- vector()
  
  col <- dim(df)[2]
  
  for (i in 1:col) {
    p<- append(p,df[,i]) 
  }

  df_train<- cbind(df_train,p)
  
}


names(df_train)<- c("Type",col_nombre)


#----------------------------------.............................#

#   * AGRUPA TODAS LAS COLS FILES TEST y TRAIN *


for (z in 1:10){
  r <- append(df_test[,z],df_train[,z])
  df_total <- cbind(df_total,r)
}
df_total$Type <- NULL  
names(df_total)<- c("Type",col_nombre)

#----------------------------------.............................#

#   * CALCULA MEDIAS y DESVIACION STANDARD 

media_test  <-apply(subset(df_total,df_total[,1]==1),2,mean)
stdesv_test <-apply(subset(df_total,df_total[,1]==1),2,sd)

media_train <-apply(subset(df_total,df_total[,1]==2),2,mean)
stdesv_train<-apply(subset(df_total,df_total[,1]==2),2,sd)

media_total <-apply(df_total,2,mean)
stdesv_total<-apply(df_total,2,sd)

resum_Calcul<-cbind(media_test,media_train,media_total,stdesv_test,stdesv_train,stdesv_total)
resum_Calcul<-subset(resum_Calcul,rownames(resum_Calcul) != "Type")

#----------------------------------.............................#

#   * GUARDAR FICHERO DE CALCULOS * 

path_save   <- "C:/Users/Jose Antonio Rios/Desktop/GIT/"
fich1       <- "rtdos_calcul.txt"
fich_calcul <-  paste(path_save,fich1,sep="")


new_calcul<- cbind("var_types"=rownames(resum_Calcul),resum_Calcul)

write.table(new_calcul,file=fich_calcul,row.name=FALSE)

#----------------------------------.............................#

#   * TIEMPO DURACION PGM * 

end <- Sys.time()

elapsed <- paste("elapsed time for del pgm   :", end-init, "  seconds")

print(elapsed)

