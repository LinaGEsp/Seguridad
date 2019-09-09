read.cutdate<-function(){
 cat("\014")
 FCorte<<-NA
 while(is.na(FCorte)){
  cat ("\n Please insert Cut Date in format (dd/mm/yyyy): \n")
  line <- readline()
  FCorte<<-as.Date.character(line, format = "%d/%m/%Y")
  if(is.na(FCorte)){
   message(" Invalid Date, Please try again!.")
  }
 }
 message("\n\n Cut Date updated to: ",FCorte, "\n\n")
}

clean.envir<-function(){
 cat("\014")
 x<-0
 cat ("\n Do you want to clean up the global environment? (Type 1 or 2): \n 1: Yes \n 2: No")
 x<-readline()
 while(!(x %in% c(1,2))==T){
  cat ("\n Do you want to clean up the global environment? (Type 1 or 2): \n 1: Yes \n 2: No")
  x<-readline()
 }
 if(x==1){
  message("Cleaning environment...")
  rm(list=ls(envir=globalenv()), envir=globalenv())
  gc()
  source("fun.R") 
  message("It's Done!")
 } else {
  message("Ok!")
 }
}

load.lib<-function(...){
 #detach("package:data.table", unload=TRUE)
 #remove.packages("data.table")
 needs<-as.vector(list(...))
 libs<-as.data.frame(installed.packages(lib.loc = NULL, priority = NULL,
                                        noCache = FALSE, fields = NULL,
                                        subarch = .Platform$r_arch))
 libs<-libs$Package
 for(i in 1:length(needs)){
  if( needs[[i]] %in% libs ){
   message("loading library ", needs[[i]])
   library(eval(needs[[i]]), character.only=TRUE)
  } else{
   message("Library ", needs[[i]], " is not installed... Installing library ", needs[i])
   install.packages(eval(needs[[i]]), dependencies = T)
   message("Loading library ", needs[[i]])
   library(eval(needs[[i]]), character.only=TRUE)
  }
 }
}

complete.dates<-function(df, meses){
      #print(names(df))
      addM<-which(as.factor(meses) %in% df$Fecha ==F)
      for(i in 1:length(addM)){
            ndf<-as.data.frame(t(c(format(meses[addM[i]], "%Y-%m-%d"),0)))
            names(ndf)<-c("Fecha","Cuenta")
            df<-rbind(df[1:addM[i]-1,],ndf,df[-(1:addM[i]-1),])
      }
      row.names(df)<-1:length(df[,1])
      names(df)<-c("Fecha","Cuenta")
      df$Cuenta<-as.numeric(as.character(df$Cuenta))
      return(df)
}

find.290 <- function(df.links, folder){
  require(stringr)
  l <- list.files(folder)
  files<-NULL
  for(j in 1:length(l)){
    n<-unlist(str_split(l[j], "\\."))[1]
    n<-unlist(str_split(n, "-"))[2]
    files<-c(files, n)
  }
  
  ## missing data
  missing.data <- df.links %>%
                  filter(!(df.links$Mes %in% files))
  already <- df.links$Mes[df.links$Mes %in% files]
  
  
  if(length(missing.data$Mes)>0){
    message("\n\n New information is available for the following periods: ")
    for(i in 1:length(missing.data$Mes)){
      print(as.character(missing.data$Mes[i]))
    }
  } else {
    message("\n\n The information is updated to the last period available on the web")
  }
  # cat ("\n Press intro to continue... \n")
  x<-readline()
  return(missing.data)
}

download.290 <- function(server, df.links, folder){
  require(dplyr)
  if(file.exists("./parameters/Meses.csv")==T){
    meses <- read.csv("./parameters/Meses.csv", header = T, sep=";", colClasses = "character")
  } 
  df.links <- find.290(df.links, folder)
  if(length(df.links[,1])>=1){
    for(i in 1:length(df.links[,1])) {
      if(file.exists("./parameters/Meses.csv")==T){
        n <- which(as.character(meses$Mes) == unlist(str_split(df.links$Mes[i], " "))[1])
        mes <- str_pad(meses$NumMes[n], 2, pad="0") 
      } else {mes <- ""}
      message("... downloading ", df.links$Mes[i])
      download.file(paste0(server_url, df.links$link[i]), 
                    destfile = paste0("./data/", unlist(str_split(df.links$Mes[i], " "))[2],
                                      str_pad(n, 2, pad="0"), "-", df.links$Mes[i], ".",
                                      unlist(str_split(as.character(df.links$link[i]), "\\."))[2]), 
                    mode="wb")
      message("\n\n")
    }
    message("\n ... ready the chicken!!!")
  } else {
    message("\n ... No hay informacion nueva disponible!!!")
  }
}

read.290 <- function(years){
  l<-list.files("./data")
  filtro<-NULL
  for(i in 1:length(l)){
    m<-str_sub(unlist(str_split(l[i], "\\."))[1], 
               start = nchar(unlist(str_split(l[i], "\\."))[1])-3,
               end = nchar(unlist(str_split(l[i], "\\."))[1]))
    filtro <- c(filtro, m)
  }
  filtrar <- which(filtro %in% years)
  
  l <- l[filtrar]
  for(i in 1:length(l)){
    message(" Reading ", l[i])
    nom <- paste0("f290_", unlist(str_split(l[i], "-"))[1])
    p <- read_excel(paste0("./data/", l[i]), skip=8, col_types="guess", n_max = 5)
    cols<-dim(p)[2]
    tipos <- c("text", "numeric", "text", rep("numeric", cols-3))
    y <- read_excel(paste0("./data/", l[i]), skip=8, col_types=tipos)
    
    if(as.numeric(str_sub(l[i], 5, 6))<12){
      FCorte <- paste("01", str_pad(as.numeric(str_sub(l[i], 5, 6))+1, 2, pad="0"), str_sub(l[i], 1, 4), sep="/")
    }else {
      FCorte <- paste("01", str_pad(1, 2, pad="0"), as.numeric(str_sub(l[i], 1, 4))+1, sep="/")
    }
    
    FCorte <- as.Date.character(FCorte, format = "%d/%m/%Y")-1
    y$FCorte <- FCorte
    assign(nom, y) #, envir=globalenv()
    
  }
  
  formatos.290 <- list()
  l <- ls()[grep("^f290", ls())]
  for(i in 1:length(l)){
    formatos.290[[i]] <- get(l[i])
  }
  names(formatos.290) <- l
  #summary(formatos.290)
  save(formatos.290, file="../datos_pruebas/formatos.290.RData")
  
  return(formatos.290)
}

## Define nuevas Características definidas a partir de la combinación de cuentas contables disponibles
## en el formato 290
new_feature.290 <- function(serie.290, counts = Ctas_Asociadas, nameFeature = NombreIndicador, keep=FALSE,
                            percentage = FALSE, abs.value=TRUE){
  for(m in 1:length(serie.290)){
    p <- serie.290[[m]]
    message(" Calculando ", nameFeature, " para el Grupo ", names(serie.290)[m])
    
    CtasIndicador <- p %>%
                     mutate(CuentaCorta = str_sub(Cuenta, 7, 12)) %>%
                     filter(CuentaCorta %in% counts) %>%
                     select(-(CuentaCorta))
    CtasIndicador$Ramo <- as.character(CtasIndicador$Ramo)   
    CtasIndicador$Nombre_cuenta <- as.character(CtasIndicador$Nombre_cuenta)
    
    Ramos <- as.character(unique(CtasIndicador$Ramo))
    Indicador <- CtasIndicador[1,]
    Indicador <- Indicador[-(1),]
    NuevosInd<-NULL
    
    for(i in 1:length(Ramos) ){
      # Agrupamos cuentas
      IndXramo <- CtasIndicador %>%
                  filter(Ramo==Ramos[i]) %>%
                  mutate(CuentaCorta = str_sub(Cuenta, 7, 12))
      # Calcula y Guarda resultados
      NuevosInd[1] <- paste0("290999", str_pad(sum(as.numeric(counts)), 5, pad="0"))
      NuevosInd[2] <- nameFeature
      NuevosInd[3] <- as.character(Ramos[i])
      
      if(length(4:length(names(CtasIndicador)))>1){
        if(percentage == FALSE){
          NuevosInd[4:length(names(CtasIndicador))] <- colSums(IndXramo[,4:length(names(CtasIndicador))])
          } else{
                n <- which(counts[1]==IndXramo$CuentaCorta) # determina numerador
                d <- which(counts[2]==IndXramo$CuentaCorta) # determina denominador
                NuevosInd[4:length(names(CtasIndicador))] <- apply(IndXramo[,4:length(names(CtasIndicador))],
                                                                   MARGIN = 2, FUN = indice.290, numerador=n, 
                                                                   denominador=d, abs.value = abs.value)
                }
        } else{
              if(percentage == FALSE){
                NuevosInd[4:length(names(CtasIndicador))] <- sum(IndXramo[,4:length(names(CtasIndicador))])
              } else{
                     n <- which(counts[1]==IndXramo$CuentaCorta) # determina numerador
                     d <- which(counts[2]==IndXramo$CuentaCorta) # determina denominador
                     NuevosInd[4:length(names(CtasIndicador))] <- indice.290(IndXramo[,4:length(names(CtasIndicador))],
                                                                             numerador = n, denominador = n,
                                                                             abs.value = abs.value)
              }
      }
      
      Indicador[i,] <- NuevosInd
      NuevosInd<-NULL
    }
    ## Coerción resultados a numéricos
    for(j in 4:length(names(Indicador))){
      Indicador[, j] <- as.numeric(Indicador[, j])
    }
    
    ## Añadimos resultado a tabla original
    p <- rbind(p, Indicador)
    serie.290[[m]] <- p
  }
  #if(keep==T){
  #  save(serie.290, file = "./results/serie.290.RData")
  #}
  
  message("\n ... ready the chicken!!!")
  return(serie.290)
}

indice.290 <- function(r, numerador, denominador, abs.value){
  if(abs.value==TRUE){
    res <- round(abs(r[numerador]/r[denominador])*100,2)
    } else{
          res <- round(-(r[numerador]/r[denominador])*100,2)
          }
  
  res <- ifelse(is.nan(res) | is.infinite(res), 0, res)
  return(res)
}

ranking.290 <- function(serie, features, gr_ramo, periods){
  message("Grupo Ramos: ", gr_ramo)
  message("Periodos:  ", periods)
  require(stringr)
  require(reshape2)
  rank_period<-list()
  message("\n")
  for(k in 1:length(periods)){
    message("Generando ranking para el periodo ", periods[k])
    ye<-ifelse(as.numeric(str_sub(periods[k], 6, 7))==12, as.numeric(str_sub(periods[k], 1, 4))+1,
               str_sub(periods[k], 1, 4))
    mo<-ifelse(as.numeric(str_sub(periods[k], 6, 7))==12, "01",
                          str_pad(as.numeric(str_sub(periods[k], 6, 7))+1, 2, pad="0"))
    fecha<-as.Date.character(paste(ye, mo, 1, sep="/"), format = "%Y/%m/%d")-1
    #message("Fecha informe: ", fecha, "sobre la serie: ")
    #print(names(serie))
    
    sel <- list()
    pos<-1
    noms<-NULL
    for(i in 1:length(serie)){
      #message("Resumen cia: ", i, "-", names(serie)[i])
      p <- serie[[i]]
      
      # Si la compañía no tiene información para el periodo, ésta se salta
      valida <-which(names(p)==as.character(fecha))
      if(length(valida)==0) {
        message("Cia ", i, "-", names(serie)[i], " no tiene info")
        next
      }
      resumen <- p %>%
                 mutate(Cuenta = as.numeric(str_sub(Cuenta, 7, 12)), Compania = names(serie)[i]) %>%
                 select(Compania, Cuenta, Nombre_cuenta, Ramo, valida) %>%
                 filter(Cuenta %in% features & Ramo %in% gr_ramo) %>%
                 dcast(Compania + Ramo ~ Nombre_cuenta, value.var=names(p)[valida])
      
      # Guarda resultado
      sel[[pos]]<-resumen
      pos<-pos+1
      noms <- c(noms, names(serie)[i])
    }
    
    ## Genera tabla de resultado por año
    #message(length(sel))
    df_response <- sel[[1]]
    for(i in 2:length(sel)){
      df_response <- rbind(df_response, sel[[i]])
    }
    
    ## Filtra compañías sin prima emitida
    df_response <- df_response %>%
      filter(`PRIMAS DEVENGADAS`>0 & !Compania %in% c("TOTAL_GRALES", "TOTAL_VIDA"))
    
    names(sel) <- noms
    summary(sel)
    rank_period[[k]] <- df_response
  }
  names(rank_period)<-paste0("P", periods)
  
  message("\n ... ready the chicken!!!")
  return(rank_period)
}

copy.table<-function(obj, size = 4096) {
  clip <- paste('clipboard-', size, sep = '')
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = FALSE, sep = '\t')
  close(f) 
}

info.loaded <- function(){
  if (!file.exists("../datos_pruebas/serie.290.RData")){
    return(NULL)
  } else{
    #require(lubridate)
    load("../datos_pruebas/serie.290.RData")
    Aseguradoras <- names(serie.290)
    desde <- NULL
    hasta <- NULL
    for(i in 1:length(Aseguradoras)){
      p <-names(serie.290[[i]])[4:(length(names(serie.290[[i]])))]
      desde <- c(desde, p[1])
      hasta <- c(hasta, p[length(p)])
    }
    df <- data.frame(Aseguradoras, desde, hasta)
    return(df)
  }
}

periodos <- function(serie){
  require(stringr)
  require(lubridate)
  serie <- readRDS("../datos_pruebas/serie.290(features).rds")
  
  maxInfo <- rownames(summary(serie))[which.max(summary(serie)[,1])]
  p<-serie[[maxInfo]]
  p2<-names(p)[4:length(p)]
  per<-paste(year(p2), str_pad(month(p2), width = 2, pad="0"), sep="-")
  return(per)
}









