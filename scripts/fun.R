
fit.dir<-function(dir){
  ## Metodología para reescribir dirección de forma legible
  words<-NA
  desde<-NA
  hasta<-NA
  esp<-NA
  
  #dir<-"Calle 152 b # 56 - 75"
  dir<-toupper(dir)
  dir<-gsub("[.]"," ",dir)
  dir<-gsub(","," ",dir)
  
  dir<-gsub("CARRERRA","arrera ",dir)
  dir<-gsub("CARRRERA","arrera ",dir)
  dir<-gsub("CARRRA","arrera ",dir)
  dir<-gsub("CARRERA","arrera ",dir)
  dir<-gsub("CARERRA","arrera ",dir)
  dir<-gsub("KARRERA","arrera ",dir)
  dir<-gsub("CARRER","arrera ",dir)
  dir<-gsub("CARREA","arrera ",dir)
  dir<-gsub("CARERA","arrera ",dir)
  dir<-gsub("CRRR","arrera ",dir)
  dir<-gsub("CARR","arrera ",dir)
  dir<-gsub("AVCRA","arrera ",dir)
  dir<-gsub("CRAQ","arrera ",dir)
  dir<-gsub("CRRA","arrera ",dir)
  dir<-gsub("KREA","arrera ",dir)
  dir<-gsub("CRR","arrera ",dir)
  dir<-gsub("KRR","arrera ",dir)
  dir<-gsub("CRA","arrera ",dir)
  dir<-gsub("CAR","arrera ",dir)
  dir<-gsub("KRA","arrera ",dir)
  dir<-gsub("KAR","arrera ",dir)
  dir<-gsub("JRA","arrera ",dir)
  dir<-gsub("CR","arrera ",dir)
  dir<-gsub("KR","arrera ",dir)
  dir<-gsub("AK","arrera ",dir)
  
  
  dir<-gsub("ACALLE","alle ",dir)
  dir<-gsub("CALLEE","alle ",dir)
  dir<-gsub("CALLAE","alle ",dir)
  dir<-gsub("CALEE","alle ",dir)
  dir<-gsub("CALLE","alle ",dir)
  dir<-gsub("CLLE","alle ",dir)
  dir<-gsub("CLLA","alle ",dir)
  dir<-gsub("CALL","alle ",dir)
  dir<-gsub("CALE","alle ",dir)
  dir<-gsub("CLL","alle ",dir)
  dir<-gsub("CLE","alle ",dir)
  dir<-gsub("CAL","alle ",dir)
  dir<-gsub("AVCL","alle ",dir)
  dir<-gsub("CL","alle ",dir)
  dir<-gsub("AC","alle ",dir)
  
  dir<-gsub("DUIAGONAL","Diagonal ",dir)
  dir<-gsub("DIAGONAL","Diagonal ",dir)
  dir<-gsub("DIGONAL","Diagonal ",dir)
  dir<-gsub("DIAGO","Diagonal ",dir)
  dir<-gsub("DIAG","Diagonal ",dir)
  dir<-gsub("DIG","Diagonal ",dir)
  dir<-gsub("DIA","Diagonal ",dir)
  dir<-gsub("DG","Diagonal ",dir)
  dir<-gsub("Dg","Diagonal ",dir)
  
  dir<-gsub("Av","AVENIDA ",dir)
  dir<-gsub("AVEBNIDA","Avenida ",dir)
  dir<-gsub("AVENIDA","Avenida ",dir)
  dir<-gsub("AVDA","Avenida ",dir)
  dir<-gsub("AVEN","Avenida ",dir)
  dir<-gsub("AVD","Avenida ",dir)
  dir<-gsub("AV","Avenida ",dir)
  dir<-gsub("AVE[.]","Avenida ",dir)
  
  dir<-gsub("TRANSVERSAL","Transversal ",dir)
  dir<-gsub("TRASNVERSAL","Transversal ",dir)
  dir<-gsub("TRASNVERSAL","Transversal ",dir)
  dir<-gsub("TARVERSAL","Transversal ",dir)
  dir<-gsub("TRANSV","Transversal ",dir)
  dir<-gsub("TRANS","Transversal ",dir)
  dir<-gsub("TRSANV","Transversal ",dir)
  dir<-gsub("TVERSAL","Transversal ",dir)
  dir<-gsub("TRSANV","Transversal ",dir)
  dir<-gsub("TVERSAL","Transversal ",dir)
  dir<-gsub("TRANVS","Transversal ",dir)
  dir<-gsub("TRNVS","Transversal ",dir)
  dir<-gsub("TRANV","Transversal ",dir)
  dir<-gsub("TRNAV","Transversal ",dir)
  dir<-gsub("TRNV","Transversal ",dir)
  dir<-gsub("TRAS","Transversal ",dir)
  dir<-gsub("TRAV","Transversal ",dir)
  dir<-gsub("Tv","Transversal ",dir)
  dir<-gsub("TRSV","Transversal ",dir)
  dir<-gsub("TVS","Transversal ",dir)
  dir<-gsub("TV","Transversal ",dir)
  dir<-gsub("TRV","Transversal ",dir)
  dir<-gsub("TRA","Transversal ",dir)
  dir<-gsub("TR","Transversal ",dir)
  
  dir<-gsub("AUTOPISTA","Autopista ",dir)
  dir<-gsub("AUTOPIESTA","Autopista ",dir)
  dir<-gsub("AUTOP","Autopista ",dir)
  dir<-gsub("AUTO","Autopista ",dir)
  dir<-gsub("AUT","Autopista ",dir)
  
  dir<-gsub("CIRCUNVALAR","Circunvalar ",dir)
  dir<-gsub("CIRCUNV","Circunvalar ",dir)
  
  dir<-gsub("NORTE"," norte ",dir)
  dir<-gsub("SUR"," sur ",dir)
  dir<-gsub("OESTE"," oeste ",dir)
  dir<-gsub("ESTE"," este ",dir)
  
  
  dir<-gsub("  "," ",dir)
  dir
  
  conv<-c("alle", "arrera", "Avenida", "Autopista", "Diagonal", "Transversal", "Circunvalar", 
    "BIS", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "O", "P", "Q", "R", "S", 
    "T", "U", "V", "W", "X", "Y", "Z","norte", "sur", "este", "oeste"
  )
  
  vias<-c("Calle", "Carrera", "Avenida", "Autopista", "Diagonal", "Transversal")
  
  coordenadas<-c("norte", "sur", "este", "oeste")
  
  ## Detecta todos los caracteres de tipo texto dentro de una palabra, que son diferentes a los del vector
  ## conv. Sirve para determinar las direcciones tipo calle 10B o carrera 107A.
  words<-regmatches(dir, gregexpr("([[:alpha:]]+)", dir))[[1]]
  ## filtramos las palabras que NO están en el vector 'conv'
  words<-words[!(words %in% conv)]
  a<-character(0)
  a<-identical(a, words)
  a
  
  ## Si hay palabras diferentes las eliminamos
  if(a==F) {
    for(i in 1:length(words)){
      dir<-gsub(words[i], " ", dir)
    }
  }
  dir
  
  ## Aplicamos words de nuevo para eliminar alguna otra plabra sobrante
  words<-regmatches(dir, gregexpr("([[:alpha:]]+)", dir))[[1]]
  words<-words[!(words %in% conv)]
  a<-character(0)
  a<-identical(a, words)
  a
  ## Si hay mas palabras diferentes las eliminamos
  if(a==F) {
    for(i in 1:length(words)){
      dir<-gsub(words[i], " ", dir)
    }
  }
  
  ## Corregimos las vias
  dir<-gsub("alle","Calle",dir)
  dir<-gsub("arrera","Carrera",dir)
  dir
  
  ## Medimos la ubicación de cada palabra de 'dir' en el vector 'Vias'. Es decir que,
  ## revisamos cuales palabras corresponden a tipo vias (Calles, diagonal, carrera, etc) y las ubicamos
  ## dentro del vector 'dir'
  desde<-rep(-1, length(vias))
  for(i in 1:length(vias)){
    desde[i]<-regexpr(vias[i], dir)
  }
  desde<-min(desde[desde>0])
  

  ## El objetivo de saber donde esta ubicada la primera plabra de tipo via y tomar la dirección a partir de ahí
  if(desde>0){
    dir<-substr(dir, desde, nchar(dir))
    ## Identificamos las plabras o letras quedan
    words<-regmatches(dir, gregexpr("([[:alpha:]]+)", dir))[[1]]
  }
  dir
  
  ## Separamos todas la palabras y números
  splitDir<-unlist(strsplit(dir," "))
  splitDir<-unlist(lapply(splitDir, function(x){strsplit(x,"-")}))
  splitDir
  words
  
  ## Separamos todos los números dentro de la dirección a corregir
  nums<-regmatches(dir, gregexpr("[[:digit:]]+", dir))[[1]]
  nums
  
  ## Esta parte separa las direcciones que no pueden ser corregidas. Las marca con NA
  #print(paste("bandera: ",splitDir))
  
  longNums<-ifelse(identical(nums,character(0)), 0, length(nums))
  longNums
  firstVia<-ifelse(is.null(splitDir), " ", splitDir[1])
  firstVia
  if(longNums<3 | !(firstVia %in% vias)){
    return(NA)
  } else { ## Corta nuevamente la dirección. Deja hasta donde se termina el tercer número
    hasta<-regexpr(nums[3], dir) + nchar(nums[3])
    
    hayCord<-rep(NA, length(splitDir))
    for (i in 1:length(splitDir)) {
      hayCord[i]<-ifelse(splitDir[i]%in%coordenadas, i, NA)
    }
    hayCord[is.na(hayCord)]<-0
    max(hayCord)
    
    hasta2<-ifelse(max(hayCord)<1, 0, regexpr(splitDir[max(hayCord)], dir) + nchar(splitDir[max(hayCord)]))
    hasta<-max(hasta,hasta2)
    
    dir<-substr(dir, 1, hasta)
    words<-regmatches(dir, gregexpr("([[:alpha:]]+)", dir))[[1]]
    
    esp<-unlist(
      strsplit(
        as.character(
          unlist(
            gregexpr(" ",dir)
          )
        )
        ," ")
    )}
    esp
    
    ## Deja la dirección sin guiones, sólo espacios
    splitDir<-unlist(strsplit(dir," "))
    splitDir<-unlist(lapply(splitDir, function(x){strsplit(x,"-")}))
    splitDir
    
    Dirspaces<-splitDir[1]
    for(k in 2:length(splitDir)){
      Dirspaces<-paste(Dirspaces, splitDir[k])
    }
    Dirspaces
    
    ## Definimos cuales letras corresponden a subcalles; calle 33B, carrera 13A, etc
    subStreet<-1:2
    #recorrer<-dir
    recorrer<-Dirspaces
    subStreet<-NA
    for (i in 1:2) {
      subcalle<-NA
      subcalle<-substr(recorrer, as.numeric(regexpr(nums[i], recorrer))+nchar(nums[i]) , nchar(dir))
      subcalle
      subcalle<-regmatches(subcalle, gregexpr("([A-Z])+", subcalle))[[1]]
      subcalle
      
      recorrer<-ifelse(identical(subcalle,character(0))==F, gsub(subcalle[i], "", recorrer), "")
      
      subcalle<-ifelse(grepl(".*[Nn].*", subcalle[1])==F, subcalle[1], "")
      subStreet[i]<-subcalle
    }
    subStreet[is.na(subStreet)]<-""
    subStreet
    
    ## Definimos la posición donde debería ir la subcalle
    POSsubStreet<-ifelse(subStreet[1]=="", 0, as.numeric(gregexpr(subStreet[1], Dirspaces)[[1]]))
    
    POSN1<-as.numeric(gregexpr(nums[1], Dirspaces)[[1]][1])
    POSN2<-ifelse( length(gregexpr(nums[1], Dirspaces)[[1]])>1, 
                   ifelse(is.na(as.numeric(gregexpr(nums[2], Dirspaces)[[1]][2])), 
                                as.numeric(gregexpr(nums[2], Dirspaces)[[1]][1]), 
                                as.numeric(gregexpr(nums[2], Dirspaces)[[1]][2])),
                   as.numeric(gregexpr(nums[2], Dirspaces)[[1]][1]))
    POSN3<-as.numeric(gregexpr(nums[3], Dirspaces)[[1]])
    POSsubStreet; POSN1; POSN2; POSN3
    
    POSsubStreet<-ifelse(POSsubStreet<POSN2, 1, 
                         ifelse(POSsubStreet<POSN3, 2 , 3))
    POSsubStreet<-ifelse(is.na(POSsubStreet), 1, POSsubStreet)
    POSsubStreet
    
    ## Definimos la posición de la coordenada (Sur, Norte, Este, Oeste) de la dirección,
    ## si la hay
    POS_NSEO<-c(NA,NA)
    cont<-1
    NSEO1<-character(0)
    NSEO2<-character(0)
    while (identical(NSEO1,character(0))==T & cont<=length(coordenadas)) {
      NSEO1<-regmatches(Dirspaces, gregexpr(coordenadas[cont], Dirspaces))[[1]]
      cont<-cont+1
    }
    NSEO1<-ifelse(identical(NSEO1,character(0))==T, "", NSEO1)
    NSEO1
    POS_NSEO[1]<-ifelse(NSEO1=="", 0, as.numeric(gregexpr(NSEO1, Dirspaces)[[1]]))
    
    
    if(cont<=length(coordenadas)){
      while (identical(NSEO2,character(0))==T & cont<=length(coordenadas)) {
        NSEO2<-regmatches(Dirspaces, gregexpr(coordenadas[cont], Dirspaces))[[1]]
        cont<-cont+1
      }
    }
    NSEO2<-ifelse(identical(NSEO2,character(0))==T, "", NSEO2)
    NSEO2
    #POS_NSEO[2]<-ifelse(NSEO2=="", 0, as.numeric(gregexpr(NSEO2, Dirspaces)[[1]]))
    
    POS_NSEO[1]<-ifelse(POS_NSEO[1]<POSN2, 1, ifelse(POS_NSEO[1]=="", 0, 2))
    POS_NSEO
    
    ## Generamos la direccion final
    newdir<-paste0(substr(dir, 1, as.numeric(esp[1])-1), " ",nums[1], ifelse(POSsubStreet==1, subStreet[1], ""),
                   " ", ifelse(POS_NSEO[1]==1, NSEO1, ""), " ", nums[2], 
                   ifelse(POSsubStreet==1, subStreet[2], subStreet[1]), "-", nums[3], " ",
                   ifelse(POS_NSEO[1]==2, NSEO1,    paste0(" ", NSEO2)       ))
    newdir<-gsub("  "," ",newdir)
    newdir
    
    return(newdir)
   
}

fit.city<-function(city){
  #city<-"BOGOTA DC"
  words<-regmatches(city, gregexpr("([[:alpha:]]+)", city))[[1]]
  words
  a<-character(0)
  a<-identical(a, words)
  a
  if(a==T){
    return(NA)
  } else {
          for(i in 1:length(words)){
            ifelse(nchar(words[i])<2, words[i]<-NA, words[i]<-words[i])
            }
          words<-words[!(is.na(words))]
          words
          NewCity<-paste(words, sep=" ", collapse=" ") 
          }
  NewCity
}

fit.dir.VEC<-function(BD, columna, filas="all", dejaError=F){
  hasta<-ifelse(filas=="all",length(BD[,columna]), filas)
  hasta<-ifelse(hasta>length(BD[,columna]), length(BD[,columna]), hasta)
  BD$NewDir<-0
  cuenta<-0
  message(paste("\n Se corregiran ", hasta, " direcciones"))
  message(" Procesando...")
  
  pb <- txtProgressBar(min = 1, max = hasta, style = 3)
  for(i in 1:hasta){
    BD$NewDir[i]<-fit.dir(BD[i,columna])
    if(dejaError==T & is.na(BD$NewDir[i])){
      cuenta<-cuenta+1
      BD$NewDir[i]<-BD[i,columna]
    }
    
    if(is.na(BD$NewDir[i])){
      cuenta<-cuenta+1
      }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  proporcion<-round((1-cuenta/hasta)*100, 2)
  message(paste("\n\t Se ha corregido el "), proporcion, "% de las direcciones.")
  if(dejaError==T){
    message(paste("\t Hay ", cuenta, " direcciones que no pueden ser corregidas, se dejaron como estaban"))
  } else {
    message(paste("\t Hay ", cuenta, " direcciones que no pueden ser corregidas, se dejaron vacias (NA)"))
  }
  datos<<-BD
  message("\n ok!")
}


















