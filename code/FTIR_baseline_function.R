
##baseline function for FTIR with csv files

##for single FTIR spectrum

##after baseline, the spectrum will be saved as txt file in another folder


ftir_baseline <- function(spc, method, range, noise = 0.001, order = 3,save = TRUE){
  # create backup spectrum
  backup <- spc
  
  # select the range of spectrum 
  if(missing(range)){
    spc <- spc
  }else{
    spc <- spc[,,range[1]:range[2]]
  }
  
  
  #m <- regexpr("sa[0-9]{5}_[0-9]{2}",ftir.files[i])
  sa <- strsplit(ftir.files[i], split = "-")[[1]][1]
  
  # Get exposure step
  
  es <- as.numeric(gsub(".*?([0.0-9.9]+).*", "\\1", str_split(ftir.files[i],"-")[[1]][2]))
  
  # Get measurement number
  
  mn <- as.numeric(gsub(".*?([0.0-9.9]+).*", "\\1", str_split(ftir.files[i],"-")[[1]][4]))
  
  
  # plot raw spectrum
  
  plot(spc)
  
  
  # baseline spectrum
  
  badBaseline <- TRUE
  while (badBaseline){
    if (method == "polynomial") {
      # baseline with "polynomial" method, the defaul order is 3
      
      bl <- spc.fit.poly.below(spc, poly.order = order, noise = noise)
      
      # substract baseline from raw data
      
      spc.bl <- spc - bl
      
    } else if (method == "rubberband") {
      # extract baseline with hyperSpec function
      
      bl <- spc.rubberband(spc, noise = noise)
      
      # substract baseline from raw data
      
      spc.bl <- spc - bl
    }
    
  # plot baselined spectrum
  plot(spc.bl)
  
  # are you satisfied after seeing the plots?
  satisfied <- readline(prompt = "Are you satisfied with this result? (y/n) > ")
  if (satisfied == "y"){
    # save the file 
    if (save == TRUE){
      # obtained filepath + name of the raw spc file
      
      step <- c(es, es,mn)
      step_name <- c("es0","ms0","mn0")
      fullname <- paste(sa, str_c(str_c(step_name, step, sep = ""), collapse = "-"), sep = "-")
      
      # the folder where the file is going to be saved has to exist first
      
      folder <- paste(dir, "baseline/", sep = "")
      
      # saving the spc file into a txt file
      exname <- str_c(folder, fullname, ".txt")
      
      write.txt.long(spc.bl, exname)
      cat("File", exname, "saved. \n")
      cat("============================================================ \n")
    }
    # change flag to FALSE, end loop
    badBaseline <- FALSE
    
  } else if (satisfied == "n"){
    
    while (TRUE){
      spc <- backup
      
      # decide baseline range
      range_full <-  readline(prompt = "baseline for full spectrum range (T or F):")
      
      
      if(range_full){
        spc <- spc
      }else{
        range_change <- readline(prompt = "change baseline range (T or F):")
        
        if(range_change){
          # re-decide partial baseline range
          
          range_l <- readline(prompt = "select basesline range: left limit: ")
          range_r <- readline(prompt = "select basesline range: right limit: ")
          
          spc <- spc[,,range_l:range_r]
        }else{
          # keep original range
          spc <- spc[,,range[1]:range[2]]
        }
       
      }
      
      # choose a new method
      met <- readline(prompt = "Method (p for polynomial or r for rubberband): ")
      if (met == "p"){
        method <- "polynomial"
      } else if (met == "r"){
        method <- "rubberband"
      } else {
        method <- met
      }
      
      if (method == "polynomial"){
        order <- as.numeric(readline(prompt = "Order (0 to 3): "))
        break
      } else if (method == "rubberband"){
        break
      } else if (method == "stop"){
        stop("User interupted")
      } else {
        cat("Please choose a method, enter p or r. \n")
        next
      }
    }
    noise <- as.numeric(readline(prompt = "Noise level: "))
    # does not prompt to ask if proceed to the next one
  }
}
}



