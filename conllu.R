library(assertthat)
source("fsm.R")

casepath <- "./SC2018"
bookpath <- "./law_books"


#---- LOAD AND VALIDATE VARIABLES -----
assert_that(grepl("nlp",getwd()),msg = "You are not in the correct working directory")
suppressWarnings(rm(filenames,scfile,fannot,saveddtm,dtm_flat_new,dtm_old,dtm_flat))
is.dir(casepath)
scfile <- list.files(pattern = "pdf|PDF",path =casepath,full.names = T)
filenames <- str_sub(scfile,3)
key_gsupload <- "1H9ApyBnTV7rHnNaQ6IWzVmQPcaVNQXA5uk1ZmgSOzMI"

#-----MAIN PROGRAM : Take user options----
choice <- readline("(N)Nothing (L)oad from local file /(R)ecompute /(A)nnotate: ")
suppressWarnings (switch(choice,
                         N = message("Moving on.."),
                         L = Readloc(),
                         R = rm(dtm_flat,dtmsc2, cross_ref,pcite,psctext,cit_flat), 
                         A = rm(dtmsc,dtmsc2,dtm_flat,cross_ref,pcite,psctext,cit_flat),
                         message("Unrecognised option, assuming 'L'oad  ")
))
if(choice=="A") {
  if(NROW(scfile)>0) message(paste("Supreme Court files found and loaded... total files:",NROW(scfile),"in:",casepath))
  if(exists("dtm_flat")) {
    pretagged <- dtm_flat$doc_id %>% unique
    fannot <- setdiff(filenames,pretagged)
    cat("\nFollowing pretagged files present:\n")
    print(dtm_flat[,.N,by=doc_id])
    cat(paste("Existing training model:",attr(dtm_flat,"tr",F)))
    message(paste("\nLast updated at:",attr(dtm_flat,"atime",F)))
    cat("----\n")
  } else fannot <- filenames  
  
  if(exists("fannot")  && length(fannot)==0) {
    message(paste("All files are already annotated in",casepath,"\nDo you want to annotate them again?")) 
    answer <-       readline("If you say N we will use the existing annotations:(Y/N) ")
  } else {
    cat("\nFiles that are not yet annotated:")
    print(fannot)
  }
  if((exists("answer") && grepl("Y",answer,ig=T)) ||( length(fannot)>0 )  ) {
    modelfiles <- list.files(pattern = ".udpipe")
    message("Models available for annotation:")
    print(modelfiles)
    sn <- readline("Which model no. to load?:") %>% as.integer
    cat("Loading..")
    model <-   udpipe_load_model(file = modelfiles[sn])
    cat(modelfiles[sn])
    message("..LOADED")
    cite<- list()
    dtm_sc <- list()
    cat("\nAnnotation and tagging started...")
    if(length(fannot)==0) fannot <- filenames
    dtmsc <- runsc_tagging(files=fannot,m=model) 
    # transform the DTM by adding doc_id to each row (doc_id is just the file name with prefix of subdir)
    dtmsc2 <- map2(dtmsc, fannot, ~ .x[, "doc_id" := as.character(.y),]) # learnt this from Stackoverlfow - terrific
    if(exists("dtm_flat")) dtm_old <- dtm_flat else dtm_old <- NULL
    dtm_flat_new <- do.call(rbind,dtmsc2) # combine all DTMs into a flat DT
    dtm_flat <- rbind(dtm_old,dtm_flat_new)
    dtm_flat$uid <- unique_identifier(x = dtm_flat,fields = c("doc_id", "sentence_id","token_id")) # add a unique id to the DTM
    attr(dtm_flat,"training_model")<- model$file
    attr(dtm_flat,"atime")<- now()
  }
}

if(choice %in% c("R","A")){
  cat("Will not be running the 'citings()' function on the merged DTM (dtm_flat)..as there is a bug\n")
  #citings(dtm = dtm_flat,consec = 3) -> cit_flat #consecutive sentences used as extracts
  #cat("..DONE.\n")
  #if(is.na(cit_flat$referred_cases)[1]) message("No citations found in any file.. aborting citation processing") else {
  #attr(cit_flat,"training_model") <- attr(dtm_flat,"tr",F)
  
  # comment : remove numbers and puncts for simplifying regex search 
  #cit_flat$referred_cases %>% str_replace_all("[\\s[:punct:][:digit:]]+","") %>% tolower() -> tmp
  # data.table(case=cit_flat$referred_cases,packed = tmp) ->pcite #this is packed citations
  # map_chr(scfile,packfile) -> tmp
  # data.table(filename=scfile,filecontent=tmp) -> psctext
  # 
  # cat("Generating cross_ref matrix...")
  # assert_that(nrow(pcite)>0,nrow(psctext)>0)
  # 
  # 
  # map2_int(.x = expand.grid(1:nrow(psctext),1:nrow(pcite))$Var1, 
  #          .y = expand.grid(1:nrow(psctext),1:nrow(pcite))$Var2,
  #          .f = ~ str_count(string = psctext[filename==scfile[.x],filecontent,with=T],
  #                           pattern = pcite[case==cit_flat$referred_cases[.y],packed])) %>% 
  #   matrix(ncol = nrow(pcite)) -> cross_ref
  # cat("generated\n")
  # colnames(cross_ref)<- substr(cit_flat$referred_cases,1,16)
  # rownames(cross_ref) <- scfile
  # 
  # map2_int(.x = expand.grid(1:nrow(psctext),1:nrow(pcite))$Var1, 
  #          .y = expand.grid(1:nrow(psctext),1:nrow(pcite))$Var2,
  #          .f = ~ str_count(string = psctext[filename==scfile[.x],filecontent,with=T],
  #                           pattern = pcite[case==cit_flat$referred_cases[.y],packed])) %>% 
  #   matrix(ncol = nrow(pcite)) -> cross_ref
  # }
} 



#---- Save the variables once recomputation done -----
if(choice %in% c("A", "R")){
  Saveloc()
  last_saved <- now()
  cat("\nALL READY\n")
}

if(exists("dtm_flat")) 
  cat(paste("\n'dtm_flat' was last modified at:",attr(dtm_flat,"atime"))) else 
  { message("\nNo dtm_flat variable exists for:",appendLF = F) 
    cat(casepath)
    }

