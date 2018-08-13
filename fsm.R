#----- FUNCTIONS for legal text analysis coded by Papa -----
library(magrittr)
library(stringr)
library(lubridate)
library(readtext)
library(pdftools)
library(udpipe)
library(textclean)
library(textrank)
library(tm)
library(purrr)
library(splitstackshape)
library(stringdist)
library(textshape)
library(assertthat)
library(htmlTable)
library(htmltools)
library(wrapr)
library(textreuse)
library(rdrop2)
library(data.table)
library(googlesheets)

Readloc <- function(vars = qc(dtm_flat,cit_flat,cross_ref,dtmsc)) {
  varfiles <- file.path(casepath,vars)
  for(i in seq_along(along.with = varfiles)){
    cat(paste("reading from disk....",varfiles[i]))
    if(file.exists(varfiles[i])) { 
      assign(vars[i],readRDS(varfiles[i]),envir = .GlobalEnv) # read the variable into the global env
      cat("..read\n")
    } else message("..file does not exist")
  }
  message("Loading udpipe models")
  model_englishud <<- udpipe_load_model(file = list.files(pattern = "english-ud.+2\\.1"))
  model_englishpartut <<- udpipe_load_model(file = list.files(pattern = "english-partut"))
  cat("Done.")
}
Saveloc <- function(vars = qc(dtm_flat,cit_flat,cross_ref,dtmsc), locpath=casepath) {
  for(i in seq_along(along.with = vars)){
    cat(paste("saving case variable....",vars[i]))
    if(exists(vars[i])) { 
      saveRDS(get(vars[i]),file.path(locpath,vars[i]))
      cat("..saved\n")  } else
        message("..var does not exist")
  }
}
Savepub <- function(vars = qc(mpj.term,aifcited,colos_mpj)) {
  for(i in seq_along(along.with = vars)){
    cat(paste("saving public variable....",vars[i]))
    if(exists(vars[i])) { 
      saveRDS(get(vars[i]),vars[i])
      cat(" ...saved\n")  } else
        message("...var does not exist")
  }
}

Readbooks <- function(books=qc(mpj.term,colos_mpj)) {
  vars <- file.path(bookpath,books)
  for(i in seq_along(along.with = vars)){
    cat(paste("reading specific case variables....",vars[i]))
    if(file.exists(vars[i])) vars[i] <- {
      readRDS(vars[i])
      cat("..read successfully\n") 
    } else message("..file does not exist")
  }
}

runsc_tagging <- function(files=scfile,mod=m){
  for(i in seq_along(files) ){
    cat(paste0("\nStarted file:",i,": ("))
    cat(paste(files[i],")"))
    dtm_sc[[i]] <- tag(file = files[i],model = mod)
  }
  message("Tagging of all files completed")
  dtm_sc # return a list of DTMs - each element corresponds to 1 file. And this is later transforned to a flat DTM.
}

cleanraw <- function(text){
  layr1 <- text %>%  str_replace_all(pattern = "[Vv]s*\\.",replacement = "vs") %>%
    str_replace_all(pattern = "(M[RrsS]s*)\\.",replacement = "\\1") %>% 
    str_replace_all(pattern = "[Nn][oO]\\.",replacement = "number ") %>% 
    str_replace_all(pattern = "D[rR]\\.",replacement = "Dr") %>% 
    str_replace_all(pattern = "[Ee]x\\.",replacement = "ex") %>% 
    str_replace_all(pattern = "eg\\.",replacement = "eg") %>% 
    str_replace_all(pattern = "etc\\.",replacement = "etc") %>% 
    str_split("\n") %>% 
    unlist %>% 
    str_replace_all(pattern = "^([1-9][0-9]{0,1})(\\. )",replacement = "\\1) ") %>%  # replace bullet number punctuation period by closing bracket
    str_replace(pattern = "([A-Za-z]{2,})\\d{1,2}\\b",replacement = "\\1") %>%  # remove footnote numbering upto 2 digits
    str_replace_all("([0-9]{2})\\.([0-9]{2})\\.([12][0-9]{2})","\\1-\\2-\\3")
  # remove rows less than 20 characters AND in vicinty of 5 lines of Signature string
  nsig <- grep(pattern = "Signature Not Verified", layr1,ignore.case = T)
  if(length(nsig) ==1) {
    rangesig <- nsig:(nsig+6)
    r1 <- intersect(which(nchar(layr1)<20) , rangesig)
    layr1[!( (1:length(layr1)) %in% r1    ) ] %>% 
      str_remove(regex("Signature.{8,15}")) %>% 
      str_remove(regex("^Digitally signed.{2,4}")) %>%
      str_trim %>% 
      drop_element(pattern = "^$")
  } else layr1
}

clean_dtm <- function(dtm){
  dtm[grepl("\\bors|\\banr",token,ignore.case = T),upos:="NOUN"]
}


tag <- function(file = NULL, model = model_englishud){
  cat("\nAutodetecting & removing headers and footers...")
  alltext <- suppressWarnings( autodet(file,ret_full = T)  )
  cat("Cleaning Vs., Mr., Mrs., Dr., No.,..")
  onetext <- alltext %>% cleanraw %>% paste(collapse = " ") 
  cat("DONE.\n")
  
  udpipe_annotate(object = model,x = onetext) %>% as.data.table(keep.rownames = F) -> x1 # main line in function.
  suppressWarnings(x1$token_id %<>% as.integer() )
  suppressWarnings(x1$sentence_id %<>% as.integer() )
  #x1$uid <- unique_identifier(x = x1,fields = c("doc_id", "sentence_id","token_id"))
  #x1$suid <- unique_identifier(x = x1,fields = c("doc_id", "sentence_id"))
  dtm_clean <- clean_dtm(dtm=x1)
  attributes(dtm_clean)$training_model <- model$file
  attributes(dtm_clean)$atime <- now()
  dtm_clean
}

clean_string <- function(string){
  mp <- tolower(string)
  # Remove everything that is not a number or letter
  # # Shrink down to just one white space
  temp <-  str_replace_all(string,"\\W+", " ")
  temp <- stringr::str_split(temp, " ")[[1]] %>% drop_element_fixed("")
  return(temp)
}

colo <- function(casedtm=NULL,lawbookdtm=NULL,ngr=2,size=3,minimum_occ=4){
  colo_terms_lawbook <- keywords_collocation(x = lawbookdtm[upos %in% c("NOUN","ADJECTIVE","VERB") & nchar(lemma)>size,
                                                            .(sentence_id,lemma=tolower(lemma))], term = "lemma", group = c( "sentence_id"), ngram_max = ngr, n_min = minimum_occ)
  
  casedtm_filt <- casedtm[upos %in% c("NOUN","ADJECTIVE","VERB") & nchar(lemma)>size,.(doc_id,sentence_id,sentence,lemma=tolower(lemma),upos)][,nextlemma:=txt_nextgram(lemma)]
  pmi_scores <- casedtm_filt[colo_terms_lawbook,on=.(nextlemma=keyword),nomatch=0][,.(doc_id,sentence_id,sentence,keyword=nextlemma,pmi)][order(doc_id,sentence_id)] %>% unique()
  if(nrow(pmi_scores)==0) { message("NO MATCH"); return("zero rows") } else return(unique(pmi_scores))
  # x2 <- casedtm_filt[colo_terms_lawbook,on=.(nextlemma=keyword),nomatch=0] %>% unique
  # pmi_case_terms <-  x2[colo_terms_lawbook,on=.(nextlemma=keyword),nomatch=0][,.(doc_id,sentence_id,nextlemma,pmi)] %>% unique()
  # pmi_case_terms[x2,on=.(nextlemma,sentence_id)][order(doc_id,sentence_id)]
}

extract_concl <- function(dtm=dtm_flat,file="kconclusion.txt",html=F){
  keyw <- readLines(file)
  str_replace_all(keyw,"\"","") -> keyw
  drop_element(keyw,pattern = "^#") -> keyw
  pat<- drop_empty_row(matrix(keyw,ncol = 1)) %>% str_trim %>% paste(collapse = "|")
  dtm[str_detect(sentence,pattern = regex(pat,ignore_case = T)),.(doc_id,matching_sentence=sentence)] %>% unique -> output
  if(html) htmlTable(x = output,align = "l") %>% htmltools::html_print() else
    output
  
}

extract_numbers <- function(dtm=NULL, file="knumber.txt",cross_table=T, full_sentence=F,art=T,ent=F,sect=F,display_width=getOption("width")-50,clip_text="center"){
  keyw <- readLines(file)
  keyw2<- drop_empty_row(matrix(keyw,ncol = 1)) %>% str_trim
  pat_vector <- keyw2 # regex to extract string inside bracket followed by digits
  # function to extract sentences matching with one regex pattern; 
  extract_sent <- function(pat=NULL) dtm[str_detect(sentence,pat),.(sentence_id, sentence),by=.(var=tolower(str_extract(sentence,pat)))]
  
  map(pat_vector,extract_sent) -> x1
  do.call(rbind,x1) -> x2
  cSplit(x2,splitCols = "var",sep = " ") %>% unique -> x3
  dcast(x3,sentence_id + sentence ~ var_1,value.var = "var_2") -> x4
  x4$sentence %<>% str_trunc(width = display_width,side=clip_text)
  #if(full_sentence) column_names <- c("sentence_id","sentence") else column_names <- c("sentence_id")
  if(!cross_table) {
    if(is.character(art)) output <- x4[grepl(pattern = art,x = article,ig=T),sentence] else if(art) output <- x4[!is.na(article),.(sent=.(sentence_id)),by=article][order(article)]
    if(is.character(sect)) output <-  x4[grepl(pattern = sect,x = section,ig=T),sentence] else if(sect) output <- x4[!is.na(section),.(sent=.(sentence_id)),by=section][order(section)]
    if(is.character(ent)) output <-  x4[grepl(pattern = ent,x = entry,ig=T),sentence] else if(ent) output <- x4[!is.na(entry),.(sent=.(sentence_id)),by=entry][order(entry)]
  } else  if(full_sentence) output<- x4 else output <- x4[,-c(2)]
  output
}

# ignore this function. Use the next one.
extr_acts <- function(dtm=NULL,param=0.5){
  all_acts <- readLines("all_acts.txt")
  pat <- c("Act$","[[:punct:]]+","[[:digit:]]+","\\[\\]") # cleanup of act names
  act_strings <- mgsub(x=all_acts,pattern = pat,replacement="",fixed=F) %>% str_trim %>% unique
  #dtm$sentence_id %<>% as.integer();  dtm$token_id %<>% as.integer() 
  dtm[token=="Act",.(doc_id,sentence_id)] -> dt1
  dtm[dt1,on=.(doc_id,sentence_id)] -> dtm_relevant
  dtm_relevant$uid_rel <- unique_identifier(x = dtm_relevant,fields = c("doc_id","sentence_id","token_id"))
  dtm_relevant[,.(doc_id,sentence_id,token_id,token,upos,uid_rel)]-> dtm_r
  
  keywords_phrases(x = dtm_r[,upos],
                   pattern="((DET)|(ADP)|(CCONJ)){1}(NOUN|ADP|PROPN){1,6}",
                   term = dtm_r$token,is_regex = T) %>% as.data.table() %>% 
    .[grepl("Act$",keyword)] -> keyw_acts
  assert_that(nrow(keyw_acts)>0)
  dtm_r[keyw_acts,.(uid_rel,doc_id,sentence_id,keyword),on=.(uid_rel>=start, uid_rel<=end)] %>% unique -> keyw_in_sent
  keyw_in_sent[,clean_keys:={ str_replace(keyword,"\\w+(.*)Act$","\\1") %>% strip(lower.case = F)}]
  keyw_in_sent[clean_keys!="",.(doc_id,sentence_id,clean_keys)] -> keyw_in_sent
  keyw_in_sent[,.(sent=paste(sentence_id,collapse = ";")),by=.(doc_id,clean_keys)] -> keyw_in_sent
  keyw_acts[,clean_keys:={ str_replace(keyword,"\\w+(.*)Act$","\\1") %>% strip(lower.case = F)}]
  keyw_acts[clean_keys!=""]-> keyw_acts
  keyw_acts[keyw_in_sent,.(clean_keys,doc_id,sent),on=.(clean_keys)][clean_keys != "Act"]  %>% unique() -> keyw_acts
  expand.grid(act_strings, keyw_acts$clean_keys,stringsAsFactors = F) %>% as.data.table() -> grid1
  grid1[,dist:=stringdist(gsub(" ","",Var1),gsub(" ","",Var2),method = "lcs")]
  grid1[,distn:=dist/nchar(Var2)]
  grid1[,is_pres:=str_detect(gsub(" ","",Var1),gsub(" ","",Var2))]
  grid1[,weight:=ifelse(is_pres,1,0.9)*(1-distn)]
  grid1[,{.SD[order(-weight)][1]},by=Var2][,.(Act_name=Var1,Ref_text=Var2,distn,is_pres,weight)] -> score_table
  score_table[keyw_acts,on=.(Ref_text=clean_keys)][order(-weight)][weight>param,.(doc_id,Ref_Text=paste(Ref_text,"Act"),Closest_Act=paste(Act_name,"Act"),distn,is_pres,weight,sentence_ids=sent)]
}

#sent <- function(dtm,n) dtm[sentence_id %in% n,unique(sentence)]

extr_capit_act <- function(dtm=null,verbose=F){
  all_acts <- readLines("all_acts.txt")
  nextstep <- function(prompt,dt,var="default"){
    message(prompt)
    if( (x <- readline(prompt =  sprintf("Enter q to quit, p to print the %s or just Enter for next step: ",var)) ) =="p") print(dt)
    stopifnot(x!="q")
  }
  
  # remove Act and remove numbers
  pat <- c("[[:punct:]]+","[[:digit:]]+","\\[\\]")
  act_strings <- mgsub_regex(x=all_acts,pattern = pat,replacement="",trim=T) %>% unique
  dtm$token_id %<>% as.integer()
  dtm[token_id<3 & str_detect(token,"Act"),first_few:=T]
  dtm[upos %in% c("PROPN","NOUN","ADJ","VERB","ADP","DET")][
    ,w1:=txt_previous(token,1),by=sentence_id][
      ,w2:=txt_previous(token,2),by=sentence_id][
        ,w3:=txt_previous(token,3),by=sentence_id][
          ,w4:=txt_previous(token,4),by=sentence_id][
            ,w5:=txt_previous(token,5),by=sentence_id][
              ,w6:=txt_previous(token,6),by=sentence_id][
                ,u1:=txt_previous(upos,1),by=sentence_id][
                  ,u2:=txt_previous(upos,2),by=sentence_id][
                    ,u3:=txt_previous(upos,3),by=sentence_id][
                      ,u4:=txt_previous(upos,4),by=sentence_id][
                        ,u5:=txt_previous(upos,5),by=sentence_id][
                          ,u6:=txt_previous(upos,6),by=sentence_id]  -> dtm_enr
  
  dtm_enr[(token=="Act") & grepl("^[A-Z].*",w1) & !(u2 %in%  c("ADP","DET")) & is.na(first_few),.(cand_string=paste(w2,w1)),by=sentence_id] -> cand_dt2
  cand_dt2$cand_string %>% unique -> candidate_words2
  if(verbose) nextstep("Filtered out all candidate act strings basis string 'Act' preceded by a capitalized word:",candidate_words2,"candidate_words2")
  sent_l2 <- dtm[cand_dt2,unique(sentence),on=.(sentence_id)]
  if(verbose) nextstep("The relevant case sentenes containing the above act words:",sent_l2,"sent_l2")
  
  dtm_enr[(token=="Act") & grepl("^[A-Z].*",w1) & (u2 == "ADP") & is.na(first_few),.(cand_string=paste(w3,w2,w1)),by=sentence_id] -> cand_dt3
  dtm_enr[(token=="Act") & grepl("^[A-Z].*",w1) & (u2 == "DET"),.(cand_string=w1),by=sentence_id] -> cand_dt1
  
  cand_dt1$cand_string %>% unique -> candidate_words1
  cand_dt3$cand_string %>% unique -> candidate_words3
  if(verbose) nextstep("Another set of candidate act strings are where a preposition is just before the last word, These are rewinded to include 3 words.",candidate_words3,"candidate_words3")
  
  sent_l3 <- dtm[cand_dt3,unique(sentence),on=.(sentence_id)]
  if(verbose) nextstep("Relevant sentences containing above",sent_l3,"sent_l3")
  
  cand_all <- c(candidate_words1,candidate_words2,candidate_words3)
  if (verbose) nextstep("Possible candidate strings with 1,2,3 words, ending with 'Act'",cand_all,"all_candidates")
  
  cand_dt_all <- rbind(cand_dt1,cand_dt2,cand_dt3)
  expand.grid(act_strings,cand_all,stringsAsFactors = F) %>% as.data.table -> grid3
  grid3[,is_pres := str_detect(Var1,Var2)]
  names(grid3)[1] <- "Actual_Act_name"
  names(grid3)[2] <- "substr_case"
  grid3[is_pres==T,substr_case] -> possible_acts
  grid3[,dist1:=stringdist(gsub(" Act","",Actual_Act_name),substr_case)]
  grid3[,distn:=dist1/nchar(substr_case)]
  
  dt_scores <- grid3[,weight:=ifelse(is_pres,1,0.5)*(1-distn)][distn<=1][
    order(-weight)][1:100][
      cand_dt_all[,.(sentences=paste(sentence_id,collapse = ";")),
                  by=cand_string],
      on=.(substr_case=cand_string),nomatch=0]
  
  output<- dt_scores[,{.SD[order(-weight)][1]},by=substr_case][order(-weight)]
  return(output)
}

extract_acts <- function(dtm=NULL,step=1,param=0.1){
  #load all acts scraped from wikipedia using
  #read_html("https://en.wikipedia.org/wiki/List_of_Acts_of_the_Parliament_of_India") %>% html_nodes(css = 'td , .text') %>% html_text() -> wiki_raw
  #and then applying command line cleanup of the raw data to refined act names
  all_acts <- readLines("all_acts.txt")
  
  # remove Act and remove numbers
  pat <- c("[, ]*Act","[[:punct:]]+","[[:digit:]]+","\\[\\]")
  act_strings <- mgsub(x=all_acts,pattern = pat,replacement="",fixed=F) %>% str_trim %>% unique
  
  # determine co occuring words (upto 8 ngrams) from the DTM and pick the ones ending with 'Act'
  x1 <- keywords_rake(x = dtm,term = "token",group = "sentence_id",ngram_max = 8,n_min = 1,relevant = dtm$upos %in% c("NOUN","PROPN","ADJ","ADP")) %>% 
    as.data.table %>% .[grepl("Act$",keyword),str_replace(pattern="(.+) Act",string = keyword,replacement = "\\1")] 
  assert_that(NROW(x1)>0)
  #remove subsumed strings of ngrams. In other words select the longer of two ngrams if one is completely included in the other. Finally remove the single word 'Act'.
  tmp<- expand.grid(x1,x1,stringsAsFactors = F) %>% as.data.table()
  tmp[str_detect(Var1,Var2) & Var1 !=Var2,Var2] -> subsumed_strings
  ngr <- setdiff(tmp$Var1,subsumed_strings)
  ngr[ngr!="Act"] -> ngr
  
  #create a data.table with all possible combinations (grid type) between co occurring keywords and act names, as two columns of a data table.
  expand.grid(ngr,act_strings,stringsAsFactors = F) %>% as.data.table() -> all_comb
  
  # compute total char and total words as new columns and call them property data tables  We can add more columns here to refine search
  act_prop<- data.table(str=act_strings,len=nchar(act_strings),wc=sapply(gregexpr("\\w+", act_strings), length))
  ngr_prop <- data.table(str=ngr,len=nchar(ngr),wc=sapply(gregexpr("\\w+", ngr), length))
  
  # Join the master data tables with property tables and compute string length variance and word count variance between all combinations
  added_prop <- all_comb[act_prop,on=.(Var2=str),nomatch=0][ngr_prop,on=.(Var1=str)]
  added_prop[,varchar:=abs(len-i.len)/len][,varwords:=abs(i.wc-wc)/i.wc] -> all_variance
  
  # select only the rows that donot have huge length or word count variance
  all_variance[varchar<3 & varwords<3] -> narrow_variance
  
  # calculate string distance between each combination
  narrow_variance[,stringd:= stringdist(Var1,Var2,method = "lcs")][,stringd_n:=stringd/i.len] -> narv_stringd
  
  # filter out only those rows with less string distance (param is passed as as func parameter)
  conf_matrix <- narv_stringd[stringd_n<param,.(act_doc=Var1,act_wiki=Var2,conf=100*(1-stringd_n))][,{
    .SD[order(-conf)][1][,.(mm=act_wiki,confid=conf)]
  }, by=act_doc][
    ,.(act_doc=paste(act_doc,"Act"),max_match=paste(mm,"Act"),confidence=confid)][
      order(-confidence)]
  dtm[,.(sentence_id,sentence)] %>% unique -> dtmu
  
  grid_sent <- expand.grid(dtmu$sentence,conf_matrix$act_doc,stringsAsFactors = F) %>% as.data.table()
  grid_sent[,match:=str_detect(Var1,Var2)]
  final_extraction <- grid_sent[match==T] %>% unique()
  dtmu[final_extraction,on=.(sentence=Var1)][order(sentence_id)][,.(sentences=paste(sentence_id,collapse = ";")),by=.(act_name=Var2)]
}

shorten <- function(upos1){
  upos <- c("ADJ" ,  "ADP"  , "ADV" ,  "AUX"  , "CCONJ" ,"DET"  , "INTJ" , "NOUN",  "NUM" ,  "PART" , "PRON"  ,"PROPN" ,"PUNCT" ,"SCONJ", "SYM" ,  "VERB" , "X", "SEP" )    
  repl <- c("A","P","M","X","C","D","X","N","X","X","X","N","B","C","X","V","X","S")
  lookup <- data.table(u1=upos,u2=repl)
  lookup[data.table(upos1),on=.(u1=upos1),u2]
}

citings <- function(dtm=NULL,consec=3,neigh_count=15){
  unique_identifier(dtm,fields = c("doc_id",  "sentence_id","token_id")) -> dtm$uid
  dtm[grepl("[Vv]ersus|VERSUS|^[Vv]\\.|^[vV][sS]\\.|^[vV]s",token),uid] -> tok_vs
  #assert_that(length(tok_vs)>0,msg = "There were no citations found in the input dtm")
  dtm[grepl("[A-Za-z]+\\d+",token),token:=str_replace(string = token,pattern = "([A-Za-z]+)\\d+",replacement = "\\1")] #remove footnote numbers
  dtm[,-c("sentence","misc")]-> dtm_lean # remove nonessential columns to make it leaner
  if(length(tok_vs)==0) {
    cat("\nNo citations found in current file.")
    return( list(referred_cases=NA,extracts=NA,tagging=NA) )
  }
  dtm_lean[uid %in% tok_vs,upos2:="SEP"];
  dtm_lean[is.na(upos2),upos2:=upos] #except SEP copy back all other upos tags to upos2
  dtm_lean[,upos3:=shorten(upos2)]
  dtm_lean[upos3=="B" & token==".",upos3:="H"] #capture a 'Halt' upos for period. B is for Bracket or Punct upos.
  dtm_lean[token %in% c("AIR"),upos3:="R"] #capture a 'Reference' upos3 for AIR...
  dtm_lean[grepl("petitioner|appellant|respondent|defendant",token,i=T) & sentence_id<6,upos3:="T"]
  #dtm_lean[sentence_id<6 & token %in% c("S","s"),upos3:="X"]
  dtm_leaner <- dtm_lean[upos3 %in% c("N","P","C","D","S","V","A","M","H","T","R","X")]
  regex_citation <- "A*(N|D)+C*N+(P|C|D)*A*NSA*(N|D)+C*N+(P|C|D)*A*N"   
  regex_title <- "N.+X*TX*SN+"   
  t3<- proc.time()
  keywords_phrases(dtm_leaner[,upos3],pattern = regex_citation,
                   term = dtm_leaner$token,is_regex = T,ngram_max = 8) -> phrase_cite
  
  t4<- proc.time()
  #keywords_phrases(dtm_leaner[,upos3],pattern = regex_title,
  #                term = dtm_leaner$token,is_regex = T,ngram_max = 16) -> phrase_title
  keyw_citings <- phrase_cite[,"keyword"]
  #keyw_title <- phrase_title[,"keyword"]
  grid <- expand.grid(keyw_citings,keyw_citings,stringsAsFactors = F) %>% as.data.table
  grid[str_detect(Var1,Var2) & nchar(Var2)<nchar(Var1) , Var2] %>% unique -> discard_list
  cases <- grid[!Var1 %in% discard_list,unique(Var1)]
  tok_vs+neigh_count -> tright; tok_vs - neigh_count -> tleft
  cbind(data.table(x=tleft, y=tright)) ->ranges
  nearby_tokens<- dtm_lean[ranges,on=.(uid > x , uid < y),.(doc_id,uid=x.uid,sentence_id,token,upos,upos2,upos3)] #non equi join used to extract nearby tokens
  lextr<- list(referred_cases=cases,extracts=dtm[uid %in% tok_vs,sentence],tagging=nearby_tokens)
  dtm[sentence %in% lextr$extracts,unique(sentence_id)] -> sid #sentence ids containing the vs token.
  dtm[uid %in% tok_vs,.(sentence_id,doc_id)][sentence_id>6] -> sid2 #data.table containing sentence_id and doc_id for all citation text
  if(nrow(sid2)==0) {message(" No Citation detected after 6 sentences.");  return( list(referred_cases=NA,extracts=NA,tagging=NA) ) } else {
    sid2[,smax:=sentence_id+consec-1] # add a column with upper limit of sentence_id
    names(sid2)[1]<- "smin" # rename sentence_id in line 
    sid2$schunk <- unique_identifier(x = sid2,fields = c("doc_id","smin")) #sentence_chunk or schunk is an id that is common for one chunk of sentences
    dtm[sid2,on=.(doc_id,sentence_id>=smin,sentence_id<=smax),.(doc_id,schunk,sentence)] %>% unique -> tmp
    tmp[,sents:=paste(sentence,collapse = " "),
        by=schunk][,
                   .(doc_id,sents=str_replace_all(string = sents,pattern = "\\s+",replacement = " "))] %>% 
      unique ->mult_sent
    lextr$extracts <- mult_sent      
    lextr$imp <- extract_summ(sentences = lextr$extracts$sents) #important sentences from the sentences inside each chunk after citation.
    
    message("\nkeywords_phrases() system running time:")
    print(t4-t3)
    
    lextr
  } 
}

bringout <- function(dt=NULL,str=NULL,n=9,original=T,packed=F){
  dt[grepl(str,token),uid] %>% unique -> uids
  uids+n -> uright
  uids-n -> uleft
  cbind(data.table(col1=uleft, col2=uright)) ->ranges
  dt[ranges,on=.(uid > col1 , uid < col2),-c(grep("misc|uid|sentence$",names(dt),val=T)),with=F]
}

extract_summ <- function(sentences=NULL,file="ksummary.txt"){
  keyw <- readLines(file)
  pat<- drop_empty_row(matrix(keyw,ncol = 1)) %>% str_trim %>%  tolower %>%  paste(collapse = "|")
  grep(x = sentences,pattern = pat,ignore.case = T, value= T) %>% unique
}

extract_summ1 <- function(sentences=NULL,file=NULL){
  keyw2 <- readLines(file)
  pat2<- drop_empty_row(matrix(keyw2,ncol = 1)) %>% str_trim %>%  tolower %>%  paste(collapse = "|")
  grepl(x = sentences,pattern = pat2,ignore.case = T)
}

packfile <- function(file=NULL){
  file %>% readtext %>% paste %>% str_replace_all("[\\s[:punct:][:digit:]]+","") %>% tolower()
}

packstr <- function(str=NULL, state="all"){
  patx <- switch(state,
                 "all" = "[\\s[:punct:][:digit:]]+",
                 "leavedigit" = "[\\s[:punct:]]+", 
                 "\\s+"
  )
  repx <- switch(state,
                 "onespace" = " ",
                 ""
  )
  str %>% paste %>% str_replace_all(patx,repx) %>% tolower()  %>%  drop_element_regex("^$")
}

seeraw <- function(filename=NULL,pageno=NULL,string="",udpipe=T,wholeword=T,modelf=modelfiles[1]){
  # sub function to get +1 -1 lines of text
  Get_text <- function(Pagetext, string, wholeword) {
    if(wholeword) string <- paste0("\\b",string,"\\b")
    text_line <- Pagetext  %>% grep(pattern = string,ig=T) 
    #message(paste0("'",string,"'"," found in ",length(text_line)," line(s) out of a total lines of ",NROW(Pagetext)))
    tot_lines <- NROW(Pagetext)
    if(string=="") return(Pagetext) else
    which(str_detect(Pagetext,string)) -> x1
    lines2show <- base::union(x1-1,x1) %>% base::union(x1+1) %>% sort
    names(Pagetext) <- seq_along(along.with = Pagetext)
    Pagetext[lines2show]
  }
  
  #use pdftools if page number is available else readtext
  if(!is.null(pageno)) 
    Pagetext <- pdf_text(filename)[[pageno]] %>% str_split(pattern = "\n") %>% .[[1]] %>% str_trim()  else 
      Pagetext <-  readtext(filename)$text %>% str_split(pattern = "\n") %>% .[[1]] %>% str_trim() 

    text <- Get_text(Pagetext, string,wholeword)
    message(paste0("file:",filename," ('",string,"')"," found in ",length(text)," line(s) out of a total lines of ",NROW(Pagetext)))
    
    if (text[1] %in% c("NONE")) return("NONE")
    if(udpipe){
      mod <- udpipe_load_model(modelf)
      x1 <- udpipe_annotate(object = mod,x = paste(text,collapse=" ")) %>% as.data.table
      x1[,sentence] %>% unique
    } else text
}

seeraw_all <- function(files = scfile,pat="ZZZZ",tokenised=F){
  x1 <- files %>% map(.f = seeraw,string=pat,udpipe=tokenised)
  names(x1) <- files
  x1
}

autodet <- function(filename=NULL,verbose=F,ret_full=F){
  npages <- pdf_info(filename)$pages
  alltext <- pdf_text(filename) %>%  str_split(pattern = "\n") %>% lapply(str_squish) %>% lapply(drop_element_regex,"^$")
  dt_allhf <- data.table(page=integer(),lines=character(),loc=character(),string=character(),cond=logical())
  
  # check basic known strings for isolated first level elemination
  # condition_true <- function(string){
  #   x1 <- as.numeric(string) %>% is.na %>% not() || 
  #     length(packstr(string,state = "all"))==0 || 
  #     packstr(string,state = "all") %in% c("pageof","page","canoofpageof")
  #   x1
  # }
  
  markzz <- function(dt_allhf, cathead, alltext) {
    if(uniqueN(dt_allhf[loc==cathead,string])<=2) {
      dt_allhf[loc==cathead,cond:=T]
      modified_alltext <- 
        lapply(alltext, function(x) {
          i <- getrow(cathead,x)
          x[i]<-"ZZZZ"
          x
        })
    } else modified_alltext <- alltext
    
    modified_alltext
  }
  
  getrow <- function(cathead, x) {
    switch(cathead, 
           h1 = 1, 
           h2 = 2, 
           ln = length(x),
           ln_1 =length(x) -1)
  }
  
  addrow <- function(dt_allhf, i,cathead, comp_page) {
    x <- packstr(str = comp_page[getrow(cathead,comp_page)],state="all")
    if (length(x)==0) x <- NA
    dt_allhf <<- rbind(dt_allhf,data.table(page=i,lines=length(comp_page),loc=cathead,string=x,cond=F))
  }
  
  for( i  in  1:npages){
    comp_page <- alltext[[i]] # take each page in a variable
    
    # poshead1 <- comp_page[1]
    # poshead2 <- comp_page[2]
    # posfoot1 <- comp_page[length(comp_page)]
    # posfoot2 <- comp_page[length(comp_page)-1]
    
    # first level filter and marking of headers and footers - we just replace the first and last lines
    # if(condition_true(comp_page[1])) comp_page[1] <- "HEADERZZZZ"
    # if(condition_true(comp_page[length(comp_page)])) comp_page[length(comp_page)] <- "FOOTERZZZZ"
    
    # store the first and last lines for each page in a data.table
    addrow(dt_allhf,i,"h1",comp_page)
    addrow(dt_allhf,i,"h2",comp_page)
    addrow(dt_allhf,i,"ln",comp_page)
    addrow(dt_allhf,i,"ln_1",comp_page)
    
    
    # dt_allhf <- rbind(dt_allhf,data.table(page=i,loc='h2',string=packstr(str = comp_page[2],state="all"),cond=F))
    # dt_allhf <- rbind(dt_allhf,data.table(page=i,loc='ln',string=packstr(str = comp_page[length(comp_page)],state="all"), cond=F))
    # dt_allhf <- rbind(dt_allhf,data.table(page=i,loc='ln_1',string= packstr(str = comp_page[length(comp_page)-1],state="all"),cond=F))
    # 
    # and also store a logical variable, if TRUE it means the header (cond1) or footer (cond2) has been replaced by the marker string.
    # dt_allhf$cond1[i]<- condition_true(poshead)
    # dt_allhf$cond2[i] <- condition_true(posfoot)
    
    #dt_allhf$packf[i] <-  ifelse(condition_true(posfoot),NA,packstr(str = posfoot,state="all"))
    
    #alltext[[i]] <- comp_page # store back the modified page if any else the same page
  }
  
  # if(!all(dt_allhf$cond1))
  #   if(uniqueN(dt_allhf$packh)<=2) markout(alltext,flag="head")  -> alltext else message("..no headers detected.",appendLF = F)
  # if(!all(dt_allhf$cond2))
  #   if(uniqueN(dt_allhf$packf)<=2) markout(alltext,flag="foot")  -> alltext else message("..no footers detected.")
  
 
  alltext <- markzz(dt_allhf,"h1",alltext)
  alltext <- markzz(dt_allhf,"h2",alltext)
  alltext <- markzz(dt_allhf,"ln",alltext)
  alltext <- markzz(dt_allhf,"ln_1",alltext)
  # if(uniqueN(dt_allhf[loc=="h2",string])<=2) lapply(alltext, function(x) {x[2]<-"ZZZZ";x}) -> alltext 
  # if(uniqueN(dt_allhf[loc=="ln",string])<=2) lapply(alltext, function(x) {x[length(x)]<-"ZZZZ";x}) -> alltext 
  # if(uniqueN(dt_allhf[loc=="ln_1",string])<=2) lapply(alltext, function(x) {x[length(x)-1]<-"ZZZZ";x}) -> alltext 
  #if()  message("..no headers detected.",appendLF = F)
  if(nrow(dt_allhf[loc %in% c("h1","h2") & cond])>0) message("headers removed") else message("NO HEADERS FOUND")
  if(nrow(dt_allhf[loc %in% c("ln","ln_1") & cond])>0) message("footers removed") else message("NO FOOTERS FOUND")
  
  #drop the rows with markers
  if(ret_full) alltext %>% 
    lapply(drop_element_regex, pattern = "ZZZ") %>% 
    lapply(c) %>% # concatenate all list elements
    unlist else 
    dt_allhf
}

autosmry <- function(dtm=dtm_flat,docno=1, valuen=100,valuebands=20, seed=123){
  message("Save the output in a variable and run summary() on the variable")
  if(is.numeric(docno)) filename <- filenames[docno] else filename <- docno
  dtm[doc_id==filename & upos %in% c("NOUN","ADJ"),.(sentence_id,lemma)] %>% unique -> lemma1
  dtm[doc_id==filename,.(sentence_id,sentence)] %>% unique -> sent1
  candidates1 <- textrank_candidates_lsh(lemma1$lemma,sentence_id = lemma1$sentence_id,
                                         minhashFUN = minhash_generator(n = valuen, seed = seed),bands = valuebands)
  textrank_sentences(data = sent1, terminology = lemma1,textrank_candidates = candidates1) 
}


# the following functions will be used in Ripple Down Rules Shiny app
genr_dt <- function(dtflat,big=T){
  dtflat$suid <- unique_identifier(dtflat,c("doc_id","sentence_id"))
dtflat[,.(suid,doc_id,sentence_id,sentence)] %>% unique -> dsent2
 # dsent2$suid <- unique_identifier(dsent2,c("doc_id","sentence_id"))
if(!big) dtflat[,.(doc_id,sentence_id,suid,sentence)] %>% unique else 
  dtflat
}

downgold <- function(googlekey = "1naAek6358YfIfz8ThIIR5zxIMzl0AUuBCvp2QX5T5ls",sheet =1){
  gs_download(gs_key(googlekey),to = "gold.xlsx")
}


# match gold standard sentence phrases with new sentence segments (after re segmentation of the pdf file)
# file is google sheet download (use downgold function to get the file)
mgold <- function(file = "gold.xlsx",sheet = 1, column="conclusion", newsent,trunc=10,verbose=T) {
  gold1 <- readxl::read_excel(file,sheet = sheet) %>% as.data.table
  gold1$issue_sentence %<>% as.logical()
  gold1$conclusion_sentence %<>% as.logical()
  concltxt <- gold1[conclusion_sentence==T,sentence]
  issuetxt <- gold1[issue_sentence==T,sentence]
  goldtxt <- switch(column,conclusion=concltxt,issue = issuetxt)
  
  goldtxt %>% map(~ str_detect(newsent, fixed(.x ))) -> l1 # create a list, the length of the goldtxt array, with logical vectors the size of the total rows in newsent DT
  retindx <- function(i) {
    x <- which(l1[[i]])
    if(length(x)==1) x else NA
  }
  indx <- seq_along(goldtxt) %>% map_int(.f = retindx)
  failed_search <- which(is.na(indx))
  if(length(failed_search) >0 ) {
  message("Following gold standard text marked by Dhruv are missing in the newly segmented sentences.")
  print(failed_search)
  print(goldtxt[failed_search])
  message("Truncating and matching again. This truncated sentence is also not matching with the new sentences.")
  goldtxt[failed_search] <- word(goldtxt[failed_search],1,trunc)
  indx <- seq_along(goldtxt) %>% map_int(.f = retindx)
  failed_search <- which(is.na(indx))
  #message("And now these are the failed searches  :-)")
  #print(failed_search)
  }
  return(ifelse(verbose,goldtxt[failed_search],indx))
}

# save rule of number of words in a sentence
fno <- function(dt,n,id="nowords"){
  dt[,eval(id) := F]
  x <- dt[,{count <- words(sentence) %>% NROW; list(col1 = ifelse(count >= n,T,F))},by=suid]
  dt[,eval(id) := x$col1]
}

fpat <- function(dt=dtm_flat,file="kconclusion.txt",id="kconclusion") {
  keyw <- readLines(file)
  pat<- drop_empty_row(matrix(keyw,ncol = 1)) %>% str_trim %>%  paste(collapse = "|")
  dt[grepl(pat,sentence,ignore.case = T),eval(id) :=T]
  dt[is.na(get(id)),eval(id) :=F]
}
fpost <- function(dt,pat="ZZZ",id="post1",n=1){
  dt[,eval(id) :=F]
  indx <- grep(pat,dt$sentence,ignore.case = T)
  i <- 1
  while(i<=n){
  dt[indx+i,eval(id) := T]
    i <- i + 1
  }
}
vec_sent <- function(ds=dtm_flat$sentence, dimension = 10000, numwords = 10000) {
unlist(ds, use.names = F) -> d1
text_tokenizer(num_words = numwords) -> die
fit_text_tokenizer(object = die,x =  d1) -> diehard
texts_to_sequences(tokenizer = diehard, texts = d1)-> diesoft
vectorize_sequences(sequences = diesoft, dimension = dimension)
}

run_model <- function(tx= trainx, ty= trainy){model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")
model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)
model %>% fit(tx, ty, epochs = 4, batch_size = 512)}
test_mod <- function(tex= textx, tey=testy){
  results <- model %>% evaluate(tex, tey)}


# Attach a new column gold to dt taking the gold standard values tagged by Dhruv passed as a DT in gold. 
# Both dt and gold must be same number of rows datatables and must have the same sentences in same sequence.
# Pass "issue" or "conclusion" to the sty (sentence type) parameter
goldpos <- function(dt,gold,sty = "issue", verbose=F){ 
  assert_that(nrow(dt)==nrow(gold))
  dt[,gold := NA]
  range <- switch(sty,issue=which(gold$issue_sentence), conclusion=which(gold$conclusion_sentence))
  #range <- which(gold1$issue_sentence)
  dt[range,gold := T]
  dt[is.na(gold),gold:=F]
  message("Marked gold  positive")
  if(verbose) dt[range,sentence][]
}

# not needed now as goldpos will tag F all other sentences not tagged as T
goldneg <- function(dt,range){
  dt[range,gold := F]
  message("Marked gold  negative")
  dt[range,sentence][]
}
fpos <- function(dtm,pos="VERB",featpat = "past",id="past") {
  dt <- genr_dt(dtm,big = F)
  dt[, eval(id) := F] #initialize the new rule column 
  sids <- dtm[grepl(pos,upos) & grepl(featpat,feats,ig=T),unique(sentence_id)]
  dt[sentence_id %in% sids, eval(id) := T]
  dt[!sentence_id %in% sids, eval(id) := F]
}
# e.g. f1score(dt,"nowords); make sure gold column is updated with the same rule
f1score <- function(dt,rule){
  dt[,c("TP","FP","TN","FN") := NA]
  dt[get(rule)==T & gold==T,TP :=T]
  dt[get(rule)==T & gold==F,FP :=T]
  dt[get(rule)==F & gold==T,FN :=T]
  dt[get(rule)==F & gold==F,TN :=T]
  tp <- dt[TP==T,.N]
  fp <- dt[FP==T,.N]
  fn <- dt[FN==T,.N]
  tn <- dt[TN==T,.N]
  assert_that((tp+fp+fn+tn) == nrow(dt))
  prec <- tp/(tp+fp)
  recall <- tp/(tp+fn)
  data.table(F1=round(2*prec*recall*100/(prec+recall),digits = 1),
             precision=round(prec*100,digits = 1),
             recall = round(recall*100,1),
             TP = as.integer(tp),TN=as.integer(tn), FP=as.integer(fp),FN=as.integer(fn),
             rule = rule
             )
}

f1array <- function(dt,rules){
  filearray <- expand.grid(filenames,rules,stringsAsFactors = F)[,1]
  rulearray <- expand.grid(filenames,rules,stringsAsFactors = F)[,2]
  map2_dfr(filearray,rulearray,~f1score(dsent[doc_id==.x],as.character(.y))) %>% cbind(file=filearray)
}

f1group <- function(dt,rules,filewise = T, join = "AND"){
  filenames <- dt$doc_id %>% unique()
  r <- paste(rules,collapse = " & ")
  cat('dt[eval(parse(text = r)),newcol := T]',file = "script")
  dt$newcol <- F
  dt <- parse("script") %>% eval
  if (filewise) map_dfr(filenames,~f1score(dt[doc_id==.x],"newcol")) %>% cbind(file=filenames) else
  f1score(dt,"newcol")
}

feats <- function(dtm=dtm_flat,pos="NOUN|VERB",listout=T) {
  #dtm[grepl(pattern = pos,x = upos,ig=T),.(upos,words = list(unique(token))),by=feats] %>% unique
  if(listout) dtm[grepl(pattern = pos,x = upos,ig=T),
                  .(words = list(unique(token))),
                  by=.(upos,feats)][order(upos,feats)] %>% 
    unique else
  dtm[grepl(pattern = pos,x = upos,ig=T),
      .(words = paste(unique(token),collapse = ";")),
      by=.(upos,feats)][order(upos,feats)] %>% unique
}

getfiles <- function(dir){
  filenames <- 
    list.files(pattern = "pdf|PDF",path = dir,full.names = T) %>% 
    str_split("/",n = 1,simplify = T) %>% 
    as.character()
}
