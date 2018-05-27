library(magrittr)
library(stringr)
library(lubridate)
library(readtext)
library(data.table)
library(pdftools)
library(udpipe)
library(textclean)
library(textrank)
library(tm)
#library(NLP)
#library(googledrive)
#library(googlesheets)
library(httr)
library(rvest)
library(purrr)
library(splitstackshape)
library(stringdist)
library(textshape)
library(assertthat)
library(htmlTable)
library(htmltools)
library(wrapr)

#---- LOAD VARIABLES AND CONSTANTS -----
assert_that(grepl("nlp",getwd()),msg = "You are not in the correct working directory")
casepath<- "./SC_2018"
is.dir(casepath)
scfile <- list.files(pattern = "pdf",path =casepath,full.names = T)
docno <- frank(scfile)
# reg_lup <- fread(file = "reglup.txt")
# scheads <- fread(file = "scheadfoot.txt")
key_gsupload <- "1H9ApyBnTV7rHnNaQ6IWzVmQPcaVNQXA5uk1ZmgSOzMI"
# trial_url <- "http://sci.gov.in/supremecourt/2013/15236/15236_2013_Judgement_07-Mar-2018.pdf"
# head_aif <- "\\s+All India Federation.+2007"
# foot_ikanoon <- "Indian Kanoon - http.+\\d"
model1 <- udpipe_load_model("english-partut-ud-2.0-170801.udpipe")
model2 <- udpipe_load_model("english-ud-2.0-170801.udpipe")

#if(!exists("model_partut")) model_partut <- udpipe_load_model(file = "english-partut-ud-2.0-170801.udpipe" )

#docno contains the unique digits of case files.. pls ensure each file has digits that are unique

    
#----- FUNCTIONS -----


clean_dtm <- function(dtm,m){
    dtm[str_detect(string = token,pattern = "[A-Za-z.]{2,}\\d{1,2}\\b"),
        token:=str_replace(string = token,pattern = "([A-Za-z]{2,})\\d{1,2}\\b",replacement = "\\1")] #to remove footnote numbers suffixed to some words.
    dtm[grepl("\\bors|\\banr",token,ignore.case = T),upos:="NOUN"]
    attributes(dtm)$training_model <- m$file
    dtm
}

tag <- function(file = NULL, model = model1){
    cat("Autodetecting headers and footers...")
    alltext <- suppressWarnings( autodet(file,ret_full = T)  )
    cat("detected and removed\nCleaning Vs., Mr., Mrs., No.,..")
  cleantext <- alltext %>% 
        str_replace_all(pattern = "[Vv]s*\\.",replacement = "vs") %>% 
        str_replace_all(pattern = "(M[rs]s*)\\.",replacement = "\\1") %>% 
        str_replace_all(pattern = "[Nn]o\\.",replacement = "number")
  cleantext %>% paste(collapse = " ") -> onetext
    cat("DONE.\n")
    
    udpipe_annotate(object = model,x = onetext) %>% as.data.table(keep.rownames = F) -> x1 # main line in function.
    suppressWarnings(x1$token_id %<>% as.integer() )
    suppressWarnings(x1$sentence_id %<>% as.integer() )
    x1$uid <- unique_identifier(x = x1,fields = c("doc_id", "sentence_id","token_id"))
    clean_dtm(dtm=x1,m=model)
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

extract_concl <- function(dtm=dtm_flat,file="kconclusion.txt",html=T){
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

sent <- function(dtm,n) dtm[sentence_id %in% n,unique(sentence)]

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
            ,u1:=txt_previous(upos,1),by=sentence_id][
                ,u2:=txt_previous(upos,2),by=sentence_id][
                    ,u3:=txt_previous(upos,3),by=sentence_id][
                        ,u4:=txt_previous(upos,4),by=sentence_id] -> dtm_enr
    
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
    keywords_phrases(dtm_leaner[,upos3],pattern = regex_citation,
                     term = dtm_leaner$token,is_regex = T,ngram_max = 16) -> phrase_cite
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
    grep(x = sentences,pattern = pat,ignore.case = T,value = T )
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

runsc_tagging <- function(files=scfile,single_dt=F){
    for(i in seq_along(files) ){
        message(paste0("\nStarted file:",i,": (",files[i],")"))
        #hregex <- reg_lup[code==scheads$header[i],pattern]
        #regex <- reg_lup[code==scheads$footer[i],pattern]
        dtm_sc[[i]] <- tag(file = files[i],model = model1)
    }
    message("\nTagging of all files completed")
    dtm_sc # return a list of DTMs - each element corresponds to 1 file. And this is later transforned to a flat DTM.
}

remv_hf <- function(files=scfile){
  adet <- data.table(file=files,hdr=character(length(files)),ftr=character(length(files)))
  for(i in seq_along(files) ){
    message(paste0("\nFinding headers and footers of file:",i,": (",files[i],")"))
    dt <- autodet(files[i])
    if(is.null(dt$header)) {
      message("Could not detect a unique header. Here are the first lines for few page:")
      print(dt$extract$first %>% unique %>% head)
      inp <- readline("Copy paste the part that seems unique enough to be a header else just enter:")
      if(inp=="") hdr <- "ZZZZ" else hdr <- inp
    }
    if(is.null(dt$footer)) {
      message("Could not detect a unique footer Here are the last lines of few pages:")
      print(dt$extract$last %>% unique %>% head)
      inp <- readline("Type NO if this file has NO footer:")
      if(inp=="") ftr <- "ZZZZ" else ftr <- inp
    }
    adet$hdr[i] <- hdr
    adet$ftr[i] <- ftr
  }
  adet
}

seeraw <- function(filename=NULL,pageno=NULL,string="",nlines1=3,nlines2=3,udpipe=T,serial=1,wholeword=T,model=model1){
  # sub function to get nlines of text
  Get_text <- function(Pagetext, string, nlines1, nlines2,serial,wholeword) {
    if(wholeword) string2 <- paste0("\\b",string,"\\b") else string2 <- string
    text_line <- Pagetext  %>% grep(pattern = string2,ig=T) 
    #message(paste0("'",string,"'"," found in ",length(text_line)," line(s) out of a total lines of ",NROW(Pagetext)))
    tot_lines <- NROW(Pagetext)
    if(string=="") return(Pagetext) else
      fline <- which(text_line>nlines1)
      lline <- which(text_line<tot_lines)
      if(length(fline)==0 & length(lline)==0) return("NONE")
    if(length(fline)==0) {nlines1 <- 0; st_line <- lline}
    if(length(lline)==0) {nlines2 <- 0; st_line <- fline}
      if(length(fline)>0 & length(lline)>0) st_line <- intersect(fline,lline)[min(serial,length(fline))]
    Pagetext[(text_line[st_line] - nlines1):(text_line[st_line] + nlines2)]
  }
  
  #use pdftools if page number is available else readtext
  if(!is.null(pageno)) 
    Pagetext <- pdf_text(filename)[[pageno]] %>% str_split(pattern = "\n") %>% .[[1]] %>% str_trim()  else 
      Pagetext <-  readtext(filename)$text %>% str_split(pattern = "\n") %>% .[[1]] %>% str_trim() 
    
    text <- Get_text(Pagetext, string, nlines1, nlines2,serial,wholeword)
    message(paste0("file:",filename," ('",string,"')"," found in ",length(text)," line(s) out of a total lines of ",NROW(Pagetext)))
    
    if (text[1] %in% c("NONE")) return("NONE")
    if(udpipe){
    x1 <- udpipe_annotate(object = model,x = paste(text,collapse=" ")) %>% as.data.table
    x1[,sentence] %>% unique
    } else text
}

seeraw_all <- function(files = scfile,pat="ZZZZ",tokenised=F){
x1 <- files %>% map(.f = seeraw,string=pat,udpipe=tokenised)
names(x1) <- files
x1
}

markout <- function(ltext,flag) {
  if(flag=="head") ltext %>% lapply(function(x) {x[1] <- "HEADERZZZZ"; x}) -> ltext2
  if(flag=="foot") ltext %>% lapply(function(x) {x[length(x)] <- "FOOTERZZZZ"; x}) -> ltext2
  ltext2
}

autodet <- function(filename=NULL,verbose=F,ret_full=F){
  npages <- pdf_info(filename)$pages
  alltext <- pdf_text(filename) %>%  str_split(pattern = "\n") %>% lapply(str_squish) %>% lapply(drop_element_regex,"^$")
  dt_allhf <- data.table(first=character(npages),last=character(npages),cond1=logical(npages),cond2=logical(npages))
  
  # check basic known strings for isolated first level elemination
  condition_true <- function(string){
    x1 <- as.numeric(string) %>% is.na %>% not() || 
      length(packstr(string,state = "all"))==0 || 
      packstr(string,state = "all") %in% c("pageof","page","canoofpageof")
    x1
  }
  
  for( i  in  1:npages){
    comp_page <- alltext[[i]] # take each page in a variable
    poshead <- comp_page[1]
    posfoot <- comp_page[length(comp_page)]
    
    # first level filter and marking of headers and footers - we just replace the forst and last lines
    if(condition_true(poshead)) comp_page[1] <- "HEADERZZZZ"
    if(condition_true(posfoot)) comp_page[length(comp_page)] <- "FOOTERZZZZ"
       
    # store the first and last lines for each page in a data.table
    dt_allhf$first[i]<- poshead
    dt_allhf$last[i] <- posfoot
    
    # and also store a logical variable, if TRUE it means the header (cond1) or footer (cond2) has been replaced by the marker string.
    dt_allhf$cond1[i]<- condition_true(poshead)
    dt_allhf$cond2[i] <- condition_true(posfoot)
    
    dt_allhf$packh[i] <-  ifelse(condition_true(poshead),NA,packstr(str = poshead,state="all"))
    dt_allhf$packf[i] <-  ifelse(condition_true(posfoot),NA,packstr(str = posfoot,state="all"))
    
    alltext[[i]] <- comp_page
  }
  if(!all(dt_allhf$cond1))
    if(uniqueN(dt_allhf$packh)==1) markout(alltext,flag="head")  -> alltext else message(paste(filename,":no header detected."))
  if(!all(dt_allhf$cond2))
  if(uniqueN(dt_allhf$packf)==1) markout(alltext,flag="foot")  -> alltext else message(paste(filename,":no footer detected."))

    if(ret_full) alltext %>% lapply(drop_element_regex, pattern = "HEADERZZZ|FOOTERZZZZ") %>% lapply(c) %>% unlist else dt_allhf
}

#-----MAIN PROGRAM : Take user options----
choice <- readline("(N)Nothing (L)oad from local file /(R)ecompute /(A)nnotate & Compute:")
suppressWarnings (switch(choice,
                         N = message("Moving on.."),
                         L = {message("loading rpub.variables..."); load("rpub.variables")}, 
                         R = rm(dtm_flat,cross_ref,pcite,psctext,cit_flat), 
                         A = rm(dtmsc,dtmsc2,dtm_flat,cross_ref,pcite,psctext,cit_flat),
                         message("Unrecognised option, assuming 'L'oad  ")
))
if(choice=="A") {
  cat(paste("Supreme Court files found and loaded... total files:",NROW(scfile),"DIR:",casepath,"\n"))
  modelfiles <- list.files(pattern = ".udpipe")
  print(modelfiles)
  sn <- readline("Which model no. to load?:") %>% as.integer
  cat("Loading..")
  model <-   udpipe_load_model(file = modelfiles[sn])
  cat("loaded in memory.")
  cite<- list()
  dtm_sc <- list()
  cat("\nAnnotation and tagging started...")
  dtmsc <- runsc_tagging(files=scfile) 
  # transform the DTM by adding doc_id to each row (doc_id is the unique number extracted from file name)
  dtmsc2 <- map2(dtmsc, docno, ~ .x[, "doc_id" := as.character(.y),]) # learnt this from Stackoverlfow - terrific
  
}

if(choice %in% c("R","A")){
  assert_that(exists("dtmsc"))
  model <- attr(dtmsc[[1]],"training_model")
  message(paste('Training model used:',model))
  
  dtm_flat <- do.call(rbind,dtmsc2) # combine all DTMs into a flat DT
  dtm_flat$uid <- unique_identifier(x = dtm_flat,fields = c("doc_id", "sentence_id","token_id")) # add a unique id to the DTM
  attr(dtm_flat,"training_model")<- attr(dtmsc[[1]],"training_model")
  
  cat("Running the 'citings()' function on the merged DTM (dtm_flat)..")
  citings(dtm = dtm_flat,consec = 3) -> cit_flat #consecutive sentences used as extracts
  cat("..DONE.\n")
  # citings(dtm = dtm_flat,consec = 4) -> cit_flat4 #consecutive sentences used as extracts
  # cat("..2.")
  if(is.na(cit_flat$referred_cases)[1]) message("No citations found in any file.. aborting citation processing") else {
  #cat("completed\n")
  #message("'cit_flat' is updated")
  attr(cit_flat,"training_model") <- attr(dtmsc[[1]],"training_model")
  #remove numbers and puncts for simplifying regex search 
  cit_flat$referred_cases %>% str_replace_all("[\\s[:punct:][:digit:]]+","") %>% tolower() -> tmp
  data.table(case=cit_flat$referred_cases,packed = tmp) ->pcite #this is packed citations
  map_chr(scfile,packfile) -> tmp
  data.table(filename=scfile,filecontent=tmp) -> psctext
  
  cat("Generating cross_ref matrix...")
  assert_that(nrow(pcite)>0,nrow(psctext)>0)
  
  
  map2_int(.x = expand.grid(1:nrow(psctext),1:nrow(pcite))$Var1, 
           .y = expand.grid(1:nrow(psctext),1:nrow(pcite))$Var2,
           .f = ~ str_count(string = psctext[filename==scfile[.x],filecontent,with=T],
                            pattern = pcite[case==cit_flat$referred_cases[.y],packed])) %>% 
    matrix(ncol = nrow(pcite)) -> cross_ref
  cat("generated\n")
  colnames(cross_ref)<- substr(cit_flat$referred_cases,1,16)
  rownames(cross_ref) <- scfile
  
  # colSums(cross_ref) %>% data.table(citation=cit_flat$referred_cases,count_ref=.) -> m1
  # rowSums(cross_ref) %>% data.table(file=scfile,count_ref=.)  -> m2
  
  map2_int(.x = expand.grid(1:nrow(psctext),1:nrow(pcite))$Var1, 
           .y = expand.grid(1:nrow(psctext),1:nrow(pcite))$Var2,
           .f = ~ str_count(string = psctext[filename==scfile[.x],filecontent,with=T],
                            pattern = pcite[case==cit_flat$referred_cases[.y],packed])) %>% 
    matrix(ncol = nrow(pcite)) -> cross_ref
  }
} 


#---- Load law text books & colocated phrases in law books - needed for SOA -----
if(!exists("mpj.term")) load("mpj.term")
if(!exists("colos_mpj")) {
  message("creating keywords from MP Jain text book DTM 'mpj.term'...")
  colo(casedtm = dtm_flat,lawbookdtm = mpj.term,minimum_occ = 4) -> colos_mpj
  cat("keywords saved in variable: colos_mpj")
}
#colos_mpj[,.(`St. Law(MPJ,count)`=.N),by=doc_id]


#---- Save the variables if not saved in a while -----
if(choice %in% c("A", "R") |  now() - last_saved > ddays(1)){
  cat("saving rpub.variables....")
  cat(qc(dtmsc,dtmsc2,scfile,colos_mpj,dtm_flat,cit_flat,cross_ref))
  last_saved <- now()
  save(dtmsc,dtmsc2,scfile,colos_mpj,dtm_flat,cit_flat,cross_ref,last_saved,file="rpub.variables")
  cat("saved")
}

cat("\nALL READY\n")
