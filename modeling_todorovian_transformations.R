### Modeling Todorovian transformations using UDpipe

# Call the library (or install it if not present...)
if (!require("udpipe")) install.packages("udpipe")

# first, download the model for English language
if(length(list.files(pattern = ".udpipe")) == 0) udpipe_download_model(language = "english")

# then upload it to an R variable
udmodel_file <- list.files(pattern = ".udpipe")
udmodel_eng <- udpipe_load_model(file = udmodel_file)

# find the files in the "corpus" folder (just .txt files!)
texts_list <- list.files("corpus", pattern = ".txt", full.names = T)

# run a loop on all the files
for(i in 1:length(texts_list)){
  
  # print progress
  cat("Processing", texts_list[i], "...\n")
  
  # read text
  text_tmp <- readLines(texts_list[i])
  text_tmp <- paste(text_tmp, collapse = "\n")

  # annotate the text using udpipe (can take some time...)
  x <- udpipe_annotate(udmodel_eng, x = text_tmp)
  x <- as.data.frame(x)
  
  cat("Text annotated with Udpipe\n")
  
  ### Todorovian simple transformation no. 1
  x$transformation_simple_1 <- 0
  
  # find modal verbs
  modal_verbs <- which(x$xpos == "MD")
  
  # for each modal verb, check if it's part of a transformation
  for(modal_verb in modal_verbs){
    
    modal_dependence <- c(x$sentence_id[modal_verb], x$head_token_id[modal_verb])

    # check if there is no main verb, like in the sentence "I can't"    
    if(modal_dependence[2] == "0")
      next
    
    # then check if the modal actually points to a verb (might be useless, though...)
    if(x$upos[which(x$sentence_id == modal_dependence[1] & x$token_id == modal_dependence[2])] == "VERB")
      x$transformation_simple_1[modal_verb] <- 1
      
  }
  
  cat("Simple transformations of type 1 (modal):",  sum(x$transformation_simple_1), "\n")
  cat("Frequence per 100 sentences:", sum(x$transformation_simple_1)/as.numeric(x$sentence_id[length(x$sentence_id)])*100, "\n")
  
  # save output
  out_file <- gsub(".txt", "_annotated.csv", texts_list[i])
  write.csv(x, out_file)
  
  cat("Processing complete\n\n")
  
}
