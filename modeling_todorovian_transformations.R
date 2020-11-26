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
  
  # test sentence
  # text_tmp <- "He must commit a crime. He plans to commit a crime. He succeed in committing the crime. He was eager to commit a crime. He is beginning to commit a crime. He didn't commit a crime. I pretend that he is committing a crime"

  # annotate the text using udpipe (can take some time...)
  x <- udpipe_annotate(udmodel_eng, x = text_tmp)
  x <- as.data.frame(x)
  
  cat("Text annotated with Udpipe\n")
  
  ############
  ### Todorovian simple transformation no. 1
  ############
  
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
  
  ############
  ### Todorovian simple transformation no. 2
  ############
  
  x$transformation_simple_2 <- 0
  
  # define intention verbs list (done manually: we should look for resources that provide it)
  intention_verbs_list <- c("plan", "hope", "intend", "try", "premeditate")
  
  # find them
  intention_verbs <- which(x$lemma %in% intention_verbs_list & x$upos == "VERB")
  
  # for each verb, check if it's part of a transformation
  for(intention_verb in intention_verbs){
    
    # check if there are are verbs in the sentence that depend on the verb (in that case, we have a transformation)    
    intention_dependences <- which(x$sentence_id == x$sentence_id[intention_verb] &
                                     x$head_token_id == x$token_id[intention_verb] &
                                     x$upos == "VERB")
    
    x$transformation_simple_2[intention_dependences] <- 1
    
  }
  
  cat("Simple transformations of type 2 (intention):",  sum(x$transformation_simple_2), "\n")
  cat("Frequence per 100 sentences:", sum(x$transformation_simple_2)/as.numeric(x$sentence_id[length(x$sentence_id)])*100, "\n")
  
  ############
  ### Todorovian simple transformation no. 3
  ############
  
  x$transformation_simple_3 <- 0
  
  # define intention verbs list (done manually: we should look for resources that provide it)
  result_verbs_list <- c("succeed", "manage", "obtain")
  
  # find them
  result_verbs <- which(x$lemma %in% result_verbs_list & x$upos == "VERB")
  
  # for each verb, check if it's part of a transformation
  for(result_verb in result_verbs){
    
    # check if there are are verbs in the sentence that depend on the verb (in that case, we have a transformation)    
    result_dependences <- which(x$sentence_id == x$sentence_id[result_verb] &
                                     x$head_token_id == x$token_id[result_verb] &
                                     x$upos == "VERB")
    
    x$transformation_simple_3[result_dependences] <- 1
    
  }
  
  cat("Simple transformations of type 3 (result):",  sum(x$transformation_simple_3), "\n")
  cat("Frequence per 100 sentences:", sum(x$transformation_simple_3)/as.numeric(x$sentence_id[length(x$sentence_id)])*100, "\n")
  
  ############
  ### Todorovian simple transformation no. 4 (manner)
  ############
  # possible issue: Todorov talks also about the possibility of making this transformation via adverbs (this has to be modelled still)
  
  x$transformation_simple_4 <- 0
  
  # find all auxiliary verbs
  auxiliaries <- which(x$upos == "AUX")
  
  for(auxiliary in auxiliaries){
    
    # check if the auxiliary depends on any adjectives in the sentence (in that case, we have a -possible- transformation)    
    manner_possible_dependences <- which(x$sentence_id == x$sentence_id[auxiliary] &
                                  x$token_id == x$head_token_id[auxiliary] &
                                  x$upos == "ADJ")
    
    for(manner_possible_dependence in manner_possible_dependences){
      
      # check if the adjective is the head of another verb (in that case, we have a transformation)
      manner_dependences <- which(x$sentence_id == x$sentence_id[manner_possible_dependence] &
                                             x$head_token_id == x$token_id[manner_possible_dependence] &
                                             x$upos == "VERB")
      
      x$transformation_simple_4[manner_dependences] <- 1
      
    }
    
    
  }
  
  cat("Simple transformations of type 4 (manner):",  sum(x$transformation_simple_4), "\n")
  cat("Frequence per 100 sentences:", sum(x$transformation_simple_4)/as.numeric(x$sentence_id[length(x$sentence_id)])*100, "\n")
  
  ############
  ### Todorovian simple transformation no. 5 (aspect)
  ############
  # possible issue: this does not model the transformation done with auxiliaries, e.g. "to be in the midst of", "in the act of", etc.
  
  x$transformation_simple_5 <- 0
  
  # define aspect verbs list (done manually: we should look for resources that provide it)
  aspect_verbs_list <- c("begin", "start", "finish", "end")
  
  # find them
  aspect_verbs <- which(x$lemma %in% aspect_verbs_list & x$upos == "VERB")
  
  # for each verb, check if it's part of a transformation
  for(aspect_verb in aspect_verbs){
    
    # check if there are are verbs in the sentence that depend on the verb (in that case, we have a transformation)    
    aspect_dependences <- which(x$sentence_id == x$sentence_id[aspect_verb] &
                                  x$head_token_id == x$token_id[aspect_verb] &
                                  x$upos == "VERB")
    
    x$transformation_simple_5[aspect_dependences] <- 1
    
  }
  
  cat("Simple transformations of type 5 (aspect):",  sum(x$transformation_simple_5), "\n")
  cat("Frequence per 100 sentences:", sum(x$transformation_simple_5)/as.numeric(x$sentence_id[length(x$sentence_id)])*100, "\n")
  
  ############
  ### Todorovian simple transformation no. 6 (status)
  ############
  # possible issue: what is modeled here is the negative form, but not the contrary form
  
  x$transformation_simple_6 <- 0
  
  # find all negations
  all_negations <- which(x$lemma == "not")
  
  # for each negation, check if it's part of a transformation
  for(negation in all_negations){
    
    # check if the negation is applied to any verbs in the sentence (in that case, we have a transformation)    
    status_dependences <- which(x$sentence_id == x$sentence_id[negation] &
                                  x$token_id == x$head_token_id[negation] &
                                  x$upos == "VERB")
    
    x$transformation_simple_6[status_dependences] <- 1
    
  }
  
  cat("Simple transformations of type 6 (status):",  sum(x$transformation_simple_6), "\n")
  cat("Frequence per 100 sentences:", sum(x$transformation_simple_6)/as.numeric(x$sentence_id[length(x$sentence_id)])*100, "\n")
  
  ############
  ### Todorovian complex transformation no. 1 (appearance)
  ############
  # possible issue: what is modeled here is the negative form, but not the contrary form
  
  x$transformation_complex_1 <- 0
  
  # define appearance verbs list (done manually: we should look for resources that provide it)
  appearance_verbs_list <- c("feign", "pretend", "claim")
  
  # find them
  appearance_verbs <- which(x$lemma %in% appearance_verbs_list & x$upos == "VERB")
  
  # for each verb, check if it's part of a transformation
  for(appearance_verb in appearance_verbs){
    
    # check if there are are verbs in the sentence that depend on the verb (in that case, we have a transformation)    
    appearance_dependences <- which(x$sentence_id == x$sentence_id[appearance_verb] &
                                  x$head_token_id == x$token_id[appearance_verb] &
                                  x$upos == "VERB")
    
    x$transformation_complex_1[appearance_dependences] <- 1
    
  }
  
  cat("Complex transformations of type 1 (appearance):",  sum(x$transformation_complex_1), "\n")
  cat("Frequence per 100 sentences:", sum(x$transformation_complex_1)/as.numeric(x$sentence_id[length(x$sentence_id)])*100, "\n")
  
  # save output
  out_file <- gsub(".txt", "_annotated.csv", texts_list[i])
  write.csv(x, out_file)
  
  cat("Processing complete\n\n")
  
}
