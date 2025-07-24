#----------------------------------------------------------
# Extract keywords in context
#---------------------------------
# Setup

library(tidyverse)
library(quanteda)
library(data.table)

#---------------------------------
# Data
#---------------------------------

setwd("C:/Users/stepa/EU_Thesis_Paper")
setwd("C:/Users/stepa/Working_Paper_FSV_MFF")
data<-fread("dataset_mentions_controls2.csv")
data<-fread("paper_data.csv")


data<-data %>% filter(chair==0) # without chairs

#----------------------------------------------------------
# VOCABULARIES
# Basic EU pattern
pattern_eu <- paste0("\\b(eu|evropsk[áéý] uni[eíi]|evropskou uni[eíi])\\b")
                           "\\beu[1-9]{1,2}\\b")

# Úplný EU vzor - komplexní pokrytí všech EU termínů v češtině
pattern_brusel <- paste0("\\bbrusel\\w*")

# Treaties
pattern_treaties <- paste0(
  "\\b(římsk\\w*|maastrichtsk\\w*|amsterdamsk\\w*|lisabonsk\\w*) smlouv\\w*|",
  "\\bsmlouv\\w* o fungování evropsk\\w* uni\\w*"
)

# Euro, economic cooperation
pattern_euro <- paste0(  "\\beurozón\\w*|",
                           "\\beur[oa]\\b|",
                           "\\beurem\\b")

  
pattern_econ <- paste0( 
  "\\bhospodářsk\\w* a měnov\\w* uni\\w*|",
  "\\b(sgp|pakt\\w* stability a růstu)\\b")
  
  # SZBP/ESBP
  #"\\b(szbp|esbp)\\b|",
  #"\\bspoleč(n[áéý]|ensk[áéý]) zahrani(č|n)[íi] a bezpečnostn[íi] politik[aey]\\b|",
  #"\\beuropsk[áéý] zahrani(č|n)[íi] a bezpečnostn[íi] politik[aey]\\b|",
  #"\\bspoleč(n[áéý]|ensk[áéý]) bezpečnostn[íi] a obrann[áéý] politik[aey]\\b|",
  #"\\beuropsk[áéý] bezpečnostn[íi] a obrann[áéý] politik[aey]\\b|",

  
pattern_parliament <- paste0("\\bevropsk\\w* parlament\\w*")

pattern_comission <- paste0("\\bevropsk\\w* komis\\w*")

pattern_council <- paste0(  "\\brad\\w* (eu|evropsk\\w* uni\\w*)\\b")

pattern_eu_council <- paste0("\\bevropsk\\w* rad\\w*")

pattern_ecb <- paste0("\\bevropsk\\w* centráln\\w* ban\\w*|",
                      "\\becb\\b")

pattern_ecj <- paste0( "\\bevropsk\\w* soudn\\w* dvůr\\b|",
                              "\\bevropsk\\w* soudn\\w* dvo\\w*")


pattern_eu_rauh_parizek <- paste0(
  pattern_brusel,         # European Parliament
  "|",  
  pattern_treaties,         # European Parliament
  "|",  
  pattern_euro,         # European Parliament
  "|",  
  pattern_econ,         # European Parliament
  "|",  
  pattern_parliament,         # European Parliament
  "|",                        # OR
  pattern_comission,          # European Commission
  "|",                        # OR
  pattern_council,            # Council of the EU (Rada EU/Rada Evropské unie)
  "|",                        # OR
  pattern_eu_council,         # European Council (Evropská rada)
  "|",                        # OR
  pattern_ecb,                # European Central Bank / ECB
  "|",                        # OR
  pattern_ecj                 # European Court of Justice
)


pattern_eu_institutions <- paste0(
  pattern_parliament,         # European Parliament
  "|",                        # OR
  pattern_comission,          # European Commission
  "|",                        # OR
  pattern_council,            # Council of the EU (Rada EU/Rada Evropské unie)
  "|",                        # OR
  pattern_eu_council,         # European Council (Evropská rada)
  "|",                        # OR
  pattern_ecb,                # European Central Bank / ECB
  "|",                        # OR
  pattern_ecj                 # European Court of Justice
)
# To verify the combined pattern (for R):
print(pattern_eu_institutions)
########

#----------------------------------------------------------
# MENTIONS EXTRACTION

data_new <- data

data$eu_mentions <- str_count(data$text, regex(pattern_eu))
sum(data$eu_mentions)

data$eu_rauh_parizek_mentions <- str_count(data$text, regex(pattern_eu_rauh_parizek))
sum(data$eu_rauh_parizek_mentions)


data$treaties_mentions <- str_count(data$text, regex(pattern_treaties))
sum(data$treaties_mentions)

data$euro_mentions <- str_count(data$text, regex(pattern_euro))
sum(data$euro_mentions)

data$brusel_mentions <- str_count(data$text, regex(pattern_brusel))
sum(data$brusel_mentions)

data$econ_mentions <- str_count(data$text, regex(pattern_econ))
sum(data$econ_mentions)

data$parliament_mentions <- str_count(data$text, regex(pattern_parliament))
sum(data$parliament_mentions)

data$comission_mentions <- str_count(data$text, regex(pattern_comission))
sum(data$comission_mentions)

data$council_mentions <- str_count(data$text, regex(pattern_council))
sum(data$council_mentions)

data$eu_council_mentions <- str_count(data$text, regex(pattern_eu_council))
sum(data$eu_council_mentions)

data$ecb_mentions <- str_count(data$text, regex(pattern_ecb))
sum(data$ecb_mentions)

data$ecj_mentions <- str_count(data$text, regex(pattern_ecj))
sum(data$ecj_mentions)

data$eu_institutions_mentions <- str_count(data$text, regex(pattern_eu_institutions))
sum(data$eu_institutions_mentions)



file_out <- "dataset_mentions.csv"
#################################
# you have to run the "Control_variables.R" script with dataset_mentions, then come back here
# we add some variables for statistical modeling, its easier to do it now
#############################################################xx
##### Now we move to key words in context (KWIC) extraction 

data$eu_mentions <- str_count(data$text, regex(pattern_eu))
sum(data$eu_mentions)


data_eu<-data %>% filter(eu_mentions>0)


# Custom function to extract context
extract_kwic_multiword <- function(text, pattern, window = 50) {
  # Tokenize full text into words
  words <- str_split(text, "\\s+")[[1]]
  
  # Reconstruct word positions to get match indices from regex
  word_start_positions <- str_locate_all(text, "\\S+")[[1]][, "start"]
  
  # Find all pattern matches in the *full string*
  match_locs <- str_locate_all(str_to_lower(text), regex(pattern, ignore_case = TRUE))[[1]]
  
  if (nrow(match_locs) == 0) return(NULL)
  
  result <- map_dfr(1:nrow(match_locs), function(i) {
    match_start <- match_locs[i, "start"]
    
    # Find which word index the match starts at
    word_index <- which(word_start_positions >= match_start)[1]
    if (is.na(word_index)) return(NULL)
    
    start_idx <- max(1, word_index - window)
    end_idx <- min(length(words), word_index + window)
    
    tibble(
      matched_word = str_sub(text, match_locs[i, "start"], match_locs[i, "end"]),
      context_full = paste(words[start_idx:end_idx], collapse = " ")
    )
  })
  
  return(result)
}


kwic_contexts <- data_eu %>%
  rowwise() %>%
  mutate(context_list = list(extract_kwic_multiword(text, pattern_eu, window = 50))) %>%
  unnest(context_list) %>%
  ungroup() %>%
  select(id, speaker, party, chair, month, year, matched_word, context_full,chamber)
unique(kwic_contexts$matched_word)

# Final dataset of 40 word windows around only left mentions
write.csv(kwic_contexts, "eu.csv")



#--------------------------

data_eu_institutions<-data %>% filter(eu_institutions_mentions>0)


kwic_contexts <- data_eu_institutions %>%
  rowwise() %>%
  mutate(context_list = list(extract_kwic_multiword(text, pattern_eu_institutions, window = 50))) %>%
  unnest(context_list) %>%
  ungroup() %>%
  select(id, speaker, party, chair, month, year, matched_word, context_full,chamber)
unique(kwic_contexts$matched_word)



kwic_contexts$bruxelles<- str_count(kwic_contexts$matched_word, regex(pattern_brusel))

kwic_contexts$treaties<- str_count(kwic_contexts$matched_word, regex(pattern_treaties))

kwic_contexts$euro<- str_count(kwic_contexts$matched_word, regex(pattern_euro))

kwic_contexts$econ<- str_count(kwic_contexts$matched_word, regex(pattern_econ))

kwic_contexts$parliament<- str_count(kwic_contexts$matched_word, regex(pattern_parliament))

kwic_contexts$comission<- str_count(kwic_contexts$matched_word, regex(pattern_comission))

kwic_contexts$council<- str_count(kwic_contexts$matched_word, regex(pattern_council))

kwic_contexts$eu_council<- str_count(kwic_contexts$matched_word, regex(pattern_eu_council))

kwic_contexts$ecb<- str_count(kwic_contexts$matched_word, regex(pattern_ecb))

kwic_contexts$ecj<- str_count(kwic_contexts$matched_word, regex(pattern_ecj))

# Final dataset of 40 word windows around only left mentions
write.csv(kwic_contexts, "eu_institutions.csv")

#############################
# Right mentions extraction
#############################
# Keywords-in-context
kwic_right <- kwic(
  tokens_speeches,
  pattern = pattern_right,
  window = 40, 
  valuetype = "regex"
)

# Create final data frame with  the exact matched word, the full context in the window around and all metadata
kwic_df_right <- as_tibble(kwic_right) %>%
  left_join(
    data.frame(
      docid = names(corpus_speeches),
      id = docvars(corpus_speeches)$id,
      speaker = docvars(corpus_speeches)$speaker,
      party = docvars(corpus_speeches)$party,
      chair = docvars(corpus_speeches)$chair,
      month = docvars(corpus_speeches)$month,
      year = docvars(corpus_speeches)$year,
      ideologid=docvars(corpus_speeches)$ideologid
    ),
    by = c("docname" = "docid")
  ) %>%
  mutate(
    matched_word = str_extract(keyword, "\\S+"),
    context_full = paste(pre, matched_word, post)
  ) %>%
  select(
    id,
    speaker,
    party,
    chair,
    month,
    year,
    ideologid,
    matched_word,
    context_full
  )
############################
# Filter out windows that also mention the left 
kwic_df_right$left_mentions <- str_count(kwic_df_right$context_full, regex(pattern_left))
kwic_df_right<-kwic_df_right %>% filter(left_mentions==0)

# Final dataset of 40 word windows around only right mentions
write.csv(kwic_df_right, "right.csv")

##################################
# SWITCH TO PYTHON FOR MACHINE TRANSLATION AND NATURAL LANGUAGE INFERENCE
##################################


######################################################################
# bonus of socialism, communism etc
###############################
# Create corpus
corpus_speeches_2 <- corpus(
  data_kwic_2$text,
  docvars = data.frame(
    id = data_kwic_2$id,
    speaker = data_kwic_2$speaker,
    party = data_kwic_2$party,
    chair = data_kwic_2$chair,
    month = data_kwic_2$month,
    year = data_kwic_2$year,
    ideologid=data_kwic_2$ideologid
  )
)

# Tokenize
tokens_speeches_2 <- tokens(corpus_speeches_2)

#############################
# Socialism mentions extraction
############################
# Keywords-in-context 
kwic_soc <- kwic(
  tokens_speeches_2,
  pattern = pattern_soc,
  window = 40, 
  valuetype = "regex"
)

# Create final data frame with  the exact matched word, the full context in the window around and all metadata
kwic_df_soc <- as_tibble(kwic_soc) %>%
  left_join(
    data.frame(
      docid = names(corpus_speeches_2),
      id = docvars(corpus_speeches_2)$id,
      speaker = docvars(corpus_speeches_2)$speaker,
      party = docvars(corpus_speeches_2)$party,
      chair = docvars(corpus_speeches_2)$chair,
      month = docvars(corpus_speeches_2)$month,
      year = docvars(corpus_speeches_2)$year,
      ideologid=docvars(corpus_speeches_2)$ideologid
    ),
    by = c("docname" = "docid")
  ) %>%
  mutate(
    matched_word = str_extract(keyword, "\\S+"),
    context_full = paste(pre, matched_word, post)
  ) %>%
  select(
    id,
    speaker,
    party,
    chair,
    month,
    year,
    ideologid,
    matched_word,
    context_full
  )

############################
# Filter out windows that also mention the right 
kwic_df_soc$left_mentions <- str_count(kwic_df_soc$context_full, regex(pattern_left))
kwic_df_soc$right_mentions <- str_count(kwic_df_soc$context_full, regex(pattern_right))
kwic_df_soc<-kwic_df_soc %>% filter(right_mentions==0)
kwic_df_soc<-kwic_df_soc %>% filter(left_mentions==0)

# Final dataset of 40 word windows around only soc mentions
write.csv(kwic_df_soc, "soc.csv") #with left mentions


#############################
# Communism mentions extraction
############################
# Keywords-in-context 
kwic_kom <- kwic(
  tokens_speeches_2,
  pattern = pattern_kom,
  window = 40, 
  valuetype = "regex"
)

# Create final data frame with  the exact matched word, the full context in the window around and all metadata
kwic_df_kom <- as_tibble(kwic_kom) %>%
  left_join(
    data.frame(
      docid = names(corpus_speeches_2),
      id = docvars(corpus_speeches_2)$id,
      speaker = docvars(corpus_speeches_2)$speaker,
      party = docvars(corpus_speeches_2)$party,
      chair = docvars(corpus_speeches_2)$chair,
      month = docvars(corpus_speeches_2)$month,
      year = docvars(corpus_speeches_2)$year,
      ideologid=docvars(corpus_speeches_2)$ideologid
    ),
    by = c("docname" = "docid")
  ) %>%
  mutate(
    matched_word = str_extract(keyword, "\\S+"),
    context_full = paste(pre, matched_word, post)
  ) %>%
  select(
    id,
    speaker,
    party,
    chair,
    month,
    year,
    ideologid,
    matched_word,
    context_full
  )
sum(data$kom_mentions)
############################
# Filter out windows that also mention the right 
kwic_df_kom$left_mentions <- str_count(kwic_df_kom$context_full, regex(pattern_left))
kwic_df_kom$right_mentions <- str_count(kwic_df_kom$context_full, regex(pattern_right))
kwic_df_kom<-kwic_df_kom %>% filter(right_mentions==0)
kwic_df_kom<-kwic_df_kom %>% filter(left_mentions==0)
# Final dataset of 40 word windows around only kom mentions
write.csv(kwic_df_kom, "kom.csv")

##################################
# SWITCH TO PYTHON FOR MACHINE TRANSLATION AND NATURAL LANGUAGE INFERENCE
##################################