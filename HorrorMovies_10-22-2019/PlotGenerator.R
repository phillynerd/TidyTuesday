install.packages("keras")
install.packages("tokenizers")

library(tidyverse)
library(keras)
library(tokenizers)
keras::install_keras()

#Source code for this modeling: https://www.r-bloggers.com/tensorflow-jane-austen-and-text-generation/


#Raw Data####
horror_movies_orig <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

#basic cleaning####
horror_movies <- horror_movies_orig %>% 
  distinct() %>% #removing duplicates
  mutate(language = as.factor(language),
         release_country = as.factor(release_country),
         movie_rating = as.factor(movie_rating),
         movie_run_time = as.numeric(str_remove(movie_run_time," min")),
         MovieYear = as.numeric(str_sub(title, start = -5, end = -2)),#only fails in 1 case
         budget = parse_number(budget), #fun little function that pulls out just the number
         release_format = case_when(movie_rating %in% c("E", "TV-14", "TV-MA", "TV-PG") ~ "TV",
                                    movie_rating %in% c("PG", "PG-13", "R") ~ "Theater",
                                    movie_rating %in% c("X", "NC-17") ~ "Adult",
                                    movie_rating %in% c("NOT RATED", "UNRATED") | is.na(movie_rating) == T ~ "Direct Release")) %>% 
  filter(MovieYear >= 2012)

#cleaning up movie plots
plots <-  horror_movies %>% 
  mutate(plot = str_remove_all(plot, pattern = "(?<!w)([A-Z])\\.")) %>% #removes middle initials followed by periods so my plot split works
  mutate(plot = str_replace_all(plot, pattern = "Jr.", "Jr"), #fix Jr. period
         plot = str_replace_all(plot, pattern = "St.", "St")) %>% #fix St. period
  separate(plot, into = c("director", "cast_sentence", "plot"),
           sep = "[//.]", 
           extra = "merge", #extra = "merge" merges anything beyond the first 2 sentences into the plot variable
           fill = "right") %>%  
  mutate(castsent_start = str_sub(cast_sentence, start = 1, end = 5), #If this parsing worked correctly, then the cast_sentence should always start with " With"
         plot_test = ifelse(castsent_start == " With", "pass", "fail")) %>% 
  filter(plot_test == "pass", !is.na(plot)) 

##tokenizing####
max_length <- 40

text <- 
  plots %>% 
  pull(plot) %>%
  #str_remove_all(pattern = "[0-9]") %>% 
  str_c(collapse = " ") %>%
  
  tokenize_characters(lowercase = FALSE, strip_non_alphanum = FALSE, simplify = TRUE)

print(sprintf("Corpus length: %d", length(text)))
## [1] "Corpus length: 652292"

chars <- text %>%
  unique() %>%
  sort()

print(sprintf("Total characters: %d", length(chars)))
## [1] "Total characters: 99"

#chopping things up 
dataset <- map(
  seq(1, length(text) - max_length - 1, by = 3), 
  ~list(sentence = text[.x:(.x + max_length - 1)], 
        next_char = text[.x + max_length])
)

dataset <- transpose(dataset)

#Vectorize
#Now it’s time to make a big set of vectors of these chunks of text. If you make max_length larger, this vectors object can get unwieldy in terms of memory.

vectorize <- function(data, chars, max_length){
  x <- array(0, dim = c(length(data$sentence), max_length, length(chars)))
  y <- array(0, dim = c(length(data$sentence), length(chars)))
  
  for(i in 1:length(data$sentence)){
    x[i,,] <- sapply(chars, function(x){
      as.integer(x == data$sentence[[i]])
    })
    y[i,] <- as.integer(chars == data$next_char[[i]])
  }
  
  list(y = y,
       x = x)
}

vectors <- vectorize(dataset, chars, max_length)

#creating the model####
create_model <- function(chars, max_length){
  keras_model_sequential() %>%
    layer_lstm(128, input_shape = c(max_length, length(chars))) %>%
    layer_dense(length(chars)) %>%
    layer_activation("softmax") %>% 
    compile(
      loss = "categorical_crossentropy", 
      optimizer = optimizer_rmsprop(lr = 0.01)
    )
}

#Let’s also make a function that fits the model for a set number of epochs.

fit_model <- function(model, vectors, epochs = 1){
  model %>% fit(
    vectors$x, vectors$y,
    batch_size = 128,
    epochs = epochs
  )
  NULL
}

#More functions for the modeling

#This one generates a phrase from a model, text, set of characters, and
#parameters like the maximum length of phrase and diversity, i.e. how WILD we
#are going to let the model be.

generate_phrase <- function(model, text, chars, max_length, diversity){
  
  # this function chooses the next character for the phrase
  choose_next_char <- function(preds, chars, temperature){
    preds <- log(preds) / temperature
    exp_preds <- exp(preds)
    preds <- exp_preds / sum(exp(preds))
    
    next_index <- rmultinom(1, 1, preds) %>% 
      as.integer() %>%
      which.max()
    chars[next_index]
  }
  
  # this function takes a sequence of characters and turns it into 
  #a numeric array for the model
  convert_sentence_to_data <- function(sentence, chars){
    x <- sapply(chars, function(x){
      as.integer(x == sentence)
    })
    array_reshape(x, c(1, dim(x)))
  }
  
  # the inital sentence is from the text
  start_index <- sample(1:(length(text) - max_length), size = 1)
  sentence <- text[start_index:(start_index + max_length - 1)]
  generated <- ""
  
  # while we still need characters for the phrase
  for(i in 1:(max_length * 20)){
    
    sentence_data <- convert_sentence_to_data(sentence, chars)
    
    # get the predictions for each next character
    preds <- predict(model, sentence_data)
    
    # choose the character
    next_char <- choose_next_char(preds, chars, diversity)
    
    # add it to the text and continue
    generated <- str_c(generated, next_char, collapse = "")
    sentence <- c(sentence[-1], next_char)
  }
  
  generated
}


#Notice that we seed the first characters for the model to use for prediction
#with a real chunk of text from Pride and Prejudice.

#This next function fits the model to the set of vectors, and then generates
#phrases from the current version of the model.

iterate_model <- function(model, text, chars, max_length, 
                          diversity, vectors, iterations){
  for(iteration in 1:iterations){
    
    message(sprintf("iteration: %02d ---------------\n\n", iteration))
    
    fit_model(model, vectors)
    
    for(diversity in c(0.2, 0.5, 1)){
      
      message(sprintf("diversity: %f ---------------\n\n", diversity))
      
      current_phrase <- 1:10 %>% 
        map_chr(function(x) generate_phrase(model,
                                            text,
                                            chars,
                                            max_length, 
                                            diversity))
      
      message(current_phrase, sep="\n")
      message("\n\n")
      
    }
  }
  NULL
}

#Actually running all of this

model <- create_model(chars, max_length)

iterate_model(model, text, chars, max_length, diversity, vectors, 50)
## NULL

#Checking results for different values of diversity####
result <- data_frame(diversity = rep(c(0.2, 0.4, 0.6), 20)) %>%
  mutate(phrase = map_chr(diversity,
                          ~ generate_phrase(model, text, chars, max_length, .x))) %>%
  arrange(diversity)

result %>%
  sample_n(10) %>%
  arrange(diversity) %>%
  kable()
