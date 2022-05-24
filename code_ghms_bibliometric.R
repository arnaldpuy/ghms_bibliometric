## ----setup, include=FALSE-------------------------------------------
knitr::opts_chunk$set(echo = TRUE, dev = "tikz", cache=TRUE)


## ----packages, results="hide", message=FALSE, warning=FALSE---------

# PRELIMINARY FUNCTIONS ----------------------------------------------------------

# Function to read in all required packages in one go 
loadPackages <- function(x) {
  for(i in x) {
    if(!require(i, character.only = TRUE)) {
      install.packages(i, dependencies = TRUE)
      library(i, character.only = TRUE)
    }
  }
}

# Load the packages
loadPackages(c(
  "bibliometrix", "tidyverse", "data.table", "scales", "pdfsearch", "pdftools", 
  "openxlsx", "cowplot", "wesanderson", "sjmisc", "ggpubr", "tm", "syuzhet", 
  "qdapRegex", "tidytext", "igraph", "ggraph"))

# Create custom theme
theme_AP <- function() {
  theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent",
                                           color = NA),
          legend.key = element_rect(fill = "transparent",
                                    color = NA), 
          strip.background = element_rect(fill = "white"), 
          legend.margin = margin(0.5, 0.1, 0.1, 0.1),
          legend.box.margin = margin(0.2,-2,-7,-7))
}

# Set checkpoint
dir.create(".checkpoint")
library("checkpoint")

checkpoint("2022-05-11", 
           R.version ="4.2.0", 
           checkpointLocation = getwd())

# FUNCTION TO CLEAN TEXT ######################################################

# Function to remove punctuation, citations, numbers, stopwords in english, 
# bring to lowercase and strip whitespace, and especial characters, etc...
clear_text <- function(x) {
  y <- gsub("-", "", x)
  y <- rm_citation(y)
  y <- tm::removePunctuation(y)
  y <- tm::removeNumbers(y)
  y <- tm::removeWords(y, stopwords::stopwords(language = "en"))
  y <- tolower(y)
  y <- str_replace_all(y, "[[:punct:]]", "") # Remove punctuation characters
  y <- str_replace_all(y, "[^[:alnum:]]", " ") # Remove all non-alphanumerical
  y <- stemDocument(y) # Stem the document and keep only the root of the word
  y <- tm::stripWhitespace(y)
  y <- str_squish(y)
  y <- tm::removeWords(y, c(" et ", "al", "table", "figure", "fig", 
                            "figs", "can", "eg", "mm", "yr", 
                            "last", "access", "see", "section"))
  y <- gsub(" ?doi\\w+ ?", "", y) # Remove words that start with doi
  y <- str_replace(y, "http", "") # Remove https
  y <- tm::removeWords(y, stopwords::stopwords(language = "en"))
  y <- trimws(y) # Remove leading/trailing white space
  y <- tm::stripWhitespace(y)
  y <- gsub("\\s[A-Za-z](?= )", "", y, perl = TRUE) # Remove isolated letters
  y <- gsub("\\s[A-Za-z]$", "", y, perl = TRUE) # Remove isolated letters end of string

  return(y)
}

## ----vector_models--------------------------------------------------

# VECTOR WITH NAME OF MODELS ###################################################

models <- c("WaterGAP", "PCR-GLOBWB", "MATSIRO", "H08", "JULES-W1", "MPI-HM", 
            "MHM", "LPJmL", "CWatM", "CLM", "DBHM", "ORCHIDEE", "GR4J")

models_vec <- paste(models, "_ref.bib", sep = "")


## ----bibliometric, results="hide", message=FALSE, dependson="vector_models"----

# BIBLIOMETRIC ANALYSIS ########################################################

output <- results <- years <- journals <- dt <- dt.clean <- list()

selected_cols <- c("title", "abstract", "keywords", "keywords.plus")

for (i in 1:length(models_vec)) {
  
  output[[i]] <- convert2df(file = models_vec[i], 
                            dbsource = "wos", 
                            format = "bibtex")
  
  # Extract title --------------------------------------------------------------
  
  title <- output[[i]]$TI
  
  # Extract Authors, Countries and Universities -------------------------------
  
  # Authors 
  tmp.authors <- output[[i]]$AU 
  first.author <- sub(" *\\;.*", "", tmp.authors)
  last.author <- sub(".*\\;","", tmp.authors)
  
  # First author affiliation and country
  country.first <- sub(".*\\,", "", output[[i]]$RP)
  university.first <- sub(" *\\;.*", "", output[[i]]$affiliations)

  # Last author affiliation and country
  last.affiliation <- sub(".*\\;", "", output[[i]]$C1)
  country.last <- sub("\\.", "", sub(".*\\, ", "", last.affiliation))
  university.last <- sub(".*\\;", "", output[[i]]$affiliations)
  
  # Extract keywords ----------------------------------------------------------

  keywords <- gsub(";;", ";", output[[i]]$DE)
  keywords.plus <- gsub(";;", ";", output[[i]]$ID)
  
  # Create data.table ----------------------------------------------------------
  
  dt[[i]] <- data.table("WOS" = output[[i]]$UT, 
                        "title" = title,
                        "year" = output[[i]]$PY,
                        "keywords" = keywords,
                        "keywords.plus" = keywords.plus,
                        "first.author" = first.author, 
                        "last.author" = last.author, 
                        "country.first" = country.first, 
                        "country.last" = country.last, 
                        "university.first" = university.first, 
                        "university.last" = university.last, 
                        "abstract" = output[[i]]$AB) 
  
  dt.clean[[i]] <- copy(dt[[i]])
  
  dt.clean[[i]][, (selected_cols):= lapply(.SD, function(x) 
    clear_text(x)), .SDcols = selected_cols]
  
  # Export data dirty and clean
  write.xlsx(dt[[i]], file = paste(models[i], "_bibliometric.xlsx", sep = ""))
  write.xlsx(dt.clean[[i]], file = paste(models[i], "_bibliometric_clean.xlsx", sep = ""))
  
  # Retrieve analysis bibliometrix ---------------------------------------------
  
  results[[i]] <- biblioAnalysis(output[[i]], sep = ";")
  years[[i]] <- data.table(results[[i]]$Years)
  journals[[i]] <- data.table(results[[i]]$Sources) %>%
    .[, SO:= str_to_title(SO)] 
}

# Add names of models
names(years) <- models
names(journals) <- models
names(dt.clean) <- models
names(dt.clean) <- models

# KEYWORDS ANALYSIS ############################################################

# Define vectors for search ---------------------------------------------------
directory <- "/Users/arnaldpuy/Documents/papers/ghms_bibliometric/"
directory_vec <- paste(directory, models, "_pdfs", sep = "") 
filename_keywords <- paste(models, "keywords", sep = "_")

# Define vectors with keywords ------------------------------------------------
#keywords_vec <- c("sensitivity analysis", "uncertainty analysis", "uncertainty")
keywords_vec <- c("uncertainty", "sensitivity")
keywords_vec_stemmed <- stemDocument(keywords_vec)

# Loop -------------------------------------------------------------------------
dt.keyword <- dt.keyword.clean <- output <- list()
for (i in 1:length(directory_vec)) {
  
  output[[i]] <- keyword_directory(directory_vec[i],
                                   keyword = keywords_vec_stemmed,
                                   split_pdf = TRUE)
  
  dt.keyword[[i]] <- data.table("name" = output[[i]]$pdf_name, 
                                "keyword" = output[[i]]$keyword, 
                                "text" = output[[i]]$line_text)
  
  dt.keyword.clean[[i]] <- copy(dt.keyword[[i]])
  
  # Clean the text where the keywords are located
  dt.keyword.clean[[i]] <- dt.keyword.clean[[i]][, text:= clear_text(text)]
  
  # Write dirty and clean data
  fwrite(dt.keyword[[i]], file = paste(filename_keywords[i], ".csv", sep = ""))
  fwrite(dt.keyword.clean[[i]], file = paste(filename_keywords[i], "_clean.csv", sep = ""))
}

names(output) <- models
names(dt.keyword) <- models
names(dt.keyword.clean) <- models

## ----arrange_data, dependson="bibliometric"-------------------------

# ARRANGE DATA #################################################################

# Bibliometric analysis -------------------------

# Correct for USA and China
colsName <- c("country.first", "country.last") 
full.dt <- rbindlist(dt.clean, idcol = "Model") %>%
  .[, (colsName):= lapply(.SD, function(x) 
    ifelse(grepl("USA", x), "USA", x)), .SDcols = colsName] %>%
  .[, (colsName):= lapply(.SD, function(x) 
    ifelse(grepl("CHINA", x), "CHINA", x)), .SDcols = colsName]

# Keywords analysis -----------------------------
full.keyword.dt <- rbindlist(dt.keyword.clean, idcol = "Model")

# DESCRIPTIVE STUDY ############################################################

# Total number of studies
total.n <- full.dt[, .(Model, WOS)] %>%
  .[, .(total.papers = .N), Model] %>%
  .[order(-total.papers)]

total.n

sum(total.n$total.papers)

# Plot cumulative number of studies through time
plot.time <- rbindlist(years, idcol = "Model")[, .N, .(V1, Model)] %>%
  .[, V1:= as.factor(V1)] %>%
  ggplot(., aes(V1, N, fill = Model)) +
  geom_col() +
  scale_x_discrete(breaks = pretty_breaks(n = 3)) +
  labs(x = "Year", y = "Nº articles") +
  theme_AP() + 
  theme(legend.position = "none")

plot.time

# Calculate fraction of studies with keywords in the abstract
full.dt[, `:=` (uncertainti = str_detect(abstract, keywords_vec_stemmed[1]), 
                sensit = str_detect(abstract, keywords_vec_stemmed[2]))] 

plot.n.keywords <- full.dt[, lapply(.SD, function(x) 
  sum(x) / .N), .SDcols = (keywords_vec_stemmed), Model] %>%
  melt(., measure.vars = keywords_vec_stemmed) %>%
  ggplot(., aes(value, variable, fill = Model)) +
  geom_bar(stat = "identity") + 
  labs(x = "Fraction of studies", y = "") +
  scale_x_continuous(breaks = pretty_breaks(n = 3), 
                     limits = c(0, 1)) +
  facet_wrap(~Model) +
  theme_AP() + 
  theme(legend.position = "none")
  
plot.n.keywords

legend <- get_legend(plot.time + theme(legend.position = "top"))
bottom <- plot_grid(plot.time, plot.n.keywords, ncol = 2, labels = "auto", 
                    rel_widths = c(0.3, 0.7))

all.plots <- plot_grid(legend, bottom, ncol = 1, rel_heights = c(0.2, 0.8))
all.plots

# Calculate fraction of studies with keywords through time
total.n.year <- rbindlist(years, idcol = "Model") %>%
  .[, .(total.n = .N), V1] %>%
  setnames(., "V1", "year")

plot.fraction.years <- full.dt[, .(WOS, uncertainti, sensit, year)] %>%
  melt(., measure.var = keywords_vec_stemmed) %>%
  .[value == TRUE, .N, .(year, variable)] %>%
  merge(., total.n.year, by = "year") %>%
  .[, fraction:= N / total.n] %>%
  ggplot(., aes(year, fraction, color = variable, group = variable)) +
  geom_line() + 
  scale_color_discrete(name = "") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Year", y = "Fraction of articles") +
  theme_AP() +
  theme(legend.position = c(0.4, 0.85))

plot.fraction.years

# Per model
plot.fraction.years.model <- full.dt[, .(WOS, uncertainti, sensit, year, Model)] %>%
  melt(., measure.var = keywords_vec_stemmed) %>%
  .[value == TRUE, .N, .(year, variable, Model)] %>%
  merge(., total.n.year, by = "year") %>%
  .[, fraction:= N / total.n] %>%
  ggplot(., aes(year, fraction, color = variable, group = variable)) +
  geom_line() + 
  geom_point() +
  facet_wrap(~Model) +
  scale_color_discrete(name = "") +
  scale_y_continuous(limits = c(0, 1), breaks = pretty_breaks(n = 3)) +
  scale_x_continuous(breaks = pretty_breaks(n = 3)) +
  labs(x = "Year", y = "") +
  theme_AP() +
  theme(legend.position = "none")

plot.fraction.years.model

bottom <- plot_grid(plot.fraction.years, plot.fraction.years.model, 
                    ncol = 2, labels = c("c", "d"), rel_widths = c(0.3, 0.7))

plot_grid(all.plots, bottom, ncol = 1, labels = "", rel_heights = c(0.6, 0.4))

# PLOT JOURNALS ###############################################################

rbindlist(journals, idcol = "Model") %>%
  .[, sum(N), .(SO, Model)] %>%
  .[order(-V1)] %>%
  .[, .SD[1:25]] %>%
  na.omit() %>%
  ggplot(., aes(reorder(SO, V1, sum), V1)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "", y = "Nº of articles") +
  theme_AP()


# TEXT MINING: TREATMENT OF UNCERTAINTIES ######################################
################################################################################

# CREATE WORDCLOUD OF WORDS IN ABSTRACT ########################################

tmp <- split(full.dt, full.dt$Model)
names(tmp) <- models

out <- dtm <- m <- v <- word.count <- list()
for (i in names(tmp)) {
  out[[i]] <- Corpus(VectorSource(tmp[[i]]$abstract))
  dtm[[i]] <- tm::TermDocumentMatrix(out[[i]])
  m[[i]] <- as.matrix(dtm[[i]])
  v[[i]] <- sort(rowSums(m[[i]]), decreasing=TRUE)
  word.count[[i]] <- data.table(word = names(v[[i]]), freq = v[[i]])
}

word.count.dt <- rbindlist(word.count, idcol = "Model")

# Plot wordcloud -------------------------------------------

plots.wordcloud <- list()

for(i in names(word.count)) {
  plots.wordcloud[[i]] <- word.count.dt[Model == i] %>%
    .[1:50] %>%
    ggplot(., aes(label = word, size = freq)) +
    ggwordcloud::geom_text_wordcloud_area(eccentricity = 1, shape = "square") +
    scale_size_area(max_size = 10) +
    theme_AP() + 
    ggtitle(names(word.count[i]))
}

plots.wordcloud

# Check rank of the terms "uncertainty" and "sensitivity" in the abstract -----

word.count.dt[, rank:= frank(-freq, ties.method = "first"), Model]

rank.keywords <- word.count.dt[word %in% keywords_vec_stemmed] %>%
  merge(., total.papers.dt, by = "Model") 

rank.keywords

rank.keywords[order(rank)]

# Plot-------------------------------------------

ggplot(rank.keywords, aes(reorder(Model, -rank), rank, color = word)) +
  geom_point() + 
  coord_flip() +
  labs(x = "", y = "Rank") +
  scale_color_discrete(name = "Keywords", 
                       labels = c("Sensitivity", "Uncertainty")) +
  theme_AP() +
  theme(legend.position = "top")

# STUDY OF N-TOKENS ###########################################################

# Number of tokens ------------------------------
N.tokens <- 2

# For loop --------------------------------------
output <- sentiment.analysis <- token.analysis <- colNames_sentiments <- 
  plot.sentiment.analysis <- vec <- plot.token <- plot.token.model <- list()

for (i in 1:length(keywords_vec)) {
  
  output[[i]] <- full.keyword.dt %>%
    .[keyword == keywords_vec_stemmed[i]]
  
  # Token analysis -------------------------------------------------------------
  
  token.analysis[[i]] <- output[[i]] %>%
    unnest_tokens(bigram, text, token = "ngrams", n = N.tokens) %>%
    separate(bigram, into = c("word1", "word2"), sep = " ") %>%
    # We count the co-occurences of words without taking into account their order
    # within the n-token
    .[, `:=`(word1= pmin(word1, word2), word2 = pmax(word1, word2))] %>%
    count(word1, word2, Model, sort = TRUE) %>%
    unite(., col = "bigram", c("word1", "word2"), sep = " ")
  
  vec[[i]] <- token.analysis[[i]] %>%
    .[, str_detect(bigram, keywords_vec_stemmed[i])]
  
  plot.token[[i]] <- token.analysis[[i]][vec[[i]]] %>%
    .[, head(.SD, 5), Model] %>%
    ggplot(., aes(reorder(bigram, n, sum), n)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_AP() +
    labs(y = "$n$", x = "") +
    theme(legend.position = "none") +
    ggtitle(keywords_vec_stemmed[i])
  
  plot.token.model[[i]] <- token.analysis[[i]][vec[[i]]] %>%
    .[, head(.SD, 5), Model] %>%
    ggplot(., aes(reorder(bigram, n, sum), n, fill = Model)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_AP() +
    labs(y = "$n$", x = "") +
    theme(legend.position = "none") +
    ggtitle(keywords_vec_stemmed[i]) +
    facet_wrap(~Model, scales = "free", ncol = 3) 
  
  # Sentiment analysis ---------------------------------------------------------
  
  # Only tokens that appear more than 10 times and then select only the words
  # that accompany uncertainty and sensitivity
  sentiment.analysis[[i]] <- token.analysis[[i]][n > 10] %>%
    .[, words:= str_remove(bigram, c("uncertainty|sensitivity"))] %>%
    .[, get_nrc_sentiment(bigram), Model]
  colNames_sentiments[[i]] <- colnames(sentiment.analysis[[i]])[-1]
  plot.sentiment.analysis[[i]] <- sentiment.analysis[[i]][, lapply(.SD, sum), 
                                                          by = Model,
                                                          .SDcols = colNames_sentiments[[i]]] %>%
    melt(., measure.vars = colNames_sentiments[[i]]) %>%
    ggplot(., aes(value, variable, fill = variable)) + 
    geom_bar(stat = "identity") +
    facet_wrap(~Model, scales = "free_x") + 
    theme_AP() +
    ggtitle(keywords_vec[i])
  
}


































token.analysis[[i]] <- output[[1]] %>%
  unnest_tokens(bigram, text, token = "ngrams", n = N.tokens) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  # We count the co-occurences of words without taking into account their order
  # within the n-token
  .[, `:=`(word1= pmin(word1, word2), word2 = pmax(word1, word2))] %>%
  count(word1, word2, Model, sort = TRUE) %>%
  unite(., col = "bigram", c("word1", "word2"), sep = " ")

vec[[i]] <- token.analysis %>%
  .[, str_detect(bigram, keywords_vec_stemmed[1])]

plot.token[[i]] <- token.analysis[[i]][vec[[i]]] %>%
  .[, head(.SD, 5), Model] %>%
  ggplot(., aes(reorder(bigram, n, sum), n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_AP() +
  labs(y = "$n$", x = "") +
  theme(legend.position = "none") +
  ggtitle(keywords_vec_stemmed[i])

plot.token.model[[i]] <- token.analysis[[i]][vec[[i]]] %>%
  .[, head(.SD, 5), Model] %>%
  ggplot(., aes(reorder(bigram, n, sum), n, fill = Model)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_AP() +
  labs(y = "$n$", x = "") +
  theme(legend.position = "none") +
  ggtitle(keywords_vec_stemmed[i]) +
  facet_wrap(~Model, scales = "free", ncol = 3) 



















# Only tokens that appear more than 10 times
sentiment.analysis <- token.analysis[[1]][n > 10] %>%
  .[, words:= str_remove(bigram, c("uncertainty|sensitivity"))] %>%
  .[, get_nrc_sentiment(bigram), Model]
colNames_sentiments[[i]] <- colnames(sentiment.analysis[[i]])[-1]
plot.sentiment.analysis[[i]] <- sentiment.analysis[[i]][, lapply(.SD, sum), 
                                                        by = Model,
                                                        .SDcols = colNames_sentiments[[i]]] %>%
  melt(., measure.vars = colNames_sentiments[[i]]) %>%
  ggplot(., aes(value, variable, fill = variable)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~Model, scales = "free_x") + 
  theme_AP() +
  ggtitle(keywords_vec[i])




token.analysis[[1]]

sentiment.analysis[[i]] <- token.analysis[[1]] %>%
  .[n > 10] %>%
  .[, get_nrc_sentiment(bigram), Model]

colNames_sentiments[[i]] <- colnames(sentiment.analysis[[i]])[-1]
plot.sentiment.analysis[[i]] <- sentiment.analysis[[i]][, lapply(.SD, sum), 
                                                        by = Model,
                                                        .SDcols = colNames_sentiments[[i]]] %>%
  melt(., measure.vars = colNames_sentiments[[i]]) %>%
  ggplot(., aes(value, variable, fill = variable)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~Model, scales = "free_x") + 
  theme_AP() +
  ggtitle(keywords_vec[i])











token.analysis <- output[[1]] %>%
  unnest_tokens(bigram, text, token = "ngrams", n = N.tokens) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  # We count the co-occurences of words without taking into account their order
  # within the n-token
  .[, `:=`(word1= pmin(word1, word2), word2 = pmax(word1, word2))] %>%
  count(word1, word2, Model, sort = TRUE) %>%
  unite(., col = "bigram", c("word1", "word2"), sep = " ")

vec <- token.analysis %>%
  .[, str_detect(bigram, keywords_vec[1])]

token.analysis[vec] %>%
  .[, head(.SD, 5), Model] %>%
  ggplot(., aes(reorder(bigram, n, sum), n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_AP() +
  labs(y = "$n$", x = "") +
  theme(legend.position = "none") +
  ggtitle(keywords_vec[i])















enframe(c("a b c a d e")) %>% 
  unnest_tokens(skipgram, value, token = "skip_ngrams", n = 5) %>% 
  mutate(n_words = str_count(skipgram, pattern = "\\S+")) %>%
  filter(n_words == 2) %>% 
  separate(col = skipgram, into = c("word1", "word2"), 
           sep = "\\s+") %>%
  transmute(word11 = pmin(word1, word2), word22 = pmax(word1, word2)) %>%
  count(word11, word22)




output[[1]] %>%
  unnest_tokens(bigram, text, token = "ngrams", n = N.tokens) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  .[, `:=`(word1= pmin(word1, word2), word2 = pmax(word1, word2))] %>%
  count(word1, word2, Model, sort = TRUE) %>%
  unite(., col = "bigram", c("word1", "word2"), sep = " ")



transmute(word11 = pmin(word1, word2), word22 = pmax(word1, word2)) %>%










bigram_graph <- token.analysis[[3]][vec[[3]]] %>%
  count(bigram, Model, sort = TRUE) %>%
  .[n >= 10] %>%
  .[, .(bigram, n)] %>%
  graph_from_data_frame()




library(ggraph)
set.seed(123)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()





for (i in 1:length(keywords_vec)) {
  
  output[[i]] <- full.keyword.dt %>%
    .[keyword == keywords_vec[i]]
  
  # Sentiment analysis ---------------------------------------------------------
  
  sentiment.analysis[[i]] <- output[[i]][, get_nrc_sentiment(text), Model]
  colNames_sentiments[[i]] <- colnames(sentiment.analysis[[i]])[-1]
  plot.sentiment.analysis[[i]] <- sentiment.analysis[[i]][, lapply(.SD, sum), 
                                                          by = Model,
                                                          .SDcols = colNames_sentiments[[i]]] %>%
    melt(., measure.vars = colNames_sentiments[[i]]) %>%
    ggplot(., aes(value, variable, fill = variable)) + 
    geom_bar(stat = "identity") +
    facet_wrap(~Model, scales = "free_x") + 
    theme_AP() +
    ggtitle(keywords_vec[i])
  
  # Token analysis -------------------------------------------------------------
  
  token.analysis[[i]] <- output[[i]] %>%
    unnest_tokens(bigram, text, 
                  token = "ngrams", 
                  n = N.tokens)
  vec[[i]] <- token.analysis[[i]] %>%
    .[, str_detect(bigram, keywords_vec[i])]
  
  plot.token[[i]] <- token.analysis[[i]][vec[[i]]] %>%
    count(bigram, Model, sort = TRUE) %>%
    .[, head(.SD, 5), Model] %>%
    ggplot(., aes(reorder(bigram, n), n, fill = Model)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    facet_wrap(~Model, scales = "free", ncol = 3) +
    theme_AP() +
    labs(y = "$n$", x = "") +
    theme(legend.position = "none") +
    ggtitle(keywords_vec[i])
  
}











dt.bigrams[vec] %>%
  count(bigram, Model, sort = TRUE) %>%
  .[, head(.SD, 5), Model] %>%
  ggplot(., aes(reorder(bigram, n), n, fill = Model)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~Model, scales = "free_y", ncol = 3) +
  theme_AP() +
  theme(legend.position = "none") +
  ggtitle(names(keywords_vec[i]))






da <- full.keyword.dt[, get_nrc_sentiment(text), Model]
colNames_sentiments <- colnames(da)[-1]

da[, lapply(.SD, sum), by = Model, .SDcols = colNames_sentiments] %>%
  melt(., measure.vars = colNames_sentiments) %>%
  ggplot(., aes(value, variable, fill = variable)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~Model, scales = "free_x") + 
  theme_AP()



# STUDY OF N-TOKENS ############################################################

# N-TOKENS --------------------------------------------------------------------

dt.bigrams <- full.keyword.dt %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 3)

vec <- dt.bigrams %>%
  .[, str_detect(bigram, "uncertainty|sensitivity|uncertainty analysis")]

dt.bigrams[vec] %>%
  count(bigram, Model, sort = TRUE) %>%
  .[, head(.SD, 5), Model] %>%
  ggplot(., aes(reorder(bigram, n), n, fill = Model)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~Model, scales = "free_y", ncol = 3) +
  theme_AP() +
  theme(legend.position = "none") +
  ggtitle(names(keywords_vec[i]))


dt.bigrams %>%
  count(bigram, Model, keyword, sort = TRUE)








########################################
########################################

### WORK IN PROGRESS -----------------------------------------------------------



# N-TOKENS --------------------------------------------------------------------

dt.bigrams <- full.keyword.dt %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 3)

vec <- dt.bigrams %>%
  .[, str_detect(bigram, "uncertainty|sensitivity")]

dt.bigrams[vec] %>%
  count(bigram, Model, sort = TRUE) %>%
  .[, head(.SD, 5), Model] %>%
  ggplot(., aes(reorder(bigram, n), n, fill = Model)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~Model, scales = "free_y", ncol = 3) +
  theme_AP() +
  theme(legend.position = "none")


dt.bigrams %>%
  count(bigram, Model, keyword, sort = TRUE)

















# SENTIMENT ANALYSIS -----------------------------------------------------------

da <- full.keyword.dt[, get_nrc_sentiment(text), Model]
colNames_sentiments <- colnames(da)[-1]

da[, lapply(.SD, sum), by = Model, .SDcols = colNames_sentiments] %>%
  melt(., measure.vars = colNames_sentiments) %>%
  ggplot(., aes(value, variable, fill = variable)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~Model, scales = "free_x") + 
  theme_AP()


dt.bigrams[vec]
 


str_detect(bi"uncertainty")


str(da)


library(qdapRegex)
watergap_keywords <- fread("LPJmL_keywords.csv")

gsub("\\s*\\B\\https[[:alpha:]]+\\b", "", da[[782]])
gsub("(f|ht)tps(s?)//(.*)[.][a-z]+", "", da[[782]])

tm::removeWords(da[[782]], "https\\*")
str_replace(da[[782]], "http", "")

da <- clear_text(watergap_keywords$text)

gsub(" ?doi\\w+ ?", "", da[[26]])


da[782]

str_split_fixed(da[[174]], "uncertainty", 2)


# check 174
str_remove(watergap_keywords$text, c("table",))

str_replace_all(da, "[[:punct:]]", " ")
########################################
########################################

# Check which studies have "Uncertainty" or "Sensitivity" in keywords,
# keywords.plus or abstract
keywords_search <- c("uncertainty|sensitivity")

tmp <- cbind(str_detect(full.dt$abstract, keywords_search), 
            str_detect(full.dt$keywords.plus, keywords_search), 
            str_detect(full.dt$keywords, keywords_search))

full.dt[, "uncertainty.sensitivity":= apply(tmp, 1, function(x) any(x, na.rm = TRUE))]

# Write dataset-
write.xlsx(full.dt, "full.dt.xlsx")

# Write dataset only with studies that have the keywords
full.dt[uncertainty.sensitivity == TRUE] %>%
  write.xlsx(., "full.dt.uncertain.xlsx")


## ----plot_time, dependson="bibliometric", warning = FALSE, fig.height=3, fig.width=4.5----

# PLOT ------------------------------------------------------------------------
names(years) <- models
tmp <- rbindlist(years, idcol = "Model")[, .N, .(V1, Model)]

# Print total number of studies 
tmp[, sum(N)]

plot.time <- tmp %>%
  .[, V1:= as.factor(V1)] %>%
  ggplot(., aes(V1, N, fill = Model)) +
  geom_col() +
  scale_x_discrete(breaks = pretty_breaks(n = 3)) +
  labs(x = "Year", y = "Nº articles") +
  theme_AP()

plot.time


## ----plot_journals, dependson="bibliometric", fig.height=6, fig.width=5.5, warning=FALSE----

# PLOT JOURNALS ---------------------------------------------------------------

rbindlist(journals, idcol = "Models") %>%
  .[, sum(N), SO] %>%
  .[order(-V1)] %>%
  .[, .SD[1:25]] %>%
  na.omit() %>%
  ggplot(., aes(x = reorder(SO, V1), y = V1)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "", y = "Nº of articles") +
  theme_AP()


## ----analyse_data, dependson="arrange_data"-------------------------

# ANALYSE DATASET --------------------------------------------------------------

dt.use <- full.dt[, .N, .(Model, university.first)] %>%
  dcast(., university.first~ Model, value.var = "N")

for(j in seq_along(dt.use)){
  set(dt.use, i = which(is.na(dt.use[[j]]) & is.numeric(dt.use[[j]])), j = j, value = 0)
}

# Total number each institute uses a model
dt.use[, total:= rowSums(.SD), .SDcols = models]

# First 50
dt.50 <- copy(dt.use)[order(-total)][1:50]

# Turn lowercase of institutions except acronyms
exceptions <- c("USA", "UK", "CNRS", "IIASA", "DOE", "PCSHE", "IIT", "NCAR", 
                "NOAA")
pattern <- sprintf("(?:%s)(*SKIP)(*FAIL)|\\b([A-Z])(\\w+)", 
                   paste0(exceptions, collapse = "|"))
dt.50 <- dt.50[, university.first:= gsub(pattern, "\\1\\L\\2", 
                                         university.first, perl = TRUE)]

tmp <- dt.50[, lapply(.SD, function(x) x / total), .SDcols = models] %>%
  .[, lapply(.SD, round, 2), .SDcols = models]


## ----histogram_legacy, dependson="analyse_data", fig.height=2.5, fig.width=2.5----

# RETRIEVE MAX VALUES PER INSTITUTE ---------------------------------

matrix.50 <- as.matrix(tmp)
colIndex <- apply(matrix.50, 1, which.max)

out <- vector()
for(i in 1:length(colIndex)) {
  out[i] <- matrix.50[[i, colIndex[i]]]
}

# Compute some statistics on the vector
f <- c(mean, median, min, max)
sapply(f, function(f) f(out, na.rm = TRUE))

# Plot
data.table(out) %>%
  ggplot(., aes(out)) +
  geom_histogram() +
  labs(x = "Fraction", y = "Counts") +
  theme_AP()

data.table(out) %>%
  ggplot(., aes(out)) +
  geom_boxplot() +
  labs(x = "Fraction", y = "Counts") +
  theme_AP()


## ----plot_use, dependson="analyse_data", fig.height=7, fig.width=6.5, warning=FALSE----

 # PLOT USE OF MODELS PER INSITUTE ---------------------------------------------

a <- melt(dt.50, measure.vars = models) %>%
  na.omit() %>%
  .[, variable:= factor(variable, levels = sort(models))] %>%
  ggplot(., aes(value, university.first, fill = variable)) + 
  scale_y_discrete(limits = rev) +
  labs(x = "Nº of articles", y = "") +
  scale_fill_discrete(name = "Model") +
  geom_bar(position = "stack", stat = "identity") + 
  theme_AP() + 
  theme(legend.position = "none", 
        axis.text.y = element_text(size = 9))

b <- melt(dt.50, measure.vars = models) %>%
  na.omit() %>%
  .[, variable:= factor(variable, levels = sort(models))] %>%
  ggplot(., aes(value, university.first, fill = variable)) + 
  scale_y_discrete(limits = rev) +
  labs(x = "Fraction", y = "") +
  scale_fill_discrete(name = "Model") +
  scale_x_continuous(breaks = pretty_breaks(n = 3)) +
  geom_bar(position = "fill", stat = "identity") + 
  theme_AP() + 
  theme(axis.text.y = element_blank(), 
        legend.position = "none")

legend <- get_legend(a + theme(legend.position = "top"))
bottom <- plot_grid(a, b, ncol = 2, labels = "auto", rel_widths = c(0.79 , 0.21))
ggarrange(legend, bottom, nrow = 2, heights = c(0.1, 0.9))


## ----keywords_analysis, dependson="arrange_data"--------------------

# ANALYSE FREQUENCY OF KEYWORDS AND KEYWORDS PLUS -----------------------------

# Keywords
strsplit(full.dt$keywords, ";", fixed = TRUE) %>%
  unlist(lapply(., str_trim)) %>%
  data.table("keywords" = .) %>%
  .[, .N, keywords] %>%
  .[order(-N)] %>%
  head(50)

# Keywords plus
strsplit(full.dt$keywords.plus, ";", fixed = TRUE) %>%
  unlist(lapply(., str_trim)) %>%
  data.table("keywords.plus" = .) %>%
  .[, .N, keywords.plus] %>%
  .[order(-N)] %>%
  head(50)


## ----plot_bars, warning = FALSE, fig.height=2.5, fig.width=4, dependson="bibliometric"----

# PLOT TOTAL NUMBER OF STUDIES AND TOTAL NUMBER STUDIES WITH KEYWORDS ----------

total.studies <- tmp[, sum(N), Model]
setnames(total.studies, "V1", "Total")

plot.bars <- full.dt[uncertainty.sensitivity == "TRUE"] %>%
  .[, sum(uncertainty.sensitivity), Model] %>%
  merge(., total.studies, by = "Model") %>%
  melt(., measure.vars = c("V1", "Total")) %>%
  ggplot(., aes(reorder(Model, value), value, fill = variable)) +
  coord_flip() +
  labs(y = "Nº of articles", x = "") +
  scale_fill_manual(values = wes_palette(2, name = "Darjeeling1"), 
                    name = "", 
                    labels = c("$N$ keywords", 
                               "$N$ total")) +
  scale_y_log10() +
  annotation_logticks(sides = "b") +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6)) +
  theme_AP() +
  theme(legend.position = c(0.8, 0.3))

plot.bars


## ----merge_plots1, dependson=c("plot_time", "plot_bars"), fig.height=3.2, fig.width=6, warning=FALSE----

# MERGE PLOTS ------------------------------------------------------------------

legend <- get_legend(plot.time + theme(legend.position = "top"))

bottom <- plot_grid(plot.time + theme(legend.position = "none"), 
                    plot.bars, ncol = 2, labels = "auto",
                    rel_widths = c(0.4, 0.6))
plot_grid(legend, bottom, ncol = 1, rel_heights = c(0.3, 0.7))


## ----keywords_search, dependson="vector_models", results="hide", message=FALSE, warning=FALSE----

# KEYWORDS SEARCH --------------------------------------------------------------

# Define vectors for search ---------------------------------------------------
directory <- "/Users/arnaldpuy/Documents/papers/ghms_bibliometric/"
directory_vec <- paste(directory, models, "_pdfs", sep = "") 
keywords_vec <- c("sensitivity analysis", "uncertainty analysis", "uncertainty")
filename_keywords <- paste(models, "keywords", sep = "_")

# Loop -------------------------------------------------------------------------
dt <- result <- list()
for (i in 1:length(directory_vec)) {
  
  result[[i]] <- keyword_directory(directory_vec[i],
                              keyword = keywords_vec,
                              split_pdf = TRUE)
  
  dt[[i]] <- data.table("name" = result[[i]]$pdf_name, 
                        "keyword" = result[[i]]$keyword, 
                        "text" = result[[i]]$line_text)
  
  fwrite(dt[[i]], file = paste(filename_keywords[i], ".csv", sep = ""))
  
}
names(result) <- models
names(dt) <- models


## ----plot_keywords, dependson="keywords_search", warning = FALSE, fig.height=3, fig.width=5.5----

# PLOT HISTOGRAMS WITH KEYWORDS ------------------------------------------------

dt.keywords <- rbindlist(dt, idcol = "Model") %>%
  .[, .N, .(Model, name, keyword)] %>%
  .[, keyword:= str_to_title(keyword)]

plot.keywords.histogram <- dt.keywords %>%
  ggplot(., aes(N, fill = keyword, color = keyword)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  facet_wrap(~Model, ncol = 6) +
  scale_y_continuous(breaks = pretty_breaks(n = 2)) +
  scale_x_continuous(breaks = pretty_breaks(n = 3)) +
  labs(x = "Nº of mentions", y = "Nº of papers") +
  theme_AP() + 
  theme(legend.position = "top", 
        strip.text.x = element_text(size = 8))

plot.keywords.histogram

