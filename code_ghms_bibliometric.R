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
  "qdapRegex"))

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
          legend.box.margin=margin(0.2,-2,-7,-7))
}

# Set checkpoint
dir.create(".checkpoint")
library("checkpoint")

checkpoint("2022-05-11", 
           R.version ="4.2.0", 
           checkpointLocation = getwd())


## ----vector_models--------------------------------------------------

# VECTOR WITH NAME OF MODELS ---------------------------------------------------

models <- c("WaterGAP", "PCR-GLOBWB", "MATSIRO", "H08", "JULES-W1", "MPI-HM", 
            "MHM", "LPJmL", "CWatM", "CLM", "DBHM", "ORCHIDEE")

models_vec <- paste(models, "_ref.bib", sep = "")


## ----bibliometric, results="hide", message=FALSE, dependson="vector_models"----

# RUN FOR LOOP -----------------------------------------------------------------
output <- results <- years <- journals <- dt <- list()

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
  
  # Retrieve analysis bibliometrix ---------------------------------------------
  results[[i]] <- biblioAnalysis(output[[i]], sep = ";")
  years[[i]] <- data.table(results[[i]]$Years)
  journals[[i]] <- data.table(results[[i]]$Sources) %>%
    .[, SO:= str_to_title(SO)] 
}

names(years) <- models
names(journals) <- models
names(dt) <- models


## ----arrange_data, dependson="bibliometric"-------------------------

# ARRANGE DATA -----------------------------------------------------------------

# Correct for USA and China
colsName <- c("country.first", "country.last") 
full.dt <- rbindlist(dt, idcol = "Model") %>%
  .[, (colsName):= lapply(.SD, function(x) 
    ifelse(grepl("USA", x), "USA", x)), .SDcols = colsName] %>%
  .[, (colsName):= lapply(.SD, function(x) 
    ifelse(grepl("CHINA", x), "CHINA", x)), .SDcols = colsName]


########################################
########################################

### WORK IN PROGRESS -----------------------------------------------------------

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

# Apply
selected_cols <- c("title", "abstract", "keywords", "keywords.plus")
full.dt[, (selected_cols):= lapply(.SD, function(x)
  clear_text(x)), .SDcols = selected_cols]

# Calculate total number of papers per Model
total.papers.dt <- full.dt[, .(Model, WOS)] %>%
  .[, .(total.papers = .N), Model]

# CREATE WORDCLOUD OF WORDS IN ABSTRACT-------------------------------------------------------

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

# CHECK RANK OF THE TERMS "UNCERTAINTY" AND "SENSITIVITY" ----------------------

keywords_vec <- c("uncertainty", "sensitivity")

word.count.dt[, rank:= frank(-freq, ties.method = "first"), Model]

rank.keywords <- word.count.dt[word %in% keywords_vec] %>%
  merge(., total.papers.dt, by = "Model") %>%
  .[, fraction:= freq / total.papers]

rank.keywords

# Plot-------------------------------------------

ggplot(rank.keywords, aes(reorder(Model, -rank), rank, color = word)) +
  geom_point() + 
  coord_flip() +
  labs(x = "", y = "Rank") +
  scale_color_discrete(name = "Keywords", 
                       labels = c("Sensitivity", "Uncertainty")) +
  theme_AP() +
  theme(legend.position = "top")

# SENTIMENT ANALYSIS -----------------------------------------------------------

da <- full.dt[, get_nrc_sentiment(abstract), Model]
colNames_sentiments <- colnames(da)[-1]
da[, colSums(.SD), .SDcols = colNames_sentiments, by]

da[, lapply(.SD, sum), by = Model, .SDcols = colNames_sentiments] %>%
  melt(., measure.vars = colNames_sentiments) %>%
  ggplot(., aes(value, variable, fill = variable)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~Model, scales = "free_x") + 
  theme_AP()

# N-TOKENS --------------------------------------------------------------------


















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

