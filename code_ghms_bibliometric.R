

# PRELIMINARY FUNCTIONS ----------------------------------------------------------

# Function to read in all required packages in one go:
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
  "openxlsx", "cowplot", "wesanderson", "stringr"))

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
          legend.box.margin=margin(0.2,-7,-7,-7))
}

# START ANALYSIS ---------------------------------------------------------------

# Vector with names of models -------------------
models <- c("WaterGAP", "PCR-GLOBWB", "MATSIRO", "H08", "JULES-W1", "MPI-HM", 
            "MHM", "LPJmL", "CWatM", "CLM", "DBHM", "ORCHIDEE")

models_vec <- paste(models, "_ref.bib", sep = "")

# RUN FOR LOOP -----------------------------------------------------------------
output <- results <- years <- journals <- full.dt <- list()

for (i in 1:length(models_vec)) {
  
  output[[i]] <- convert2df(file = models_vec[i], 
                            dbsource = "wos", 
                            format = "bibtex")
  
  # Extract Authors, Countries and Universities --------------------------------
  
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
  
  # Create data.table -----------------------------------------------------------
  full.dt[[i]] <- data.table("WOS" = output[[i]]$UT, 
                             "year" = output[[i]]$PY,
                             "first.author" = first.author, 
                             "last.author" = last.author, 
                             "country.first" = country.first, 
                             "country.last" = country.last, 
                             "university.first" = university.first, 
                             "university.last" = university.last) 
  
  # Retrieve analysis bibliometrix ---------------------------------------------
  results[[i]] <- biblioAnalysis(output[[i]], sep = ";")
  years[[i]] <- data.table(results[[i]]$Years)
  journals[[i]] <- data.table(results[[i]]$Sources) %>%
    .[, SO:= str_to_title(SO)] 
}



# PLOT ------------------------------------------------------------------------

names(years) <- models
names(journals) <- models
names(full.dt) <- models

rbindlist(full.dt, idcol = "Model") %>%
  openxlsx::write.xlsx(., "full.dt.xlsx")

tmp <- rbindlist(years, idcol = "Model")[, .N, .(V1, Model)]

# Total number of studies ----------
tmp[, sum(N)]

plot.time <- tmp %>%
  .[, V1:= as.factor(V1)] %>%
  ggplot(., aes(V1, N, fill = Model)) +
  geom_col() +
  scale_x_discrete(breaks = pretty_breaks(n = 4)) +
  labs(x = "Year", y = "Nº articles") +
  theme_AP()

plot.time

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







rbindlist(journals, idcol = "Model") %>%
  .[, .N, .(Model, V1)] %>%                             
  arrange(desc(N)) %>% 
  group_by(Model) %>%
  slice(1:5) %>%
  print(n = Inf)


  .[order(.$V1, decreasing = TRUE), ]
  .[, head(.SD, 5), Model]



data_new3 <- da[order(da$V1, decreasing = TRUE), ]  # Top N highest values by group
data_new3 <- data.table(data_new3, key = "Model")
data_new3 <- data_new3[ , head(.SD, 3), by = Model]
data_new3

# KEYWORDS SEARCH --------------------------------------------------------------

directory <- "/Users/arnaldpuy/Documents/papers/ghms_bibliometric/"
directory_vec <- paste(directory, models, "_pdfs", sep = "") 
filename_keywords <- paste(models, "keywords", sep = "_")

dt <- result <- list()
for (i in 1:length(directory_vec)) {
  
  result[[i]] <- keyword_directory(directory_vec[i],
                              keyword = c("sensitivity analysis", 
                                          "uncertainty analysis", 
                                          "uncertainty"),
                              split_pdf = TRUE)
  
  dt[[i]] <- data.table("name" = result[[i]]$pdf_name, 
                        "keyword" = result[[i]]$keyword, 
                        "text" = result[[i]]$line_text)
  
  fwrite(dt[[i]], file = paste(filename_keywords[i], ".csv", sep = ""))
  
}

names(result) <- models
names(dt) <- models

# PLOT FREQUENCY OF KEYWORDS --------------------------------------------------

dt.keywords <- rbindlist(dt, idcol = "Model") %>%
  .[, .N, .(Model, name, keyword)] %>%
  .[, keyword:= str_to_title(keyword)]

dt.keywords %>%
  ggplot(., aes(N, fill = keyword, color = keyword)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  facet_wrap(~Model, ncol = 6) +
  scale_y_continuous(breaks = pretty_breaks(n = 2)) +
  scale_x_continuous(breaks = pretty_breaks(n = 3)) +
  labs(x = "Nº of mentions", y = "Nº of papers") +
  theme_AP() + 
  theme(legend.position = "top")

total.studies <- tmp[, sum(N), Model]
setnames(total.studies, "V1", "Total")

new.colnames <- c("N with keywords", "N Total")

dt.bars <- dt.keywords[, unique(name), Model] %>%
  .[, .N, Model] %>%
  merge(., total.studies, by = "Model") %>%
  setnames(., c("N", "Total"), new.colnames)
  
plot.bars <- melt(dt.bars, measure.vars = new.colnames) %>%
  ggplot(., aes(reorder(Model, value), value, fill = variable)) +
  coord_flip() +
  labs(y = "Nº of articles", x = "") +
  scale_fill_manual(values = wes_palette(2, name = "Darjeeling1"), 
                    name = "") +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6)) +
  theme_AP() +
  theme(legend.position = c(0.55, 0.25))

plot_grid(plot.time, plot.bars, ncol = 2, labels = "auto",
          rel_widths = c(0.55, 0.45))















blabla <-  keyword_directory(directory_vec[1],
                           keyword = c("sensitivity analysis", 
                                       "uncertainty analysis", 
                                       "uncertainty"),
                           split_pdf = TRUE)

fwrite(blabla, "bla.csv")

data.table(blabla)[, .(pdf_name, keyword, line_text)] %>%
  write.xlsx(., "bla.xlsx")


str(blabla)


my.corpus = load.corpus(corpus.dir = "/Users/arnaldpuy/Documents/papers/ghms_bibliometric/CWatM_pdfs",
                        files = c("000496780900039.pdf", "000552273100001.pdf") )


convertpdf2txt <- function(dirpath){
  files <- list.files(dirpath, full.names = T)
  x <- sapply(files, function(x){
    x <- pdftools::pdf_text(x) %>%
      paste0(collapse = " ") %>%
      stringr::str_squish()
    return(x)
  })
}

txts <- convertpdf2txt(here::here("/Users/arnaldpuy/Documents/papers/ghms_bibliometric/CWatM_pdfs"))
rm(txts)

dt[[1]]

# Directory containing all files
parent.folder<-"/Users/arnaldpuy/Documents/papers/ghms_bibliometric/WaterGAP_pdfs2"

# Return a list of the existing PDF names in parent.folder:
list_of_files <- list.files(parent.folder, 
                            pattern = "*.pdf", full.names = TRUE)
# Rename all files
for(i in 1:length(list_of_files)){
  file.rename(list_of_files[i], paste0("Watergap", i, ".pdf"))
}


didi <- keyword_directory(directory_vec[9],
                  keyword = c("sensitivity analysis", 
                              "uncertainty analysis", 
                              "uncertainty"),
                  split_pdf = TRUE)

didi$



dt[[1]]


result[[1]]$token_text



result <- keyword_directory(directory_vec[10],
                                 keyword = c("sensitivity analysis", 
                                             "uncertainty analysis"),
                                 split_pdf = TRUE)

unique(data.table("name" = result$pdf_name, 
                  "keyword" = result$keyword)) %>%
  write.xlsx(., "PCR-GLOBWB_uncertain.xlsx")

















output <- convert2df(file = "lpjml.txt", 
                          dbsource = "wos", 
                          format = "plaintext")

results <- biblioAnalysis(output, sep = ";")
years[[i]] <- data.table(results[[i]]$Years)

results$

output$CR



da <- biblioNetwork(output[[1]], analysis = "coupling", network = "references")
networkPlot(da, n = 10)


















library("bib2df")

da <- bib2df("lpjml_ref.bib")
colnames(da) <- tolower(colnames(da))

da %>%
  select(author, affiliations) %>%
  separate_rows(author, sep = " ")

str(da$author)

da$abstract

lapply(da$author, function(x) gsub("[[:punct:]]*and[[:punct:]]*", "", x))








gsub("[[:punct:]]*span[[:punct:]]*", "", words)


removeWords(da$author, "and")

da$author
da$affiliations

clean_names

da %>% 
  janitor::clean_names() %>% 
  .$author
  select_if(~!all(is.na(.))) %>%  # removing all empty columns
  mutate(id_ref = 1:n()) %>% 
  select(-c(citation_number, arxiv, director, source))

da$CITED.REFERENCES


library(bibliometrixData)
# Example 1: Term extraction from titles

data(scientometrics, package = "bibliometrixData")

# vector of compound words
keep.terms <- c("co-citation analysis","bibliographic coupling")

# term extraction
scientometrics <- bibliometrix::termExtraction(scientometrics, Field = "TI", ngrams = 1,
                                 remove.numbers=TRUE, remove.terms=NULL, keep.terms=keep.terms, verbose=TRUE)

# terms extracted from the first 10 titles
scientometrics$TI_TM[1:10]


#Example 2: Term extraction from abstracts

data(scientometrics)

# term extraction
scientometrics <- bibliometrix::termExtraction(scientometrics, Field = "AB", ngrams = 2, 
                                 stemming=TRUE,language="english",
                                 remove.numbers=TRUE, remove.terms=NULL, keep.terms=NULL, verbose=TRUE)

# terms extracted from the first abstract
scientometrics$AB_TM[1]

# Example 3: Term extraction from keywords with synonyms

data(scientometrics)

# vector of synonyms 
synonyms <- c("citation; citation analysis", "h-index; index; impact factor")

# term extraction
scientometrics <- termExtraction(scientometrics, Field = "ID", ngrams = 1,
                                 synonyms=synonyms, verbose=TRUE)











M <- convert2df(file = "references_test.bib", 
                dbsource = "wos", 
                format = "bibtex")

M$AU

results <- biblioAnalysis(M, sep = ";")
results$CO %>%
  data.table(.) %>%
  .[, .N, .]


data.table(results$FirstAffiliation)
data.table(results$Affiliations)

data.table(results$Years)[, .N, V1] %>%
  ggplot(., aes(V1, N, fill = fct_rev(Years))) +
  geom_bar()

S <- summary(object = results, k = 10, pause = FALSE)
plot(x = results, k = 10, pause = FALSE)


S$MainInformationDF

results$Authors











install.packages("pdftools")
install.packages("pdfsearch")

library(pdfsearch)










result$ID

/Users/arnaldpuy/Documents/papers/ghms_bibliometric/CLM_pdfs

result <- keyword_search(file, 
                         keyword = c('measurement', 'error'),
                         path = TRUE)






library(bibliometrix)
library(bib2df)
install.packages('RefManageR')



library(igraph)

library(bibtex)


df <- bib2df("watergap_ref.bib")
mybib <- RefManageR::ReadBib("watergap_ref.bib", check = FALSE) 
data.table::fwrite(data.table::data.table(df$UNIQUE.ID), "watergap_id.csv")













df$AUTHOR <- vapply(df$AUTHOR, paste, collapse = ' and ', '')
df$EDITOR <- vapply(df$EDITOR, paste, collapse = ' and ', '')

dat <- read.table(text="A B 
D23 A76
C34 T43", header=TRUE)

da <- get.adjacency(graph.edgelist(as.matrix(dat), directed=FALSE))

as.matrix(da)

dat <- read.table(text="A B 
1 2
1 3
1 4
2 5
3 7", header=TRUE)

x <- table(dat)
x %*% t(x)















library(bibliometrix)


results <- biblioAnalysis(M, sep = ";")
plot(results, k = 10)

# Citations
M$CR[1]
# For papers
CR <- citations(M, field = "article", sep = ";")
cbind(CR$Cited[1:10])

topAU <- authorProdOverTime(M, k = 10, graph = TRUE)

A <- cocMatrix(M, Field = "SO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
netstat <- networkStat(NetMatrix)
summary(netstat, k=10)

# Create a country collaboration network
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", 
                           network = "countries", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], 
                Title = "Country Collaboration", 
                type = "circle", size=TRUE, 
                remove.multiple=FALSE,
                labelsize=0.7,
                cluster="none")



# Create keyword co-occurrences network

NetMatrix <- biblioNetwork(M, analysis = "co-occurrences",
                           network = "keywords", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, normalize="association",
                weighted=T, n = 30, 
                Title = "Keyword Co-occurrences", 
                type = "fruchterman", 
                size=T,edgesize = 5,labelsize=0.7)



# Create a historical citation network
options(width=130)
histResults <- histNetwork(M, min.citations = 1, sep = ";")
library(bibliometrix)



# Bibliometrix analysis
results <- biblioAnalysis(M, sep = ";")

results$Years
plot(results, k = 10, pause = FALSE)
CR <- citations(M, field = "article", sep = ";")


NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
net=networkPlot(NetMatrix, n = 30, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5)


histResults <- histNetwork(M, min.citations = 4, sep = ";")












# EXTRACT AUTHORS' NAME --------------------------------------------------------

tmp.authors <- output[[1]]$AU 
first.author <- sub(" *\\;.*", "", tmp.authors)
last.author <- sub('.*\\;', '', tmp.authors)

# EXTRACT COUNTRIES AND UNIVERSITIES -------------------------------------------



country.first <- sub('.*\\,', '', output[[1]]$RP)
university.first <- sub(" *\\;.*", "", output[[1]]$affiliations)

last.affiliation <- sub('.*\\;', '', output[[1]]$C1)
country.last <- sub("\\.", "", sub(".*\\, ", "", last.affiliation))
university.last <- sub('.*\\;', '', output[[1]]$affiliations)

full.dt <- data.table("WOS" = output[[1]]$UT, 
                      "first.author" = first.author, 
                      "last.author" = last.author, 
                      "country.first" = country.first, 
                      "country.last" = country.last, 
                      "university.first" = university.first, 
                      "university.last" = university.last) 




sub(" *\\;.*", "", output[[1]]$affiliations)

sub('.*\\;', '', output[[1]]$affiliations)


output[[1]]$affiliations
