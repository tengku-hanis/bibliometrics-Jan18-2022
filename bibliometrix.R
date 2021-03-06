# Bibliometric analysis
# Tengku Hanis (https://tengkuhanis.netlify.app/)
# Jan18, 2022


# Packages ----------------------------------------------------------------

## Install first if not previously installed
install.packages(c("bibliometrix", "igraph"))
install.packages("tidyverse")

## Load packages 
library(bibliometrix)
library(tidyverse)


# Data --------------------------------------------------------------------

## Read multiple files ----

## Unzip the zip files
file_zip <- 
  list.files(getwd(), pattern = "*.zip", full.names = T) %>% 
  unzip()

## Get path to he bib files
file_bib <- list.files(path = getwd(), pattern = "*.bib", full.names = T)

## Read and combine
mbc <- 
  file_bib %>%
  map(convert2df, dbsource = "scopus", format = "bibtex") %>% 
  bind_rows(.id = "file")

## Read one by one ----
mbc1 <- convert2df(file = "mbc1.bib", dbsource = "scopus", format = "bibtex")
mbc2 <- convert2df(file = "mbc2.bib", dbsource = "scopus", format = "bibtex")
mbc_man <- 
  mbc1 %>% 
  bind_rows(mbc2)

# Field tag
?scopusCollection()
?isiCollection()


# NAs ---------------------------------------------------------------------

mbc %>% 
  select(TI, AB) %>% 
  summarise(TI = sum(is.na(TI)), AB = sum(is.na(AB)))

mbc %>% 
  filter(is.na(AB)) %>% 
  select(TI, AB) %>% 
  slice(1:3) 


# Duplicates --------------------------------------------------------------

mbc %>% 
  select(TI, AB) %>%
  summarise(TI = sum(duplicated(TI)), AB = sum(duplicated(AB)))

## Extract all duplicates - TI
mbc[duplicated(mbc$TI) | duplicated(mbc$TI, fromLast = T), "TI"] %>% 
  head()
## Extract one duplicate only - AB
mbc[duplicated(mbc$AB), "TI"] %>% 
  head(1)
  

# Cleaned data ------------------------------------------------------------

mbc2 <- 
  mbc %>% 
  filter(!is.na(TI) | !is.na(AB)) %>% 
  distinct(TI, .keep_all = T) %>% 
  distinct(AB, .keep_all = T)


# Descriptive -------------------------------------------------------------

res <- biblioAnalysis(mbc2) #most of publications and citations related metrics
summary(res, k=10)


# Basic plot --------------------------------------------------------------

p <- plot(res, k=10)
p$MostProdAuthors
p$MostProdCountries
p$AnnualScientProd
p$AverArtCitperYear
p$AverTotCitperYear

## Further edit basic plot ----
p$AverTotCitperYear +
  labs(title = "") +
  theme_bw()

## Manual plot - author per paper ----

### Data for author per paper frequency
no_author <- stringi::stri_count_regex(mbc2$AU, c(";"))
auth_data <- 
  data.frame(paper = mbc2$TI,
             author = mbc2$AU, 
             no_auth = no_author + 1, 
             type = mbc2$DT) %>% 
  group_by(no_auth, type) %>% 
  summarise(freq = n(), .groups = "drop")
auth_data

### Percentage of papers with 10 author or less
sum(auth_data$freq[1:14])/sum(auth_data$freq) * 100

### Plot
auth_data %>% 
  mutate(type = as.factor(type), 
         type = fct_recode(type, 
                           Article = "ARTICLE", 
                           "Conference paper" = "CONFERENCE PAPER",
                           Review = "REVIEW")) %>% 
  filter(!is.na(type)) %>% 
  filter(no_auth < 50) %>% 
  ggplot(aes(no_auth, freq, fill = type)) + 
  geom_bar(stat = "identity") + 
  theme_bw() + 
  xlab("Number of authors") +
  ylab("Frequency") + 
  scale_fill_brewer("Type:") +
  theme(legend.position = "top") #not a good plot for this data


# Funded research ---------------------------------------------------------

table(is.na(mbc2$FU)) %>% 
  prop.table()*100 #20% funded


# Citation related metrics  -----------------------------------------------

## References for first paper
mbc2$CR[1] #separator is ;
res$MostCitedPapers %>% 
  head()

## 1) Frequently cited manuscripts ----
fcr <- citations(mbc2, field = "article", sep = ";")
cbind("frequency" = fcr$Cited[1:5])

## 2) Frequently cited first authors ----
fcfa <- citations(mbc2, field = "author", sep = ";")
cbind("frequency" = fcfa$Cited[1:10])


# Relationship related metrics --------------------------------------------

#Details see ?biblioNetwork

## 1) Collaboration ----

#authors, universities, countries
MT <- metaTagExtraction(mbc2, Field = "AU_CO", sep = ";")
country_collab <- biblioNetwork(MT, analysis = "collaboration",  network = "countries")
summary(networkStat(country_collab))

# Plot
set.seed(123)
ccPlot <- networkPlot(country_collab, n = 30, cluster = "none", #try "optimal"
                      Title = "Countries collaboration", type = "sphere",
                      size.cex = T)

# Further edit the countries names
country_names <- 
  rownames(ccPlot$nodeDegree %>% as.data.frame()) %>% 
  stringr::str_to_title()

country_names[1:30] #should be the same as n in networkPlot()
country_names[1] <- "USA"

# Replace the country names in the plot
library(igraph)
vertex_attr(ccPlot$graph, "label", index = V(ccPlot$graph)) <- country_names[1:30]
plot(ccPlot$graph)

## 2) Co-citation ----

#authors, references, sources
ref_cc <- biblioNetwork(mbc2, analysis = "co-citation", network = "references", sep = ";")

set.seed(123)
networkPlot(ref_cc, n = 30, cluster = "none", 
            Title = "Co-citation of references", type = "circle",
            size.cex = T)

## 3) Coupling ----

#authors, references, sources, countries
auth_couple <- biblioNetwork(mbc2, analysis = "coupling", network = "authors", sep = ";")

set.seed(123)
networkPlot(auth_couple, n = 30, cluster = "none", 
            Title = "Bibliographic coupling of the authors", type = "sphere",
            size.cex = T)

## 4) Co-word analysis ----

#authors, sources, keywords, author_keywords, titles, abstracts
kw_co <- biblioNetwork(mbc2, analysis = "co-occurrences", network = "keywords", sep = ";")

set.seed(123)
networkPlot(kw_co, n = 30, cluster = "none", 
                             Title = "Keyword co-occurrences", type = "fruchterman",
                             size.cex = T)


# Theory related metrics --------------------------------------------------

## 1) Lotka's law ----

L <- lotka(res)

L$AuthorProd #observed distribution of author productivity
L$Beta #beta coeeficient of Lotka's law
L$R2 #GOF of Lotka's law (r^2)

# P value of K-S two sample test
L$p.value #there is a sig diff btwn observed and theoretical distribut.

# Theoretical distribution with Beta = 2
Theoretical <- 10^(log10(L$C)-2*log10(L$AuthorProd[,1]))

# Using ggplot
ldata <- 
  L$AuthorProd %>% 
  bind_cols(theory = Theoretical) %>% 
  pivot_longer(cols = c(Freq, theory), names_to = "distribution", values_to = "val_distr") %>% 
  mutate(distribution = as.factor(distribution), 
         distribution = fct_recode(distribution, Observed = "Freq", Theoretical = "theory"))

ldata %>% 
  ggplot(aes(N.Articles, val_distr, color = distribution)) +
  geom_line() +
  theme_minimal() +
  labs(color = "Distribution:") +
  ylab("Frequency of authors") +
  xlab("Number of articles") +
  theme(legend.position = "top")

## 2) Bradford's law ----

bl <- bradford(mbc2)
bl

# Summary for each zone
bl$table %>% 
  group_by(Zone) %>% 
  summarise(n = n())

# Core journals
bl$table %>% 
  filter(Zone == "Zone 1") %>% 
  select(-SO)


# Miscellaneous metrics ----------------------------------------------------

# Top 100 - smaller data
mbc3 <- 
  mbc2 %>% 
  arrange(desc(TC)) %>% 
  slice(1:100)

## 1) Conceptual structure ----

conceptualStructure(mbc3, field = "ID", stemming = F)

## 2) History network ----

histData <- histNetwork(mbc3, sep = ";")
histPlot(histData)

## 3) Thematic map ----

Map <- thematicMap(mbc2, field = "ID", #"ID","DE", "TI", "AB"
                   minfreq = 3, stemming = FALSE, n.labels = 3, repel = T)
plot(Map$map)

# Further customisation
th_map <- plot(Map$map + 
                 theme_bw() + 
                 theme(axis.line = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(), 
                       axis.ticks = element_blank(),
                       legend.position = "none"))

th_map$layers[[6]] <- NULL #remove logo, specific to this plot
th_map

## 4) Thematic evolution ----

years <- c(2000)
thematicEvolution(mbc2, field = "DE", #"ID","DE", "TI", "AB"
                  years = years, n = 100, minFreq = 3)

## 5) Trending keywords ----

trend_kw <- fieldByYear(mbc2, field = "ID", timespan = c(2010,2019),
                  min.freq = 1, n.items = 5, graph = TRUE) 
trend_kw$graph +
  labs(title = "") +
  theme_bw()

# Another way to plot trending keywords
dat <- trend_kw$df_graph

ggplot(dat, aes(year_med, freq)) + 
  geom_point() +
  ggrepel::geom_text_repel(aes(label = tolower(dat$item)), max.overlaps = 30) +
  scale_x_continuous(breaks = seq(2010, 2019, 1)) +
  theme_bw() +
  xlab("Year") +
  ylab("Frequency")

## 6) Authors' dominance ----

dom <- dominance(res, k=10)
dom
?dominance #detail how dominance factor calculated

## 7) Top-author productivity over time ----

topAU <- authorProdOverTime(mbc2, k=10)
topAU$graph +
  labs(title = "") +
  theme_minimal()

head(topAU$dfAU) #author's productivity per year
head(topAU$dfPapersAU) #author's document list


# Biblioshiny -------------------------------------------------------------

biblioshiny()


