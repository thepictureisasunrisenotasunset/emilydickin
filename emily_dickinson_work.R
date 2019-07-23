library(tidyverse)


# importing and initializing data ------------------------------
emily <- read_file("emily_dickinson_titleless.txt")
str(emily) # a character vector, 1 entry

poems <- unlist(str_split(emily, "\r\n\r\n\r\n"))
poems # a character vector, 356 entries representing 356 poems

ED <- data.frame(poem = poems, stringsAsFactors = FALSE) # creation of the main data frame



# rough structuring of data --------------------------------------
a <- list()
for (i in 1:356) {
  a[i] <- str_split(poems[i], "\r\n\r\n")
}
a # a list of poems; each poem is a vector of stanzas 

ED <- ED %>% mutate(stanza = a) # this creates a list-column
ED <- ED %>% unnest() # before, a row was a particular poem and an associated list of stanzas; now, a row is a particular stanza in a poem

b <- list()
dim(ED) # there are 909 rows
for(i in 1:909) {
  b[i] <- str_split(ED[i,2], "\r\n")
}
b # a list of stanzas; each stanza is a vector of lines

ED <- ED %>% mutate(line = b) # another list column has been created
ED <- ED %>% unnest() # now a row represents a particular line in a particular stanza in a particular poem



# refining our structuring of the data -------------------------------
c <- unique(ED$poem)
for (i in 1:4149) {
  for (j in 1:356) {
    if (identical(ED$poem[i], c[j])) {
      ED$poem[i] <- j
    }
  }
}
ED <- ED %>% mutate(poem = as.integer(poem))
# here, we convert the poem column into an integer column

for (i in 1:356){
  
  # gives a subset of ED, but still a data frame
  tmp <- ED %>% filter(poem == i)
  
  # gives the number of stanzas in the ith poem
  len <- length(a[[i]])
  
  # gives the number of observations/lines in tmp
  lin <- (dim(tmp))[1]
  
  for (j in 1:lin) {
    for (k in 1:len) {
      if (tmp$stanza[j] == a[[i]][k]) {
        # change value of data frame
        tmp$stanza[j] <- k
      }
    }
  }
  
  # concatenates the partial data.frames on top of each other
  if (i == 1) {
    prior <- tmp
  } else {
  prior <- rbind(prior, tmp)
  }
}
ED <- prior %>% mutate(stanza = as.integer(stanza))
# here, we convert the stanza column into an integer column

ED # the final data frame, now in a representational format that is easy to work with



# Exploring the Data -------------------------------------------
View(ED)

# the number of poems (n = 356) written is impressive, but doesn't tell us much- I have no
# clue whether this file of poems is exhaustive for ED or not (I doubt it)

# we can look at:
# (1) stanzas per poem
# (2) lines per poem
# (3) lines per stanza
# (4) finally, from the content and structure of the words used (this is NLP)

# (1)
spp <- ED %>% group_by(poem) %>% summarize(S = max(stanza)) # tibble of number of stanzas per poem
hist(spp$S) # a histogram
boxplot(spp$S) # a boxplot

# (2)
lpp <- ED %>% group_by(poem) %>% summarize(L = n()) # a tibble of number of lines per poem
hist(lpp$L) # a histogram
boxplot(lpp$L) # a boxplot

# (3)
lps <- ED %>% group_by(poem, stanza) %>% summarize(L = n())
hist(lps$L) # a histogram
boxplot(lps$L) # a boxplot


# We can make these graphs nicer by using ggplot2:

# histogram of stanzas per poem
moder <- function(x) {
  as.double(names(sort(table(x))[length(table(x))]))
} # this defines a mode function since mode() has a different meaning in base R

ggplot(spp) +
  geom_histogram(mapping = aes(spp$S), bins = max(spp$S)) +
  labs(title = c("Distribution of stanzas per poem"), subtitle = paste("mean: ", round(mean(spp$S), digits = 2), " ; median: ", median(spp$S), " ; mode: ", moder(spp$S)), x = "stanzas per poem") +
  scale_x_continuous(breaks = 1:12)

# boxplot of stanzas per poem
is.outlier <- function(x) {
  a <- (x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
  return(a)
} # returns a logical vector with TRUE cooresponding to the indices of outliers in x
spp <- spp %>% mutate(outliers = ifelse(is.outlier(S), poem, NA)) # creates a column of outliers and NAs that can be usefully passed to ggplot
# NOTE: this method inspired by user JasonAizkalns on https://stackoverflow.com/questions/33524669/labeling-outliers-of-boxplots-in-r
library(ggrepel) # we need this package to use the geom_label_repel() function
spp %>% filter(!(is.na(outliers))) %>% dim() %>% .[1] # there are 8 outliers

ggplot(spp) +
  geom_boxplot(mapping = aes(x = "", y = spp$S), outlier.color = "red") +
  scale_y_continuous(breaks = 2*(1:6)) +
  labs(x = "", y = "count", title = c("Boxplot distribution of stanzas per poem")) +
  geom_label_repel(mapping = aes(x = "", y = spp$S, label = outliers), na.rm = TRUE, min.segment.length = 0)



# histogram of lines per poem
ggplot(lpp) +
  geom_histogram(mapping = aes(lpp$L), bins = max(lpp$L)) +
  labs(title = c("Distribution of lines per poem"), subtitle = paste("mean: ", round(mean(lpp$L), digits = 2), " ; median: ", median(lpp$L), " ; mode: ", moder(lpp$L)), x = "lines per poem") +
  scale_x_continuous(breaks = 2*(1:25))

# boxplot of lines per poem
lpp <- lpp %>% mutate(outliers = ifelse(is.outlier(L), poem, NA))
lpp %>% filter(!(is.na(outliers))) %>% dim() %>% .[1] # there are 8 outliers

ggplot(lpp) +
  geom_boxplot(mapping = aes(x = "", y = lpp$L), outlier.color = "red") +
  scale_y_continuous(breaks = 2*(1:25)) +
  labs(x = "", y = "count", title = c("Boxplot distribution of lines per poem")) +
  geom_label_repel(mapping = aes(x = "", y = lpp$L, label = outliers), na.rm = TRUE, min.segment.length = 0)



# histogram of lines per stanza
ggplot(lps) +
  geom_histogram(mapping = aes(lps$L), bins = max(lps$L)) +
  labs(title = c("Distribution of lines per stanza"), subtitle = paste("mean: ", round(mean(lps$L), digits = 2), " ; median: ", median(lps$L), " ; mode: ", moder(lps$L)), x = "lines per stanza") +
  scale_x_continuous(breaks = 1:16)

# boxplot of lines per stanza
lps <- ungroup(lps) %>% mutate(outliers = ifelse(is.outlier(lps$L), paste("p", poem, ",", "s", stanza), NA_character_))
lps %>% filter(!(is.na(outliers))) %>% dim() %>% .[1]
# there are 222 outliers here, so annotating the outliers of this graph will not help

ggplot(lps) +
  geom_boxplot(mapping = aes(x = "", y = lps$L), outlier.color = "red") +
  scale_y_continuous(breaks = 1:16) +
  labs(x = "", y = "count", title = c("Boxplot distribution of lines per stanza"))










# add graphics
# add analysis of possible significance of outliers
# possible NLP analysis in the future
# initial conclusions --------------------
# 