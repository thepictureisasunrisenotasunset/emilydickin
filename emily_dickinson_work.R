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

# the number of poems written is impressive, but doesn't tell me much- I have no clue whether this file of poems is exhaustive for ED or not (I doubt it)

lpp <- ED %>% group_by(poem) %>% summarize("_lines_" = n()) # a tibble of number of lines per poem
hist(lpp$`_lines_`) # a histogram
boxplot(lpp$`_lines_`) # a boxplot

spp <- ED %>% group_by(poem) %>% summarize("_stanzas_" = max(stanza)) # tibble of number of stanzas per poem
hist(spp$`_stanzas_`) # a histogram
boxplot(spp$`_stanzas_`) # a boxplot

# add graphics
# add exhaustive analysis of combinations of variables and significance
# add analysis of possible significance of outliers