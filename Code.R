# -----------------------------------------------------------
# Finding keyswords in Two decades United Nations General Debate
# -----------------------------------------------------------

# -----------------------------------------------------------
# Setting Enviroment
# -----------------------------------------------------------
# install packages if needed
# install.packages(c("tidyverse", "mclm", "devtools"))

rm(list = ls())

library(devtools)
devtools::install_github("Christine-Z/cufu") # install the self-customized package from Github
library(cufu) # self-customized function to simplify the code
library(tidyverse)
library(mclm)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# -----------------------------------------------------------
# Import and Defining the subcorpora
# -----------------------------------------------------------

# United Nations General Debate from the period 2001-2020
A_fnames <-    get_fnames("Data/2001-2010")
B_fnames <-    get_fnames("Data/2011-2020")

# -----------------------------------------------------------
# building the frequency lists (after dropping stop words)
# [use whitespace as token splitter]
# [drop tokens containing ":", "[", or "]" ] by using a
# customized function cufu_freqlist
# -----------------------------------------------------------

# reading the stop list
stop_list <- read_types("stop_list.txt")

# building the frequency lists using customized function 'cufu_freqlist'

A_flist <- cufu_freqlist(A_fnames,stop_list)%>% print()
B_flist <- cufu_freqlist(B_fnames,stop_list)%>% print()

# Looking for the pattern of '！'

A_fnames %>%
  conc("！") %>%
  print()

B_fnames %>%
  conc("！") %>%
  print()
# -----------------------------------------------------------
# Storing top freq. items in separate objects
# -----------------------------------------------------------
A_top_freq <- A_flist %>%
  drop_types(c("！")) %>%
  drop_types(stop_list) %>%
  keep_pos(1:100) %>%
  as_types() %>%
  print(n = 50)


B_top_freq <- B_flist %>%
  drop_types(c("！")) %>%
  drop_types(stop_list) %>%
  keep_pos(1:100) %>%
  as_types() %>%
  print(n = 50)

# -----------------------------------------------------------
# Comparing the top freq. items from different subcorpora
# -----------------------------------------------------------

# shared by all
A_top_freq %>%
  keep_types(B_top_freq) %>%
  print(n = 50)

# unique to A
A_top_freq %>%
  drop_types(B_top_freq) %>%
  print(n = 50)

# unique to B
B_top_freq %>%
  drop_types(A_top_freq) %>%
  print(n = 50)

# ------------------------------------------------------------------
# Keyword analysis with B as target and A as reference
# ------------------------------------------------------------------

# define target and reference frequency lists
target_flist <- B_flist
ref_flist <- A_flist

# calculate association scores
scores <- assoc_scores(target_flist, ref_flist)

# print scores, sorted by PMI
print(scores, sort_order = "PMI")

# print scores, sorted by G_signed
print(scores, sort_order = "G_signed")

# top_scores
top_scores <- scores %>%
  filter(PMI >= 0.5 & G_signed >= 4)

# print top_scores, sorted by PMI
top_scores %>%
  print(sort_order = "PMI")

# print top_scores, sorted by G_signed
top_scores %>%
  print(sort_order = "G_signed")

B_fnames %>%
  conc(r"--[(?xi) \b socioeconomic \b ]--") %>%
  print(n = 10)

B_fnames %>%
  conc(r"--[(?xi) \b mediation \b ]--") %>%
  print(n = 10)

# copy top_scores to a tibble named top_scores_df
top_scores_df <- as_tibble(top_scores)

# plot PMI by G_signed (without names of the types)
top_scores_df %>%
  ggplot(aes(x = PMI, y = G_signed)) +
  geom_point()

# plot PMI by G_signed (with names of the types)
top_scores_df %>%
  ggplot(aes(x = PMI, y = G_signed)) +
  geom_text(aes(label = type))

# ------------------------------------------------------------------
# Correspondence analysis
# ------------------------------------------------------------------
# get short names
A_short_fnames <- short_names(A_fnames)
B_short_fnames <- short_names(B_fnames)

# -----------------------------------------------------------------------------
# First CA: function words as features
# -----------------------------------------------------------------------------

# set the function words as features

features <- read_types("function-words.txt") %>%
  as_types() %>%
  print()

# create words matrix using customized function 'cufu_funword'

d_A <- cufu_funword(features,A_fnames, A_short_fnames)
d_B <- cufu_funword(features,B_fnames, B_short_fnames)

# perform the correspondence analysis and then we ask for a summary
d_A_ca <- ca(d_A)
d_B_ca <- ca(d_B)

# make preparations for a biplot
texts_df <- cufu_textcoord(d_A_ca, d_B_ca)
words_df <- cufu_wordcoord(d_A_ca, d_B_ca, d_A, d_B)

# biplot, color coding sub_corp
ggplot(words_df, aes(x = x, y = y)) +
  geom_text(aes(label = word), col = "gray") +
  geom_point(data = texts_df, aes(x = x, y = y, col = sub_corp))+
  xlim(-1,1)+ ylim(-1,1)

# -----------------------------------------------------------------------------
# Second CA: top content words
# -----------------------------------------------------------------------------
# retrieve list of high frequency content words
stop_list <- read_types("function-words.txt")

# For the First Decade
features <- freqlist(A_fnames) %>%
  drop_types(stop_list) %>%
  keep_bool(ranks(.) <= 1000) %>%
  as_types()

d_A <- cufu_funword(features,A_fnames, A_short_fnames)

# For the Second Decade
features <- freqlist(B_fnames) %>%
  drop_types(stop_list) %>%
  keep_bool(ranks(.) <= 1000) %>%
  as_types()

d_B <- cufu_funword(features,B_fnames, B_short_fnames)

# conduct CA
d_A_ca <- ca(d_A)
d_B_ca <- ca(d_B)

# make preparations for a biplot
#
texts_df <- cufu_textcoord(d_A_ca, d_B_ca)
words_df <- cufu_wordcoord(d_A_ca, d_B_ca, d_A, d_B)

# biplot, color coding sub_corp
ggplot(words_df, aes(x = x, y = y)) +
  geom_text(aes(label = word), col = "gray") +
  geom_point(data = texts_df, aes(x = x, y = y, col = sub_corp))

# x-axis roughly is the most important First Decade (down) vs. Second Decade
# (up+right) axis So what are the 10 right-most words/items?
words_df %>%
 arrange(desc(y)) %>%
 head(10) %>%
  print()

words_df %>%
  arrange(desc(x)) %>%
  head(10) %>%
  print()

# x-axis roughly is the most important First Decade (right) vs. Second Decade
# (left) axis So what are the 10 left-most words/items?
words_df %>%
  arrange(y) %>%
  head(10) %>%
  print()
