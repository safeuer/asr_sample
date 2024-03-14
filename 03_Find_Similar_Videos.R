# Original File located at https://colab.research.google.com/drive/1W-NMHZY-0C9kwU_twKk0j_fWCzYpT1IZ?usp=sharing

library(quanteda)
library(dplyr)
library(quanteda.textstats)

# Sampled ads (from asr_sample_generation.R)
df = read.csv("data/sampled_asrs_042723.csv")

# Create word-level tokens and create a Document-Term Matrix
tks <- tokens(df$google_asr_text)
dtm <- dfm(tks)

# Calculate text similarities (correlation coefficients) between each pair of ads
sim_mat <- textstat_simil(dtm)
sim_mat2 <- as.matrix(sim_mat)

# Remove self-similarity from consideration
diag(sim_mat2) <- 0

# Identify pairs of ads with over .98 similarity 
# (We assume these are the same ad that were transcribed slightly differently by Google)
sim_mat2[upper.tri(sim_mat2)] <- 0
sim_mat3 = apply(sim_mat2, c(1,2), function(x){if (x>.98) {1} else {0}})

# Remove all but one of the similar ads (removes 7 ads with our data)
similar_ads = data.frame(colsum = colSums(sim_mat3)) %>%
  mutate(found_similar = ifelse(colsum > 0, TRUE, FALSE))
df_similar_removed = df[!similar_ads$found_similar,]
write.csv(df_similar_removed, "data/sampled_asrs_042723_dedup.csv")

removed_csvs = df[similar_ads$found_similar,]
write.csv(removed_csvs, "data/sampled_asrs_042723_removed.csv")

# This process was completed in Google CoLab, so the packages that were installed during its execution are found in the file "03_Similar_Videos_Requirements.txt"

