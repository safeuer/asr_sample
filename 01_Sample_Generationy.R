library(dplyr)
library(ggplot2)
library(readxl)
library(caret)
library(quanteda)

# FaceBook ads captured by WMP from 9/5/22 to 11/8/22
df = read.csv("data/asr_fb2022_0905_1108.csv")

# Remove ads with blank ASRs (they have no video, no audio, or there is nothing spoken in the video)
df_asr = df %>% 
  mutate(google_asr_text = trimws(google_asr_text)) %>%
  filter(google_asr_text != "")

# Join in ad-level metadata (spend, sponsor, impressions...)
master = read.csv("data/fb2022_master_0905_1108.csv.gz")
asr_data = df_asr %>%
  inner_join(master, by = c("ad_id"))

# Political entity and candidate-level data obtained by WMP
entities = read.csv("data/wmp_fb_2022_entities_v120122.csv")
candidate_info = read.csv("data/wmpcand_012523_wmpid.csv")

# Join in Cook competitiveness scores for 2022 House midterms
# 2=likely D, 3=lean D, 4=tossup D, 5=tossup R, 6=lean R, 7=likely R, NA=not competitive
comp = read_excel("data/cook_competitiveness_scores.xlsx")
candidate_info = candidate_info %>% left_join(comp, by=c("cand_office_st" = "state", "cand_office_dist"="district"))

# create a summarised competitiveness score that doesn't care about party
# 1 = not competitive, 2 = likely D/R, 3 = lean D/R, 4 = tossup D/R
candidate_info$comp_summarised = ifelse(is.na(candidate_info$comp), 1, ifelse(candidate_info$comp %in% c(2,3,4), candidate_info$comp, 9-candidate_info$comp))
candidate_info$race_comp = ifelse(candidate_info$cand_office == "H", candidate_info$comp_summarised, NA)
candidate_info = candidate_info %>% select(-c("comp", "comp_summarised")) 

# Filter out candidates eliminated before general election and non-federal candidates
gen_elect_candidates = candidate_info %>%
  filter(genelect_cd == 1)
federal_candidates = entities %>%
  inner_join(gen_elect_candidates, by=c("wmpid")) %>% 
  filter(wmp_spontype=="campaign")

# Ads from federal candidates
asr_federal = asr_data %>%
  inner_join(federal_candidates, by = c("pd_id"))

# Ads from house candidates
asr_house = asr_federal %>%
  filter(cand_office == "H")

# Save this file
# write.csv(asr_house, "house_asr_0905_1108.csv")

# Calculate # of distinct ASRs for each house candidate 
house_counts = asr_house %>%
  distinct(google_asr_text, wmpid) %>%
  group_by(wmpid) %>%
  count(sort = TRUE) %>%
  ungroup() %>%
  inner_join(candidate_info, by=c("wmpid"))

# Calculate total # of distinct ASRs
# sum(house_counts$n)

# Remove candidates missing data (most are repeats/candidates who ran for 2 different offices in 2022)
# Then weight candidates to oversample challenger candidates and candidates in open races
house_counts2 = house_counts %>%
  filter(cand_incumbent_challenger_open_s != '') %>%
  filter(gender_wmp %in% c("M", "F")) %>%
  filter(!is.na(race_comp)) %>%
  mutate(weight_challenger = ifelse(cand_incumbent_challenger_open_s == "CHALLENGER", .6, 
                                  ifelse(cand_incumbent_challenger_open_s == "INCUMBENT", .4, .5)))

# Take sample of 200 candidates with above weighting
set.seed(1234)
sample_random_comp = sample.int(nrow(house_counts2), 200, prob=house_counts2$weight_challenger)
sample_rc_df = house_counts2[sample_random_comp,]

# See how many ads we get if we take up to 3 from each candidate (we got 485)
sample_rc_df2 = sample_rc_df %>% 
  mutate(n_alt = ifelse(n < 3, n, 3)) 
sum(sample_rc_df2$n_alt)

# Sample (up to) 3 ASRs from each candidate, and save one ad with that ASR
asr_house_distinct = asr_house %>% 
  distinct(google_asr_text, wmpid, .keep_all=T)
sampled_ad_ids = c() 
for (i in 1:nrow(sample_rc_df2)) {
  id = as.character(sample_rc_df2$wmpid[i]) 
  num_ads_to_sample = as.numeric(sample_rc_df2$n_alt[i]) # Obtain number of ASRs to sample (3 or less)
  temp = asr_house_distinct %>% filter(wmpid == id) # Obtain ASRs from above candidate
  set.seed(1234)
  temp_sample = as.character(sample(temp$ad_id, num_ads_to_sample)) # Sample the correct number of ads
  sampled_ad_ids = c(sampled_ad_ids, temp_sample) # Save the sample ad IDs
}

# Obtain sampled ads
sampled_asrs = asr_house_distinct %>%
  filter(ad_id %in% sampled_ad_ids)
write.csv(sampled_asrs, "sampled_asrs_042723.csv")





