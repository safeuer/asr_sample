library(dplyr)

# DF with old transcriptions for >2 minute videos
old_df = read.csv("data/asr_fb2022_0905_1108.csv", stringsAsFactors = F)

# DF with new transcriptions for >2 minute videos
over2_df = read.csv("data/asr_over2min.csv", stringsAsFactors = F)

# Join the metadata from the old DF into the new DF
over2_df_info = over2_df %>%
  select(-c("stt_confidence")) %>%
  inner_join(old_df %>% select(-c("google_asr_text")), by=c("ad_id"))

# Replace the old transcript rows with the new one and save the new DF
new_df = old_df %>%
  filter(!(ad_id %in% over2_df_info$ad_id)) %>%
  rbind(over2_df_info)
#write.csv(new_df, "data/asr_fb2022_over2_edited.csv")


# Do the same thing with our edited transcript files
old_edited_df = read.csv("data/transcripts_edited.csv")
over2_info2 = over2_df %>%
  select(-c("stt_confidence")) %>%
  inner_join(old_edited_df %>% select(-c("google_asr_text")), by=c("ad_id"))

new_edited_df = old_edited_df %>%
  filter(!(ad_id %in% over2_info2$ad_id)) %>%
  rbind(over2_info2)
write.csv(new_edited_df, "data/transcripts_edited_v062923.csv")
