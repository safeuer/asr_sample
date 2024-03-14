# Alter Google ASR transcriptions and hand transcriptions of advertisements. Consider the fact that numbers may be transcribed by the ASR model as numbers (i.e. 23) or words (i.e. twenty-three). Standardize these to words.
# Then remove punctuation and create new data file with updated transcriptions.

library(readxl)
library(quanteda)
library(dplyr)
library(english)


# Hand transcriptions of sampled ASRs
hand_transcripts = read_xlsx("data/ASR_cleaned_1.0_060623.xlsx") %>%
  rename(c("manual_text" = "Q3", "non_english" = "Q4", "candidate_voice" = "Q5", "non_candidate_voice" = "Q6"))

# Google transcriptions
google_transcripts = read.csv("data/asr_fb2022_0905_1108_v062923.csv") %>%
  mutate_at(c("google_asr_text", "example_video"), as.character)

# Join datasets together
compare_transcripts = hand_transcripts %>%
  rename("ad_id" = "adid") %>%
  inner_join(google_transcripts, by=c("ad_id"))


# Detect fractions, decimals, dollars/cents, and dates in the Google ASR text (so we can manually change them)
google_fracs = compare_transcripts %>%
  filter(grepl("[0-9]+\\/[0-9]+", google_asr_text))
google_decimals = compare_transcripts %>%
  filter(grepl("[0-9]+\\.[0-9]+", google_asr_text))

write.csv(compare_transcripts, "data/transcripts_for_manual_editing.csv")

# At this point, go edit the transcripts. Name the edited file "transcripts_edited.csv." Afterwards, go to wer_calc_datamanage_postedit.R.