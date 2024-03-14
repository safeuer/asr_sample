# Alter Google ASR transcriptions and hand transcriptions of advertisements. Consider the fact that numbers may be transcribed by the ASR model as numbers (i.e. 23) or words (i.e. twenty-three). Standardize these to words.
# Then remove punctuation and create new data file with updated transcriptions.

library(readxl)
library(quanteda)
library(dplyr)
library(english)

# Now join in the new ASRs for ads over 2 minutes.

over2_df = read.csv("data/asr_over2min.csv", stringsAsFactors = F)
old_edited_df = read.csv("data/transcripts_edited.csv")
over2_info2 = over2_df %>%
  select(-c("stt_confidence")) %>%
  inner_join(old_edited_df %>% select(-c("google_asr_text")), by=c("ad_id"))

new_edited_df = old_edited_df %>%
  filter(!(ad_id %in% over2_info2$ad_id)) %>%
  rbind(over2_info2)
write.csv(new_edited_df, "data/transcripts_edited_v062923.csv")

# Continue to manage the data.
compare_trans_edited = read.csv("data/transcripts_edited_v062923.csv") %>%
  mutate_at(c("manual_text", "google_asr_text"), as.character)

# Tokenize texts
google_toks = quanteda::tokens(compare_trans_edited$google_asr_text) %>%
  tokens_tolower()
names(google_toks) <- compare_trans_edited$ad_id
hand_toks = quanteda::tokens(compare_trans_edited$manual_text) %>%
  tokens_tolower()
names(hand_toks) <- compare_trans_edited$ad_id

# "Wordify" Google text: removes words with numeric characters and replaces them with their plain English equivalent (i.e. 14th --> fourteenth)
# Also, when combined with numbers, removes "$"s and replaces them with "dollar/dollars" and removes "%"s and replaces them with "percent"
wordified_google_text = data.frame("ad_id" = c(), "google_asr_text_alt" = c())
for (i in 1:length(google_toks)) {
  text_id = names(google_toks)[i]
  curr_tokens = as.character(google_toks[text_id])
  # Detect words with numeric characters in the tokens
  numbers = grepl("[0-9]+", curr_tokens)
  if (length(numbers) > 0) { 
    # For each numeric word...
    for (j in 1:length(numbers)) {
      if (numbers[j] == TRUE) {
        # If it is preceded by a $, replace '$ [number]' with '[number] dollars', with a special case for $1
        if (j > 1 && curr_tokens[j-1] == "$") {
          if (curr_tokens[j] == "1") {
            curr_tokens[j-1] = as.character(english(as.numeric(curr_tokens[j])))
            curr_tokens[j] = "dollar"
          } else {
            curr_tokens[j-1] = as.character(english(as.numeric(curr_tokens[j])))
            curr_tokens[j] = "dollars"
          }
          # If it is followed by a %, replace '[number] %' with '[number] percent'
        } else if (j < length(numbers) && curr_tokens[j+1] == "%") {
          curr_tokens[j] = as.character(english(as.numeric(curr_tokens[j])))
          curr_tokens[j+1] = "percent"
          # If it is an ordinal number with numeric characters (i.e. 1st, 2nd, 3rd, 4th) then it ends with st, nd, rd, or th
          # Then replace it with its ordinal equivalent (i.e. 1st -> first)
        } else if (endsWith(curr_tokens[j], "st") || endsWith(curr_tokens[j], "nd") || endsWith(curr_tokens[j], "rd") 
                   || endsWith(curr_tokens[j], "th")) {
          curr_tokens[j] = as.character(ordinal(as.numeric(gsub("[^0-9]", "", curr_tokens[j]))))
          # If none of these are true, then just replace it with its plain English equivalent (i.e. 11 --> eleven)
        } else {
          curr_tokens[j] = as.character(english(as.numeric(curr_tokens[j])))
        }
      }
    }
    wordified_google_text = rbind(wordified_google_text, data.frame("ad_id" = text_id, "google_asr_text_alt" = paste(curr_tokens, collapse=" ")))
  }
}

wordified_google_text$google_asr_text_alt = as.character(wordified_google_text$google_asr_text_alt)

# Do the same thing to the hand text
wordified_hand_text = data.frame("ad_id" = c(), "manual_text_alt" = c())
for (i in 1:length(hand_toks)) {
  text_id = names(hand_toks)[i]
  curr_tokens = as.character(hand_toks[text_id])
  # Detect words with numeric characters in the tokens
  numbers = grepl("[0-9]+", curr_tokens)
  if (length(numbers) > 0) { 
    # For each numeric word...
    for (j in 1:length(numbers)) {
      if (numbers[j] == TRUE) {
        # If it is preceded by a $, replace '$ [number]' with '[number] dollars', with a special case for $1
        if (j > 1 && curr_tokens[j-1] == "$") {
          if (curr_tokens[j] == "1") {
            curr_tokens[j-1] = as.character(english(as.numeric(curr_tokens[j])))
            curr_tokens[j] = "dollar"
          } else {
            curr_tokens[j-1] = as.character(english(as.numeric(curr_tokens[j])))
            curr_tokens[j] = "dollars"
          }
          # If it is followed by a %, replace '[number] %' with '[number] percent'
        } else if (j < length(numbers) && curr_tokens[j+1] == "%") {
          curr_tokens[j] = as.character(english(as.numeric(curr_tokens[j])))
          curr_tokens[j+1] = "percent"
          # If it is an ordinal number with numeric characters (i.e. 1st, 2nd, 3rd, 4th) then it ends with st, nd, rd, or th
          # Then replace it with its ordinal equivalent (i.e. 1st -> first)
        } else if (endsWith(curr_tokens[j], "st") || endsWith(curr_tokens[j], "nd") || endsWith(curr_tokens[j], "rd") 
                   || endsWith(curr_tokens[j], "th")) {
          curr_tokens[j] = as.character(ordinal(as.numeric(gsub("[^0-9]", "", curr_tokens[j]))))
        } else {
          # If none of these are true, then just replace it with its plain English equivalent (i.e. 11 --> eleven)
          curr_tokens[j] = as.character(english(as.numeric(curr_tokens[j])))
        }
      }
    }
    wordified_hand_text = rbind(wordified_hand_text, data.frame("ad_id" = text_id, "manual_text_alt" = paste(curr_tokens, collapse=" ")))
  }
}

wordified_hand_text$manual_text_alt = as.character(wordified_hand_text$manual_text_alt)

# Tokenize the new, wordified text for both hand and Google and remove punctuation
clean_google_toks = quanteda::tokens(wordified_google_text$google_asr_text_alt, remove_punct = T)
names(clean_google_toks) <- wordified_google_text$ad_id
clean_hand_toks = quanteda::tokens(wordified_hand_text$manual_text_alt, remove_punct = T)
names(clean_hand_toks) <- wordified_hand_text$ad_id

# Repaste the tokens together to recreate the text without the punctuation or other symbols
paste_toks = function(words) {
  return(paste(words, collapse=" "))
}
google_toks2 = sapply(clean_google_toks, paste_toks)
google_toks_df = data.frame("clean_google_trans" = google_toks2, "ad_id" = names(google_toks2))
hand_toks2 = sapply(clean_hand_toks, paste_toks)
hand_toks_df = data.frame("clean_hand_trans" = hand_toks2, "ad_id" = names(hand_toks2))

# Join these new cleaned texts with the original texts
compare_trans2 = compare_trans_edited %>%
  inner_join(google_toks_df, by="ad_id") %>%
  inner_join(hand_toks_df, by="ad_id") %>%
  mutate_at(c("clean_google_trans", "clean_hand_trans"), as.character)

write.csv(compare_trans2, "data/clean_wordified_transcripts_v062923.csv")
