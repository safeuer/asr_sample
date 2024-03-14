# ASR Word Error Rate Correlation for Applications in Political Advertising

In this project, we are investigating the differences between transcriptions of political advertisements generated using Automatic Speech Recognition (ASR) and the true transcriptions of these ads. Specifically, we are examining Google's Speech API and its effectiveness on ads from candidates in the 2022 U.S. House midterm elections. We compare hand transcriptions of these ads to Google's automatic transcriptions and test for potential correlation between the word error rate (WER) of these transcriptions and certain candidate- and ad-level characteristics such as candidate race, gender, or incumbency, ad- or campaign-level spend, and whether the candidate is speaking in the ad. Then, we look at the impact of these transcription errors on certain downstream applications of ASR, such as structural topic modeling (STM) and named entity recognition (NER). 

Below is a summary of all of the code and data in this repository, for documentation and replication purposes. If you have questions, contact Sam Feuer (sfeuer@wesleyan.edu).

## Main Directory

### Sampling
- **01_Sample_Generation.R**: Sampling (U.S. house) candidates and ads (on Facebook, with sound, from 09/05-11/08) to hand transcribe, accounting for candidate characteristics.
- **02_Truncate_Videos.py**: Cutting all sampled videos to 2 minutes.
- **03_Find_Similar_Videos.R**: Find ASRs in the sample that were not the same but had at least 0.98 text similarity—we remove all but one of them.

### Data Management
- **04_Fix_2Min_Ads.R**: Standardizing the transcriptions for ads over 2 minutes.
- **05_WER_Calc_Datamanage_PreEdit.R**: Preparing for manual editing of a few hand transcriptions with non-traditional representations of numerals.
- **06_WER_Calc_Datamanage_PostEdit.R**: All necessary data management to calculate word error rate between Google ASR and hand transcriptions.
- **07_Calculate_WER.py**: Calculation of WER (using Levenshtein distance metric).


### Regression & Data Analysis
- **08_WER_Correlation_Datamanage.R**: All necessary data management to perform beta regression with WER as the dependent variable, as well as create a few plots.
- **09_Beta_Regression.R**: Performing beta regression with candidate/ad-level data and WER and predicting WER while varying each of these variables.

### Downstream Applications
- **10_STM_Compare.R**: Creating a structural topic model (STM) with each of the ad- and candidate-level characteristics and WER as predictor variables.
<!-- - **named_entity_rec_wer.py**: Performing named entity recognition (NER) on the hand transcriptions and the Google ASR transcriptions.
- **named_entity_compare.py**: Comparing the recognized entities from named_entity_rec_wer.py using a Levenshtein distance metric. -->
<!-- - **wer_entitysim_correlation.R**: Testing if there is a correlation between WER and Levenshtein distance between recognized entities in Google and hand transcriptions. -->

### Requirements

To be updated.

## Data Directory

### Raw Data
- **asr_fb2022_0905_1108.csv**: ASR Transcriptions of all Google ads captured by WMP with sound from 9/5 to 11/8.
- **ASR_cleaned_1.0_060623.xlsx**: Hand transcriptions of all ads from our sample, with answers to accompanying questions.
- **wmpcand_012523_wmpid.csv**: Candidate characteristics collected by WMP.
- **wmpcand_012523_wmpid_edited.csv**: The above file with the missing race data filled in for 6 candidates in the sample.
- **wmp_fb_2022_entities_v120122.csv**: Facebook page/sponsor level information collected by WMP.
- **Cds_GandFB_thru110822_v011223.csv**: Total campaign-level spend data calculated by WMP, over multiple time periods.
- **cook_competitiveness_scores.xlsx**: Competitiveness scores created by the Cook Political Report for the 2022 US House Midterm elections.

### Sampling
- **sampled_asrs_042723.csv**: Full set of sampled ASRs with candidate-level metadata. 
- **sampled_asrs_042723_dedup.csv**: Sample of ads with near-matches (text-similarity > .98) removed (7 ads removed).
- **sampled_asrs_042723_removed.csv**: The ads removed by the procedure above.

### Data Management
- **asr_over2min.csv**: New transcriptions of ads over 2 minutes from the sample.
- **asr_fb2022_0905_1108_v062923.csv**: The above file with the ads over 2 minutes from our sample edited to have 2 minute transcriptions.
- **transcripts_edited_v062923.csv**: The hand and Google transcripts joined together, with (2) transcripts edited to deal with non-traditional presentations of numbers (fractions and decimals).
- **clean_wordified_transcripts_v062923.csv**: The above file with punctuation removed, letters lowercased, and numbers transformed to words.

### Results
- **transcript_wers_v062923.csv**: Word Error Rates (WERs) between hand and Google transcripts.
- **df_wer_datamanaged_v063023.csv**: Each ad from the sample with WER, survey responses, and metadata.
- More to be added.

## Replication Instructions

https://docs.google.com/document/d/138C06puCSYNTHGtwAI_ZdiySDEX9qzUFlDgpfS_8xIQ/edit?usp=sharing