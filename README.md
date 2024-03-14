# ASR Word Error Rate Correlation for Applications in Political Advertising: Sample Code

This project contains the code used for this project, but not the data (for data licensing reasons). If you have questions, contact Sam Feuer (sfeuer45@gmail.com).


In this project, we are investigating the differences between transcriptions of political advertisements generated using Automatic Speech Recognition (ASR) and the true transcriptions of these ads. Specifically, we are examining Google's Speech API and its effectiveness on ads from candidates in the 2022 U.S. House midterm elections. We compare hand transcriptions of these ads to Google's automatic transcriptions and test for potential correlation between the word error rate (WER) of these transcriptions and certain candidate- and ad-level characteristics such as candidate race, gender, or incumbency, ad- or campaign-level spend, and whether the candidate is speaking in the ad. Then, we look at the impact of these transcription errors on certain downstream applications of ASR, such as structural topic modeling (STM) and named entity recognition (NER). 

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