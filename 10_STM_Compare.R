library(dplyr)
library(stm)
library(quanteda)
library(english)
library(ggplot2)
library(grid)

df = read.csv("data/df_wer_datamanaged_v063023.csv", stringsAsFactors = F)
df_wer_spend2 = df %>%
  mutate_at(c("race_wmp", "cand_incumbent_challenger_open_s", "gender_wmp", "candidate_voice", "race_comp", "non_candidate_voice"), as.factor) %>%
  mutate(race_wmp = relevel(race_wmp, "White"),
         cand_incumbent_challenger_open_s = relevel(cand_incumbent_challenger_open_s, "INCUMBENT"),
         gender_wmp = relevel(gender_wmp, "M"),
         candidate_voice = relevel(candidate_voice, "No"),
         cand_party_affiliation = relevel(as.factor(cand_party_affiliation), "DEM"),
         non_candidate_voice = relevel(non_candidate_voice, "No"))

# Select only relevant variables
df_compare = df_wer_spend2 %>%
  select(ad_id, clean_google_trans, clean_hand_trans, race_wmp, cand_incumbent_challenger_open_s, gender_wmp, candidate_voice, non_candidate_voice,
         race_comp, cand_party_affiliation, wmpid, wer, logspend, total_spend_since_010122, total_spend_since_090522, log_total_010122, log_total_090522)

# Tokenize the new, wordified text for both hand and Google and remove punctuation
clean_google_toks = quanteda::tokens(df_wer_spend2$clean_google_trans, remove_punct = T)
names(clean_google_toks) <- df_wer_spend2$ad_id
clean_hand_toks = quanteda::tokens(df_wer_spend2$clean_hand_trans, remove_punct = T)
names(clean_hand_toks) <- df_wer_spend2$ad_id

# Convert to DFM, remove stopwords, and stem
dfm_google = dfm_remove(dfm(clean_google_toks), stopwords("en"))
dfm_hand = dfm_remove(dfm(clean_hand_toks), stopwords("en"))
dfm_google_stem = dfm_wordstem(dfm_google, language="en")
dfm_hand_stem = dfm_wordstem(dfm_hand, language="en")
docvars(dfm_google) <- df_compare %>% select(-c(clean_hand_trans))
docvars(dfm_hand) <- df_compare %>% select(-c(clean_google_trans))
docvars(dfm_google_stem) <- df_compare %>% select(-c(clean_hand_trans))
docvars(dfm_hand_stem) <- df_compare %>% select(-c(clean_google_trans))

# Convert DFM to STM-DFM and remove infrequent words
df_stm_google = convert(dfm_google, to = "stm", docvars = docvars(dfm_google))
df_stm_hand = convert(dfm_hand, to = "stm", docvars = docvars(dfm_hand))
google_prep = prepDocuments(df_stm_google$documents, df_stm_google$vocab, df_stm_google$meta, lower.thresh=3)
hand_prep = prepDocuments(df_stm_hand$documents, df_stm_hand$vocab, df_stm_hand$meta, lower.thresh=3)
df_stm_google_stem = convert(dfm_google_stem, to = "stm", docvars = docvars(dfm_google_stem))
df_stm_hand_stem = convert(dfm_hand_stem, to = "stm", docvars = docvars(dfm_hand_stem))
google_stem_prep = prepDocuments(df_stm_google_stem$documents, df_stm_google_stem$vocab, df_stm_google_stem$meta, lower.thresh=3)
hand_stem_prep = prepDocuments(df_stm_hand_stem$documents, df_stm_hand_stem$vocab, df_stm_hand_stem$meta, lower.thresh=3)

# # search K — determine best using held-out log likelihood + residuals
K_hand = searchK(dfm_hand, K=10:25, N=floor(0.1*nrow(dfm_hand))) 
plot(K_hand) # 14 topics seems best, maintaining high held-out likelihood and coherence while having lower residuals
K_hand_stem_freq = searchK(hand_stem_prep$documents, vocab=hand_stem_prep$vocab, K=10:25) 
plot(K_hand_stem_freq) # 14 also seems best

# Fit STMs for data with no pre-processing
stm_google_pred = stm(dfm_google, K=14, prevalence=~candidate_voice+non_candidate_voice+gender_wmp
                      +race_wmp+cand_party_affiliation+cand_incumbent_challenger_open_s+logspend+race_comp+log_total_090522, seed=123)
stm_hand_pred = stm(dfm_hand, K=14, prevalence=~candidate_voice+non_candidate_voice+gender_wmp
                    +race_wmp+cand_party_affiliation+cand_incumbent_challenger_open_s+logspend+race_comp+log_total_090522, seed=123)

# Fit STMs for data with infrequent words removed
# stm_google_freq_pred = stm(google_prep$documents, vocab=google_prep$vocab, K=12, prevalence=~candidate_voice+non_candidate_voice+gender_wmp
#                            +race_wmp+cand_party_affiliation+cand_incumbent_challenger_open_s+logspend+race_comp+log_total_090522, data=google_prep$meta)
# stm_hand_freq_pred = stm(hand_prep$documents, vocab=hand_prep$vocab, K=12, prevalence=~candidate_voice+non_candidate_voice+gender_wmp
#                          +race_wmp+cand_party_affiliation+cand_incumbent_challenger_open_s+logspend+race_comp+log_total_090522, data=hand_prep$meta)

# Fit STMs for stemmed data with infrequent words removed
stm_google_both_pred = stm(google_stem_prep$documents, vocab=google_stem_prep$vocab, K=14, prevalence=~candidate_voice+non_candidate_voice+gender_wmp
                           +race_wmp+cand_party_affiliation+cand_incumbent_challenger_open_s+logspend+race_comp+log_total_090522, data=google_stem_prep$meta, seed=123)
stm_hand_both_pred = stm(hand_stem_prep$documents, vocab=hand_stem_prep$vocab, K=14, prevalence=~candidate_voice+non_candidate_voice+gender_wmp
                         +race_wmp+cand_party_affiliation+cand_incumbent_challenger_open_s+logspend+race_comp+log_total_090522, data=hand_stem_prep$meta, seed=123)

# Fit STMs for stemmed data with infrequent words removed, using WER as an additional predictor
stm_google_both_all = stm(google_stem_prep$documents, vocab=google_stem_prep$vocab, K=14, prevalence=~wer+candidate_voice+non_candidate_voice+gender_wmp
                           +race_wmp+cand_party_affiliation+cand_incumbent_challenger_open_s+logspend+race_comp+log_total_090522, data=google_stem_prep$meta, seed=123)

# Topic Descriptions
summary(stm_google_pred) # 8: immigration, 12: abortion
# More detailed summary: ??, generic+, inflation/pelosi, police/biden, ??, donate, working, immigration, trump, taxes, generic+, abortion, generic, ??
summary(stm_hand_pred) # 4: abortion (seems more related to certain candidates' messaging), 5: immigration, 12: also abortion
# More detailed summary: ??, inflation/biden, ??, abortion, immigration, donate, centrism?, trump, ??, congress, unity, abortion, election results?, generic+

summary(stm_google_both_pred) # 6: police, 10: abortion, 11: taxes/gas/inflation
# More detailed: small business, generic/trump, ??, american dream, generic, crime, working people, generic, drug prices+immigration, abortion, inflation/taxes/gas, vote, donate, ??
summary(stm_hand_both_pred) # 9: abortion, 12: also abortion, 13: immigration, 14: police/crime
# More detailed summary: small business, ??, gas/economy, vote, american dream, generic, ??, generic, abortion, working, donate, abortion, immigration/border, crime

summary(stm_google_both_all) # 6: police/crime, 10: abortion, 11: inflation/taxes/gas
# very similar to stm_google_both_pred
# More detailed summary: small business, generic/trump, ??, america/american drema, generic/biden, police/crime, fight for policy (healthcare/loans), ??, drug prices/immigration, abortion, inflation/gas/taxecs, vote, donate, ??

# Calculating effects of predictors on issue topic prevalence
set.seed(123) # 8: immigration, 12: abortion
google_effect = estimateEffect(c(8, 12)~candidate_voice+non_candidate_voice+gender_wmp
               +race_wmp+cand_party_affiliation+cand_incumbent_challenger_open_s+logspend+race_comp+log_total_090522, 
               stm_google_pred, metadata = docvars(dfm_google))
summary(google_effect)
# Immigration~cand_party_affiliationREP+**
# Abortion~cand_party_affiliationREP-***, candidate_voiceOnlyEndorsement-*, race_comp4+*

set.seed(123) # 4: abortion (seems more related to certain candidates' messaging), 5: immigration, 12: also abortion
hand_effect = estimateEffect(c(4, 5, 12)~candidate_voice+non_candidate_voice+gender_wmp
                             +race_wmp+cand_party_affiliation+cand_incumbent_challenger_open_s+logspend+race_comp+log_total_090522, 
                             stm_hand_pred, metadata = docvars(dfm_hand))
summary(hand_effect)
# Abortion1~candidate_voiceYes-* & race_wmpBlack+* & cand_party_affiliationREP-* & race_comp4+*
# Immigration~candidate_voiceOnlyEndorsement-* & cand_party_affiliationREP+***
# Abortion2~candidate_voiceOnlyEndorsement-* & race_wmpBlack-* & cand_party_affiliationREP-***

set.seed(1234) # 6: police, 10: abortion, 11: taxes/gas/inflation, 14: drug prices
google_prep_effect = estimateEffect(c(6, 10, 11)~candidate_voice+non_candidate_voice+gender_wmp
                                                    +race_wmp+cand_party_affiliation+cand_incumbent_challenger_open_s+logspend+race_comp+log_total_090522, 
                                                    stm_google_both_pred, metadata = google_stem_prep$meta)
summary(google_prep_effect)
# Police~incumbencyCHALLENGER+*, cand_party_affiliationREP+*, race_comp2+**
# Abortion~cand_party_affiliationREP-***, cand_voiceOnlyEndorsement-*

set.seed(1234) # 9: abortion, 12: also abortion, 13: immigration, 14: police
hand_prep_effect = estimateEffect(c(9, 12, 13, 14)~candidate_voice+non_candidate_voice+gender_wmp
                                                    +race_wmp+cand_party_affiliation+cand_incumbent_challenger_open_s+logspend+race_comp+log_total_090522, 
                                                    stm_hand_both_pred, metadata = hand_stem_prep$meta)
summary(hand_prep_effect)

# Abortion1: non_candidate_voiceYes+*, cand_party_affiliationREP-**
# Abortion2: candidate_voiceOnlyEndorsement-*, race_wmpBlack-*, cand_party_affiliationREP-*
# Police: cand_party_affiliationREP+**, race_comp2+*, race_comp4+**

# In general there aren't a ton of correlations with proportions of "important" topics (those with clear meanings related to messaging)
# Out of these, some trends persist between Google and hand effects:
# Republican ads have much less proportion in abortion and more in police + immigration
# Others are less consistant: people in the most competitive races talking more about abortion, for instance, and spend trends
# However there aren't any clear patterns as to which ones persist and which don't; perhaps because of the amount of randomness involved in STM creation.

# Calculating effects of WER on issue topic prevalence
set.seed(1234) # 6: police, 10: abortion, 11: taxes/gas/inflation
google_all_effect = estimateEffect(c(6, 10, 11)~wer+candidate_voice+non_candidate_voice+gender_wmp
                                                    +race_wmp+cand_party_affiliation+cand_incumbent_challenger_open_s+logspend+race_comp+log_total_090522, 
                                                    stm_google_both_all, metadata = google_stem_prep$meta)
summary(google_all_effect)
# WER has no significant correlations

# Plot WER correlation with issue topic prevalence
jpeg("plots/wer_topic_corrs.jpeg", width=720, height=600)
par(cex=2)
plot.estimateEffect(google_all_effect, covariate="wer", model=stm_google_both_all, topics=c(10),
                    method="continuous", npoints=100, xlim=c(0,0.5), xlab="Word Error Rate", 
                    main = "Word Error Rate vs. Abortion Topic Proportions",
                    labeltype="custom", linecol=c("red"), custom.labels = c("Abortion (abort, right, ban)"))
dev.off()

# Plot effect of WER on party's prediction of abortion/police topic prevalence
jpeg("plots/compare_wer_party_corrs.jpeg", width=720, height=600)
par(col="blue4", lwd=4, col.axis = "black", pch=19, cex=1.5, mar=c(4, 6, 4, 2))
plot.estimateEffect(google_all_effect, covariate="cand_party_affiliation", model=stm_google_both_all, topics=c(6,10),
                    method="pointestimate", labeltype = "custom", custom.labels = c("", "", "", ""),
                    main="Party Proportions in Police and Abortion Topics\n by Inclusion of WER as Predictor",
                    xlab="Mean Proportion in Topic")
par(col="orange", lwd=2, col.axis = "black", pch=20, cex=1.5, mar=c(4, 6, 4, 2))
plot.estimateEffect(google_prep_effect, covariate="cand_party_affiliation", model=stm_google_both_pred, topics=c(6,10),
                    method = "pointestimate", add=TRUE, labeltype="custom", custom.labels = c("DEM Police", "REP Police", "DEM Abortion", "REP Abortion"))
dev.off()

par(col="black", lwd=1, col.axis = "black", pch=19)

# Plot topic proportions
jpeg("plots/google_raw_topics.jpeg", width=720, height=600)
plot(stm_google_pred, type="summary", n=5, main="Topic Proportions for Google ASR Transcriptions")
dev.off()
jpeg("plots/hand_raw_topics.jpeg", width=720, height=600)
plot(stm_hand_pred, type="summary", n=5, main="Topic Proportions for Manual Transcriptions")
dev.off()

# Plot topics with labels
jpeg("plots/google_prepped_topics.jpeg", width=500, height=500)
par(cex=1.5)
plot(stm_google_both_pred, type="labels", n=5, main="Topics for Google ASR Transcriptions",
     topic.names = c("Small Business", "Generic/Trump", "Unclear", "America", "Generic", "Crime", "Working People",
                     "Generic", "Drug Prices/Immigration", "Abortion", "Inflation", "Vote", "Donate", "Unclear"))
dev.off()



# Plot party vs. abortion topic proportion
jpeg("plots/topics_abortion_vs_party_google.jpeg", width=600, height=300)
par(cex=1.5, mar=c(4, 7, 2, 2))
plot.estimateEffect(google_prep_effect, covariate = "cand_party_affiliation", model=stm_google_both_pred,
                                      topics = c(10), method="difference", cov.value1 = "DEM", cov.value2 = "REP", xlim=c(0, 0.2),
                    main="Difference in Proportion of Abortion Topic(s)\nby Party for Google Transcriptions",
                    labeltype = "custom", custom.labels = c("Abortion (abort, right, ban)"), xlab="Proportion More Democratic")
dev.off()

jpeg("plots/topics_abortion_vs_party_hand.jpeg", width=600, height=300)
par(cex=1.5, mar=c(4, 7, 2, 2))
plot.estimateEffect(hand_prep_effect, covariate = "cand_party_affiliation", model=stm_hand_both_pred,
                                    topics = c(9, 12), method="difference", cov.value1 = "DEM", cov.value2 = "REP", xlim=c(0, 0.2),
                    main="Difference in Proportion of Abortion Topic(s)\nby Party for Hand Transcriptions",
                    labeltype = "custom", custom.labels = c("Abortion 1 (abort, ban, right)", "Abortion 2 (take, right, even)"), xlab="Proportion More Democratic")

dev.off()

# Plot party vs. police topic proportion
jpeg("plots/topics_police_vs_party_google.jpeg", width=600)
plot.estimateEffect(google_prep_effect, covariate = "cand_party_affiliation", model=stm_google_both_pred,
                    topics = c(6), method="difference", cov.value1 = "DEM", cov.value2 = "REP", xlim=c(-0.15, 0.05),
                    main="Proportion of Policing Topic(s) by Party for Google Transcriptions",
                    labeltype = "prob", n = 5)
dev.off()

jpeg("plots/topics_police_vs_party_hand.jpeg", width=600)
plot.estimateEffect(hand_prep_effect, covariate = "cand_party_affiliation", model=stm_hand_both_pred,
                    topics = c(14), method="difference", cov.value1 = "DEM", cov.value2 = "REP", xlim=c(-0.15, 0.05),
                    main="Proportion of Policing Topic(s) by Party for Hand Transcriptions",
                    labeltype = "prob", n=5)

dev.off()


# WER effects
# summary(stm_google_both_wer) # 6: police, 10: abortion, 11: inflation/taxes/gas
# set.seed(123)
# google_prep_wer_effect = estimateEffect(c(6, 10, 11)~wer, stm_google_both_wer, metadata = google_stem_prep$meta)
# plot(google_prep_wer_effect, covariate="wer", model = stm_google_both_wer, method="continuous", 
#      main = "Effect of WER on Topic Proportion")
# 
# google_prep_wer_effect$parameters[[1]]

# Semantic Coherence and Exclusivity Analysis
google_cohs = semanticCoherence(stm_google_pred, dfm_google)
google_excs = exclusivity(stm_google_pred)
hand_cohs = semanticCoherence(stm_hand_pred, dfm_hand)
hand_excs = exclusivity(stm_hand_pred)
google_prep_cohs = semanticCoherence(stm_google_both_pred, google_stem_prep$documents)
google_prep_excs = exclusivity(stm_google_both_pred)
hand_prep_cohs = semanticCoherence(stm_hand_both_pred, hand_stem_prep$documents)
hand_prep_excs = exclusivity(stm_hand_both_pred)

google_qual_df = data.frame(trans_type = "google", topic = 1:14, semantic_coherence = google_cohs, exclusivity = google_excs)
hand_qual_df = data.frame(trans_type = "hand", topic = 1:14, semantic_coherence = hand_cohs, exclusivity = hand_excs)
qual_df = rbind(google_qual_df, hand_qual_df)
mean_qual_df = qual_df %>%
  mutate(trans_type = as.factor(trans_type)) %>%
  group_by(trans_type) %>%
  summarize(mean_sem_coh = mean(semantic_coherence), 
            mean_exclusivity = mean(exclusivity))
google_prep_qual_df = data.frame(trans_type = "google_prep", topic = 1:14, semantic_coherence = google_prep_cohs, exclusivity = google_prep_excs)
hand_prep_qual_df = data.frame(trans_type = "hand_prep", topic = 1:14, semantic_coherence = hand_prep_cohs, exclusivity = hand_prep_excs)
prep_qual_df = rbind(google_prep_qual_df, hand_prep_qual_df)
mean_prep_qual_df = prep_qual_df %>%
  mutate(trans_type = as.factor(trans_type)) %>%
  group_by(trans_type) %>%
  summarize(mean_sem_coh = mean(semantic_coherence), 
            mean_exclusivity = mean(exclusivity))


ggplot(data=qual_df, aes(x=semantic_coherence, y=exclusivity, color=trans_type))+
  geom_point()+
  geom_point(data=mean_qual_df, mapping=aes(x=mean_sem_coh, y=mean_exclusivity),size=3, shape=2)


ggplot(data=prep_qual_df, aes(x=semantic_coherence, y=exclusivity, color=trans_type))+
  geom_point()+
  geom_point(data=mean_prep_qual_df, mapping=aes(x=mean_sem_coh, y=mean_exclusivity),size=3, shape=2)+
  labs(x = "Semantic Coherence", y = "Exclusivity", title="Semantic Coherence and Exclusivity of Topics by Transcription Type")+
  scale_color_manual(name="Transcription Type", values = c("blue", "brown"), labels=c("Google ASR", "Manual"))


