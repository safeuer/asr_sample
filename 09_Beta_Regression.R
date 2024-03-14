# Original file can be found here: https://colab.research.google.com/drive/1xK_8wo9EKeGuRruMKGbyTfbUGuR89G-6?usp=sharing 

library(glmmTMB)
library(ggeffects)
library(dplyr)
library(ggplot2)
library(scales)

# Make sure reference levels are correct
df = read.csv("data/df_wer_datamanaged_v063023.csv", stringsAsFactors = F)
df_wer_spend2 = df %>%
  mutate_at(c("race_wmp", "cand_incumbent_challenger_open_s", "gender_wmp", "candidate_voice", "race_comp", "non_candidate_voice"), as.factor) %>%
  mutate(race_wmp = relevel(race_wmp, "White"),
         cand_incumbent_challenger_open_s = relevel(cand_incumbent_challenger_open_s, "INCUMBENT"),
         gender_wmp = relevel(gender_wmp, "M"),
         candidate_voice = relevel(candidate_voice, "No"),
         cand_party_affiliation = relevel(as.factor(cand_party_affiliation), "DEM"),
         non_candidate_voice = relevel(non_candidate_voice, "No"))

# 0 and 1 are invalid outputs for beta regression, so change them to 0.0001 and 0.9999
df_wer_spend2$wer[df_wer_spend2$wer == 0] <- 0.0001
df_wer_spend2$wer[df_wer_spend2$wer == 1] <- 0.9999

# Perform beta regression
wer_beta = glmmTMB(wer~race_wmp+cand_incumbent_challenger_open_s+gender_wmp+race_comp+cand_party_affiliation+logspend+log_total_090522
                   +candidate_voice+non_candidate_voice+(1|wmpid),
                   data = df_wer_spend2, family = beta_family())
summary(wer_beta)

# Interpreting coefficients
# Predict values of wer for different values of each of these predictor variables (given all other variables are at reference level/mean)
ggpredict(wer_beta, "race_wmp")
ggpredict(wer_beta, "cand_incumbent_challenger_open_s")
ggpredict(wer_beta, "race_comp")
ggpredict(wer_beta, "cand_party_affiliation")
ggpredict(wer_beta, "logspend")
ggpredict(wer_beta, "candidate_voice")
ggpredict(wer_beta, "non_candidate_voice")

# Plots
ggplot(data=df_wer_spend2, aes(x=cand_party_affiliation, y=wer))+
  geom_boxplot()

spend_pred = as.data.frame(ggpredict(wer_beta, terms="logspend"))
spend_pred$mean_spend_bounds = exp(spend_pred$x)

ggplot(data=spend_pred, aes(x=mean_spend_bounds, y=predicted))+
  geom_line()+
  scale_x_continuous(trans = log_trans(), 
                     breaks = trans_breaks("log2", function(x) 2^x))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha=.1)+
  labs(x="Approximate Spend (on Log Scale)", y="Predicted WER",
         title="Approximate Spend vs. Predicted WER")
ggsave("plots/spend_vs_predicted_wer.png")

ggplot(data=spend_party_pred, aes(x=mean_spend_bounds, y=predicted))+
  geom_line(aes(color=group))+
  scale_x_continuous(trans = log_trans(), 
                     breaks = trans_breaks("log2", function(x) 2^x))+
  labs(x="Approximate Spend (on Log Scale)", y="Predicted WER",
       title="Approximate Spend vs. Predicted WER by Party")+
  ylim(0, .15)+
  scale_color_manual(name = "Party", values=c("#00AEF3", "#E81B23"))+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=group), alpha=.1)+
  scale_fill_manual(name = "Party", values=c("#00AEF3", "#E81B23"))+
  guides(fill="none")+
  theme(title = element_text(size=18),
        axis.text = element_text(size=15),
        legend.text = element_text(size=13),
        legend.title = element_text(size=13))
ggsave("plots/spend_vs_predicted_wer_by_party.png", width = 8, height=6)

ggplot(data=spend_noncand_pred, aes(x=mean_spend_bounds, y=predicted))+
  geom_line(aes(color=group))+
  scale_x_continuous(trans = log_trans(), 
                     breaks = trans_breaks("log2", function(x) 2^x))+
  labs(x="Approximate Spend (on Log Scale)", y="Predicted WER",
       title="Approximate Spend vs. Predicted WER by Presence of\nNon-candidate Voice")+
  ylim(0, .2)+
  scale_color_manual(name = "Does Someone Besides\nthe Candidate Speak?", values=c("orange", "blue"))+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=group), alpha=.1)+
    scale_fill_manual(name = "Does Someone Besides\nthe Candidate Speak?", values=c("orange", "blue"))+
    guides(fill="none")+
    theme(title = element_text(size=18),
          axis.text = element_text(size=15),
          legend.text = element_text(size=13),
          legend.title = element_text(size=13))
ggsave("plots/spend_vs_predicted_wer_by_noncandvoice.png", width = 9, height=6)

# ggplot(data=spend_cand_pred, aes(x=mean_spend_bounds, y=predicted))+
#   geom_line(aes(color=group))+
#   scale_x_continuous(trans = log_trans(), 
#                      breaks = trans_breaks("log2", function(x) 2^x))+
#   labs(x="Approximate Spend (on Log Scale)", y="Predicted WER",
#        title="Approximate Spend vs. Predicted WER by Presence of\nCandidate Voice")+
#   ylim(0, .2)+
#   scale_color_manual(name = "Does the Candidate\nSpeak?", values=c("red", "yellow", "blue"))+
#   geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=group), alpha=.1)+
#   scale_fill_manual(name = "Does the Candidate\nSpeak?", values=c("red", "yellow", "blue"))+
#   guides(fill="none")+
#   theme(title = element_text(size=18),
#         axis.text = element_text(size=15),
#         legend.text = element_text(size=13),
#         legend.title = element_text(size=13))
# ggsave("plots/spend_vs_predicted_wer_by_noncandvoice.png", width = 9, height=6)
