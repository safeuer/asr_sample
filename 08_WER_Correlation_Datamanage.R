library(dplyr)
library(jsonlite)
library(data.table)
library(lme4)
library(ggplot2)
library(forcats)
library(readxl)
library(scales)

df_wer = read.csv("data/transcript_wers_v062923.csv", stringsAsFactors = F) %>% 
  select(-c("Unnamed..0"))

# Political entity and candidate-level data obtained by WMP
entities = read.csv("data/wmp_fb_2022_entities_v120122.csv")
cand_info = read.csv("data/wmpcand_012523_wmpid_edited.csv")

# Join in Cook competitiveness scores for 2022 House midterms
# 2=likely D, 3=lean D, 4=tossup D, 5=tossup R, 6=lean R, 7=likely R, NA=not competitive
comp = read_excel("data/cook_competitiveness_scores.xlsx")
candidate_comp = cand_info %>% left_join(comp, by=c("cand_office_st" = "state", "cand_office_dist"="district"))

# Join in campaign-level spend data
full_spend = read.csv("data/Cds_GandFB_thru110822_v011223.csv")
candidate_info = candidate_comp %>% left_join(full_spend %>% select(wmpid,f_010122to110822,f_090522to110822), by=c("wmpid")) %>%
  rename(c("total_spend_since_010122"="f_010122to110822", "total_spend_since_090522"="f_090522to110822"))


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

# Join in ad-level data like spend and impressions
master = read.csv("data/fb2022_master_0905_1108.csv.gz")

df_wer_master = df_wer %>%
  inner_join(master, by=c("ad_id"))
# Join in candidate level data from above
df_wer_cand = df_wer_master %>%
  inner_join(federal_candidates, by=c("pd_id")) %>%
  mutate_at(c("spend"), as.character)

# no_spend = df_wer_cand %>%
#   filter(total_spend_since_010122 == 0 | is.na(total_spend_since_010122) | total_spend_since_090522 == 0 | is.na(total_spend_since_090522))

# Parse spend variable (in JSON format) and average lower and upper bounds of spend for each ad
spend_parsed = apply(df_wer_cand %>% select(spend), 1, function (x) {fromJSON(x)})
spend_df = rbindlist(spend_parsed)
df_wer_spend = cbind(df_wer_cand, spend_df) %>%
  rename(c("spend_lower" = "lower_bound", "spend_upper" = "upper_bound")) %>%
  mutate_at(c("spend_lower", "spend_upper"), as.integer) %>%
  mutate(spend_boundmean = round((spend_lower + spend_upper)/2))

# Data management
df_wer_spend2 = df_wer_spend %>%
  mutate(cand_party_affiliation = as.character(cand_party_affiliation)) %>%
  # Replace DFL with DEM (DFL is a niche party that functions as DEM in Minnesota)
  mutate(cand_party_affiliation = ifelse(cand_party_affiliation == "DFL", "DEM", cand_party_affiliation)) %>%
  # Remove rare parties IND and LIB
  filter(cand_party_affiliation %in% c("DEM", "REP")) %>%
  # Remove candidates with missing data for race and Indiginous candidates (small sample size)
  filter(race_wmp != "NULL") %>%
  filter(race_wmp != "Indiginous") %>%
  # Remove ads where it is unclear if the candidate is speaking (small sample size) — should we manually adjudacate these instead?
  filter(candidate_voice != "I’m not sure who is speaking") %>%
  filter(non_candidate_voice != "Not Sure") %>%
  mutate_at(c("race_wmp", "cand_incumbent_challenger_open_s", "gender_wmp", "cand_party_affiliation", "race_comp",
              "non_english", "candidate_voice", "non_candidate_voice"), as.factor) %>% 
  # Remove ads missing campaign-level spend data
  filter(!is.na(total_spend_since_010122), !is.na(total_spend_since_090522), total_spend_since_010122>0, total_spend_since_090522>0) %>%
  # Take the log of spend to normalize it
  mutate(logspend = log(spend_boundmean),
         log_total_010122 = log(total_spend_since_010122),
         log_total_090522 = log(total_spend_since_090522)) %>%
  mutate_at(c("wmpid"), as.factor) %>%
  # Relevel variables so reference levels are correct
  mutate(race_wmp = relevel(race_wmp, "White"),
         cand_incumbent_challenger_open_s = relevel(cand_incumbent_challenger_open_s, "INCUMBENT"),
         gender_wmp = relevel(gender_wmp, "M"),
         candidate_voice = relevel(candidate_voice, "No"))

#write.csv(df_wer_spend2, "data/df_wer_datamanaged_v063023.csv")

# # Predict WER as a function of other candidate- and ad-level variables, adding random effects for candidate
# wer_logm = glmer(wer~race_wmp+cand_incumbent_challenger_open_s+gender_wmp+race_comp+cand_party_affiliation+logspend
#                  +candidate_voice+non_candidate_voice+(1|wmpid),
#       data = df_wer_spend2, family = "binomial", control = glmerControl(optimizer ="Nelder_Mead"))
# summary(wer_logm)
# 
# ggpredict(wer_logm, "logspend")
# 
# 
# 
# 
# # Join in candidate-level SPEND data to main dataset
# # df_wer_total_spend = df_wer_spend2 %>%
# #   inner_join(master_cand_spend, by=c("wmpid")) %>%
# #   mutate(log_totalspend = log(total_avg_spend))
# # wer_totalspend_logm = glmer(wer~race_wmp+cand_incumbent_challenger_open_s+gender_wmp+race_comp+cand_party_affiliation+log_totalspend
# #                  +candidate_voice+non_candidate_voice+(1|wmpid),
# #                  data = df_wer_total_spend, family = "binomial", control = glmerControl(optimizer ="Nelder_Mead"))
# # summary(wer_totalspend_logm)

# Calculate candidate-level WER data (mean, min, max)
df_by_candidate = df_wer_spend2 %>%
  group_by(wmpid, pd_id) %>%
  summarise(mean_wer = mean(wer),
            num_ads = n(),
            min_wer = min(wer),
            max_wer = max(wer)) %>%
  ungroup() %>%
  mutate(diff_wer = max_wer - min_wer) %>%
  arrange(diff_wer)

# Join in candidate-level WER data to main dataset
df_wer_spend3 = df_wer_spend2 %>%
  inner_join(df_by_candidate, by=c("wmpid", "pd_id")) %>%
  mutate(name_label = paste(substr(first_name, 1, 1), last_name))
df_wer_spend3$party_label[df_wer_spend3$cand_party_affiliation == "DEM"] = "D"
df_wer_spend3$party_label[df_wer_spend3$cand_party_affiliation == "REP"] = "R"
df_wer_spend3$inc_label[df_wer_spend3$cand_incumbent_challenger_open_s == "CHALLENGER"] = "C"
df_wer_spend3$inc_label[df_wer_spend3$cand_incumbent_challenger_open_s == "INCUMBENT"] = "I"
df_wer_spend3$inc_label[df_wer_spend3$cand_incumbent_challenger_open_s == "OPEN"] = "O"
df_wer_spend3 = df_wer_spend3 %>%
  mutate(label = paste(name_label, " (", party_label, "/", as.character(gender_wmp), "/", inc_label, ")", sep=""))

# Plots
cand_points = ggplot(data = df_wer_spend3, aes(x=wer, y=fct_reorder(label, wer, .fun = mean)))+
  geom_point(aes(colour=candidate_voice))+
  geom_linerange(aes(xmin=min_wer, xmax=max_wer), linewidth=0.3)+
  theme(legend.position = "bottom", axis.ticks.y = element_blank())+
  labs(x="WER", y="Candidates, ordered by mean WER", title="Range of WERs by candidate and whether candidate speaks",
       colour = "Is the candidate speaking?")
ggsave("plots/cand_wer_ranges.png", width=10, height=20)

ggplot(data= df_wer_spend3, aes(x=spend_boundmean, y=wer))+
  geom_jitter(aes(colour=cand_party_affiliation, shape=cand_incumbent_challenger_open_s), size=2)+
  labs(x="Approximate Spend (on log scale)", y="WER", title="Approximate Spend vs. WER", colour="Party", shape="Candidate Incumbency")+
  scale_colour_discrete(type=c("#00AEF3", "#E81B23"))+
  theme(legend.position="bottom", legend.box="1", legend.background = element_rect(fill="lightgray"),
        title = element_text(size=18),
        axis.text = element_text(size=15),
        legend.text = element_text(size=12),
        legend.title = element_text(size=15))+
  scale_x_continuous(trans = log_trans(), 
                     breaks = trans_breaks("log2", function(x) 2^x))
ggsave("plots/spend_vs_wer.png", width=8, height=6)


df_by_cand2 = df_by_candidate %>% # This will produce a warning--it is OK.
  inner_join(full_spend %>% select(wmpid,f_010122to110822,f_090522to110822), by=c("wmpid")) %>%
  rename(c("total_spend_since_010122"="f_010122to110822", "total_spend_since_090522"="f_090522to110822")) %>%
  inner_join(federal_candidates %>% select(wmpid, race_wmp, race_comp, gender_wmp, cand_party_affiliation, cand_incumbent_challenger_open_s),
             by=c("wmpid")) %>%
  filter(total_spend_since_010122 > 0) %>%
  filter(total_spend_since_090522 > 0)

totalspend09_vs_meanwer = ggplot(data=df_by_cand2, aes(x=total_spend_since_090522, y=mean_wer))+
  geom_point(aes(colour=cand_party_affiliation, shape=cand_incumbent_challenger_open_s))+
  scale_colour_discrete(type=c("#00AEF3", "#E81B23"))+
  labs(x="Total Spend", y="Mean WER", title="Campaign Total Spend (09/05/22-11/08/22) vs. Mean WER",
       colour="Party", shape="Candidate Incumbency")+
  scale_x_log10()
ggsave("plots/totalspend09_vs_meanwer.png", width=8, height=6)

totalspend01_vs_meanwer = ggplot(data=df_by_cand2, aes(x=total_spend_since_010122, y=mean_wer))+
  geom_point(aes(colour=cand_party_affiliation, shape=cand_incumbent_challenger_open_s))+
  scale_colour_discrete(type=c("#00AEF3", "#E81B23"))+
  labs(x="Total Spend", y="Mean WER", title="Campaign Total Spend (01/01/22-11/08/22) vs. Mean WER",
       colour="Party", shape="Candidate Incumbency")+
  scale_x_log10()
ggsave("plots/totalspend01_vs_meanwer.png", width=8, height=6)

# do candidate total spend vs. mean WER

# cand_ranges = ggplot(data=df_wer_spend3, aes(x=fct_reorder(wmpid, wer, .fun=mean), y=diff_wer))+
#   geom_bar(stat="summary", fun=mean)+
#   geom_point(aes(x=fct_reorder(wmpid, wer, .fun=mean), y=wer), stat="summary", fun=mean, inherit.aes=F)+
#   theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+
#   xlab("Candidate")+
#   ylab("Magnitude of Range of WER vs. Mean WER")+
#   ggtitle("Magnitude of Range and Mean of WER by Candidate")

# mean_vs_diff = ggplot(data=df_by_candidate, aes(x=mean_wer, y=diff_wer))+
#   geom_point()+
#   xlim(0, 1)+
#   ylim(0, 1)+
#   xlab("Mean WER")+
#   ylab("Magnitude of WER Range")+
#   ggtitle("Mean WER vs. Magnitude of Range of WER by Candidate")+
#   geom_abline(slope=1, intercept=0, linetype="dashed")
# mean_vs_diff



