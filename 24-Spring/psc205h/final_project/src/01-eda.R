
# 0 Load in data and packages ---------------------------------------------

library(rstan)
library(dplyr)
library(ggplot2)

data <- read.csv("final_project/data/clean_data.csv") |> 
  dplyr::mutate(randomizacao = factor(randomizacao)) |> 
  dplyr::select(
    record_id, redcap_event_name, randomizacao, igi_1a:spaq_8, ebas_1:ebas_6
  ) |>
  dplyr::filter(redcap_event_name %in% c(
    "elegibilidade_arm_1", "elegibilidade_mini_arm_1", 
    "desfechos_arm_1", "followup_arm_1"
  )) |> 
  dplyr::mutate(
    redcap_event_name = factor(dplyr::case_when(
      redcap_event_name == "elegibilidade_mini_arm_1" ~ 0,
      redcap_event_name == "elegibilidade_arm_1" ~ 1,
      redcap_event_name == "desfechos_arm_1" ~ 2,
      redcap_event_name == "followup_arm_1" ~ 3
    )
    
    ),
    randomizacao = dplyr::case_when(
      randomizacao == 1 ~ "act",
      randomizacao == 2 ~ "cbt",
      randomizacao == 3 ~ "wl"
    ))|>
  dplyr::mutate(
    act_v_cbt = ifelse(randomizacao ==  "cbt", -1/3, 2/3),
    act_v_wl = ifelse(randomizacao ==  "wl", -1/3, 2/3),
    pre_v_post = ifelse(redcap_event_name ==  1, 2/3, -1/3),
    post_v_pre = ifelse(redcap_event_name ==  2, 2/3, -1/3),
    fu_v_pre = ifelse(redcap_event_name ==  3, 2/3, -1/3),
    fu_v_post = ifelse(redcap_event_name ==  3, 2/3, -1/3))

data_reduced <- data |> 
  dplyr::select(record_id, redcap_event_name, randomizacao, igi_escore) |> 
  dplyr::filter(randomizacao != "wl",
                redcap_event_name!= 0)

data_reduced |> 
  dplyr::with_groups(c(redcap_event_name, randomizacao),
                     summarise,
                     n = n())
                     #n_missing = sum(is.na(igi_escore)))


# 1 Visualizing interindividual differences in change ---------------------

data |> 
  #dplyr::filter(redcap_event_name != 0) |> 
  ggplot(aes(x = redcap_event_name, y = igi_escore)) +
  stat_smooth(aes(group = record_id),
              method = "lm", se = FALSE, size = 1/6 ) +
  stat_smooth(method = "lm", se = F, size = 2) +
  scale_color_viridis_d(option = "B", begin = .33, end = .67) +
  theme(legend.position = "none",
        panel.grid = element_blank()) +
  scale_x_discrete(breaks = c(1, 2,3 )) +
  facet_wrap(~ randomizacao) +
  theme_minimal()

## Plotting the outcome variable (ISI) ----

data |> 
  dplyr::filter(redcap_event_name != 0) |> 
  ggplot(aes( x = igi_escore)) +
  geom_density() +
  #facet_wrap(~ randomizacao) +
  facet_grid(randomizacao ~ redcap_event_name)

# 2 Intercept only model --------------------------------------------------

intercept_model <- brms::brm(igi_escore ~ 1 + (1|record_id),  
                          data = data, 
                          warmup = 1000, iter = 3000, 
                          cores = 16, chains = 4, 
                          seed = 444)


# 3 Full model ------------------------------------------------------------

full_model <- brms::brm(igi_escore ~ (1|record_id) + ((act_v_cbt + act_v_wl) * (post_v_pre + fu_v_pre)),  
                        data = data, 
                        warmup = 1000, iter = 3000, 
                        cores = 16, chains = 4, 
                        seed = 444)

red_model <- brms::brm(igi_escore ~ (1|record_id) + ((act_v_wl) * (post_v_pre + fu_v_pre)),  
                       data = data, 
                       warmup = 1000, iter = 3000, 
                       cores = 16, chains = 4, 
                       seed = 444)

m1 <- add_criterion ( full_model , "loo" )
m2 <- add_criterion (red_model , "loo" )
lcomp <- loo_compare (m1 , m2)
print ( lcomp )

lmerTest::lmer(igi_escore ~ (1|record_id) + ((act_v_cbt + act_v_wl) * (post_v_pre + fu_v_pre)), data=data) |> 
  summary()


# MLE model ---------------------------------------------------------------

data2 <- data |> 
  dplyr::filter(randomizacao != "wl",
                redcap_event_name!= 0)

fit_mle <- lmerTest::lmer(igi_escore ~ (1 |record_id) + randomizacao, data=data2)
summary(fit_mle)
