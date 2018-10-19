questionnaire_result <- questionnaire_result %>%
  mutate(
    digital_user_type = case_when(
      digital_media_use %in% 1:9 ~ "Average",
      digital_media_use %in% 10:16 ~ "Savvy"
    )
  )
questionnaire_result <- rename(questionnaire_result, act_citizen = `actualizing citizenship (non-tradition)`)
questionnaire_result <- questionnaire_result %>%
  mutate(act_citizen = act_citizen*9, `traditional dutiful citizenship` = `traditional dutiful citizenship`*11)
questionnaire_result <- questionnaire_result %>%
  mutate(`average citizenship` = act_citizen + `traditional dutiful citizenship`)
group_by(questionnaire_result, digital_user_type) %>%
  count()
p <- ggplot(questionnaire_result)
p + geom_bar(aes(x = digital_user_type))
p + geom_boxplot(aes(x = digital_user_type, y = act_citizen))
p + geom_boxplot(aes(x = digital_user_type, y = `average citizenship`))
p + geom_boxplot(aes(x = digital_user_type, y = `traditional dutiful citizenship`))
p + geom_bar(aes(x = UM_participation)) + facet_wrap(~ digital_user_type)
p + geom_histogram(aes(x = act_citizen), binwidth = 5) + labs( x = "Actualizing Citizenship") + facet_wrap(~ digital_user_type)
p + geom_histogram(aes(x = `average citizenship`), binwidth = 5) + facet_wrap(~ digital_user_type)
p + geom_histogram(aes(x = `traditional dutiful citizenship`), binwidth = 5) + facet_wrap(~ digital_user_type)
p + geom_histogram(aes(x = act_citizen, fill = digital_user_type), binwidth = 2)
