# Columns for data information
col_info <- c('CaseID', 'Group', 'Week', 'Day', 'Date')

# Features with fixed effects
col_fix <- c(
  'Pre_Weight',     'Post_Weight', # 'Weight_Change1',
  'Pre_BMI',        'Post_BMI', # 'BMI_Change1',
  'Pre_FatM',       'Post_FatM', # 'FatM_Change1',
  'Pre_Muscle',     'Post_Muscle',
  'Pre_Buffet',     'Post_Buffet', # 'Buffet_Change',
  'Pre_KBDI',       'Post_KBDI', # 'KBDI_Change1',
  'Pre_ATQ30',      'Post_ATQ30', # 'ATQ30_Change1',
  'Pre_BSQ8C',      'Post_BSQ8C', # 'BSQ8C_Change1',
  'Pre_TAI',        'Post_TAI', # 'TAI_Change1',
  'Pre_RSES',       'Post_RSES', # 'RSES_Change1',
  'Pre_DEBQ_RE',    'Post_DEBQ_RE', # 'DEBQ_RE_Change1',
  'Pre_DEBQ_EM',    'Post_DEBQ_EM', # 'DEBQ_EM_Change1',
  'Pre_DEBQ_ENV',   'Post_DEBQ_ENV', # 'DEBQ_ENV_Change1',
  'Pre_DEBQ_total', 'Post_DEBQ_total', # 'DEBQ_total_Change1',
  'Pre_YFAS_total', 'Post_YFAS_total', # 'YFAS_total_Change1',
  'Pre_SIMS'
)
col_fix_pre  <- col_fix[str_starts(col_fix, 'Pre_')]
col_fix_post <- col_fix[str_starts(col_fix, 'Post_')]

col_fix_pre_beh <- col_fix_pre[c(11:13, 15)]
col_fix_pre_cog <- col_fix_pre[7]
col_fix_pre_emo <- col_fix_pre[c(6, 9, 8, 10)]
col_fix_pre_mot <- col_fix_pre[16]

# Features with random effects
col_rep_beh <- c(
  # Continuous variables
  'Carb', 'Protein', 'Fat', 'CarbP', 'ProteinP', 'FatP',
  'TotalKcal', 'Na', 'Sugar', 'BreakKcal', 'Snack1Kcal',
  'LunchKcal', 'Snack2Kcal', 'DinnerKcal', 'Snack3Kcal',
  'BreakNum', 'Snack1Num', 'LunchNum', 'Snack2Num',
  'DinnerNum', 'Snack3Num', 'RedP', 'YellowP', 'GreenP',
  'Logged', 'Steps', 'Exercise',
  # Categorical variables
  'BreakPlace', 'Snack1Place', 'LunchPlace', 'Snack2Place', 'DinnerPlace', 'Snack3Place',
  'BreakTime', 'Snack1Time', 'LunchTime', 'Snack2Time',
  'DinnerTime', 'Snack3Time', 'BreakSpeed', 'Snack1Speed',
  'LunchSpeed', 'Snack2Speed', 'DinnerSpeed', 'Snack3Speed',
  'BreakType', 'Snack1Type', 'LunchType', 'Snack2Type',
  'DinnerType', 'Snack3Type', 'BehaviorScore'
)
col_rep_cog <- c('CognitionNum', 'CognitionScore')
col_rep_emo <- c(
  'Irritated', 'Lonely', 'Nervous', 'Bored', 'Depressed',
  'EmotionScore'
)
col_rep_mot <- c(
  'Will', 'Rank', 'Confidence', 'Satisfaction', 'MotivationScore'
)

col_rep_score <- c(
  'BehaviorScore', 'CognitionScore', 'EmotionScore', 'MotivationScore'
)
col_rep <- c(col_rep_beh, col_rep_cog, col_rep_emo, col_rep_mot)

col_enbody <- c(
  'EnbodyBMI', 'EnbodyWeight', 'EnbodyPercentBodyFat', 'EnbodyFatMass'
)

col_msg <- c(
  'NumMessages'
)

col_continuous <- c(
  col_fix, col_rep_beh[1:27], col_rep_cog, col_rep_emo, col_rep_mot,
  col_enbody, col_msg
)

# save(col_info, col_fix, col_fix_pre, col_fix_post,
#      col_rep_beh, col_rep_cog, col_rep_emo, col_rep_mot, col_rep_score, col_rep,
#      col_enbody, col_msg, col_continuous,
#      file = "./data/columns.rda")
