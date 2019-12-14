#cleaning demographic data

#turn RIDAGEYR into binary outcomes (age)
demo1 <- demographic %>% mutate(
  Age_btwn_0_10 = if_else(RIDAGEYR < 10, 1, 0),
  Age_btwn_10_20 = if_else(RIDAGEYR < 20 & RIDAGEYR >10 , 1, 0),
  Age_btwn_20_30 = if_else(RIDAGEYR < 30 & RIDAGEYR >20 , 1, 0),
  Age_btwn_20_30 = if_else(RIDAGEYR < 40 & RIDAGEYR >30 , 1, 0),
  Age_btwn_30_40 = if_else(RIDAGEYR < 50 & RIDAGEYR >40 , 1, 0),
  Age_btwn_40_50 = if_else(RIDAGEYR < 60 & RIDAGEYR >50 , 1, 0),
  Age_btwn_50_60 = if_else(RIDAGEYR < 70 & RIDAGEYR >60 , 1, 0),
  Age_btwn_60_70 = if_else(RIDAGEYR < 80 & RIDAGEYR >70 , 1, 0),
  Age_over_80 = if_else(RIDAGEYR >80 , 1, 0),
#turn gender into binary outcomes
  Male = if_else(RIAGENDR == 1, 1, 0),
  Female = if_else(RIAGENDR == 2, 1, 0),
#turn ethnicities into binary outcomes, RIDRETH3
  Mexi = if_else(RIDRETH3 == 1, 1, 0),
  other_hispanic = if_else(RIDRETH3 == 2, 1, 0),
  white = if_else(RIDRETH3 == 3, 1, 0),
  black = if_else(RIDRETH3 == 4, 1, 0),
  asian = if_else(RIDRETH3 == 6, 1, 0),
  other_race = if_else(RIDAGEYR == 7, 1, 0),
#active duty armed services
  Served_in_armed_forces = if_else(DMQMILIZ == 1, 1, 0),
#served in foreign country in peace-keeping mission
  Served_in_foreign_country = if_else(DMQADFC == 1, 1, 0),
#born in U.S. or not
  Born_in_US = if_else(DMDBORN4 == 1, 1, 0),
#U.S. citizen?
  US_citizen = if_else(DMDCITZN == 1, 1, 0),
#6 months when exam occurred
  Nov_April = if_else(RIDEXMON == 1, 1, 0),
  May_Oct = if_else(RIDEXMON == 2, 1, 0),
#Length of time in US
  In_US_btwn_0_1_years = if_else(DMDYRSUS == 1, 1, 0),
  In_US_btwn_1_5_years = if_else(DMDYRSUS == 2, 1, 0),
  In_US_btwn_5_10_years = if_else(DMDYRSUS == 3, 1, 0),
  In_US_btwn_10_15_years = if_else(DMDYRSUS == 4, 1, 0),
  In_US_btwn_15_20_years = if_else(DMDYRSUS == 5, 1, 0),
  In_US_btwn_20_30_years = if_else(DMDYRSUS == 6, 1, 0),
  In_US_btwn_30_40_years = if_else(DMDYRSUS == 7, 1, 0),
  In_US_btwn_40_50_years = if_else(DMDYRSUS == 8, 1, 0),
  In_US_over_50_years = if_else(DMDYRSUS == 9, 1, 0),
#level of education for adults
  Less_than_9th_grade = if_else(DMDEDUC2 == 1, 1, 0),
  Ninth_to_12th_grade = if_else(DMDEDUC2 == 2, 1, 0),
  High_school_or_GED = if_else(DMDEDUC2 == 3, 1, 0),
  Some_college_or_AA = if_else(DMDEDUC2 == 4, 1, 0),
  College_or_above = if_else(DMDEDUC2 == 5, 1, 0),
#Marital Status
  Married = if_else(DMDMARTL == 1, 1, 0),
  Widowed = if_else(DMDMARTL == 2, 1, 0),
  Divorced = if_else(DMDMARTL == 3, 1, 0),
  Separated = if_else(DMDMARTL == 4, 1, 0),
  Never_married = if_else(DMDMARTL == 5, 1, 0),
  Living_with_partner = if_else(DMDMARTL == 6, 1, 0),
#pregnant?
  Pregnant = if_else(RIDEXPRG ==1, 1, 0),
  not_pregnant = if_else(RIDEXPRG == 2, 1, 0),
#language of sp interview
  English = if_else(SIALANG == 1, 1, 0),
  Spanish = if_else(SIALANG == 2, 1, 0),
#Proxy used in SP interview?
  Proxy_yes = if_else(SIAPROXY == 1, 1, 0),
  Proxy_no = if_else(SIAPROXY == 2, 1, 0),
#interpreter used in SP interview?
  Interpreter_yes = if_else(SIAINTRP == 1, 1, 0),
  Interpreter_no = if_else(SIAINTRP == 2, 1, 0),
#language of family interview
  English_family_interview = if_else(FIALANG == 1, 1, 0),
  Spanish_family_interview = if_else(FIALANG == 2, 1, 0),
#proxy in family interview
  Proxy_yes_family_interview = if_else(FIAPROXY == 1, 1, 0),
  Proxy_no_family_interview = if_else(FIAPROXY == 2, 1, 0),
#interpreter used in family interview?
  Interpreter_yes_family = if_else(FIAINTRP == 1, 1, 0),
  Interpreter_no_family = if_else(FIAINTRP == 2, 1, 0),

)



#DMDEDUC3 what to do????

#delete RIDAGEYR
#delete RIDAGEMN
#delete RIDRETH1
#delete RIDEXAGM 
#delete DMQMILIZ (armed services, made new variable above)
#delete DMDBORN4 (born in US, made new variable)
#delete DMDCITZN (citizenship, made new variable)
#delete DMQADFC (peace-keeping mission, made new variable)

clean_demo <- subset(demo1, select = -c(RIDAGEYR, RIDAGEMN, RIAGENDR,
                                   RIDEXAGM, DMQMILIZ, DMDBORN4,
                                   DMDCITZN, DMQADFC, RIDEXMON,DMDYRSUS,
                                   DMDEDUC2, DMDMART,RIDEXPRG, SIAINTRP,
                                   SIAPROXY, FIAPROXY
                                   ))



