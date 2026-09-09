library(tidyverse)
library(DescTools)
library(vcd)
library(writexl)
library(rio)
#DescToolsmeasures Krippendorff's alpha. Function is KrippAlpha

# Set wd before imrpoting
pc <- import("peoplecentrism_final.xlsx")

# rename and reclassify variables
pc <- pc %>% 
  rename(coder_1_score_pc = rapha_final,
         coder_2_score_pc = paolo_final)

pc$coder_1_score_pc <- as.numeric(pc$coder_1_score_pc)
pc$coder_2_score_pc <- as.numeric(pc$coder_2_score_pc)

# check for and avoid repetitions
pc <- pc %>% 
  distinct(index, .keep_all = T)

kapc <- pc %>% 
  select(coder_1_score_pc, coder_2_score_pc)


#transform data frame
kapc <- as.matrix(kapc)
ktestpc <- t(kapc)

#run inter-reliability test
kalpha_pc <- KrippAlpha(ktestpc, method = "nominal")

# Alpha for people centirsm:
kalpha_pc$value

#################################################
## Compute Krippendorff's alpha for anti-elitism
# Set wd before imrpoting
ae <- import("antielitism_final.xlsx")

# rename and reclassify variables
ae <- ae %>% 
  rename(coder_1_score_pc = gus,
         coder_2_score_pc = yuri)

ae$coder_1_score_pc <- as.numeric(ae$coder_1_score_pc)
ae$coder_2_score_pc <- as.numeric(ae$coder_2_score_pc)

# check for and avoid repetitions
ae <- ae %>% 
  distinct(index, .keep_all = T)

kaae <- ae %>% 
  select(coder_1_score_pc, coder_2_score_pc)


#transform data frame
kaae <- as.matrix(kaae)
ktestae <- t(kaae)

#run inter-reliability test
kalpha_ae <- KrippAlpha(ktestae, method = "nominal")

# Alpha for anti-elitism:
kalpha_ae$value


### Compute alpha for CAP model validation process. 
## coders annotation
cap_annotation <- import("cap_annotation_policy_codes.xlsx")

#selec columnbs
kacap <- cap_annotation %>% 
  select(coder_1_policy_num,  coder_2_policy_num, coder_3_policy_num)



#transform data for computing alpha
kacap <- as.matrix(kacap)
ktestcap <- t(kacap)

#rodei o teste de inter-reliability

kalpha_cap <- KrippAlpha(ktestcap, method = "nominal")


# Alpha for CAP:
kalpha_cap$value





### Compute alpha for antagonism between the people and the elite - validation process. 
## coders annotation
antagonism <- import("sample_antagonism_validation.xlsx")

#selec columnbs
kacap <- antagonism %>% 
  select(g_antagonism,  p_antagonism)



#transform data for computing alpha
kacap <- as.matrix(kacap)
ktestcap <- t(kacap)

#rodei o teste de inter-reliability

kalpha_cap <- KrippAlpha(ktestcap, method = "nominal")


# Alpha for CAP:
kalpha_cap$value



