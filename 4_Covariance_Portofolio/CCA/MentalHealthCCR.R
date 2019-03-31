library(CCA, GGally)
library(varhandle)

file = read.csv("Mental_Health_survey.csv")

Policies_on_MH = file[,c("benefits", "care_options", "wellness_program", 
                         "seek_help", "anonymity", "leave")]
levels(Policies_on_MH[,1]) = c(0, -1, 1)   #recode survey responses to likery scale
levels(Policies_on_MH[,2]) = c(-1, 0, 1)   # -1 = "No"; 0 = "Don't know"; 1 = "Yes"
levels(Policies_on_MH[,3]) = c(0, -1, 1)   #recode survey responses to likery scale
levels(Policies_on_MH[,4]) = c(0, -1, 1)   #recode survey responses to likery scale
levels(Policies_on_MH[,5]) = c(0, -1, 1)   #recode survey responses to likery scale
levels(Policies_on_MH[,6]) = c(0, -1, 1, -2, 2)   #recode survey responses to likery scale

Opennes_of_MH = file[,c("mental_health_consequence", "phys_health_consequence", "coworkers",
                        "supervisor", "mental_health_interview", "phys_health_interview",
                        "mental_vs_physical", "obs_consequence")]

levels(Opennes_of_MH[,1]) = c(0, -1, 1)    #recode survey responses to likery scale
levels(Opennes_of_MH[,2]) = c(0, -1, 1)    #recode survey responses to likery scale
levels(Opennes_of_MH[,3]) = c(-1, 0, 1)    #recode survey responses to likery scale
levels(Opennes_of_MH[,4]) = c(-1, 0, 1)    #recode survey responses to likery scale
levels(Opennes_of_MH[,5]) = c(0, -1, 1)    #recode survey responses to likery scale
levels(Opennes_of_MH[,6]) = c(0, -1, 1)    #recode survey responses to likery scale
levels(Opennes_of_MH[,7]) = c(0, -1, 1)    #recode survey responses to likery scale
levels(Opennes_of_MH[,8]) = c(-1, 1)    #recode survey responses to likery scale

#using varhandle package here
Opennes_of_MH = unfactor(Opennes_of_MH)    #convert factors to numerics
Policies_on_MH = unfactor(Policies_on_MH)

correlation_matrix = matcor(Policies_on_MH, Opennes_of_MH)
cca = cc(Policies_on_MH, Opennes_of_MH)


