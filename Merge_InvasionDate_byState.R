#Merge Dates of First Invasion based on State
library(dplyr)

#left join  
SLF_State_BeginningDateMatch = SLF_State_BeginningDateMatch_Corrected
SLF_matched = left_join(SLF_AllData_Modified_tobeassigned, SLF_State_BeginningDateMatch, 
                        by='State')
write.csv(SLF_matched, "SLF_SinceiNat_all-correctiNatDatev2.csv")

#Filter only states of interest:
SLF_filtered = filter(SLF_matched, State %in%  c("PA", "NJ", "OH", "NY"))
write.csv(SLF_filtered, "SLF_filtered_all_v4.csv")

#Export to excel to get day counts
#SLF_filtered_all-v4 is import file SLF_filtered_all_v5

#Sum counts per day
SLF_aggregated2 = SLF_filtered_all_v5 %>% 
  group_by(ObservedDate, County, State, Year, DOY, FirstiNatReport, FirstInvasion, iNatSince,
           SinceOfficialInvasion) %>% 
  summarise(Count2 = sum(Count))
write.csv(SLF_aggregated2, "SLF_aggregated_2b.csv")

#Sum counts per day more strictly to run joinpoint
SLF_aggregated3 = SLF_filtered_all_v5 %>% 
  group_by(ObservedDate, State, Year, DOY, FirstiNatReport, FirstInvasion, iNatSince,
           SinceOfficialInvasion) %>% 
  summarise(Count2 = sum(Count))
write.csv(SLF_aggregated3, "SLF_aggregated3c.csv")



#Ascending order by state then by date
#for sheet v2 - no county to aggregate across state
SLF_ascending3 <- SLF_aggregated3 %>%
  arrange(State, SinceOfficialInvasion)
write.csv(SLF_ascending3, "SLF_ascending4b.csv")
