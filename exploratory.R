
library(tidyverse)

#default theme for ggplot objects to theme_bw()
theme_set(theme_bw())
theme_update(panel.grid = element_blank())


compl<- readxl::read_xlsx(
    "C:\\Users\\au710823\\OneDrive - Aarhus universitet\\Grøntsagsforsøg\\veg_conc_131123.xlsx",
  sheet = "Sheet2") |> filter(!is.na(`Nitrat  mg/L`)) |> 
  tbl_df() %>% 
  mutate(newar = ymd(paste0(`År`,"-01-01"))) |>
  #mutate(aruge=paste(print(`År`),ifelse(Uge>10,print(Uge),print(paste("0",Uge,sep=""))),sep="")) |> 
  mutate(date_week=newar + weeks(Uge))

summary(compl$date_week)

compl |> ggplot(aes(
  x = date_week,
  y = `Nitrat  mg/L`,
  col = Sted #interaction(Sted, Område)
)) +
  geom_point() +
  geom_smooth()+
  theme(axis.text.x = element_text(angle = 90))
