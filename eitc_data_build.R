#EITC graph data
#dataframe to create graph
graph_df <- data.frame(income=numeric())
for(i in 1:60000){
  vec <- c(i)
  graph_df[i,] <-vec
}
graph_df$fedM0 <- NA
graph_df$fedM1 <- NA
graph_df$fedM2 <- NA
graph_df$fedM3 <- NA
graph_df$fedS0 <- NA
graph_df$fedS1 <- NA
graph_df$fedS2 <- NA
graph_df$fedS3 <- NA
View(graph_df)
#Finish cleaning up outcome using pipe operator, mutate, and case_when()
graph_df <- graph_df %>%
  mutate(fedM0 = case_when(
    income<7320 ~ round(income*.0765,0),
    income>7320 & income<15290~ 560,
    income>15290 & income<22610~ round((22610-income)*.0765, 0),
    #income>22610 ~ NA
  )) %>%
  mutate(fedM1 = case_when(
    income<10980 ~ round(income*.34,0),
    income>10980 & income<26260 ~ 3733,
    income>26260 & income<49622~ round((49622-income)*.1598, 0),
    #income>49622 ~ NA
  )) %>%
  mutate(fedM2 = case_when(
    income<15410 ~ round(income*.4,0),
    income>15410 & income<26260 ~ 6164,
    income>26260 & income<55529~ round((55529-income)*.2106, 0),
    #income>55529 ~ NA
  )) %>%
  mutate(fedM3 = case_when(
    income<15410 ~ round(income*.45,0),
    income>15410 & income<26260 ~ 6935,
    income>26260 & income<59187 ~round((59187-income)*.2106, 0),
    #income>59187 ~ NA
  )) %>%
  mutate(fedS0 = case_when(
    income<7320 ~ round(income*.0765,0),
    income>7320 & income<9160 ~ 560,
    income>9160 & income<16480 ~ round((16480-income)*.0765, 0),
    #income>16480 ~ NA
  )) %>%
  mutate(fedS1 = case_when(
    income<10980 ~ round(income*.34,0),
    income>10980 & income<20130 ~ 3733,
    income>20130 & income<43492 ~ round((43492-income)*.1598, 0),
    #income>43492 ~ NA
  )) %>%
  mutate(fedS2 = case_when(
    income<15410 ~ round(income*.4,0),
    income>15410 & income<20130 ~ 6164,
    income>20130 & income<49399 ~ round((49399-income)*.2106, 0),
    #income>49399 ~ NA
  )) %>%
  mutate(fedS3 = case_when(
    income<15410 ~ round(income*.45,0),
    income>15410 & income<20130 ~ 6935,
    income>20130 & income<53057 ~round((53057-income)*.2106, 0),
    #income>53057 ~ NA
  ))

save(graph_df, file="eitc_calc.Rda")