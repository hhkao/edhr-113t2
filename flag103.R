# flag103: 本校在職期間（本校到職日期與資料填報基準日的差距）幾近或同於本校任職須扣除年資。 -------------------------------------------------------------------
#1. 扣除年資 = 在職年資
#2. 扣除年資的年份 = 在職年資的年份，且年份數值 >= 5

flag_person <- drev_person_1

flag_person$survey_year <- 2025
flag_person$survey_mon <- 3
flag_person$onbodaty <- ""
flag_person$onbodatm <- ""
flag_person$onbodatd <- ""

flag_person$onbodaty <-
  if_else(
    nchar(flag_person$onbodat) == 6,
    substr(flag_person$onbodat, 1, 2),
    flag_person$onbodaty
  )
flag_person$onbodatm <-
  if_else(
    nchar(flag_person$onbodat) == 6,
    substr(flag_person$onbodat, 3, 4),
    flag_person$onbodatm
  )
flag_person$onbodatd <-
  if_else(
    nchar(flag_person$onbodat) == 6,
    substr(flag_person$onbodat, 5, 6),
    flag_person$onbodatd
  )
flag_person$onbodaty <-
  if_else(
    nchar(flag_person$onbodat) == 7,
    substr(flag_person$onbodat, 1, 3),
    flag_person$onbodaty
  )
flag_person$onbodatm <-
  if_else(
    nchar(flag_person$onbodat) == 7,
    substr(flag_person$onbodat, 4, 5),
    flag_person$onbodatm
  )
flag_person$onbodatd <-
  if_else(
    nchar(flag_person$onbodat) == 7,
    substr(flag_person$onbodat, 6, 7),
    flag_person$onbodatd
  )

flag_person$onbodaty <- as.numeric(flag_person$onbodaty)
flag_person$onbodatm <- as.numeric(flag_person$onbodatm)
flag_person$onbodatd <- as.numeric(flag_person$onbodatd)

#本校服務年資
flag_person$tser <- 0
flag_person$tser <-
  if_else(
    flag_person$survey_year %% 4 != 0,
    ((flag_person$survey_year - 1911) + 3 / 12 + 31 / 365) - (
      flag_person$onbodaty + (flag_person$onbodatm / 12) + (flag_person$onbodatd /
                                                              365)
    ),
    flag_person$tser
  )
flag_person$tser <-
  if_else(
    flag_person$survey_year %% 4 == 0,
    ((flag_person$survey_year - 1911) + 3 / 12 + 31 / 366) - (
      flag_person$onbodaty + (flag_person$onbodatm / 12) + (flag_person$onbodatd /
                                                              366)
    ),
    flag_person$tser
  )

#本次本校任職需扣除之年資
flag_person$desey <-
  substr(flag_person$desedym, 1, 2) %>% as.numeric()
flag_person$desem <-
  substr(flag_person$desedym, 3, 4) %>% as.numeric()

flag_person$dese <- (flag_person$desey + (flag_person$desem / 12))

#本校服務年資-本校任職需扣除之年資 才是實際在本校的服務年資
flag_person$tser <- flag_person$tser - flag_person$dese

#本校到職前學校服務總年資
flag_person$beoby <-
  substr(flag_person$beobdym, 1, 2) %>% as.numeric
flag_person$beobm <-
  substr(flag_person$beobdym, 3, 4) %>% as.numeric

flag_person$beob <- (flag_person$beoby + (flag_person$beobm / 12))

#學校教學工作總年資
flag_person$tsch <- flag_person$tser + flag_person$beob

#換算到職至資料基準日的日期
flag_person$tser_ndese <- flag_person$tser + flag_person$dese
flag_person$tser_ndesey <- floor(flag_person$tser_ndese)
flag_person$tser_ndesem <-
  ceiling((flag_person$tser_ndese - floor(flag_person$tser_ndese)) * 12)
flag_person$ndesey <- floor(flag_person$dese)
flag_person$ndesem <-
  ceiling((flag_person$dese - floor(flag_person$dese)) * 12)

temp <- c("tser_ndesey", "tser_ndesem", "ndesey", "ndesem")
for (x in temp) {
  flag_person[[x]] <- flag_person[[x]] %>% as.character()
}

flag_person$err_flag <- 0
#1. 扣除年資 = 在職年資
flag_person$err_flag <-
  if_else((flag_person$tser_ndesey == flag_person$ndesey) &
            (flag_person$tser_ndesem == flag_person$ndesem) &
            (
              flag_person$ndesey %>% as.numeric() > 0 |
                flag_person$ndesem %>% as.numeric() > 0
            ),
          1,
          flag_person$err_flag
  )
#2. 扣除年資的年份 = 在職年資的年份，且年份數值 >= 5
flag_person$err_flag <-
  if_else((flag_person$tser_ndesey == flag_person$ndesey) &
            (flag_person$tser_ndesey %>% as.numeric() >= 5),
          2,
          flag_person$err_flag
  )

flag_person$err_flag <- if_else(flag_person$err_flag > 0, 1, flag_person$err_flag)

#加註到職至資料基準日的時間，和扣除年資
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <- case_when(
  flag_person$err_flag == 1 ~ paste(
    flag_person$name,
    flag_person$onbodat,
    "到職（到職至資料基準日為",
    flag_person$tser_ndesey,
    "年",
    flag_person$tser_ndesem,
    "個月，且扣除年資亦為",
    flag_person$ndesey,
    "年",
    flag_person$ndesem,
    "個月",
    "）",
    sep = ""
  ),
  TRUE ~ flag_person$err_flag_txt
)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag103 <- flag_person %>%
    subset(select = c(
      organization_id,
      idnumber,
      err_flag_txt,
      edu_name2,
      source,
      err_flag
    )) %>%
    subset(err_flag == 1) %>%
    dcast(organization_id + source ~ err_flag_txt, value.var = "err_flag_txt")
  
  #合併所有name
  temp <-
    colnames(flag_person_wide_flag103)[3:length(colnames(flag_person_wide_flag103))]
  flag_person_wide_flag103$flag103_r <- NA
  for (i in temp) {
    flag_person_wide_flag103$flag103_r <-
      paste(flag_person_wide_flag103$flag103_r,
            flag_person_wide_flag103[[i]],
            sep = " ")
  }
  flag_person_wide_flag103$flag103_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag103$flag103_r)
  flag_person_wide_flag103$flag103_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag103$flag103_r)
  
  #產生檢誤報告文字
  flag103_temp <- flag_person_wide_flag103 %>%
    group_by(organization_id) %>%
    mutate(flag103_txt = paste(source, "：", flag103_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag103_txt)) %>%
    distinct(organization_id, flag103_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag103 <- flag103_temp %>%
    dcast(organization_id ~ flag103_txt, value.var = "flag103_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag103)[2:length(colnames(flag103))]
  flag103$flag103 <- NA
  for (i in temp) {
    flag103$flag103 <- paste(flag103$flag103, flag103[[i]], sep = "； ")
  }
  flag103$flag103 <- gsub("NA； ", replacement = "", flag103$flag103)
  flag103$flag103 <- gsub("； NA", replacement = "", flag103$flag103)
  
  #產生檢誤報告文字
  flag103 <- flag103 %>%
    subset(select = c(organization_id, flag103)) %>%
    distinct(organization_id, flag103) %>%
    mutate(flag103 = paste("請確認該員之「本校到職日期」、「本校任職需扣除之年資」，", flag103, sep = ""))
} else{
  #偵測flag103是否存在。若不存在，則產生NA行
  if ('flag103' %in% ls()) {
    print("flag103")
  } else{
    flag103 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag103$flag103 <- ""
  }
}
