# flag102: 本學期聘任訖日範圍應為113年8月1日-114年7月31日。 -------------------------------------------------------------------
flag_person <- drev_person_1

flag_person$endy <-
  substr(flag_person$enddate, 1, 3) %>% as.numeric()
flag_person$endm <-
  substr(flag_person$enddate, 4, 5) %>% as.numeric()

flag_person$err_spe <-
  if_else(((flag_person$endy * 12 + flag_person$endm) > (114 * 12 + 7) | 
           (flag_person$endy * 12 + flag_person$endm) < (113 * 12 + 8)),
          1,
          0
  )

#加註
flag_person$name <-
  paste(flag_person$name, "（", flag_person$enddate, "）", sep = "")
flag_person$name <- gsub("；）", replacement = "）", flag_person$name)
flag_person$name <- gsub("（）", replacement = "", flag_person$name)

#呈現姓名
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <-
  case_when(flag_person$err_spe == 1 ~ flag_person$name,
            TRUE ~ flag_person$err_flag_txt)

if (dim(flag_person %>% subset(err_spe == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag102 <- flag_person %>%
    subset(select = c(
      organization_id,
      idnumber,
      err_flag_txt,
      edu_name2,
      source,
      err_spe
    )) %>%
    subset(err_spe == 1) %>%
    dcast(
      organization_id + source ~ err_flag_txt,
      value.var = "err_flag_txt",
      fun.aggregate = dplyr::first
    )
  
  #合併所有name
  temp <-
    colnames(flag_person_wide_flag102)[3:length(colnames(flag_person_wide_flag102))]
  flag_person_wide_flag102$flag102_r <- NA
  for (i in temp) {
    flag_person_wide_flag102$flag102_r <-
      paste(flag_person_wide_flag102$flag102_r,
            flag_person_wide_flag102[[i]],
            sep = " ")
  }
  flag_person_wide_flag102$flag102_r <-
    gsub("NA ", replacement = "", flag_person_wide_flag102$flag102_r)
  flag_person_wide_flag102$flag102_r <-
    gsub(" NA", replacement = "", flag_person_wide_flag102$flag102_r)
  
  #產生檢誤報告文字
  flag102_temp <- flag_person_wide_flag102 %>%
    group_by(organization_id) %>%
    mutate(flag102_txt = paste(source, "：", flag102_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag102_txt)) %>%
    distinct(organization_id, flag102_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag102 <- flag102_temp %>%
    dcast(organization_id ~ flag102_txt, value.var = "flag102_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag102)[2:length(colnames(flag102))]
  flag102$flag102 <- NA
  for (i in temp) {
    flag102$flag102 <- paste(flag102$flag102, flag102[[i]], sep = "； ")
  }
  flag102$flag102 <- gsub("NA； ", replacement = "", flag102$flag102)
  flag102$flag102 <- gsub("； NA", replacement = "", flag102$flag102)

#聘任訖日早於本學期
#（『聘任訖日』係指聘期最後一日，惟上述留職停薪人員並未自學校離職或退休，爰請確認並依欄位說明修正上開人員之聘任訖日，若確實為其聘任期程之最後一日，請來電告知。）
#（『聘任訖日』係指聘期最後一日，爰請確認並依欄位說明修正全體教職員(工)之聘任訖日，若有特殊情況，請來電告知。）
      
  #產生檢誤報告文字
  flag102 <- flag102 %>%
    subset(select = c(organization_id, flag102)) %>%
    distinct(organization_id, flag102) %>%
    mutate(flag102 = paste(flag102, "（請確認並依欄位說明修正上開人員之聘任訖日，若確實為其聘任期程之最後一日，請來電告知。）", sep = ""))
} else{
  #偵測flag102是否存在。若不存在，則產生NA行
  if ('flag102' %in% ls()) {
    print("flag102")
  } else{
    flag102 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag102$flag102 <- ""
  }
}
