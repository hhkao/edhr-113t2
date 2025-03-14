rm(list=ls())

#整理歷年資料 匯出每學校最新一期的資料

#####1131資料#####

path <- "\\\\192.168.110.245\\Plan_edhr\\教育部高級中等學校教育人力資源資料庫建置第8期計畫(1130801_1140731)\\14. 分析報告\\113學年度上學期高級中等學校教育人力資源資料庫（全國學校人事）.xlsx"

data_teacher <- readxl :: read_excel(path, sheet = "教員資料表")
colname <- data_teacher[1, ]
colnames(data_teacher) <- colname
data_teacher <- data_teacher[-1, ]

data_staff   <- readxl :: read_excel(path, sheet = "職員(工)資料表")
colname <- data_staff[1, ]
colnames(data_staff) <- colname
data_staff <- data_staff[-1, ]

data_retire   <- readxl :: read_excel(path, sheet = "離退教職員(工)資料表")
colname <- data_retire[1, ]
colnames(data_retire) <- colname
data_retire <- data_retire[-1, ]

# 合併人事資料表 ----------------------------------------------------------------
data_teacher <- data_teacher %>%
  mutate(source = 1)

data_staff <- data_staff %>%
  mutate(source = 2)

drev_person_1131 <- bind_rows(data_teacher, data_staff) %>%
  mutate(year = 1131)

drev_person_1131$source  <-
  factor(
    drev_person_1131$source,
    levels = c(1, 2),
    labels = c("教員資料表", "職員(工)資料表")
  )

#####1122資料#####

path <- "\\\\192.168.110.245\\Plan_edhr\\教育部高級中等學校教育人力資源資料庫建置第7期計畫(1120201_1130731)\\分析報告\\原始資料\\112學年度下學期高級中等學校教育人力資源資料庫（國立學校人事）_原始資料.xlsx"

data_teacher <- readxl :: read_excel(path, sheet = "教員資料表")
colname <- data_teacher[1, ]
colnames(data_teacher) <- colname
data_teacher <- data_teacher[-1, ]

data_staff   <- readxl :: read_excel(path, sheet = "職員(工)資料表")
colname <- data_staff[1, ]
colnames(data_staff) <- colname
data_staff <- data_staff[-1, ]

data_retire   <- readxl :: read_excel(path, sheet = "離退教職員(工)資料表")
colname <- data_retire[1, ]
colnames(data_retire) <- colname
data_retire <- data_retire[-1, ]

# 合併人事資料表 ----------------------------------------------------------------
data_teacher <- data_teacher %>%
  mutate(source = 1)

data_staff <- data_staff %>%
  mutate(source = 2)

drev_person_1122 <- bind_rows(data_teacher, data_staff) %>%
  mutate(year = 1122)

drev_person_1122$source  <-
  factor(
    drev_person_1122$source,
    levels = c(1, 2),
    labels = c("教員資料表", "職員(工)資料表")
  )

#####1121資料#####

path <- "\\\\192.168.110.245\\Plan_edhr\\教育部高級中等學校教育人力資源資料庫建置第7期計畫(1120201_1130731)\\分析報告\\原始資料\\112學年度上學期高級中等學校教育人力資源資料庫（全國學校人事）_原始資料.xlsx"

data_teacher <- readxl :: read_excel(path, sheet = "教員資料表")
colname <- data_teacher[1, ]
colnames(data_teacher) <- colname
data_teacher <- data_teacher[-1, ]

data_staff   <- readxl :: read_excel(path, sheet = "職員(工)資料表")
colname <- data_staff[1, ]
colnames(data_staff) <- colname
data_staff <- data_staff[-1, ]

data_retire   <- readxl :: read_excel(path, sheet = "離退教職員(工)資料表")
colname <- data_retire[1, ]
colnames(data_retire) <- colname
data_retire <- data_retire[-1, ]

# 合併人事資料表 ----------------------------------------------------------------
data_teacher <- data_teacher %>%
  mutate(source = 1)

data_staff <- data_staff %>%
  mutate(source = 2)

drev_person_1121 <- bind_rows(data_teacher, data_staff) %>%
  mutate(year = 1121)

drev_person_1121$source  <-
  factor(
    drev_person_1121$source,
    levels = c(1, 2),
    labels = c("教員資料表", "職員(工)資料表")
  )

#####合併歷年資料#####
drev_person <- bind_rows(drev_person_1121, drev_person_1122, drev_person_1131) %>%
  select( "name", 
          "idnumber", 
          "gender", 
          "birthdate", 
          "implcls", 
          "nation", 
          "degree", 
          "ddegreen1", 
          "ddegreeu1", 
          "ddegreeg1", 
          "ddegreen2", 
          "ddegreeu2", 
          "ddegreeg2", 
          "mdegreen1", 
          "mdegreeu1", 
          "mdegreeg1", 
          "mdegreen2", 
          "mdegreeu2", 
          "mdegreeg2", 
          "bdegreen1", 
          "bdegreeu1", 
          "bdegreeg1", 
          "bdegreen2", 
          "bdegreeu2", 
          "bdegreeg2", 
          "adegreen1", 
          "adegreeu1", 
          "adegreeg1", 
          "adegreen2", 
          "adegreeu2", 
          "adegreeg2", 
          "sertype", 
          "emptype", 
          "emsub", 
          "empunit", 
          "skillteacher", 
          "counselor", 
          "speteacher", 
          "joiteacher", 
          "expecter", 
          "workexp", 
          "study", 
          "admintitle", 
          "adminunit", 
          "admintitle1", 
          "adminunit1", 
          "admintitle2", 
          "adminunit2", 
          "admintitle3", 
          "adminunit3", 
          "leave", 
          "levpay", 
          "brtype", 
          "negle", 
          "pxytype", 
          "suspend", 
          "onbodat", 
          "enddate", 
          "desedym", 
          "beobdym", 
          "organization_id", 
          "source", 
          "year"
          ) %>%
  group_by(organization_id) %>%
  filter(year == max(year)) %>%
  ungroup()

openxlsx::write.xlsx(drev_person,
                     file = "C:\\edhr-113t2\\edhr.xlsx",
                     rowNames = FALSE,
                     overwrite = TRUE)
