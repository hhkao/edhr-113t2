library(DBI)
library(odbc)
library(magrittr)
library(dplyr)
library(rJava)
library(xlsx)
library(readxl)
library(stringr)
library(haven)
library(openxlsx)
library(tidyr)
library(maditr)

# 統計處高級中等學校科別資料 -----------------------------------------------------------
url <- "https://stats.moe.gov.tw/files/detail/113/113_base2.xlsx"
filename <- "113_base2.xlsx"

if (!file.exists(filename)) {
  download.file(url, destfile = filename, mode = "wb")
} else {
  cat(paste(filename, "already exists.\n"))
}

# 讀取 Excel 檔案
data_schtype <- read_excel(filename)

colnames(data_schtype) <- data_schtype[2, ]
data_schtype <- data_schtype[-c(1:2), ]

write.csv(data_schtype, "113_base2_revise.csv", row.names = FALSE)
openxlsx :: write.xlsx(data_schtype, file = "113_base2_revise.xlsx", rowNames = FALSE, overwrite = TRUE)


# 教育部統計處公布專任教師/兼任教師/職員人數 -----------------------------------------------------------
url <- "https://stats.moe.gov.tw/files/detail/113/113_base0.xlsx"
filename <- "113_base0.xlsx"

if (!file.exists(filename)) {
  download.file(url, destfile = filename, mode = "wb")
} else {
  cat(paste(filename, "already exists.\n"))
}

# 讀取 Excel 檔案
moe_113_base0 <- read_excel(filename)

colnames(moe_113_base0) <- moe_113_base0[3, ]
moe_113_base0 <- moe_113_base0[-c(1:4), ] %>%
  select(c("學校代碼", "學校名稱", "專任教師數", "兼任教師數", "職員數")) %>%
  rename(organization_id = 學校代碼, 
         edu_name2 = 學校名稱, 
         count_emptype1_1 = 專任教師數, 
         count_emptype2_1 = 兼任教師數, 
         count_staff_1 = 職員數)

write.csv(moe_113_base0, "113_base0_revise.csv", row.names = FALSE)
openxlsx :: write.xlsx(moe_113_base0, file = "113_base0_revise.xlsx", rowNames = FALSE, overwrite = TRUE)
