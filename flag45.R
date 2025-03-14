# flag45: 聘任科別應填入服務身分別為「教師」、「主任教官」、「教官」之聘任科別中文名稱。 -------------------------------------------------------------------
flag_person <- drev_person_1

#聘任科別不合理處
flag_person$err_flag <- 0
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "NA" |
         flag_person$emsub == "N" |
         flag_person$emsub == "N/A" |
         flag_person$emsub == "ＮＡ" |
         flag_person$emsub == "Na" |
         flag_person$emsub == "無") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "不分科") &
      (flag_person$sertype == "主任教官" |
         flag_person$sertype == "教官"),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "教師") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )

flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "代理") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "教官") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "主任教官") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "副校長") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "小學") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "主任") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "本土") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "本土語") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "本土語支援教師") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "本土語文") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "本土語文科") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "本土語言") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "本土語言科") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "本土語科") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "多元彈性") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "多元選修") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "多元選修") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "不分科多元選修課程") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "自然") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "自然科") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "自然科學") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "自然然科學") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "自然類") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "助教") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "其他") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "或學") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "科技") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "英雄") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "校訂必修課程") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "特殊教育") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "特殊教育科") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "特殊教育領域") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "特殊教育學系") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "特教") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "特教科") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "特教班") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "特教教師") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "特教-獨輪車") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "留停") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "留職停薪代理") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "高中自然") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "自然科領域") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "健康與體育") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "健康與體育領域") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "健體科") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "健體") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "國小") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "國小部") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "國中") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "國民小學級任教師") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "專任") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "第二外國語文") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "第二外語") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "設計") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "設計群") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "設計群科") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "普通科") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "幹事") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "微課程") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "實(懸)缺代理") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "語文") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "彈性學習") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "彈性學習多元選修") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "彈性學習時間") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "選修課程") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "選修") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "餐旅") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "餐旅服務") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "餐旅群") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "應用外語") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "應用外語科") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "高中外國文") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "藝術") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "體育健教") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "學") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("室$", flag_person$emsub),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("處$", flag_person$emsub),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("^資處$", flag_person$emsub),
    0,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("教官室", flag_person$emsub),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("教務處", flag_person$emsub),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("學務處", flag_person$emsub),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("人事室", flag_person$emsub),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("總務處", flag_person$emsub),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("會計室", flag_person$emsub),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("輔導室", flag_person$emsub),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("實習處", flag_person$emsub),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("圖書館", flag_person$emsub),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("校長室", flag_person$emsub),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("校安", flag_person$emsub),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("(全時支援他校)", flag_person$emsub),
    0,
    flag_person$err_flag
  )

#社團 聘任類別為"鐘點教師"或"兼任"
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("^社團$", flag_person$emsub),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("社$", flag_person$emsub),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("^普通$", flag_person$emsub),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("^管樂$", flag_person$emsub) &
      (flag_person$emptype == "鐘點教師" |
         flag_person$emptype == "兼任"),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("^合唱$", flag_person$emsub) &
      (flag_person$emptype == "鐘點教師" |
         flag_person$emptype == "兼任"),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("^中正之家$", flag_person$emsub) &
      (flag_person$emptype == "鐘點教師" |
         flag_person$emptype == "兼任"),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("^熱門音樂$", flag_person$emsub) &
      (flag_person$emptype == "鐘點教師" |
         flag_person$emptype == "兼任"),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("^吉他$", flag_person$emsub) &
      (flag_person$emptype == "鐘點教師" |
         flag_person$emptype == "兼任"),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("^魔術$", flag_person$emsub) &
      (flag_person$emptype == "鐘點教師" |
         flag_person$emptype == "兼任"),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("^話劇$", flag_person$emsub) &
      (flag_person$emptype == "鐘點教師" |
         flag_person$emptype == "兼任"),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("^國術$", flag_person$emsub) &
      (flag_person$emptype == "鐘點教師" |
         flag_person$emptype == "兼任"),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("^劍道$", flag_person$emsub) &
      (flag_person$emptype == "鐘點教師" |
         flag_person$emptype == "兼任"),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("^飛盤$", flag_person$emsub) &
      (flag_person$emptype == "鐘點教師" |
         flag_person$emptype == "兼任"),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("^機器人研究$", flag_person$emsub) &
      (flag_person$emptype == "鐘點教師" |
         flag_person$emptype == "兼任"),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("^儀隊$", flag_person$emsub) &
      (flag_person$emptype == "鐘點教師" |
         flag_person$emptype == "兼任"),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      grepl("^滑板$", flag_person$emsub) &
      (flag_person$emptype == "鐘點教師" |
         flag_person$emptype == "兼任"),
    1,
    flag_person$err_flag
  )

#僅填寫領域或類群名稱
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "食品群") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "家政群") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "商管群") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "商管群科") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "商管群領域") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "設計群") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "設計群科") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "電機電子群") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "電機電子群科") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "電機與電子群") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "電機與電子群科") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "機械群") &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ),
    1,
    flag_person$err_flag
  )

#「專門」指導學生「社團活動」之外聘指導教員，暫不納入填報。請依欄位說明，確認貴校教職員工名單是否正確。
#（請依欄位說明，修正聘任科別中文名稱；如為職員工，請將資料填至職員工資料表。）
#（請依欄位說明，加註第二外語或本土語之語言別。）
#（請依欄位說明，加註特教之專長別。）
#（請依欄位說明，加註特教之專長別，並刪除贅字『學系』。）
#（請依欄位說明，修正聘任科別中文名稱，請註記科目別。）

#自然科
#（請依欄位說明，修正聘任科別中文名稱，填寫自然科學領域科目別，如：物理，化學等。）

#（請依欄位說明，修正聘任科別中文名稱；如為**領域，請註記科目別。）
#（請依欄位說明，修正聘任科別中文名稱，填寫**群之科別。）

#多元選修
#（*員如為專門教授選修課程之教師，請依欄位說明，填寫專長、授課科目或課程名稱。）

#小學部教師
#（*師如於本學期有教授國中或高中課程，請填寫國中或高中之科別，若沒有教授國中或高中課程，則不需再調整。）

#NA
#（請依欄位說明，修正填寫聘任科別中文名稱。）

#職員
#（請依欄位說明，修正聘任科別中文名稱。如該員為職員，請將資料填寫至職員(工)資料表。）

#若校長的服務身份別填錯，且聘任科別填「NA」，則flag45不呈現，在flag47呈現
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (flag_person$emsub == "NA" |
         flag_person$emsub == "N") &
      (flag_person$sertype == "校長"),
    0,
    flag_person$err_flag
  )
#若聘任科別填校長，需抓出
flag_person$err_flag <-
  if_else(flag_person$source == "教員資料表" &
            (flag_person$emsub == "校長"),
          1,
          flag_person$err_flag)

#加註聘任科別
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <- case_when(
  flag_person$err_flag == 1 ~ paste(flag_person$name, "（", flag_person$emsub, "）", sep = ""),
  TRUE ~ flag_person$err_flag_txt
)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag45 <- flag_person %>%
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
    colnames(flag_person_wide_flag45)[3:length(colnames(flag_person_wide_flag45))]
  flag_person_wide_flag45$flag45_r <- NA
  for (i in temp) {
    flag_person_wide_flag45$flag45_r <-
      paste(flag_person_wide_flag45$flag45_r,
            flag_person_wide_flag45[[i]],
            sep = " ")
  }
  flag_person_wide_flag45$flag45_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag45$flag45_r)
  flag_person_wide_flag45$flag45_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag45$flag45_r)
  
  #產生檢誤報告文字
  flag45_temp <- flag_person_wide_flag45 %>%
    group_by(organization_id) %>%
    mutate(flag45_txt = paste(source,
                              "需修改聘任科別(括號內為該員所對應之聘任科別欄位內容)：",
                              flag45_r,
                              sep = ""),
           "") %>%
    subset(select = c(organization_id, flag45_txt)) %>%
    distinct(organization_id, flag45_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag45 <- flag45_temp %>%
    dcast(organization_id ~ flag45_txt, value.var = "flag45_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag45)[2:length(colnames(flag45))]
  flag45$flag45 <- NA
  for (i in temp) {
    flag45$flag45 <- paste(flag45$flag45, flag45[[i]], sep = "； ")
  }
  flag45$flag45 <- gsub("NA； ", replacement = "", flag45$flag45)
  flag45$flag45 <- gsub("； NA", replacement = "", flag45$flag45)
  
  #（請依欄位說明，修正上開「教師」之聘任科別中文名稱）
  
  #產生檢誤報告文字
  flag45 <- flag45 %>%
    subset(select = c(organization_id, flag45)) %>%
    distinct(organization_id, flag45) %>%
    mutate(flag45 = paste(flag45, "（請依欄位說明，修正聘任科別中文名稱。）", sep = ""))
} else{
  #偵測flag45是否存在。若不存在，則產生NA行
  if ('flag45' %in% ls()) {
    print("flag45")
  } else{
    flag45 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag45$flag45 <- ""
  }
}
