library(rENA)
library(readxl)

# 1. 讀取資料
file_path <- "C:/Users/b7869/Desktop/社群網路分析/期末報告/ENA_FinalProject_Complete.xlsx"
df <- read_excel(file_path)
df <- as.data.frame(df)

# 2. 清洗欄位名稱
if ("speaker" %in% colnames(df)) names(df)[names(df) == "speaker"] <- "Speaker"
# 自動將 - 改為 _ 並移除中文
colnames(df) <- gsub("-", "_", colnames(df))
colnames(df) <- gsub("（.*）", "", colnames(df))

# 3. 過濾角色
top_speakers <- c("柯南", "工藤優作", "毛利蘭", "澤田弘樹", "阿笠博士", "諾亞方舟")
df_clean <- df[df$Speaker %in% top_speakers, ]

# 4. 【關鍵修正】自動移除「全是 0」的維度
# 原始的 8 個維度
all_codes <- c("BG_INFO", "NEWS_REPORT", "EMO_NEUTRAL", "EMO_SAD", 
               "AUTH_POWER", "TECH_EXPLAIN", "CHAR_EMO", "GOAL_MISSION")

# 計算這些主角實際上有用到的維度
valid_codes <- c()
for (code in all_codes) {
  if (code %in% colnames(df_clean) && sum(df_clean[[code]]) > 0) {
    valid_codes <- c(valid_codes, code)
  } else {
    print(paste("移除無效維度:", code)) # 這裡應該會顯示移除 NEWS_REPORT
  }
}

# 5. 設定參數 (使用篩選後的 valid_codes)
# 為了避免圖表空白，我們改用最標準的「連續對話」模式
# 讓整部電影變成一條時間軸
df_clean$Conversation <- "Movie" 

accum <- ena.accumulate.data(
  units = df_clean[, c("Speaker"), drop=FALSE],
  conversation = df_clean[, c("Conversation"), drop=FALSE], # 改成 "Movie" 讓大家在同一個宇宙對話
  codes = df_clean[, valid_codes], # 只用有效維度
  window.size.back = 6 # 設定視窗為 6
)

# 6. 畫圖
set <- ena.make.set(accum)

# 印出座標檢查 (如果這裡有數字，圖一定會出來)
print("檢查點位座標：")
print(set$points)

plot(set, title = "主要角色認知網絡分析")

# --- 進階繪圖步驟 ---

# 1. 建立一個空的繪圖基底
plot_obj <- ena.plot(set, title = "角色認知網絡分析 (ENA Network)")

# 2. 畫出「網絡連線」(這就是您想看到的網狀結構！)
# 紅色線條代表各個維度之間的關聯強度
plot_obj <- ena.plot.network(
  plot_obj, 
  network = colMeans(set$line.weights), # 取所有人的平均連線
  colors = "red"                        # 設定連線為紅色
)

# 3. 畫出「角色點位」(柯南、弘樹等人的位置)
# 藍色點點代表角色
plot_obj <- ena.plot.points(
  plot_obj, 
  labels = unique(df_clean$Speaker),    # 加上角色名字標籤
  colors = "blue",                      # 設定點為藍色
  shape = "circle"                      # 設定形狀
)

# 4. 【關鍵】最後一定要執行這行來「顯示」圖表
print(plot_obj)