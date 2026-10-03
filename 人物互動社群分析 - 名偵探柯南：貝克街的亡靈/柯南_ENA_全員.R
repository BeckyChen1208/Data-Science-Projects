library(rENA)
library(readxl)

# ==========================================
# 步驟 1：資料準備 (維持不變)
# ==========================================
file_path <- "C:/Users/b7869/Desktop/社群網路分析/期末報告/ENA_FinalProject_Complete.xlsx"
df <- read_excel(file_path)
df <- as.data.frame(df) 

if ("speaker" %in% colnames(df)) names(df)[names(df) == "speaker"] <- "Speaker"
colnames(df) <- gsub("-", "_", colnames(df))
colnames(df) <- gsub("（.*）", "", colnames(df))
df_clean <- df[!is.na(df$Speaker), ]

all_codes <- c("BG_INFO", "NEWS_REPORT", "EMO_NEUTRAL", "EMO_SAD", 
               "AUTH_POWER", "TECH_EXPLAIN", "CHAR_EMO", "GOAL_MISSION")
valid_codes <- c()
for (code in all_codes) {
  if (code %in% colnames(df_clean) && sum(as.numeric(df_clean[[code]]), na.rm = TRUE) > 0) {
    valid_codes <- c(valid_codes, code)
  }
}

df_clean$Conversation <- "Movie"
accum <- ena.accumulate.data(
  units = df_clean[, c("Speaker"), drop=FALSE],
  conversation = df_clean[, c("Conversation"), drop=FALSE],
  codes = df_clean[, valid_codes],
  window.size.back = 6
)
set <- ena.make.set(accum)

# ==========================================
# 步驟 2：最終繪圖 (無標籤版)
# ==========================================

# 1. 建立畫布
plot_obj <- ena.plot(set, title = "角色與認知類別分佈圖")

# 2. 畫紅色的網狀連線
line_weights_df <- as.data.frame(set$line.weights)
numeric_cols <- sapply(line_weights_df, is.numeric)
mean_network <- colMeans(line_weights_df[, numeric_cols])

plot_obj <- ena.plot.network(
  plot_obj, 
  network = mean_network, 
  colors = "red",
  alpha = 0.4 
)

# -------------------------------------------------------
# 資料分流
# -------------------------------------------------------
points_df <- as.data.frame(set$points)
top_speakers <- names(sort(table(df_clean$Speaker), decreasing = TRUE)[1:10])
my_colors <- rainbow(length(top_speakers))
names(my_colors) <- top_speakers
is_hero <- points_df$Speaker %in% top_speakers

# -------------------------------------------------------
# [Layer A] 路人 (灰色背景)
# -------------------------------------------------------
plot_obj <- ena.plot.points(
  plot_obj, 
  points = points_df[!is_hero, c("SVD1", "SVD2")], 
  colors = "grey60",  
  shape = "circle",
  point.size = 4,
  labels = NULL       
)

# -------------------------------------------------------
# [Layer B] 主角 (彩色 + 名字)
# -------------------------------------------------------
hero_data <- points_df[is_hero, ]
plot_obj <- ena.plot.points(
  plot_obj, 
  points = hero_data[, c("SVD1", "SVD2")], 
  labels = hero_data$Speaker,              # ★ 這裡保留名字
  colors = my_colors[hero_data$Speaker], 
  shape = "circle",           
  point.size = 10                          
)

# -------------------------------------------------------
# [Layer C] 類別 (純黑點，無字)
# -------------------------------------------------------
code_nodes <- as.data.frame(set$rotation$nodes)

plot_obj <- ena.plot.points(
  plot_obj,
  points = code_nodes[, c("SVD1", "SVD2")], 
  labels = NULL,                 # ★ 關鍵：強制不顯示標籤
  shape = "circle",              
  colors = "black",              
  point.size = 10                 
)

# 3. 輸出圖表
print(plot_obj)

# ---------------------------------
# 跟以上分開執行
# ---------------------------------

# ==========================================
# 步驟 5：繪製「比較圖」 (Subtraction Plot) - 無標籤版
# ==========================================

# 1. 設定要比較的兩個人
person_A <- "柯南"      # 紅色代表 (正值)
person_B <- "工藤優作"  # 藍色代表 (負值)

# 2. 提取並計算差異
# 確保格式正確
line_weights_df <- as.data.frame(set$line.weights)
numeric_cols <- sapply(line_weights_df, is.numeric)
points_df <- as.data.frame(set$points)

# 檢查名字是否存在
if(!person_A %in% points_df$Speaker || !person_B %in% points_df$Speaker) {
  stop("錯誤：找不到您輸入的名字，請檢查拼字！")
}

# 抓出兩人的平均連線
net_A <- colMeans(line_weights_df[points_df$Speaker == person_A, numeric_cols, drop=FALSE])
net_B <- colMeans(line_weights_df[points_df$Speaker == person_B, numeric_cols, drop=FALSE])

# 計算差異 (柯南 - 弘樹)
subtraction_network <- net_A - net_B

# 3. 開始繪圖
plot_sub <- ena.plot(set, title = paste(person_A, " (紅) vs ", person_B, " (藍) 認知差異比較"))

# --- (A) 畫出差異連線 ---
plot_sub <- ena.plot.network(
  plot_sub, 
  network = subtraction_network, 
  colors = c("blue", "red"),  # 負值=藍色(弘樹), 正值=紅色(柯南)
  alpha = 0.8                 # 線條深一點
)

# --- (B) 畫出兩人的中心點 ---
group_labels <- c(person_A, person_B)
group_colors <- c("red", "blue")
points_target <- points_df[points_df$Speaker %in% group_labels, ]

plot_sub <- ena.plot.points(
  plot_sub,
  points = points_target[, c("SVD1", "SVD2")], # 只取座標
  labels = points_target$Speaker,              # ★ 顯示主角名字
  colors = group_colors, 
  shape = "square",      
  point.size = 8
)

# --- (C) 畫出類別節點 (純黑點，無字) ---
code_nodes <- as.data.frame(set$rotation$nodes)

plot_sub <- ena.plot.points(
  plot_sub,
  points = code_nodes[, c("SVD1", "SVD2")],
  labels = NULL,         # ★ 關鍵：強制不顯示標籤
  colors = "black",
  shape = "circle",
  point.size = 5         # 大小設定為 5 (跟主圖保持一致)
)

# 4. 輸出圖表
print(plot_sub)