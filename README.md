病例趋势代码
代码：
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(scales)
Sys.setlocale("LC_TIME", "English")

covid_data <- read_excel("D:\\桌面\\胡晨-社会流行病\\数据\\每日新增病例和重症病例.xlsx") %>% 
  mutate(
    date = ymd(date),
    新增病例 = 新增病例,
    重症病例 = 重症病例
  ) %>% 
  drop_na(新增病例, 重症病例)

emergency <- tribble(
  ~start,        ~end,
  "2020-04-01",  "2020-05-30",
  "2021-01-01",  "2021-03-30",
  "2021-05-01",  "2021-06-30",
  "2021-07-15",  "2021-10-01"
) %>% 
  mutate(start = ymd(start), end = ymd(end))

phases <- tribble(
  ~phase,      ~start,        ~end,        ~label,
  "Phase 1",   "2020-05-11",  "2020-05-12", "Phase 1\nMay 11-12, 2020",
  "Phase 2",   "2021-06-14",  "2021-06-20", "Phase 2\nJune 14-20, 2021", 
  "Phase 3",   "2022-05-13",  "2022-05-30", "Phase 3\nMay 13-30, 2022"
) %>% 
  mutate(
    start = ymd(start), 
    end = ymd(end),
    mid = start + (end - start)/2
  )

scale_factor <- 120000 / 2000

ggplot(covid_data, aes(x = date)) +
  geom_rect(
    data = emergency,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    fill = "#D0E5F5", alpha = 0.7, inherit.aes = FALSE  
  ) +
  geom_col(
    aes(y = 新增病例, fill = "COVID-19 positive cases"), 
    alpha = 0.8, width = 0.8, show.legend = TRUE, color = NA
  ) +
  geom_line(
    aes(y = 重症病例 * 60, color = "Severe cases"),
    linewidth = 1, show.legend = TRUE
  ) +
  geom_text(
    data = phases,
    aes(x = mid, y = 107500, label = label),
    color = "red", size = 3.5, hjust = 0.5, fontface = "bold", lineheight = 0.8
  ) +
  geom_segment(
    data = phases,
    aes(x = mid, xend = mid, y = 100000, yend = 85000),
    color = "red", linewidth = 0.6, 
    arrow = arrow(length = unit(0.5, "cm"), type = "closed"),
    inherit.aes = FALSE
  ) +
  scale_y_continuous(
    name = "Number of COVID-19 positive cases",
    limits = c(0, 120000),
    breaks = c(0, 20000, 40000, 60000, 80000, 100000, 120000),
    labels = label_comma(),
    sec.axis = sec_axis(
      ~ . / scale_factor, 
      name = "Number of severe cases",
      breaks = c(0, 200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800, 2000)
    )
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%B %d, %Y",
    limits = c(ymd("2020-4-01"), ymd("2022-5-30")),
    expand = expansion(mult = 0.01)
  ) +
  scale_fill_manual(
    values = c("COVID-19 positive cases" = "#FF8C00"),
    name = ""
  ) +
  scale_color_manual(
    values = c("Severe cases" = "#6A0DAD"),
    name = ""
  ) +
  labs(
    caption = "COVID-19 positive cases        The period of the state of emergency        Severe cases        The time of our survey"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5, 
      face = "bold", 
      size = 12,
      margin = margin(b = 15)
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 0.5,
      size = 8,
      color = "black"
    ),
    axis.text.y = element_text(
      size = 8,
      color = "black"
    ),
    axis.title.y = element_text(
      size = 9,
      margin = margin(r = 10),
      face = "bold"
    ),
    axis.title.y.right = element_text(
      size = 9,
      margin = margin(l = 10),
      face = "bold"
    ),
    axis.title.x = element_text(
      size = 9,
      margin = margin(t = 10),
      face = "bold"
    ),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.text = element_text(
      size = 8,
      margin = margin(r = 15),
      face = "bold"
    ),
    legend.spacing.x = unit(0.8, "cm"),
    legend.margin = margin(t = 5, b = 0),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(
      size = 8,
      hjust = 0.5,
      margin = margin(t = 10),
      face = "bold"
    ),
    plot.margin = margin(15, 15, 15, 15),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  guides(
    fill = guide_legend(order = 1),
    color = guide_legend(order = 2)
  )

方差分析
library(readxl)
library(tidyverse)
library(rstatix)

data <- read_excel("D:/桌面/胡晨-社会流行病/数据/osfstorage-archive/Data/胡晨已汉化SPSS版本.xlsx")

data$id <- 1:nrow(data)

variables <- c(
  "心理困扰量表", "患者健康问卷9总分", "孤独感量表第三版", "社交网络量表", 
  "主观幸福感量表", "躯体症状量表", "锻炼", "健康饮食", "健康睡眠", 
  "兴趣爱好", "线下与家人朋友见面", "线上与家人朋友聊天", "自发的戴口罩", 
  "积极未来", "家庭负担", "关系恶劣", "容易沮丧", "看鑫冠新闻紧张", 
  "由于鑫冠不能睡觉", "日常生活短缺", "工作和学业中止"
)

results <- data.frame(
  变量 = character(),
  F值 = numeric(),
  p值 = numeric(),
  效应值η² = numeric(),
  检验方法 = character(),
  stringsAsFactors = FALSE
)

for (var in variables) {
  time_points <- paste0(var, 1:3)
  
  if (all(time_points %in% names(data))) {
    cat("正在分析:", var, "\n")
    
    # 转换为长格式
    long_data <- data %>%
      select(id, all_of(time_points)) %>%
      pivot_longer(
        cols = -id,
        names_to = "time",
        values_to = "score"
      ) %>%
      mutate(
        time = factor(gsub(var, "", time)),
        id = factor(id)
      ) %>%
      filter(!is.na(score))
    
    tryCatch({
      res_anova <- anova_test(
        data = long_data,
        dv = score,
        wid = id,
        within = time
      )
      
      results <- rbind(results, data.frame(
        变量 = var,
        F值 = res_anova$F,
        p值 = res_anova$p,
        效应值η² = res_anova$ges,
        检验方法 = "重复测量方差分析",
        stringsAsFactors = FALSE
      ))
      
      cat("  完成: F =", round(res_anova$F, 2), 
          ", p =", round(res_anova$p, 4), 
          ", η² =", round(res_anova$ges, 3), "\n")
      
    }, error = function(e) {
      cat("  分析失败:", e$message, "\n")
    })
  }
}

if (all(c("酒精使用障碍2", "酒精使用障碍3") %in% names(data))) {
  cat("分析酒精使用障碍...\n")
  
  alc_data <- data %>%
    select(id, 酒精使用障碍2, 酒精使用障碍3) %>%
    filter(!is.na(酒精使用障碍2) & !is.na(酒精使用障碍3))
  
  if (nrow(alc_data) > 1) {
    t_test <- t.test(alc_data$酒精使用障碍2, alc_data$酒精使用障碍3, paired = TRUE)
    
    differences <- alc_data$酒精使用障碍2 - alc_data$酒精使用障碍3
    cohens_d <- abs(mean(differences)) / sd(differences)
    eta_sq <- cohens_d^2 / (cohens_d^2 + 4)
    
    results <- rbind(results, data.frame(
      变量 = "酒精使用障碍",
      F值 = NA,
      p值 = t_test$p.value,
      效应值η² = eta_sq,
      检验方法 = "配对t检验",
      stringsAsFactors = FALSE
    ))
    
    cat("  完成: p =", round(t_test$p.value, 4), 
        ", η² =", round(eta_sq, 3), "\n")
  }
}

final_results <- results %>%
  mutate(
    F值 = ifelse(is.na(F值),, NA, round(F值, 2)),
    p值 = round(p值, 4),
    效应值η² = round(效应值η², 3),
    显著性 = case_when(
      p值 < 0.001 ~ "***",
      p值 < 0.01 ~ "**",
      p值 < 0.05 ~ "*",
      TRUE ~ "不显著"
    )
  )

cat("\n=== 最终结果 ===\n")
print(final_results, row.names = FALSE)

write.csv(final_results, "统计分析结果.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\n结果已保存到 '统计分析结果.csv'\n")

significant <- final_results %>% filter(p值 < 0.05)
if (nrow(significant) > 0) {
  cat("\n=== 显著的结果 ===\n")
  print(significant, row.names = FALSE)
}

cat("\n=== 描述性统计（时间1）===\n")
desc_stats <- data.frame(
  变量 = character(),
  均值 = numeric(),
  标准差 = numeric(),
  stringsAsFactors = FALSE
)

for (var in variables) {
  time1 <- paste0(var, "1")
  if (time1 %in% names(data)) {
    mean_val <- mean(data[[time1]], na.rm = TRUE)
    sd_val <- sd(data[[time1]], na.rm = TRUE)
    desc_stats <- rbind(desc_stats, data.frame(
      变量 = var,
      均值 = round(mean_val, 2),
      标准差 = round(sd_val, 2)
    ))
  }
}
