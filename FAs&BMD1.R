library(ggeffects)
library(ggtext)
library(dplyr)
library(readxl)
library(stringr)
library(ggplot2)
library(gridExtra)
library(rms)
library(pdftools)#pdf_combine
library(ggrcs)
library(grid)#textGrob
library(qgcomp)
library(compareGroups)#table 1
library(tidyr)
library(patchwork)  # 加载 patchwork 包
library(lmerTest)
library(lme4)
library(ggpubr)
library(Hmisc)
library(Cairo)
library(haven)#读取sav格式文件
library(cowplot)#拼图
library(igraph)
library(Hmisc)
library(caret)
library(xgboost)
library(ROCit)
library(pROC)
library(shapviz)
library(extrafont)#loadfonts函数
library(patchwork)
library(gridGraphics)
library(gridExtra)
library(igraph)
library(openxlsx)
library(circlize)
library(psych)
library(grid)
library(gridExtra)
library(figpatch)# 外部导图进来
library(dplyr)
library(readxl)
library(ggplot2)
library(cowplot)#拼图
library(pracma)
library(metafor)
library(forestploter)
library(grid)
library(gridExtra)
library(grid)
library(gridExtra)
library(lubridate)
library(purrr)
library(tibble)
rm(list=ls())

####画图的主题theme####
theme1 <- theme_bw() +
  theme(
  text = element_text(family = "Helvetica", size = 14), 
  strip.text = element_text(face = "bold", size = 16, color = "white"),  
  strip.background = element_rect(fill = "darkblue", color = "black", linewidth = 1.5), 
  axis.title = element_text(face = "bold", size = 16, color = "black"), 
  axis.text.y = element_text(hjust = 1, vjust = 1),
  panel.grid.major = element_line(color = "grey90", linewidth = 0.4), 
  panel.grid.minor = element_blank(), 
  legend.position = "bottom", 
  legend.title = element_text(face = "bold", size = 14) 
  #panel.border = element_rect(color = "black", fill = NA, linewidth = 1.0)
)

theme2 <- theme_bw() +
  theme(
    text = element_text(family = "Helvetica", size = 14), 
    strip.text = element_text(face = "bold", size = 16, color = "white"),  
    strip.background = element_rect(fill = "darkblue", color = "black", linewidth = 1.5), 
    axis.title = element_text(face = "bold", size = 16, color = "black"), 
    axis.text.y = element_text(hjust = 1, vjust = 1), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.4),
    panel.grid.minor = element_blank(), 
    legend.position = "bottom", 
    legend.title = element_text(face = "bold", size = 14), 
    plot.title = element_text(face = "bold", size = 18, color = "black", hjust = 0)
    #panel.border = element_rect(color = "black", fill = NA, linewidth = 1.0)
  )

theme_shared <-   theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold", color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(face = "bold", size = 14, color = "black"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(1.5, "lines"),
    axis.ticks = element_line(color = "black", linewidth = 0.6)
  )
####修改变量名的函数####
# 同时修改F0和F3的脂肪酸
recode_F0F3_FA3 <- function(data, predictor_col = "Predictor") {
  # 定义脂肪酸映射规则（注意顺序，先匹配具体的 n3/n6）
  fa_patterns <- list(
    # 饱和脂肪酸 (SFA)
    ".*RBC14_0.*" = "C14:0",
    ".*RBC15_0.*" = "C15:0",
    ".*RBC16_0.*" = "C16:0",
    ".*RBC17_0.*" = "C17:0",
    ".*RBC18_0.*" = "C18:0",
    ".*RBC20_0.*" = "C20:0",
    ".*RBC22_0.*" = "C22:0",
    ".*RBC23_0.*" = "C23:0",
    ".*RBC24_0.*" = "C24:0",
    
    # 单不饱和脂肪酸 (MUFA)
    ".*RBC16_1.*" = "C16:1 n-7", 
    ".*RBC18_1.*" = "C18:1 n-9", 
    ".*RBC20_1.*" = "C20:1 n-9",
    ".*RBC22_1.*" = "C22:1 n-9",
    ".*RBC24_1.*" = "C24:1 n-9", 
    
    # n-3多不饱和脂肪酸
    ".*RBCa?18_3.*" = "C18:3 n-3",
    ".*RBC20_3.*" = "C20:3 n-3",
    ".*RBC20_5.*" = "C20:5 n-3", 
    ".*RBC22_5.*" = "C22:5 n-3",
    ".*RBC22_6.*" = "C22:6 n-3",
    
    # n-6多不饱和脂肪酸
    ".*RBC18_2.*" = "C18:2 n-6",
    ".*RBC20_2.*" = "C20:2 n-6", 
    ".*RBC20_4.*" = "C20:4 n-6",
    ".*RBC22_4.*" = "C22:4 n-6",
    
    # 类别：先匹配 n3PUFA / n6PUFA / n6n3ratio
    ".*n3PUFA.*" = "n3PUFA",
    ".*n6PUFA.*" = "n6PUFA",
    ".*n6n3ratio.*" = "n6PUFA/n3PUFA",
    
    # PUFA 匹配（确保不包含前面已匹配的 n3PUFA 或 n6PUFA）
    "(?<!n3)(?<!n6)PUFA" = "PUFA",
    
    # 其他类别
    ".*SFA.*" = "SFA",
    ".*MUFA.*" = "MUFA",
    ".*LA_ALA.*" = "LA/ALA", 
    ".*AA_EPA_DHA.*" = "AA/(EPA+DHA)"
  )
  
  # 智能重新编码函数
  smart_recode <- function(x) {
    for (pattern in names(fa_patterns)) {
      if (grepl(pattern, x, ignore.case = TRUE, perl = TRUE)) {
        return(fa_patterns[[pattern]])
      }
    }
    return(x)
  }
  
  # 应用重新编码
  data[[predictor_col]] <- sapply(data[[predictor_col]], smart_recode)
  
  # 设置因子水平（倒序）
  fa_levels <- c(
    "C14:0", "C15:0", "C16:0", "C17:0", "C18:0", "C20:0", "C22:0", "C23:0", "C24:0",
    "C16:1 n-7", "C18:1 n-9", "C20:1 n-9", "C22:1 n-9","C24:1 n-9",
    "C18:3 n-3", "C20:3 n-3", "C20:5 n-3","C22:5 n-3", "C22:6 n-3",
    "C18:2 n-6", "C20:2 n-6", "C20:4 n-6","C22:4 n-6",
    "n3PUFA", "n6PUFA","SFA", "MUFA", "PUFA",  "n6PUFA/n3PUFA",
    "LA/ALA", "AA/(EPA+DHA)"
  )
  
  data[[predictor_col]] <- factor(data[[predictor_col]], levels = rev(fa_levels))
  
  return(data)
}
recode_F0F3_FA2 <- function(data, predictor_col = "Predictor") {
  # 定义脂肪酸映射规则（注意顺序，先匹配具体的 n3/n6）
  fa_patterns <- list(
    # 饱和脂肪酸 (SFA)
    ".*RBC14_0.*" = "C14:0",
    ".*RBC15_0.*" = "C15:0",
    ".*RBC16_0.*" = "C16:0",
    ".*RBC17_0.*" = "C17:0",
    ".*RBC18_0.*" = "C18:0",
    ".*RBC20_0.*" = "C20:0",
    ".*RBC22_0.*" = "C22:0",
    ".*RBC23_0.*" = "C23:0",
    ".*RBC24_0.*" = "C24:0",
    
    # 单不饱和脂肪酸 (MUFA)
    ".*RBC16_1.*" = "C16:1 n-7", 
    ".*RBC18_1.*" = "C18:1 n-9", 
    ".*RBC20_1.*" = "C20:1 n-9",
    ".*RBC22_1.*" = "C22:1 n-9",
    ".*RBC24_1.*" = "C24:1 n-9", 
    
    # n-3多不饱和脂肪酸
    ".*RBCa?18_3.*" = "C18:3 n-3",
    ".*RBC20_3.*" = "C20:3 n-3",
    ".*RBC20_5.*" = "C20:5 n-3", 
    ".*RBC22_5.*" = "C22:5 n-3",
    ".*RBC22_6.*" = "C22:6 n-3",
    
    # n-6多不饱和脂肪酸
    ".*RBC18_2.*" = "C18:2 n-6",
    ".*RBC20_2.*" = "C20:2 n-6", 
    ".*RBC20_4.*" = "C20:4 n-6",
    ".*RBC22_4.*" = "C22:4 n-6",
    
    # 类别：先匹配 n3PUFA / n6PUFA / n6n3ratio
    ".*n3PUFA.*" = "n3PUFA",
    ".*n6PUFA.*" = "n6PUFA",
    ".*n6n3ratio.*" = "n6PUFA/n3PUFA",
    
    # PUFA 匹配（确保不包含前面已匹配的 n3PUFA 或 n6PUFA）
    "(?<!n3)(?<!n6)PUFA" = "PUFA",
    
    # 其他类别
    ".*SFA.*" = "SFA",
    ".*MUFA.*" = "MUFA",
    ".*LA_ALA.*" = "LA/ALA", 
    ".*AA_EPA_DHA.*" = "AA/(EPA+DHA)"
  )
  
  # 智能重新编码函数
  smart_recode <- function(x) {
    for (pattern in names(fa_patterns)) {
      if (grepl(pattern, x, ignore.case = TRUE, perl = TRUE)) {
        return(fa_patterns[[pattern]])
      }
    }
    return(x)
  }
  
  # 应用重新编码
  data[[predictor_col]] <- sapply(data[[predictor_col]], smart_recode)
  
  # 设置因子水平（倒序）
  fa_levels <- c(
    "C14:0", "C15:0", "C16:0", "C17:0", "C18:0", "C20:0", "C22:0", "C23:0", "C24:0",
    "C16:1 n-7", "C18:1 n-9", "C20:1 n-9", "C22:1 n-9","C24:1 n-9",
    "C18:3 n-3", "C20:3 n-3", "C20:5 n-3","C22:5 n-3", "C22:6 n-3",
    "C18:2 n-6", "C20:2 n-6", "C20:4 n-6","C22:4 n-6",
    "n3PUFA", "n6PUFA","SFA", "MUFA", "PUFA",  "n6PUFA/n3PUFA",
    "LA/ALA", "AA/(EPA+DHA)"
  )
  
  data[[predictor_col]] <- factor(data[[predictor_col]], levels = fa_levels)
  
  return(data)
}

# 修改靶向脂肪酸
recode_fatty_target1 <- function(data, var) {
  # 确保输入的列名存在
  if (!var %in% colnames(data)) {
    stop(paste("Column", var, "not found in the data."))
  }
  
  # 定义脂肪酸名称映射
  fa_mapping <- list(
    "Myristic.acid" = "C14:0",
    "Palmitoleic.acid" = "C16:1 n-7",
    "Alpha_Linolenic.acid" = "C18:3 n-3",
    "Linoleic.acid" = "C18:2 n-6",
    "Eicosapentaenoic.acid" = "C20:5 n-3",
    "Arachidonic.acid" = "C20:4 n-6",
    "Docosahexaenoic.acid" = "C22:6 n-3",
    "Docosapentaenoic.acid" = "C22:5 n-3",
    "Adrenic.acid" = "C22:4 n-6",
    "Oleic.acid" = "C18:1 n-9"
  )
  
  # 应用智能重新编码
  for (fa_name in names(fa_mapping)) {
    data[[var]] <- ifelse(grepl(fa_name, data[[var]]), 
                          fa_mapping[[fa_name]], 
                          data[[var]])
  }
  
  # 将 Predictor 列转换为因子，并指定顺序
  levels_original <- c("C14:0", "C16:1 n-7","C18:1 n-9",
                       "C18:3 n-3","C20:5 n-3","C22:5 n-3","C22:6 n-3",
                       "C18:2 n-6","C20:4 n-6","C22:4 n-6")
  
  # 倒序排列
  data[[var]] <- factor(data[[var]],  levels = levels_original)
  
  return(data)
}
recode_fatty_target2 <- function(data, var) {
  # 确保输入的列名存在
  if (!var %in% colnames(data)) {
    stop(paste("Column", var, "not found in the data."))
  }
  
  # 定义脂肪酸名称映射
  fa_mapping <- list(
    "Myristic.acid" = "C14:0",
    "Palmitoleic.acid" = "C16:1 n-7",
    "Alpha_Linolenic.acid" = "C18:3 n-3",
    "Linoleic.acid" = "C18:2 n-6",
    "Eicosapentaenoic.acid" = "C20:5 n-3",
    "Arachidonic.acid" = "C20:4 n-6",
    "Docosahexaenoic.acid" = "C22:6 n-3",
    "Docosapentaenoic.acid" = "C22:5 n-3",
    "Adrenic.acid" = "C22:4 n-6",
    "Oleic.acid" = "C18:1 n-9"
  )
  
  # 应用智能重新编码
  for (fa_name in names(fa_mapping)) {
    data[[var]] <- ifelse(grepl(fa_name, data[[var]]), 
                          fa_mapping[[fa_name]], 
                          data[[var]])
  }
  
  # 将 Predictor 列转换为因子，并指定顺序
  levels_original <- c("C14:0", "C16:1 n-7","C18:1 n-9",
                       "C18:3 n-3","C20:5 n-3","C22:5 n-3","C22:6 n-3",
                       "C18:2 n-6","C20:4 n-6","C22:4 n-6")
  
  # 倒序排列
  data[[var]] <- factor(data[[var]],  levels = rev(levels_original))
  
  return(data)
}

# 修改F0骨量名
recode_AUC_Bone_enhanced <- function(data, outcome_col = "Outcome") {
  # 创建映射规则（键名要精确对应原始变量的 base name）
  bone_mapping <- list(
    "WBTOT_BMD" = "Whole Body BMD",
    "TOT_BMD"  = "Lumbar Spine BMD", 
    "HTOT_BMD" = "Total Hip BMD",
    "NECK_BMD" = "Femur Neck BMD"
  )
  
  # 智能重新编码函数：先去掉可能的后缀（如 _AUC 或 _AUCxxx），再精确匹配
  smart_recode <- function(x) {
    # 保护 NA
    if (is.na(x)) return(NA_character_)
    # 去掉可能的后缀（例如 "_AUC", "_AUC_F1" 等）
    base <- sub("_AUC.*$|_Z$", "", x)
    # 如果 base 精确匹配 mapping 的键，则返回映射值
    if (base %in% names(bone_mapping)) {
      return(bone_mapping[[base]])
    }
    # 如果没有匹配，返回原始值（或返回 base，按需求可改）
    return(x)
  }
  
  # 应用重新编码（保持原列中 NA 不变）
  data[[outcome_col]] <- vapply(data[[outcome_col]], smart_recode, FUN.VALUE = character(1), USE.NAMES = FALSE)
  
  # 将 Outcome 列转换为因子，并指定顺序（只包含 mapping 中的有效级别）
  valid_levels <- c("Whole Body BMD", "Lumbar Spine BMD", "Total Hip BMD", "Femur Neck BMD")
  # 仅当值在 valid_levels 中才设为 factor，否则保留原始值但不在因子水平内（可按需调整）
  data[[outcome_col]] <- factor(data[[outcome_col]], levels = valid_levels, ordered = TRUE)
  
  return(data)
}
####共用的函数####
calculate_auc_slope <- function(prefixes = c("F0", "F2", "F3", "F4"), 
                                data, 
                                base_names) {
  # 创建结果数据框
  Metabolites_target_AUC_slope <- data.frame(ID = data$ID)
  
  # 遍历每个代谢物
  for (metab in base_names) {
    # 提取该代谢物在不同时间点的变量名
    metab_vars <- paste0(prefixes, metab)
    
    # 确保变量名都存在
    if (all(metab_vars %in% names(data))) {
      auc_values <- numeric(nrow(data))
      slope_values <- numeric(nrow(data))
      
      # 对每个受试者计算 AUC 和 slope
      for (i in 1:nrow(data)) {
        # 根据 prefixes 动态生成对应的 age 列名
        age_vars <- paste0(prefixes, "age")  # 去掉 "_" 后再加 age
        ages <- unlist(data[i, age_vars])
        
        # 取代谢物值
        values <- unlist(data[i, metab_vars])
        
        # 检查是否有缺失值
        if (any(is.na(ages)) || any(is.na(values))) {
          auc_values[i] <- NA
          slope_values[i] <- NA
        } else {
          # AUC：用梯形法积分
          auc_values[i] <- trapz(ages, values)
          # slope：线性回归的斜率
          slope_values[i] <- coef(lm(values ~ ages))[2]
        }
      }
      
      # 将结果添加到输出数据框
      Metabolites_target_AUC_slope[[paste0(metab, "_AUC")]] <- auc_values
      Metabolites_target_AUC_slope[[paste0(metab, "_slope")]] <- slope_values
    }
  }
  
  return(Metabolites_target_AUC_slope)
}
#***************************GLM分析
process_glm2 <- function(FAs_names, Y, data, covariates, gender_label) {
  gender_forest_data <- data.frame()
  for (x in FAs_names) {
    for (y in Y) {
      model <- glm(as.formula(paste(y, "~", x, "+", paste(covariates, collapse = "+"))), 
                   data = data, family = gaussian())
      summary_model <- summary(model)$coefficients
      p_value <- summary_model[rownames(summary_model) == x, "Pr(>|t|)"]
      beta_value <- summary_model[rownames(summary_model) == x, "Estimate"]
      std_error <- summary_model[rownames(summary_model) == x, "Std. Error"]
      ci_low <- beta_value - 1.96 * std_error
      ci_high <- beta_value + 1.96 * std_error
      is_significant <- ifelse(p_value < 0.05, "Significant", "Not Significant")
      gender_forest_data <- rbind(gender_forest_data, 
                                  data.frame(Outcome = y, Predictor = x, 
                                             Estimate = beta_value, 
                                             Std.Error = std_error, 
                                             CI_low = ci_low, 
                                             CI_high = ci_high, 
                                             P_value = p_value,
                                             Significance = is_significant,
                                             Gender = switch(as.character(gender_label),
                                                             "0" = "Female",
                                                             "1" = "Male",
                                                             "2" = "All",
                                                             NA),
                                             stringsAsFactors = FALSE))
    }
  }
  return(gender_forest_data)
}
# process_glm3可以做性别交互分析
process_glm3 <- function(FAs_names, Y, data, covariates, gender_label) {
  gender_forest_data <- data.frame()
  
  for (x in FAs_names) {
    for (y in Y) {
      
      # 构建公式
      if (gender_label == 2) {
        formula_str <- paste0(y, " ~ ", x, " * Sex_F0 + ", paste(covariates, collapse = " + "))
      } else {
        formula_str <- paste0(y, " ~ ", x, " + ", paste(covariates, collapse = " + "))
      }
      
      model <- glm(as.formula(formula_str), data = data, family = gaussian())
      summary_model <- summary(model)$coefficients
      
      # 提取主效应
      terms_to_extract <- x
      
      # 如果是交互模型，提取所有 x 与 Sex_F0 的交互项
      if (gender_label == 2) {
        interaction_terms <- grep(paste0("^", x, ":"), rownames(summary_model), value = TRUE)
        terms_to_extract <- c(terms_to_extract, interaction_terms)
      }
      
      # 遍历每个要提取的term
      for (term in terms_to_extract) {
        if (!(term %in% rownames(summary_model))) next
        
        beta_value <- summary_model[term, "Estimate"]
        std_error <- summary_model[term, "Std. Error"]
        ci_low <- beta_value - 1.96 * std_error
        ci_high <- beta_value + 1.96 * std_error
        p_value <- summary_model[term, "Pr(>|t|)"]
        is_significant <- ifelse(p_value < 0.05, "Significant", "Not Significant")
        
        gender_forest_data <- rbind(gender_forest_data,
                                    data.frame(
                                      Outcome = y,
                                      Predictor = term,
                                      Estimate = beta_value,
                                      Std.Error = std_error,
                                      CI_low = ci_low,
                                      CI_high = ci_high,
                                      P_value = p_value,
                                      Significance = is_significant,
                                      Gender = switch(as.character(gender_label),
                                                      "0" = "Female",
                                                      "1" = "Male",
                                                      "2" = "All",
                                                      NA),
                                      stringsAsFactors = FALSE
                                    ))
      }
    }
  }
  
  return(gender_forest_data)
}


scale_selected_columns <- function(data, cols) {
  # 遍历目标列，进行标准化
  for (col in cols) {
    new_col <- paste0(col, "_Z")
    data[[new_col]] <- as.numeric(scale(data[[col]], center = TRUE, scale = TRUE))
  }
  return(data)
}

calc_mean_zscore_FAs <- function(data, prefix1 = "F0", prefix2 = "F3") {
  # 找出指定时间点的Z分数列
  cols1 <- grep(paste0("^", prefix1, ".*_Z$"), colnames(data), value = TRUE)
  cols2 <- grep(paste0("^", prefix2, ".*_Z$"), colnames(data), value = TRUE)
  
  # 去掉前缀，只保留脂肪酸名部分
  names1 <- sub(paste0("^", prefix1), "", cols1)
  names2 <- sub(paste0("^", prefix2), "", cols2)
  
  # 找出共有脂肪酸
  common_names <- intersect(names1, names2)
  
  # 为每个共有脂肪酸计算Z分数均值
  for (name in common_names) {
    col1 <- paste0(prefix1, name)
    col2 <- paste0(prefix2, name)
    mean_col <- paste0("Mean", name)  # 新变量名，如 Mean_RBC14_0_Z
    
    data[[mean_col]] <- rowMeans(
      data[, c(col1, col2)], 
      na.rm = TRUE
    )
  }
  
  return(data)
}
#***************************LMM分析
process_lmer <- function(data, X, Y, covariates, gender_label) {
  gender_forest_data <- data.frame()
  for (x in X) {
    for (y in Y) {
      formula <- as.formula(paste(y, "~", x, "+", paste(covariates, collapse = "+"), "+ (1|ID)"))
      lme_model <- lmer(formula, data = data)
      summary_model <- summary(lme_model)$coefficients
      p_value <- summary_model[rownames(summary_model) == x, "Pr(>|t|)"]
      beta_value <- summary_model[rownames(summary_model) == x, "Estimate"]
      std_error <- summary_model[rownames(summary_model) == x, "Std. Error"]
      ci_low <- beta_value - 1.96 * std_error
      ci_high <- beta_value + 1.96 * std_error
      is_significant <- ifelse(p_value < 0.05, "Significant", "Not Significant")
      gender_forest_data <- rbind(gender_forest_data, 
                                  data.frame(Outcome = y, Predictor = x, 
                                             Estimate = beta_value, 
                                             Std.Error = std_error,
                                             CI_low = ci_low, 
                                             CI_high = ci_high, 
                                             Significance = is_significant,
                                             P_value = p_value,  # Adding P-value here
                                             Gender = switch(as.character(gender_label),
                                                             "0" = "Female",
                                                             "1" = "Male",
                                                             "2" = "All",
                                                             NA),
                                             stringsAsFactors = FALSE))
    }
  }
  return(gender_forest_data)
}
# process_lmer3可以做性别交互分析
process_lmer3 <- function(data, X, Y, covariates, gender_label) {
  gender_forest_data <- data.frame()
  
  for (x in X) {
    for (y in Y) {
      
      # 构建公式
      if (gender_label == 2) {
        # 性别交互模型
        formula_str <- paste0(y, " ~ ", x, " * Sex_F0 + ", paste(covariates, collapse = " + "), " + (1|ID)")
      } else {
        formula_str <- paste0(y, " ~ ", x, " + ", paste(covariates, collapse = " + "), " + (1|ID)")
      }
      
      lme_model <- lmer(as.formula(formula_str), data = data)
      summary_model <- summary(lme_model)$coefficients
      
      # 提取要保存的terms
      terms_to_extract <- x
      
      if (gender_label == 2) {
        # 匹配所有交互项 x:Sex_F0
        interaction_terms <- grep(paste0("^", x, ":"), rownames(summary_model), value = TRUE)
        terms_to_extract <- c(terms_to_extract, interaction_terms)
      }
      
      # 遍历每个term
      for (term in terms_to_extract) {
        if (!(term %in% rownames(summary_model))) next
        
        beta_value <- summary_model[term, "Estimate"]
        std_error <- summary_model[term, "Std. Error"]
        ci_low <- beta_value - 1.96 * std_error
        ci_high <- beta_value + 1.96 * std_error
        p_value <- summary_model[term, "Pr(>|t|)"]
        is_significant <- ifelse(p_value < 0.05, "Significant", "Not Significant")
        
        # 保存结果
        gender_forest_data <- rbind(gender_forest_data,
                                    data.frame(
                                      Outcome = y,
                                      Predictor = term,
                                      Estimate = beta_value,
                                      Std.Error = std_error,
                                      CI_low = ci_low,
                                      CI_high = ci_high,
                                      P_value = p_value,
                                      Significance = is_significant,
                                      Gender = switch(as.character(gender_label),
                                                      "0" = "Female",
                                                      "1" = "Male",
                                                      "2" = "All",
                                                      NA),
                                      stringsAsFactors = FALSE
                                    ))
      }
      
    }
  }
  
  return(gender_forest_data)
}
#*************************森林图可视化函数
plot_forest4_enhanced <- function(data, x_title, title, model1, model2, colors = c("#2E86AB", "#A23B72")) {
  
  # 计算缩放比例
  m1_median <- median(abs(data$Estimate[data$Model == model1]), na.rm = TRUE)
  m2_median <- median(abs(data$Estimate[data$Model == model2]), na.rm = TRUE)
  scale_factor <- m2_median / m1_median
  
  # 缩放第二个模型
  data <- data %>%
    mutate(
      Estimate_scaled = ifelse(Model == model2, Estimate / scale_factor, Estimate),
      CI_low_scaled   = ifelse(Model == model2, CI_low / scale_factor, CI_low),
      CI_high_scaled  = ifelse(Model == model2, CI_high / scale_factor, CI_high)
    )
  
  # Predictor 顺序
  predictor_levels <- levels(factor(data$Predictor))
  n_predictors <- length(predictor_levels)
  
  # 自动生成颜色向量，名字对应模型
  model_colors <- setNames(colors, c(model1, model2))
  
  # 绘图
  p <- ggplot(
    data, 
    aes(
      x = Predictor, 
      y = Estimate_scaled, 
      ymin = CI_low_scaled, 
      ymax = CI_high_scaled,
      color = Model,
      fill = Model
    )
  ) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray20", linewidth = 0.3) +
    geom_vline(xintercept = seq(1.5, n_predictors - 0.5, by = 1),
               linetype = "dashed", color = "gray20", linewidth = 0.3) +
    geom_pointrange(position = position_dodge(width = 0.6), size = 0.8, fatten = 1.2) +
    coord_flip() +
    facet_grid(~ Gender + Outcome, space = "free_x") +
    scale_color_manual(values = model_colors, name = "Model") +
    scale_fill_manual(values = model_colors, name = "Model") +
    scale_y_continuous(
      name = paste0(substr(model1, 1, 3), " Estimate (95% CI)"),
      sec.axis = sec_axis(~ . * scale_factor, name = paste0(substr(model2, 1, 3), " Estimate (95% CI)"))
    ) +
    labs(x = x_title, title = title) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white"),
      strip.text = element_markdown(face = "bold", size = 11, color = "black"),
      axis.text.y = element_markdown(size = 10, color = "black"),
      axis.text.x = element_text(size = 9, color = "black"),
      axis.text.x.top = element_text(size = 9, color = "black",angle = 45, vjust = 0.5),
      axis.title = element_text(size = 11, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold"),
      #axis.title.y.right = element_text(color = model_colors[2]),
      #axis.title.y.left = element_text(color = model_colors[1]),
      legend.title = element_text(face = "bold", size = 10),
      legend.position = "bottom",
      legend.key = element_rect(fill = "white"),
      legend.direction = "horizontal", 
      legend.text = element_text(size = 9),
      legend.spacing = unit(0.1, "cm"),
      strip.background = element_rect(fill = "#EAF0F6", color = "grey50", size = 0.5),
      panel.border = element_rect(color = "grey50", fill = NA, size = 0.5),
      panel.spacing = unit(2.0, "lines"),
      plot.margin = unit(c(0.5, 0.6, 0, 0.3), "cm"),# 上、右、下、左的边距
      plot.title = element_text(
        face = "bold",
        size = 16,
        color = "black",
        hjust = 0   
      )
    ) +
    geom_text(
      aes(label = ifelse(P_value < 0.05, "*", "")),
      position = position_dodge(width = 0.6),
      hjust = -0.8, color = "red", size = 4, fontface = "bold"
    )+
    guides(
      color = guide_legend(override.aes = list(
        size = 0.3,
        linewidth = 0.5,
        pointsize = 1
      )),
      fill = guide_legend(override.aes = list(
        size = 0.3,
        linewidth = 0.5,
        pointsize = 1
      ))
    ) 
  
  return(p)
}
# 可以根据分面几行
plot_forest4_enhanced2 <- function(
    data,
    x_title,
    title,
    model1,
    model2,
    colors = c("#2E86AB", "#A23B72"),
    facet_nrow = 1,   # 分面行数,
    legend.key.size = 0.4
) {
  
  # 计算缩放比例
  m1_median <- median(abs(data$Estimate[data$Model == model1]), na.rm = TRUE)
  m2_median <- median(abs(data$Estimate[data$Model == model2]), na.rm = TRUE)
  scale_factor <- m2_median / m1_median
  
  # 缩放第二个模型
  data <- data %>%
    mutate(
      Estimate_scaled = ifelse(Model == model2, Estimate / scale_factor, Estimate),
      CI_low_scaled   = ifelse(Model == model2, CI_low / scale_factor, CI_low),
      CI_high_scaled  = ifelse(Model == model2, CI_high / scale_factor, CI_high)
    )
  
  # Predictor 顺序
  predictor_levels <- levels(factor(data$Predictor))
  n_predictors <- length(predictor_levels)
  
  # 模型颜色
  model_colors <- setNames(colors, c(model1, model2))
  
  # 绘图
  p <- ggplot(
    data, 
    aes(
      x = Predictor, 
      y = Estimate_scaled, 
      ymin = CI_low_scaled, 
      ymax = CI_high_scaled,
      color = Model,
      fill = Model
    )
  ) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray20", linewidth = 0.3) +
    geom_vline(
      xintercept = seq(1.5, n_predictors - 0.5, by = 1),
      linetype = "dashed", color = "gray20", linewidth = 0.3
    ) +
    geom_pointrange(
      position = position_dodge(width = 0.6),
      size = 0.8,
      fatten = 1.2
    ) +
    coord_flip() +
    
    ## 改这里：facet_wrap + nrow 参数
    facet_wrap(
      ~ Gender + Outcome,
      nrow = facet_nrow,
      scales = "free_x"
    ) +
    
    scale_color_manual(values = model_colors, name = "Model") +
    scale_fill_manual(values = model_colors, name = "Model") +
    scale_y_continuous(
      name = paste0(substr(model1, 1, 3), " Estimate (95% CI)"),
      sec.axis = sec_axis(
        ~ . * scale_factor,
        name = paste0(substr(model2, 1, 3), " Estimate (95% CI)")
      )
    ) +
    labs(x = x_title, title = title) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white"),
      strip.text = element_markdown(face = "bold", size = 11, color = "black"),
      axis.text.y = element_markdown(size = 10, color = "black"),
      axis.text.x = element_text(size = 9, color = "black"),
      axis.text.x.top = element_text(size = 9, color = "black",angle = 45, vjust = 0.5),
      axis.title = element_text(size = 11, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold"),
      #axis.title.y.right = element_text(color = model_colors[2]),
      #axis.title.y.left = element_text(color = model_colors[1]),
      legend.title = element_text(face = "bold", size = 10),
      legend.position = "bottom",
      legend.key = element_rect(fill = "white"),
      legend.direction = "horizontal", 
      legend.text = element_text(size = 9),
      legend.spacing = unit(0.1, "cm"),
      strip.background = element_rect(fill = "#EAF0F6", color = "grey50", size = 0.5),
      panel.border = element_rect(color = "grey50", fill = NA, size = 0.5),
      panel.spacing = unit(2.0, "lines"),
      plot.margin = unit(c(0.5, 0.6, 0, 0.3), "cm"),# 上、右、下、左的边距
      plot.title = element_text(
        face = "bold",
        size = 16,
        color = "black",
        hjust = 0   
      )
    ) +
    geom_text(
      aes(label = ifelse(P_value < 0.05, "*", "")),
      position = position_dodge(width = 0.6),
      hjust = -0.8,
      color = "red",
      size = 4,
      fontface = "bold"
    ) +
    guides(
      color = guide_legend(
        override.aes = list(size = legend.key.size, linewidth = 0.8)
      ),
      fill = guide_legend(
        override.aes = list(size =legend.key.size, linewidth = 0.8)
      )
    )
  
  return(p)
}
# 只有一共模型时（比如UKB只有LMM模型时）
plot_forest4_enhanced3 <- function(
    data,
    x_title,
    y_title = "LMM Estimate (95% CI)",
    title,
    model1,
    model2 = NULL,
    colors = c("#2E86AB", "#A23B72"),
    facet_nrow = 1
) {
  
  library(dplyr)
  library(ggplot2)
  
  n_model <- length(unique(data$Model))
  
  ## 情况 1：只有一个模型
  if (n_model == 1) {
    
    p <- ggplot(
      data,
      aes(
        x = Predictor,
        y = Estimate,
        ymin = CI_low,
        ymax = CI_high
      )
    ) +
      geom_hline(yintercept = 0, linetype = "dotted",
                 color = "gray20", linewidth = 0.3) +
      geom_pointrange(
        color = colors[1],
        size = 0.8,
        fatten = 1.2
      ) +
      coord_flip() +
      facet_wrap(
        ~ Gender + Outcome,
        nrow = facet_nrow,
        scales = "free_x"
      ) +
      labs(
        x = x_title,
        y = y_title,
        title = title
      ) +
      theme_bw(base_size = 12) +
      theme(
        panel.grid = element_blank(),
        strip.text = element_markdown(face = "bold", size = 11),
        axis.text.y = element_markdown(size = 10),
        axis.text.x = element_text(size = 9),
        axis.title = element_text(size = 11, face = "bold"),
        strip.background = element_rect(fill = "#EAF0F6", color = "grey50"),
        panel.border = element_rect(color = "grey50", fill = NA),
        panel.spacing = unit(2, "lines"),
        plot.title = element_text(face = "bold", size = 16, hjust = 0),
        plot.margin = unit(c(0.5, 0.6, 0, 0.3), "cm")
      ) +
      geom_text(
        aes(label = ifelse(P_value < 0.05, "*", "")),
        hjust = -0.6,
        color = "red",
        size = 4,
        fontface = "bold"
      )
    
    return(p)
  }
  
  ## 情况 2：两个模型（你原来的逻辑）
  m1_median <- median(abs(data$Estimate[data$Model == model1]), na.rm = TRUE)
  m2_median <- median(abs(data$Estimate[data$Model == model2]), na.rm = TRUE)
  scale_factor <- m2_median / m1_median
  
  data <- data %>%
    mutate(
      Estimate_scaled = ifelse(Model == model2, Estimate / scale_factor, Estimate),
      CI_low_scaled   = ifelse(Model == model2, CI_low / scale_factor, CI_low),
      CI_high_scaled  = ifelse(Model == model2, CI_high / scale_factor, CI_high)
    )
  
  predictor_levels <- levels(factor(data$Predictor))
  n_predictors <- length(predictor_levels)
  model_colors <- setNames(colors, c(model1, model2))
  
  p <- ggplot(
    data,
    aes(
      x = Predictor,
      y = Estimate_scaled,
      ymin = CI_low_scaled,
      ymax = CI_high_scaled,
      color = Model,
      fill = Model
    )
  ) +
    geom_hline(yintercept = 0, linetype = "dotted",
               color = "gray20", linewidth = 0.3) +
    geom_vline(
      xintercept = seq(1.5, n_predictors - 0.5, by = 1),
      linetype = "dashed", color = "gray20", linewidth = 0.3
    ) +
    geom_pointrange(
      position = position_dodge(width = 0.6),
      size = 0.8,
      fatten = 1.2
    ) +
    coord_flip() +
    facet_wrap(
      ~ Gender + Outcome,
      nrow = facet_nrow,
      scales = "free_x"
    ) +
    scale_color_manual(values = model_colors, name = "Model") +
    scale_fill_manual(values = model_colors, name = "Model") +
    scale_y_continuous(
      name = paste0(substr(model1, 1, 3), " Estimate (95% CI)"),
      sec.axis = sec_axis(
        ~ . * scale_factor,
        name = paste0(substr(model2, 1, 3), " Estimate (95% CI)")
      )
    ) +
    labs(x = x_title, title = title) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      strip.text = element_markdown(face = "bold", size = 11),
      axis.text.y = element_markdown(size = 10),
      axis.text.x = element_text(size = 9),
      axis.text.x.top = element_text(size = 9, angle = 45, vjust = 0.5),
      axis.title = element_text(size = 11, face = "bold"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      strip.background = element_rect(fill = "#EAF0F6", color = "grey50"),
      panel.border = element_rect(color = "grey50", fill = NA),
      panel.spacing = unit(2, "lines"),
      plot.title = element_text(face = "bold", size = 16, hjust = 0),
      plot.margin = unit(c(0.5, 0.6, 0, 0.3), "cm")
    ) +
    geom_text(
      aes(label = ifelse(P_value < 0.05, "*", "")),
      position = position_dodge(width = 0.6),
      hjust = -0.8,
      color = "red",
      size = 4,
      fontface = "bold"
    )
  
  return(p)
}
#*************************其它可视化函数
plot_fatty_acid2 <- function(data, ncol_facet = 5, plot_title = "GNHS (Erythrocyte Membrane)", y_title) {
  
  # 计算中位数和四分位间距
  FA_summary <- data %>%
    group_by(Sex_F0, Times, Fatty_Acid) %>%
    summarise(
      median_value = median(Value, na.rm = TRUE),
      q1 = quantile(Value, 0.25, na.rm = TRUE),  # 第一四分位数
      q3 = quantile(Value, 0.75, na.rm = TRUE),  # 第三四分位数
      .groups = 'drop'
    )
  
  # 绘图
  p <- ggplot(FA_summary, aes(x = Times, y = median_value, color = factor(Sex_F0))) +
    geom_point(position = position_dodge(width = 0.3), size = 0.8) +
    geom_errorbar(aes(ymin = q1, ymax = q3),  # IQR范围
                  position = position_dodge(width = 0.3), width = 0.2) +
    facet_wrap(~Fatty_Acid, scales = "free_y", ncol = ncol_facet) +
    
    # 修改：使用平方根变换替代伪对数变换
    scale_y_continuous(trans = "sqrt",  # 平方根变换
                       breaks = scales::pretty_breaks(n = 5)) +
    
    scale_color_manual(values = c("0" = "#FFBE7A", "1" = "#8ECFC9"),
                       labels = c("Female", "Male")) +
    labs(x = "", y = y_title, color = "Sex", title = plot_title) +
    theme_shared +
    theme(
      legend.position="bottom",
      strip.background = element_rect(fill = "#C8E6C9", color = "black", linewidth = 0.5)
    )
  
  return(p)
}
# 性别交互效应可视化
plot_sex_interaction_heatmap <- function(
    data,
    predictor_levels,
    title = "Sex Interaction Heatmap"
) {
  
  library(dplyr)
  library(ggplot2)
  
  plot_df <- data %>%
    mutate(
      sig_label = ifelse(Significance == "Significant", "*", ""),
      Predictor = factor(Predictor, levels = predictor_levels)
    ) %>%
    arrange(Predictor)
  
  p <- ggplot(
    plot_df,
    aes(
      y = Predictor,
      x = Outcome,
      fill = Estimate
    )
  ) +
    geom_tile(
      color = "white",
      linewidth = 0.6
    ) +
    geom_text(
      aes(label = sig_label),
      size = 6,
      fontface = "bold"
    ) +
    scale_fill_gradient2(
      low = "#2166AC",
      mid = "white",
      high = "#B2182B",
      midpoint = 0,
      name = "Interaction Estimate"
    ) +
    facet_wrap(~ Model, nrow = 1) +
    labs(
      x = NULL,
      y = NULL,
      title = title
    ) +
    theme_bw(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        vjust = 1
      ),
      strip.background = element_rect(
        fill = "grey90",
        color = "black",
        linewidth = 0.6
      ),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 14, hjust = 0),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.width = unit(0.8, "cm")
    )
  
  return(p)
}
#*************************线形+非线形混合模型
process_lmer_linear_rcs <- function(
    data,
    X,
    Y,
    covariates,
    gender_label,
    knots = 4
) {
  
  results <- data.frame()
  
  for (x in X) {
    for (y in Y) {
      
      ## 1. 构造公式

      formula_null <- as.formula(
        paste(y, "~", paste(covariates, collapse = "+"), "+ (1|ID)")
      )
      
      formula_linear <- as.formula(
        paste(y, "~", x, "+", paste(covariates, collapse = "+"), "+ (1|ID)")
      )
      
      formula_rcs <- as.formula(
        paste(
          y, "~ rcs(", x, ",", knots, ") +",
          paste(covariates, collapse = "+"), "+ (1|ID)"
        )
      )
      
      ## 2. 拟合模型（ML）

      model_null   <- lmer(formula_null,   data = data, REML = FALSE)
      model_linear <- lmer(formula_linear, data = data, REML = FALSE)
      model_rcs    <- lmer(formula_rcs,    data = data, REML = FALSE)
      
      ## 3. 线性模型的 P 值

      coef_linear <- summary(model_linear)$coefficients
      
      p_linear <- coef_linear[x, "Pr(>|t|)"]
      beta_linear <- coef_linear[x, "Estimate"]
      se_linear <- coef_linear[x, "Std. Error"]
      
      ci_low_linear  <- beta_linear - 1.96 * se_linear
      ci_high_linear <- beta_linear + 1.96 * se_linear
      
      ## 4. RCS 的 LRT P 值

      # Overall association
      lrt_overall <- anova(model_null, model_rcs)
      p_overall <- lrt_overall$`Pr(>Chisq)`[2]
      
      # Nonlinear association
      lrt_nonlinear <- anova(model_linear, model_rcs)
      p_nonlinear <- lrt_nonlinear$`Pr(>Chisq)`[2]
      
      ## 5. 汇总结果

      results <- rbind(
        results,
        data.frame(
          Outcome = y,
          Predictor = x,
          
          # Linear LMM
          Beta_linear = beta_linear,
          CI_low_linear = ci_low_linear,
          CI_high_linear = ci_high_linear,
          P_linear = p_linear,
          
          # RCS
          Knots = knots,
          P_overall = p_overall,
          P_nonlinear = p_nonlinear,
          
          Gender = switch(
            as.character(gender_label),
            "0" = "Female",
            "1" = "Male",
            "2" = "All",
            NA
          ),
          
          stringsAsFactors = FALSE
        )
      )
    }
  }
  
  return(results)
}
####*****************Covariates****************####
####只取基线的混杂####
#*****************************************4000之前基线混杂
Cov_F0 <- read_excel("/Users/hozan/Library/CloudStorage/OneDrive-Personal/Papers/2.2 中大队列/database/大队列数据库整理/原始数据库/GNHS_基本信息_F01234_20230214_VF.xlsx",sheet="F0")
# 计算准确的F0年龄
F0_surveyTimes <- read_sav("/Users/hozan/Library/CloudStorage/OneDrive-Personal/Papers/2.2 中大队列/database/大队列数据库整理/原始数据库/5010调查时间及随访间隔（重新编制）__6-1-2017_VF.sav")
Cov_F0 <- merge(Cov_F0, F0_surveyTimes[,c("编号","调查时间_F0")], by.x = "ID", by.y = "编号")
Cov_F0$`调查时间_F0` <- as.Date(Cov_F0$`调查时间_F0`)
head(Cov_F0$`出生日期_F0`)
Cov_F0$`出生日期_F0` <- as.Date(
  as.numeric(Cov_F0$`出生日期_F0`),
  origin = "1899-12-30"
)
Cov_F0$Age_F0 <- time_length(
  difftime(Cov_F0$调查时间_F0, Cov_F0$出生日期_F0), 
  unit = "years"
)
# 只保留想要的变量
Cov_F0 <- Cov_F0[,c("ID","Age_F0","出生日期_F0","性别_F0","本人教育程度分类_3_F0","家庭人均收入_3_F0","是否吸烟_F0","是否饮酒_F0","是否喝茶_F0","过去一年是否钙片_F0","过去一年是否复合维生素_F0",
                    "脑卒中_F0","心脏病_F0","癫痫_F0","帕金森_F0","老年痴呆_F0","糖尿病_F0","癌症_F0","骨折_F0","雌激素_F0","绝经年限_F0"
)]
colnames(Cov_F0) <- dplyr::recode(colnames(Cov_F0),
                                  "出生日期_F0" = "Birthday_F0",
                                  "性别_F0" = "Sex_F0",
                                  "本人教育程度分类_3_F0" = "Education_F0",
                                  "家庭人均收入_3_F0" = "Income_F0",
                                  "是否吸烟_F0" = "Smoke_F0",
                                  "是否饮酒_F0" = "Alcohol_F0",
                                  "是否喝茶_F0" = "Tea_F0",
                                  "过去一年是否钙片_F0" = "Calcium_F0",
                                  "过去一年是否复合维生素_F0" = "Vitamin_F0",
                                  "骨折_F0" = "Fracture_F0")
#绝经方法：女(0)：取值为0或NA的话即还没绝经赋值为0，如果有年龄数字的话则赋值为1；男(0)：都赋值为0
Cov_F0$Menopause_F0 <- ifelse(Cov_F0$Sex_F0 == 1,0, 
                              ifelse(Cov_F0$`绝经年限_F0`=="NA" | Cov_F0$`绝经年限_F0` == 0, 0, 1))
#只保留4000以内的观测
Cov_F0 <- Cov_F0 %>%
  filter(!grepl("^NL4", ID))
#*****************************************4000之后基线混杂
Cov_F1 <- read_excel("/Users/hozan/Library/CloudStorage/OneDrive-Personal/Papers/2.2 中大队列/database/大队列数据库整理/原始数据库/GNHS_基本信息_F01234_20230214_VF.xlsx",sheet="F1")
Cov_F1$ID <- sub("^F1", "", Cov_F1$CODE_F1)
Cov_F1 <- Cov_F1 %>%
  filter(grepl("^NL4", ID))
Cov_F1 <- Cov_F1[,c("ID","性别_F1","家庭人均收入_3_F1","是否吸烟_F1","是否饮酒_F1","是否喝茶_F1","过去一年是否钙片_F1","过去一年是否复合维生素_F1",
                    "脑卒中_F1","心脏病_F1","癫痫_F1","帕金森病_F1","老年痴呆_F1","糖尿病_F1","癌症_F1","骨折_F1","绝经年限_F1", "雌激素_F1"
)]
Birthday_NL4 <- read_sav("/Users/hozan/Library/CloudStorage/OneDrive-Personal/Papers/2.2 中大队列/database/大队列数据库整理/其它/F1NL教育20210806_VF.sav") 
Cov_F1 <- merge(Cov_F1, Birthday_NL4[,c("CODE", "出生日期","本人教育程度_3")], by.x = "ID", by.y = "CODE")
Cov_F1$`出生日期` <- as.Date(Cov_F1$`出生日期`)
#绝经方法：女(0)：取值为0或NA或-1的话即还没绝经赋值为0，如果有年龄数字的话则赋值为1；男(0)：都赋值为0
Cov_F1$Menopause_F0 <- ifelse(Cov_F1$`性别_F1` == 1,0, 
                              ifelse(Cov_F1$`绝经年限_F1`=="NA" | Cov_F1$`绝经年限_F1` == 0 | Cov_F1$`绝经年限_F1` == -1, 0, 1))
colnames(Cov_F1) <- dplyr::recode(colnames(Cov_F1),
                                  "出生日期" = "Birthday_F0",
                                  "性别_F1" = "Sex_F0",
                                  "本人教育程度_3" = "Education_F0",
                                  "家庭人均收入_3_F1" = "Income_F0",
                                  "是否吸烟_F1" = "Smoke_F0",
                                  "是否饮酒_F1" = "Alcohol_F0",
                                  "是否喝茶_F1" = "Tea_F0",
                                  "过去一年是否钙片_F1" = "Calcium_F0",
                                  "过去一年是否复合维生素_F1" = "Vitamin_F0",
                                  "骨折_F1" = "Fracture_F0",
                                  "脑卒中_F1" = "脑卒中_F0",
                                  "心脏病_F1" = "心脏病_F0",
                                  "癫痫_F1" = "癫痫_F0",
                                  "帕金森病_F1" = "帕金森_F0",
                                  "老年痴呆_F1" = "老年痴呆_F0",
                                  "糖尿病_F1" = "糖尿病_F0",
                                  "癌症_F1" = "癌症_F0",
                                  "绝经年限_F1" = "绝经年限_F0",
                                  "雌激素_F1" = "雌激素_F0")
# 计算F1的年龄
Cov_F1$CODE <- paste0("F1",Cov_F1$ID)
Follow_time <- read.csv("/Users/hozan/Library/CloudStorage/OneDrive-Personal/Papers/2.2 中大队列/database/大队列数据库整理/原始数据库/FollowData_VF.csv")
Follow_time$ID <- substr(Follow_time$CODE, 3, nchar(Follow_time$CODE))
Follow_time_BD  <- merge(Follow_time, Cov_F1[,c("CODE","Birthday_F0")], by = "CODE")
Follow_time_BD <- Follow_time_BD[complete.cases(Follow_time_BD),]

Follow_time_BD$FollowDate <- as.Date(Follow_time_BD$FollowDate)
Follow_time_BD$Birthday_F0 <- as.Date(Follow_time_BD$Birthday_F0)
Follow_time_BD$Age_F0 <- time_length(
  difftime(Follow_time_BD$FollowDate, Follow_time_BD$Birthday_F0), 
  unit = "years"
)
# 合并年龄
Cov_F1 <- merge(Cov_F1, Follow_time_BD[,c("ID","Age_F0")], by = "ID")
Cov_F1 <- dplyr::select(Cov_F1,-c("CODE"))
#*****************************************合并基线4000之前之后的基线混杂
Cov_F0_all <- rbind(Cov_F0, Cov_F1)
#雌激素为NA的是男性，所以除了女性有1外，其它直接赋值为0即可
Cov_F0_all$Estrogen_F0 <- ifelse(Cov_F0_all$`雌激素_F0` == 1,1,0)
table(Cov_F0_all$`雌激素_F0`)
table(Cov_F0_all$Estrogen)
#疾病只要之一为1，则新变量Disease就为1
Cov_F0_all$Disease_F0 <- ifelse(Cov_F0_all$`脑卒中_F0` == 1 | Cov_F0_all$`心脏病_F0` == 1 | Cov_F0_all$`癫痫_F0` == 1 | Cov_F0_all$`帕金森_F0` == 1 | Cov_F0_all$`老年痴呆_F0` == 1 | Cov_F0_all$`糖尿病_F0` == 1 | Cov_F0_all$`癌症_F0` == 1, 1, 0)
colnames(Cov_F0_all)
Cov_F0_all <- Cov_F0_all[,c("ID","Age_F0","Birthday_F0","Sex_F0","Education_F0","Smoke_F0", "Alcohol_F0","Tea_F0","Calcium_F0",
                            "Vitamin_F0","Fracture_F0","Menopause_F0","Estrogen_F0","Disease_F0")]
#取值为9也定义为无骨折
Cov_F0_all$Fracture_F0[is.na(Cov_F0_all$Fracture_F0) | Cov_F0_all$Fracture_F0 == "NA" | Cov_F0_all$Fracture_F0 == 9] <- 0
#剔除变量值取值缺失或者为NA的
Cov_F0_all <- Cov_F0_all[!apply(Cov_F0_all, 1, function(row) any(is.na(row) | row == "NA")), ]
####查看每个变量的取值情况####
Cov_F0_all_subset <- Cov_F0_all[, !colnames(Cov_F0_all) %in% c("ID", "Birthday_F0")]
# 创建一个空的数据框，用于存储所有变量的频率表
table_df <- data.frame(Variable = character(), Level = character(), Count = numeric(), stringsAsFactors = FALSE)
# 循环处理每一列，生成频率表
for (col in colnames(Cov_F0_all_subset)) {
  freq_table <- as.data.frame(table(Cov_F0_all_subset[[col]]))
  colnames(freq_table) <- c("Level", "Count")
  freq_table$Variable <- col
  table_df <- rbind(table_df, freq_table)
}
####年龄数据####
#****************长数据
Age_long <- left_join(Follow_time, Cov_F0_all[,c("ID","Birthday_F0")], by = "ID")
Age_long$FollowDate <- as.Date(Age_long$FollowDate)
Age_long$Birthday_F0 <- as.Date(Age_long$Birthday_F0)
Age_long$Age <- time_length(
  difftime(Age_long$FollowDate, Age_long$Birthday_F0), 
  unit = "years" 
)

Age_long$Times <- substr(Age_long$CODE, 1, 2)
#****************转换为宽数据
Age_wide <- Age_long %>%
  select(ID, Times, Age) %>%
  pivot_wider(
    names_from  = Times,
    values_from = Age,
    names_glue  = "{Times}age"
  )
####多次随访的混杂####
#*************************************Met-宽数据
Cov2 <- read_excel('/Users/hozan/Library/CloudStorage/OneDrive-Personal/Papers/2.2 中大队列/database/大队列数据库整理/原始数据库/GNHS_身高体重MET_VF.xlsx', 
                   sheet=1)
Cov2_wide <- Cov2 %>%
  mutate(across(-ID, as.numeric))
# 4000之前的取F0、F1、F3的均值；4000之后的取F1、F2、F3的均值;允许有缺失值。
Cov2_wide$Met_mean <- ifelse(
  grepl("^NL4", Cov2_wide$ID), 
  rowMeans(Cov2_wide[, c("一天总MET_F1", "一天总MET_F2", "一天总MET_F3")], na.rm = TRUE),  
  rowMeans(Cov2_wide[, c("一天总MET_F0", "一天总MET_F1", "一天总MET_F3")], na.rm = TRUE) 
)
#*************************************营养素-宽数据
Nutrition <- read_excel('/Users/hozan/Library/CloudStorage/OneDrive-Personal/Papers/2.2 中大队列/database/大队列数据库整理/原始数据库/GNHS_F0123膳食营养素数据_20230206_VF.xlsx',
                        sheet="GNHS_膳食数据_20230206")
Nutrition$ID <- substr(Nutrition$`编号`, 3, nchar(Nutrition$`编号`))
colnames(Nutrition)
Nutrition <- Nutrition[,c("ID","Followup","能量摄入")]
####宽数据
Nutrition_wide <- Nutrition %>%
  # 使用 pivot_wider 转换为宽格式
  pivot_wider(
    names_from = Followup, 
    values_from = c(`能量摄入`),
    names_glue = "F{Followup}{.value}"
  ) %>%
  # 保留 ID 列
  dplyr::select(ID, everything()) %>%
  as.data.frame()
####计算均值
colnames(Nutrition_wide)
Nutrition_wide <- Nutrition_wide %>%
  mutate(across(where(is.list), ~ as.character(.))) %>%  # 先转换 list 为字符
  mutate(across(-ID, as.numeric))  # 再转换数值

Nutrition_wide$Energy_mean <- ifelse(
  grepl("^NL4", Nutrition_wide$ID), 
  rowMeans(Nutrition_wide[, c("F1能量摄入","F2能量摄入","F3能量摄入")], na.rm = TRUE),  
  rowMeans(Nutrition_wide[, c("F0能量摄入", "F1能量摄入", "F3能量摄入")], na.rm = TRUE) 
)
#*************************************鱼油补充剂
Oil1 <- read_excel('/Users/hozan/Library/CloudStorage/OneDrive-Personal/Papers/2.2 中大队列/database/大队列数据库整理/原始数据库/GNHS_基本信息_F01234_20230214_VF.xlsx',
                        sheet=1)
Oil1 <- Oil1[,c("ID","是否使用深海鱼油胶囊_F2","过去一年是否服用深海鱼油_F3","否服用过深海鱼油胶囊_F1")]

Oil2 <- read_excel('/Users/hozan/Library/CloudStorage/OneDrive-Personal/Papers/2.2 中大队列/database/大队列数据库整理/原始数据库/GNHS_基本信息_F01234_20230214_VF.xlsx',
                   sheet="F4")
Oil2 <- Oil2[,c("CODE1","深海鱼油")]
colnames(Oil2) <- recode(colnames(Oil2),
                         "深海鱼油" = "深海鱼油_F4")

Oil2$CODE1 <- substr(Oil2$CODE1, 3, nchar(Oil2$CODE1))

Oil <- dplyr::left_join(Oil1, Oil2,  by = c("ID" = "CODE1"))
Oil_complete <- Oil %>%
  filter(!if_all(
    c("是否使用深海鱼油胶囊_F2", 
      "过去一年是否服用深海鱼油_F3", 
      "否服用过深海鱼油胶囊_F1", 
      "深海鱼油_F4"),
    ~ is.na(.) | . == "NA"
  ))
colnames(Oil_complete)
# 宽数据
fish_cols <- grep("鱼油", colnames(Oil), value = TRUE)
Oil_complete_wide <- Oil_complete
Oil_complete_wide$Oil_sup_follow <- ifelse(
  rowSums(Oil_complete_wide[, fish_cols] == 1, na.rm = TRUE) > 0, 
  1, 0
)

# 长数据
factor_name<-c("否服用过深海鱼油胶囊_F1", 
               "是否使用深海鱼油胶囊_F2", 
               "过去一年是否服用深海鱼油_F3", 
               "深海鱼油_F4")
idx <- which(names(Oil_complete)%in% factor_name)
for(i in idx ){
  Oil_complete[[i]] <-  as.factor(Oil_complete[[i]])
}

Oil_long <- Oil_complete %>%
  pivot_longer(
    cols = c("否服用过深海鱼油胶囊_F1", 
             "是否使用深海鱼油胶囊_F2", 
             "过去一年是否服用深海鱼油_F3", 
             "深海鱼油_F4"),
    names_to = "Times",
    values_to = "Oil_sup_follow_lmer"
  ) %>%
  mutate(
    Times = case_when(
      grepl("_F1$", Times) ~ "F1",
      grepl("_F2$", Times) ~ "F2",
      grepl("_F3$", Times) ~ "F3",
      grepl("_F4$", Times) ~ "F4",
      TRUE ~ NA_character_
    )
  )

Oil_long <- Oil_long[complete.cases(Oil_long) & Oil_long$Oil_sup_follow_lmer != "NA",]
####合并所有混杂，并定义定性定量变量####
data_list <- list(Cov_F0_all,Cov2_wide[,c("ID", "Met_mean")],Nutrition_wide[,c("ID","Energy_mean")], Oil_complete_wide[,c("ID","Oil_sup_follow")])
Cov_merge <- data_list %>% purrr::reduce(dplyr::inner_join, by = "ID")
Cov_merge <- Cov_merge[complete.cases(Cov_merge),]
#*************************定性定量定义好
numeric_name <- c("Met_mean","Energy_mean","Age_F0")
for(n in numeric_name){
  Cov_merge[[n]] <- as.numeric(Cov_merge[[n]])
}

factor_name<-c("Sex_F0","Smoke_F0","Alcohol_F0","Tea_F0","Calcium_F0","Vitamin_F0","Disease_F0",
               "Fracture_follow","Fracture_Medicine","Estrogen_F0","Menopause_F0","Fracture_F0","Oil_sup_follow")
idx <- which(names(Cov_merge)%in% factor_name)
for(i in idx ){
  Cov_merge[[i]] <-  as.factor(Cov_merge[[i]])
}
####*****************结局和暴露*****************####
####体成分####
DXA_all <- read_excel("/Users/hozan/Library/CloudStorage/OneDrive-Personal/Papers/2.2 中大队列/database/大队列数据库整理/原始数据库/GNHS_DXA_follow_10807条记录_20241014_2024.10.18recheck_VF.xlsx")
DXA_all <- DXA_all[,-1]
#*****************************把4个重要结局BMD取值为0或缺失的观测剔除
cols_to_check <- c("WBTOT_BMD", "TOT_BMD", "HTOT_BMD", "NECK_BMD")
# 筛选出这些列中任意一列为0的观测并剔除
DXA <- DXA_all %>%
  filter(rowSums(across(all_of(cols_to_check)) == 0 |is.na(across(all_of(cols_to_check)))) == 0)
DXA <- DXA[,c("CODE","WBTOT_FAT")]
#*****************************采用第四次骨量校正过的数据
DXA_adjusted <- read_excel("/Users/hozan/Library/CloudStorage/OneDrive-Personal/Papers/2.1 中老年人骨量肌肉和脂肪酸/Database/R导出/BMD_adjusetd.xlsx")

DXA <- merge(DXA, DXA_adjusted[,c("CODE","WBTOT_BMD","TOT_BMD","HTOT_BMD","NECK_BMD")], by = "CODE")
#*****************************查看骨量均数和标准差
DXA$Times <- substr(DXA$CODE, 1, 2)
DXA$ID <- substr(DXA$CODE, 3, nchar(DXA$CODE))
colnames(DXA)
# 查看骨量均数和标准差
DXA_summary <- DXA %>%
  group_by(Times) %>%
  summarise(
    WBTOT_BMD_mean = mean(WBTOT_BMD, na.rm = TRUE),
    WBTOT_BMD_sd = sd(WBTOT_BMD, na.rm = TRUE),

    TOT_BMD_mean = mean(TOT_BMD, na.rm = TRUE),
    TOT_BMD_sd = sd(TOT_BMD, na.rm = TRUE),
    
    HTOT_BMD_mean = mean(HTOT_BMD, na.rm = TRUE),
    HTOT_BMD_sd = sd(HTOT_BMD, na.rm = TRUE),
    
    NECK_BMD_mean = mean(NECK_BMD, na.rm = TRUE),
    NECK_BMD_sd = sd(NECK_BMD, na.rm = TRUE),
    
    n = n(),
    .groups = 'drop'
  )
# 剔除随访次数F4和F5
DXA <- DXA[DXA$Times != "F5", ]
table(DXA$Times)
#*****************************宽数据
DXA_wide <- DXA %>%
  pivot_wider(
    id_cols = c(ID),  # 标识变量
    names_from = Times,      # 从Times获取新变量名
    values_from = c(WBTOT_FAT, WBTOT_BMD, TOT_BMD, HTOT_BMD, NECK_BMD),  # 要转换的变量
    names_glue = "{Times}{.value}"  # 命名格式：Times_变量名
  )
# 有多少次随访的就计算多少次的体脂均值,允许有缺失值
DXA_wide$WBTOT_FAT_mean <- rowMeans(DXA_wide[, c("F1WBTOT_FAT","F2WBTOT_FAT","F3WBTOT_FAT","F4WBTOT_FAT")], na.rm = TRUE)
#*****************************逐行进行AUC和slope计算-F1234
DXA_wide_age <- merge(Age_wide[,c("ID","F1age","F2age","F3age","F4age")], DXA_wide, by = "ID")
DXA_wide_age <- DXA_wide_age[complete.cases(DXA_wide_age),]

vars <- grep("^F[1-4]", setdiff(colnames(DXA_wide_age),c("ID","F1age","F2age","F3age","F4age","WBTOT_FAT_mean","F1WBTOT_FAT","F2WBTOT_FAT","F3WBTOT_FAT","F4WBTOT_FAT")), value = TRUE)
base_names <- unique(sub("^F[1-4]", "", vars))

DXA_wide_age_AUC_slope <- calculate_auc_slope(
  prefixes = c("F1", "F2", "F3","F4"),
  data = DXA_wide_age,
  base_names = base_names
)
DXA_wide_age_AUC_slope <- merge(DXA_wide_age[,c("ID","WBTOT_FAT_mean")], DXA_wide_age_AUC_slope, by = "ID")
#*****************************逐行进行 AUC和slope计算-F234
DXA_wide_F234 <- DXA_wide %>%
  dplyr::select(ID, starts_with("F2"), starts_with("F3"), starts_with("F4"))
DXA_wide_age_F234 <- merge(Age_wide[,c("ID","F2age","F3age","F4age")], DXA_wide_F234, by = "ID")
DXA_wide_age_F234 <- DXA_wide_age_F234[complete.cases(DXA_wide_age_F234),]

vars <- grep("^F[2-4]", setdiff(colnames(DXA_wide_age),c("ID","F2age","F3age","F4age","WBTOT_FAT_mean","F2WBTOT_FAT","F3WBTOT_FAT","F4WBTOT_FAT")), value = TRUE)
base_names <- unique(sub("^F[2-4]", "", vars))

DXA_wide_age_AUC_slope_F234 <- calculate_auc_slope(
  prefixes = c("F2", "F3","F4"),
  data = DXA_wide_age_F234,
  base_names = base_names
)
DXA_wide_age_AUC_slope_F234 <- merge(DXA_wide[,c("ID","WBTOT_FAT_mean")], DXA_wide_age_AUC_slope_F234, by = "ID")
####验证DXA的AUC和slope是否计算正确
# colnames(DXA_wide_age)
# a <- DXA_wide_age[2,c("F1age","F2age","F3age","F4age","F1WBTOT_BMD","F2WBTOT_BMD","F3WBTOT_BMD","F4WBTOT_BMD")]
# ages <- unlist(a[1,c("F1age","F2age","F3age","F4age")])
# values <- unlist(a[1,c("F1WBTOT_BMD","F2WBTOT_BMD","F3WBTOT_BMD","F4WBTOT_BMD")])
# 
# trapz(ages, values)
# coef(lm(values ~ ages))[2]
####红细胞膜脂肪酸####
#********************F0脂肪酸
F0_FAs <- read_excel('/Users/hozan/Library/CloudStorage/OneDrive-Personal/Papers/2.2 中大队列/database/大队列数据库整理/原始数据库/GNHS_F0RBC_VF.xlsx',
                     sheet=1)
colnames(F0_FAs) <- sub("RBC_", "RBC", colnames(F0_FAs))

# 剔除F0RBC_a18_3取值为0的观测
F0_FAs <- F0_FAs[F0_FAs$F0RBCa18_3!=0,]

F0_FAs <- F0_FAs %>%
  mutate(
    F0SFA = F0RBC14_0 + F0RBC16_0 + F0RBC18_0 + F0RBC20_0 + F0RBC24_0 + F0RBC22_0,
    F0MUFA = F0RBC16_1 + F0RBC18_1 + F0RBC20_1 + F0RBC22_1 + F0RBC24_1,
    F0PUFA = F0RBC18_2 + F0RBCr18_3 + F0RBCa18_3 + F0RBC20_2 + F0RBC20_4 + F0RBC20_3 + F0RBC20_5 + F0RBC22_4 + F0RBC22_5 + F0RBC22_6,
    F0n3PUFA = F0RBCr18_3 + F0RBCa18_3 + F0RBC20_3 + F0RBC20_5 + F0RBC22_5 + F0RBC22_6,
    F0n6PUFA = F0RBC18_2 + F0RBC20_2 + F0RBC20_4 + F0RBC22_4,
    F0n6n3ratio = F0n6PUFA / F0n3PUFA,#注意n6/n3
    F0LA_ALA = F0RBC18_2 / F0RBCa18_3,
    F0AA_EPA_DHA = F0RBC20_4 / (F0RBC20_5 + F0RBC22_6),
  )
colnames(F0_FAs)
# 剔除F0RBCr18_3
F0_FAs <- dplyr::select(F0_FAs,-c("F0RBCr18_3"))
#********************F3脂肪酸
F3_FAs <- read_excel('/Users/hozan/Library/CloudStorage/OneDrive-Personal/Papers/2.2 中大队列/database/大队列数据库整理/原始数据库/GNHS_F3RBC_VF.xlsx',
                     sheet="P1与校正后P2合并（（%）最终分析用）")
F3_FAs <- dplyr::select(F3_FAs, -c("date", "number", "P", "SUM"))
# 有重复的ID，取均值
duplicated_IDs <- F3_FAs$ID[duplicated(F3_FAs$ID)]
c <- F3_FAs[F3_FAs$ID %in% c("NL0890","NL1700"),]
F3_FAs <- F3_FAs %>%
  group_by(ID) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

F3_FAs <- F3_FAs %>%
  mutate(
    F3SFA = F3RBC14_0 + F3RBC16_0 + F3RBC18_0 + F3RBC20_0 + F3RBC24_0 + F3RBC22_0 + F3RBC15_0 + F3RBC17_0 + F3RBC23_0,
    F3MUFA = F3RBC16_1 + F3RBC18_1 + F3RBC20_1 + F3RBC24_1,
    F3PUFA = F3RBC18_2 + F3RBCa18_3 + F3RBC20_2 + F3RBC20_3 + F3RBC20_4 + F3RBC20_5 + F3RBC22_6,
    F3n3PUFA = F3RBCa18_3 + F3RBC20_3 + F3RBC20_5 + F3RBC22_6,
    F3n6PUFA = F3RBC18_2 + F3RBC20_2 + F3RBC20_4,
    F3n6n3ratio = F3n6PUFA / F3n3PUFA,
    F3LA_ALA = F3RBC18_2 / F3RBCa18_3,
    F3AA_EPA_DHA = F3RBC20_4 / (F3RBC20_5 + F3RBC22_6),
  )
####血清F0234脂肪酸####
Metabolites_target <- as.data.frame(read_excel("/Users/hozan/Library/CloudStorage/OneDrive-Personal/Papers/2.2 中大队列/database/大队列数据库整理/原始数据库/GNHSmetabolitesall_VF.xlsx",sheet=1))
rownames(Metabolites_target) <- Metabolites_target$Sample
Metabolites_target2 <- dplyr::select(Metabolites_target,-c("Sample","Common Name","Group","HMDB"))
Metabolites_target3 <- as.data.frame(t(Metabolites_target2))
Metabolites_target4 <- cbind(ID = rownames(Metabolites_target3), Metabolites_target3)
Metabolites_target5 <- Metabolites_target4[!grepl("^P", Metabolites_target4$ID), ]
#对于ID中出现两个NL的，删除一个，比如NLNL4628--NL4628
Metabolites_target5$ID <- sub("^NLNL", "NL", Metabolites_target5$ID)
a <- Metabolites_target5[grepl("^F4", Metabolites_target5$ID), ]
# 只筛选出脂肪酸
fatty_acids <- c("Myristic acid", "Palmitoleic acid", 
                 "Alpha-Linolenic acid", "Linoleic acid", 
                 "Eicosapentaenoic acid", "Arachidonic acid", 
                 "Docosahexaenoic acid", "Docosapentaenoic acid (22n-3)", 
                 "Oleic acid") #"Adrenic acid",
pattern <- paste(fatty_acids, collapse = "|")
Metabolites_target5 <- Metabolites_target5[, c("ID", grep(pattern, colnames(Metabolites_target5), value = TRUE))]
#****************************脂肪酸长数据
Metabolites_target_long <- Metabolites_target5 %>%
  mutate(Times = substr(ID, 1, 2),     
         ID = substr(ID, 3, nchar(ID)))
#****************************脂肪酸宽数据
Metabolites_target_wide <- Metabolites_target5 %>%
  mutate(ID_prefix = substr(ID, 1, 2),     
         ID = substr(ID, 3, nchar(ID)))  %>% 
  pivot_wider(
    names_from = ID_prefix,  
    values_from = -c(ID, ID_prefix),  
    names_glue = "{ID_prefix}{.value}" 
  )
#****************************提取F2脂肪酸
Metabolites_target_F2 <- Metabolites_target_wide %>%
  dplyr::select(ID, starts_with("F2"))
#****************************提取F0脂肪酸
Metabolites_target_F0 <- Metabolites_target_wide %>%
  dplyr::select(ID, starts_with("F0"))
#****************************F234代谢物与年龄合并
Metabolites_target_wide_F234 <- Metabolites_target_wide %>%
  dplyr::select(ID, starts_with("F2"), starts_with("F3"), starts_with("F4"))
Metabolites_target_wide_age <- merge(Age_wide[, c("ID","F2age","F3age","F4age")],Metabolites_target_wide_F234, by = "ID")
Metabolites_target_full <- Metabolites_target_wide_age[complete.cases(Metabolites_target_wide_age),]
# 对代谢物进行log2转换
Metabolites_target_full_log <- Metabolites_target_full %>%
  mutate(across(
    all_of(setdiff(colnames(Metabolites_target_wide_F234), c("ID"))),
    ~ log2(.+ 1e-6)  # 若有0值，加1防止取log(0)
  ))
#*****************************计算代谢物的AUC和slope
# 获取所有以F0、F2、F3、F4开头的代谢物变量名（排除age变量）
metabolite_vars <- grep("^F[2-4]", names(Metabolites_target_wide), value = TRUE)
base_names_met <- unique(sub("^F[2-4]", "", metabolite_vars))

Metabolites_target_AUC_slope <- calculate_auc_slope(
  prefixes = c("F2", "F3","F4"),
  data = Metabolites_target_full_log,
  base_names = base_names_met
)
####血清脂肪酸质量控制####
library(purrr)
QC_serum <- Metabolites_target4[grepl("^P", Metabolites_target4$ID), ]
pattern <- paste(fatty_acids, collapse = "|")
QC_serum <- QC_serum[, c("ID", grep(pattern, colnames(QC_serum), value = TRUE))]
cv_results <- QC_serum %>%
  dplyr::select(-ID) %>%  # 排除ID列
  map_dfr(~ {
    data_vec <- .x[!is.na(.x)]
    cv <- sd(data_vec) / mean(data_vec) * 100
    data.frame(CV = round(cv, 2))
  }, .id = "Fatty_Acid") %>%
  arrange(CV)  
max(cv_results$CV)
min(cv_results$CV)
####混杂定义####
Covariates_all = c("Age_F0","Sex_F0","WBTOT_FAT_mean","Met_mean","Energy_mean","Alcohol_F0","Tea_F0","Smoke_F0","Calcium_F0","Vitamin_F0","Disease_F0","Estrogen_F0","Menopause_F0","Fracture_F0","Oil_sup_follow")
Covariates_female = c("Age_F0","WBTOT_FAT_mean","Met_mean","Energy_mean","Alcohol_F0","Tea_F0","Calcium_F0","Vitamin_F0","Disease_F0","Estrogen_F0","Menopause_F0","Fracture_F0","Oil_sup_follow")
Covariates_male = c("Age_F0","WBTOT_FAT_mean","Met_mean","Energy_mean","Alcohol_F0","Tea_F0","Smoke_F0","Calcium_F0","Vitamin_F0","Disease_F0","Fracture_F0","Oil_sup_follow")

Covariates_all_lmer = c("Age","Sex_F0","WBTOT_FAT","Met_mean","Energy_mean","Alcohol_F0","Tea_F0","Smoke_F0","Calcium_F0","Vitamin_F0","Disease_F0","Estrogen_F0","Menopause_F0","Fracture_F0","Oil_sup_follow_lmer")
Covariates_female_lmer = c("Age","WBTOT_FAT","Met_mean","Energy_mean","Alcohol_F0","Tea_F0","Calcium_F0","Vitamin_F0","Disease_F0","Estrogen_F0","Menopause_F0","Fracture_F0","Oil_sup_follow_lmer")
Covariates_male_lmer = c("Age", "WBTOT_FAT","Met_mean","Energy_mean","Alcohol_F0","Tea_F0","Smoke_F0","Calcium_F0","Vitamin_F0","Disease_F0","Fracture_F0","Oil_sup_follow_lmer")
####提取GNHSF0和F3共有的脂肪酸种类####
F0_cols <- setdiff(colnames(F0_FAs), "ID")
F3_cols <- setdiff(colnames(F3_FAs), "ID")

F0_names <- sub("^F0", "", F0_cols)
F3_names <- sub("^F3", "", F3_cols)

common_names <- intersect(F0_names, F3_names)

F0_common_cols <- paste0("F0", common_names)
F3_common_cols <- paste0("F3", common_names)
####*****************F0F3RBC脂肪酸&体成分AUC*****************####
####血清和红细胞膜脂肪酸相关性分析####
#**************************************相关性数据库整理
fa_mapping_complete <- list(
  # 完整脂肪酸名 -> 简化名
  "Myristic acid" = "C14:0",
  "Palmitoleic acid" = "C16:1 n-7",
  "Alpha Linolenic acid" = "C18:3 n-3",  # 空格版本
  "Alpha_Linolenic.acid" = "C18:3 n-3",  # 点版本
  "Linoleic acid" = "C18:2 n-6",
  "Eicosapentaenoic acid EPA" = "C20:5 n-3",
  "Eicosapentaenoic.acid.EPA" = "C20:5 n-3",  # 可能的其他格式
  "Arachidonic acid" = "C20:4 n-6",
  "Docosahexaenoic acid DHA" = "C22:6 n-3",
  "Docosapentaenoic acid" = "C22:5 n-3",
  "Adrenic acid" = "C22:4 n-6",
  "Oleic acid" = "C18:1 n-9"
)

# 通用重命名函数
rename_fa_columns <- function(df, mapping_list) {
  col_names <- colnames(df)
  new_names <- col_names
  
  for (i in seq_along(col_names)) {
    col_name <- col_names[i]
    if (col_name %in% names(mapping_list)) {
      new_names[i] <- mapping_list[[col_name]]
    }
  }
  
  colnames(df) <- new_names
  return(df)
}


Metabolites_target_long_F0C <- Metabolites_target_long[Metabolites_target_long$Times == "F0",]
Metabolites_target_long_F0C <- rename_fa_columns(Metabolites_target_long_F0C, fa_mapping_complete)
Metabolites_target_long_F0C <- Metabolites_target_long_F0C %>%
  rename_with(
    ~ paste0(., " (Serum)"),
    .cols = !c(ID, Times)  # 排除ID和Times列
  )

Metabolites_target_long_F3C <- Metabolites_target_long[Metabolites_target_long$Times == "F3",]
Metabolites_target_long_F3C <- rename_fa_columns(Metabolites_target_long_F3C, fa_mapping_complete)
Metabolites_target_long_F3C <- Metabolites_target_long_F3C %>%
  rename_with(
    ~ paste0(., " (Serum)"),
    .cols = !c(ID, Times)  # 排除ID和Times列
  )

rename_fa_columns_Erythrocyte <- function(data, time_prefix = NULL) {
  # 内嵌完整的脂肪酸映射关系
  fa_mapping <- c(
    # 饱和脂肪酸 (SFA)
    "RBC14_0" = "C14:0",
    "RBC15_0" = "C15:0",
    "RBC16_0" = "C16:0", 
    "RBC17_0" = "C17:0",
    "RBC18_0" = "C18:0",
    "RBC20_0" = "C20:0",
    "RBC22_0" = "C22:0", 
    "RBC23_0" = "C23:0",
    "RBC24_0" = "C24:0",
    
    # 单不饱和脂肪酸 (MUFA)
    "RBC16_1" = "C16:1 n-7", 
    "RBC18_1" = "C18:1 n-9", 
    "RBC20_1" = "C20:1 n-9",
    "RBC22_1" = "C22:1 n-9", 
    "RBC24_1" = "C24:1 n-9",
    
    # n-3多不饱和脂肪酸
    "RBCa18_3" = "C18:3 n-3",  # α-亚麻酸
    "RBC20_3" = "C20:3 n-3",   # 二高-γ-亚麻酸
    "RBC20_5" = "C20:5 n-3",   # EPA
    "RBC22_5" = "C22:5 n-3",   # DPA n-3
    "RBC22_6" = "C22:6 n-3",   # DHA
    
    # n-6多不饱和脂肪酸  
    "RBC18_2" = "C18:2 n-6",   # 亚油酸
    "RBC20_2" = "C20:2 n-6",   # 二十碳二烯酸
    "RBC20_4" = "C20:4 n-6",   # 花生四烯酸(AA)
    "RBC22_4" = "C22:4 n-6",   # 肾上腺酸
    
    # 脂肪酸类别
    "SFA" = "SFA",
    "MUFA" = "MUFA", 
    "PUFA" = "PUFA",
    "n3PUFA" = "n3PUFA",
    "n6PUFA" = "n6PUFA",
    "n6n3ratio" = "n6PUFA/n3PUFA",
    "LA_ALA" = "LA/ALA",
    "AA_EPA_DHA" = "AA/(EPA+DHA)"
  )
  
  # 获取当前列名
  original_names <- colnames(data)
  new_names <- original_names
  
  # 记录重命名日志
  rename_log <- data.frame(
    Original = character(),
    New = character(),
    stringsAsFactors = FALSE
  )
  
  # 处理每一列
  for (i in seq_along(original_names)) {
    current_name <- original_names[i]
    
    # 跳过ID列
    if (current_name == "ID") {
      next
    }
    
    # 如果指定了时间前缀，先去除前缀
    base_name <- current_name
    if (!is.null(time_prefix) && startsWith(current_name, time_prefix)) {
      base_name <- sub(paste0("^", time_prefix), "", current_name)
    }
    
    # 查找映射
    if (base_name %in% names(fa_mapping)) {
      new_names[i] <- fa_mapping[base_name]
      rename_log <- rbind(rename_log, 
                          data.frame(Original = current_name, 
                                     New = new_names[i],
                                     stringsAsFactors = FALSE))
    } else {
      # 尝试模糊匹配（处理F0RBCa18_3这种形式）
      matched <- FALSE
      for (pattern in names(fa_mapping)) {
        if (grepl(pattern, base_name)) {
          new_names[i] <- fa_mapping[pattern]
          rename_log <- rbind(rename_log, 
                              data.frame(Original = current_name, 
                                         New = new_names[i],
                                         stringsAsFactors = FALSE))
          matched <- TRUE
          break
        }
      }
      
      if (!matched) {
        warning(paste("No mapping found for:", current_name))
      }
    }
  }
  
  # 应用新列名
  colnames(data) <- new_names
  
  return(data)
}
F0_FAs_C <- F0_FAs
F0_FAs_C <- rename_fa_columns_Erythrocyte(F0_FAs_C, time_prefix = "F0")
F0_FAs_C <- F0_FAs_C %>%
  rename_with(
    ~ paste0(., " (Erythrocyte)"),
    .cols = !c(ID)  # 排除ID和Times列
  )

F3_FAs_C <- F3_FAs
F3_FAs_C <- rename_fa_columns_Erythrocyte(F3_FAs_C, time_prefix = "F3")
F3_FAs_C <- F3_FAs_C %>%
  rename_with(
    ~ paste0(., " (Erythrocyte)"),
    .cols = !c(ID)  # 排除ID和Times列
  )

FA_cor_F0 <- merge(Metabolites_target_long_F0C, F0_FAs_C, by = "ID")
FA_cor_F3 <- merge(Metabolites_target_long_F3C, F3_FAs_C, by = "ID")
#**************************************相关分析并可视化
run_fa_spearman_cor <- function(
    data,
    fa_pairs = c(
      "C14:0",
      "C16:1 n-7",
      "C18:1 n-9",
      "C18:2 n-6",
      "C20:5 n-3",
      "C22:6 n-3",
      "C20:4 n-6"
    ),
    serum_suffix = " (Serum)",
    rbc_suffix   = " (Erythrocyte)",
    method = "spearman",
    exact = FALSE,
    use = "complete.obs"
) {
  

  
  map_dfr(fa_pairs, function(fa) {
    
    serum_col <- paste0(fa, serum_suffix)
    rbc_col   <- paste0(fa, rbc_suffix)
    
    # 列不存在，返回 NA（防止函数中断）
    if (!all(c(serum_col, rbc_col) %in% colnames(data))) {
      return(
        tibble(
          FA = fa,
          rho = NA_real_,
          p_value = NA_real_
        )
      )
    }
    
    x <- data[[serum_col]]
    y <- data[[rbc_col]]
    
    # NA 处理
    idx <- complete.cases(x, y)
    
    if (sum(idx) < 3) {
      return(
        tibble(
          FA = fa,
          rho = NA_real_,
          p_value = NA_real_
        )
      )
    }
    
    test <- suppressWarnings(
      cor.test(
        x[idx],
        y[idx],
        method = method,
        exact = exact
      )
    )
    
    tibble(
      FA = fa,
      rho = as.numeric(test$estimate),
      p_value = test$p.value
    )
  })
}
cor_results_F0 <- run_fa_spearman_cor(
  data = FA_cor_F0
)
cor_results_F0$Times <- "F0"

cor_results_F3 <- run_fa_spearman_cor(
  data = FA_cor_F3
)
cor_results_F3$Times <- "F3"

plot_df <- rbind(cor_results_F0, cor_results_F3) %>%
  mutate(
    logP = -log10(p_value),
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE ~ ""
    )
  ) %>%
  mutate(FA = factor(FA, levels = rev(c("C14:0", "C16:1 n-7", "C18:1 n-9", 
                                        "C20:5 n-3", "C22:6 n-3", "C18:2 n-6", "C20:4 n-6"))))

FA_cor_plot <- ggplot(plot_df,
                      aes(
                        x = "Serum",
                        y = FA,
                        size = logP,
                        fill = rho
                      )) +
  geom_point(
    shape = 21,
    color = "black",
    alpha = 0.85
  ) +
  geom_text(
    aes(label = sig),
    vjust = -1.2,
    size = 5
  ) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    name = "Spearman rho"
  ) +
  scale_size_continuous(
    name = "-log10(P)"
  ) +
  facet_wrap(~ Times, nrow = 1) +
  theme_bw(base_size = 13) +
  labs(
    title = "",
    x = NULL,
    y = NULL
  ) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.position = "right",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(face = "bold"),
    strip.background = element_rect(
      fill = "grey90",
      color = "black",
      linewidth = 0.6
    ),
    strip.text = element_text(face = "bold")
  )

pdf("FA.cor.pdf", width = 7, height = 4.8)
print(FA_cor_plot)
dev.off()
#**********************正文写作
a <- plot_df[plot_df$p_value < 0.05,]
round(max(a$rho), 3)
round(min(a$rho), 3)
round(max(a$p_value), 3)
round(min(a$p_value), 3)
####验证相关性####
# cor.test(
#   FA_cor_F0$`C18:1 n-9 (Serum)`,          # 血清脂肪酸浓度
#   FA_cor_F0$`C18:1 n-9 (Erythrocyte)`,    # 红细胞膜脂肪酸百分比
#   method = "spearman"
# )

####GLM合并数据集+数据集前处理####
# 合并数据集
data_list <- list(Cov_merge,
                  DXA_wide_age_AUC_slope, 
                  F0_FAs[,c("ID",F0_common_cols)] , F3_FAs[,c("ID",F3_common_cols)])
data_analyse5 <- data_list %>% purrr::reduce(dplyr::inner_join, by = "ID")
data_analyse5 <- data_analyse5[complete.cases(data_analyse5),]
#*************************Z分数转换
var <- c(colnames(F0_FAs[,F0_common_cols]), colnames(F3_FAs[,F3_common_cols]), "WBTOT_BMD_AUC","TOT_BMD_AUC","HTOT_BMD_AUC","NECK_BMD_AUC")
data_analyse5_all <- scale_selected_columns(data_analyse5, var)
data_analyse5_female <- scale_selected_columns(data_analyse5[data_analyse5$Sex_F0 == 0,], var)
data_analyse5_male <- scale_selected_columns(data_analyse5[data_analyse5$Sex_F0 == 1,], var)
colnames(data_analyse5_all)
#*************************计算Z分数的平均值
data_analyse5_all <- calc_mean_zscore_FAs(data_analyse5_all,"F0","F3")
a <- unique(data_analyse5_all$ID)
data_analyse5_female <- calc_mean_zscore_FAs(data_analyse5_female,"F0","F3")
data_analyse5_male <- calc_mean_zscore_FAs(data_analyse5_male,"F0","F3")
####GLM####
X <- grep("^Mean.*Z$", colnames(data_analyse5_all), value = TRUE)
Y <- grep("BMD.*(AUC.*_Z$)", colnames(data_analyse5_all), value = TRUE, perl = TRUE)

forest_data3_female <- process_glm2(X, Y,data_analyse5_female,  Covariates_female, 0)

forest_data3_male <- process_glm2(X, Y, data_analyse5_male, Covariates_male,  1)

#forest_data3_all <- process_glm2(X, Y, data_analyse5_all, Covariates_all, 2)
# 注意用process_glm3函数
forest_data3_all <- process_glm3(X, Y, data_analyse5_all, Covariates_all, 2)
forest_data3_all_sex <- forest_data3_all[grep(":Sex_F01", forest_data3_all$Predictor, fixed = TRUE),]

b1 <- forest_data3_female[forest_data3_female$P_value <0.05,]
b2 <- forest_data3_male[forest_data3_male$P_value < 0.05,]
b3 <- forest_data3_all[forest_data3_all$P_value < 0.05,]
####验证GLM/交互效应####
# x <- "MeanRBC20_5_Z"
# y <- "WBTOT_BMD_AUC_Z"
# covariates <- Covariates_female
# data <- data_analyse5_female
# formula <- as.formula(paste(y, "~", x, "+", paste(covariates, collapse = "+")))
# model <- glm(formula,
#              data = data, family = gaussian())
# summary(model)

# x <- "MeanRBC20_5_Z"
# y <- "WBTOT_BMD_AUC_Z"
# covariates <- Covariates_all
# data <- data_analyse5_all
####性别交互
# formula_str <- paste0(y, " ~ ", x, " * Sex_F0 + ", paste(covariates, collapse = " + "))
# model <- glm(as.formula(formula_str), data = data, family = gaussian())
# summary(model)
####*****************F0F3RBC脂肪酸均值&体成分*****************####
####Lmer:合并数据集+数据前处理-暴露用均值####
data_list <- list(Age_long, DXA, Oil_long)
DXA_age_oil <-  data_list %>% purrr::reduce(dplyr::inner_join, by = c("ID","Times"))

data_list <- list(DXA_age_oil, Cov_merge, F0_FAs[,c("ID",F0_common_cols)] , F3_FAs[,c("ID",F3_common_cols)])
data_analyse6 <- data_list %>% purrr::reduce(dplyr::inner_join, by = "ID")
data_analyse6 <- data_analyse6[complete.cases(data_analyse6),]
#data_analyse6 <- data_analyse6[data_analyse6$ID %in% data_analyse5$ID,]
# 计算每一个ID都有四次随访的观测量
id_complete <- data_analyse6 %>%
  distinct(ID, Times) %>%
  group_by(ID) %>%
  summarise(
    n_times = n_distinct(Times),
    .groups = "drop"
  ) %>%
  filter(n_times == 4)

a <- unique(data_analyse6$ID)
colnames(data_analyse6) <- make.names(colnames(data_analyse6))
#*************************Z分数转换
var <- c(colnames(F0_FAs[,F0_common_cols]), colnames(F3_FAs[,F3_common_cols]),"WBTOT_BMD","HTOT_BMD","NECK_BMD","TOT_BMD")
data_analyse6_all <- scale_selected_columns(data_analyse6, var)
data_analyse6_female <- scale_selected_columns(data_analyse6[data_analyse6$Sex_F0 == 0,], var)
data_analyse6_male <- scale_selected_columns(data_analyse6[data_analyse6$Sex_F0 == 1,], var)
#*************************计算Z分数的平均值
data_analyse6_all <- calc_mean_zscore_FAs(data_analyse6_all,"F0","F3")
data_analyse6_female <- calc_mean_zscore_FAs(data_analyse6_female,"F0","F3")
data_analyse6_male <- calc_mean_zscore_FAs(data_analyse6_male,"F0","F3")
####Lmer-暴露用均值####
X <- grep("^Mean.*Z$", colnames(data_analyse6_all), value = TRUE)
Y <- c("WBTOT_BMD_Z","HTOT_BMD_Z","NECK_BMD_Z","TOT_BMD_Z")
Lmer_data3_female <- process_lmer(data_analyse6_female, 
                                  X,Y, Covariates_female_lmer, 0)
Lmer_data3_male <- process_lmer(data_analyse6_male, 
                                X, Y, Covariates_male_lmer, 1)

d1 <- Lmer_data3_female[Lmer_data3_female$P_value < 0.05,]
d2 <- Lmer_data3_male[Lmer_data3_male$P_value < 0.05,]
####Lmer:合并数据集+数据前处理-暴露用F0####
data_list <- list(Age_long, DXA, Oil_long)
DXA_age_oil <-  data_list %>% purrr::reduce(dplyr::inner_join, by = c("ID","Times"))

data_list <- list(DXA_age_oil, Cov_merge, F0_FAs[,c("ID",F0_common_cols)])
data_analyse6B <- data_list %>% purrr::reduce(dplyr::inner_join, by = "ID")
data_analyse6B <- data_analyse6B[complete.cases(data_analyse6B),]
colnames(data_analyse6B) <- make.names(colnames(data_analyse6B))

#data_analyse6 <- data_analyse6[data_analyse6$ID %in% data_analyse5$ID,]
# 计算每一个ID都有四次骨密度随访的观测量
id_complete <- DXA %>%
  distinct(ID, Times) %>%
  group_by(ID) %>%
  summarise(
    n_times = n_distinct(Times),
    .groups = "drop"
  ) %>%
  filter(n_times == 4)

a <- unique(data_analyse6B$ID)
#*************************Z分数转换
var <- c(colnames(F0_FAs[,F0_common_cols]), "WBTOT_BMD","HTOT_BMD","NECK_BMD","TOT_BMD")
data_analyse6B_all <- scale_selected_columns(data_analyse6B, var)
data_analyse6B_female <- scale_selected_columns(data_analyse6B[data_analyse6B$Sex_F0 == 0,], var)
data_analyse6B_male <- scale_selected_columns(data_analyse6B[data_analyse6B$Sex_F0 == 1,], var)
####Lmer-暴露用基线####
X <- paste0(colnames(F0_FAs[,F0_common_cols]),"_Z")
Y <- c("WBTOT_BMD_Z","HTOT_BMD_Z","NECK_BMD_Z","TOT_BMD_Z")
Lmer_data3B_female <- process_lmer(data_analyse6B_female, 
                                  X,Y, Covariates_female_lmer, 0)
Lmer_data3B_male <- process_lmer(data_analyse6B_male, 
                                X, Y, Covariates_male_lmer, 1)
# 注意用process_lmer3函数，做性别交互效应
Lmer_data3B_all <- process_lmer3(data_analyse6B_all,X,Y, Covariates_all_lmer, 2)
Lmer_data3B_all_sex <- Lmer_data3B_all[grep(":Sex_F01", Lmer_data3B_all$Predictor, fixed = TRUE),]

d1 <- Lmer_data3B_female[Lmer_data3B_female$P_value < 0.05,]
d2 <- Lmer_data3B_male[Lmer_data3B_male$P_value < 0.05,]
d3 <- Lmer_data3B_all_sex[Lmer_data3B_all_sex$P_value < 0.05,]
####验证Lmer/性别交互效应####
# x <- "MeanRBC20_5_Z"
# y <- "WBTOT_BMD"
# covariates <- Covariates_female_lmer
# data <- data_analyse6_female
# formula <- as.formula(paste(y, "~", x, "+", paste(covariates, collapse = "+"), "+ (1|ID)"))
# lme_model <- lmer(formula, data = data)
# summary(lme_model)


# x <- "MeanRBCa18_3_Z"
# y <- "WBTOT_BMD_Z"
# covariates <- Covariates_all_lmer
# data <- data_analyse6_all
# formula_str <- paste0(y, " ~ ", x, " * Sex_F0 + ", paste(covariates, collapse = " + "), " + (1|ID)")
# lme_model <- lmer(formula_str, data = data)
# summary(lme_model)
####性别交互效应可视化####
Lmer_data3B_all_sex$Model <- "LMM"
forest_data3_all_sex$Model <- "GLM"

Sex_intersect_plot <- rbind(Lmer_data3B_all_sex, forest_data3_all_sex)
Sex_intersect_plot1 <- recode_F0F3_FA3(Sex_intersect_plot, "Predictor")
Sex_intersect_plot1$Predictor <- paste0(Sex_intersect_plot1$Predictor, ": Sex")
Sex_intersect_plot1 <- recode_AUC_Bone_enhanced(Sex_intersect_plot1, "Outcome")

pdf("GNHS_RBC.pdf", width = 8, height = 9)
fa_levels <- rev(c(
  "C14:0: Sex", "C16:0: Sex", "C18:0: Sex", "C20:0: Sex", "C22:0: Sex", "C24:0: Sex",
  "C16:1 n-7: Sex", "C18:1 n-9: Sex", "C20:1 n-9: Sex", "C24:1 n-9: Sex",
  "C18:3 n-3: Sex", "C20:3 n-3: Sex", "C20:5 n-3: Sex", "C22:6 n-3: Sex",
  "C18:2 n-6: Sex", "C20:2 n-6: Sex", "C20:4 n-6: Sex",
  "n3PUFA: Sex", "n6PUFA: Sex", "SFA: Sex", "MUFA: Sex", "PUFA: Sex",
  "n6PUFA/n3PUFA: Sex", "LA/ALA: Sex", "AA/(EPA+DHA): Sex"
))

plot_sex_interaction_heatmap(
  data = Sex_intersect_plot1,
  predictor_levels = fa_levels,
  title = "GNHS (Erythrocyte Membrane Fatty Acids)"
)

dev.off()
####*****************RBC脂肪酸一起可视化*****************####
forest_data3 <- rbind(forest_data3_female,forest_data3_male)
forest_data3$Model <- "GLM-Fatty acid (F0&F3) & BMD AUC (F1-4)"
# 暴露用均值
Lmer_data3 <- rbind(Lmer_data3_female, Lmer_data3_male)
Lmer_data3$Model <- "LMM-Fatty acid (F0&F3) & BMD (F1-4)"
# 暴露用基线值
Lmer_data3B <- rbind(Lmer_data3B_female, Lmer_data3B_male)
Lmer_data3B$Model <- "LMM-Fatty acid (F0) & BMD (F1-4)"

GLM_Lmer_plot <- rbind(Lmer_data3)
GLM_Lmer_plot <- recode_F0F3_FA3(GLM_Lmer_plot, "Predictor")
GLM_Lmer_plot <- recode_AUC_Bone_enhanced(GLM_Lmer_plot, "Outcome")


GLM_Lmer_plotB <- rbind(forest_data3, Lmer_data3B)
GLM_Lmer_plotB <- recode_F0F3_FA3(GLM_Lmer_plotB, "Predictor")
GLM_Lmer_plotB <- recode_AUC_Bone_enhanced(GLM_Lmer_plotB, "Outcome")
#***************可视化
# 放主图
pdf("Figure 5.pdf", width = 16.5, height = 14)
plot_forest4_enhanced(
  data = GLM_Lmer_plotB,
  x_title = "Erythrocyte Membrane",
  title = "GNHS",
  model1 = "GLM-Fatty acid (F0&F3) & BMD AUC (F1-4)",
  model2 = "LMM-Fatty acid (F0) & BMD (F1-4)",
  colors = c("#2E86AB", "#A23B72")#,
  #facet_nrow = 1 
)
dev.off()
# 放附加材料
pdf("Figure 5_sup.pdf", width = 10, height = 14)
plot_forest4_enhanced3(
  data = GLM_Lmer_plot,
  x_title = "Erythrocyte Membrane",
  y_title = "LMM Estimate (95% CI)-Fatty acid (F0&F3) & BMD (F1-4)",
  title = "GNHS",
  model1 = "LMM-Fatty acid (F0&F3) & BMD (F1-4)",
  colors = c("#A5A002"),
  facet_nrow = 2
)
dev.off()
####正文写作####
a <- GLM_Lmer_plotB[GLM_Lmer_plotB$P_value < 0.05,]
a$Estimate <- round(a$Estimate,3)
a$P_value <- round(a$P_value,3)

a1 <- a[a$Gender == "Female",]
a11 <- a1[a1$Predictor %in% c("C18:0","C20:3 n-3"),]
max(a11$Estimate)
min(a11$Estimate)
max(a11$P_value)
min(a11$P_value)

a12 <- a1[a1$Predictor %in% c("C22:0","C20:0"),]
max(a12$Estimate)
min(a12$Estimate)
max(a12$P_value)
min(a12$P_value)

a13 <- a1[a1$Predictor == "C20:5 n-3",]
max(a13$Estimate)
min(a13$Estimate)
max(a13$P_value)
min(a13$P_value)
a14 <- a1[a1$Predictor == "C16:1 n-7",]
a15 <- a1[a1$Predictor == "C20:1 n-9",]


#**************男性
a2 <- a[a$Gender == "Male",]
a2 <- a2[order(a2$Predictor),]

a22 <- a2[a2$Estimate <0,]
a22_2 <- a22[a22$Predictor != "C16:1 n-7",]
min(a22_2$Estimate)
max(a22_2$Estimate)
min(a22_2$P_value)
max(a22_2$P_value)

a22_1 <- a22[a22$Predictor == "C16:1 n-7",]
min(a22_1$Estimate)
max(a22_1$Estimate)
min(a22_1$P_value)
max(a22_1$P_value)


a21 <- a2[a2$Estimate >0,]
a21_1 <- a21[a21$Predictor %in% c("C16:0","SFA","C24:1 n-9"),]
max(a21_1$Estimate)
min(a21_1$Estimate)
max(a21_1$P_value)
min(a21_1$P_value)
a21_2 <- a21[a21$Predictor %in% c("C18:3 n-3","C22:6 n-3"),]
max(a21_2$Estimate)
min(a21_2$Estimate)
max(a21_2$P_value)
min(a21_2$P_value)
####敏感性分析比较####
a1 <- GLM_Lmer_plotB[GLM_Lmer_plotB$Model == "LMM-Fatty acid (F0) & BMD (F1-4)",]
a1 <- a1[a1$P_value < 0.05,]

a2 <- GLM_Lmer_plot
a2 <- a2[a2$P_value < 0.05,]

a <- merge(a1, a2, by = c("Predictor","Outcome","Gender"))
####*****************F0血清脂肪酸&体成分AUC*****************####
####GLM：合并数据集+数据前处理####
data_list <- list(Cov_merge,
                  Metabolites_target_F0, DXA_wide_age_AUC_slope)
data_analyse3 <- data_list %>% purrr::reduce(dplyr::inner_join, by = "ID")
data_analyse3 <- data_analyse3[complete.cases(data_analyse3),]
colnames(data_analyse3) <- make.names(colnames(data_analyse3))
a <- merge(Metabolites_target_F0,DXA_wide_age_AUC_slope, by = "ID")
#*************************Log2转换
Var <- make.names(setdiff(colnames(Metabolites_target_F0),"ID"))
data_analyse3_all <- data_analyse3 %>%
  mutate(across(all_of(Var), ~ log2(.+ 1e-6)))
data_analyse3_female <- data_analyse3[data_analyse3$Sex_F0 == 0,] %>%
  mutate(across(all_of(Var), ~ log2(.+ 1e-6)))
data_analyse3_male <- data_analyse3[data_analyse3$Sex_F0 == 1,] %>%
  mutate(across(all_of(Var), ~ log2(.+ 1e-6)))
#*************************Z分数转换
Var <- c(make.names(setdiff(colnames(Metabolites_target_F0),"ID")), "WBTOT_BMD_AUC","TOT_BMD_AUC","HTOT_BMD_AUC","NECK_BMD_AUC")
data_analyse3_all <- scale_selected_columns(data_analyse3_all, Var)
data_analyse3_female <- scale_selected_columns(data_analyse3_female, Var)
data_analyse3_male <- scale_selected_columns(data_analyse3_male, Var)
####GLM####
#X <- paste0(make.names(setdiff(colnames(Metabolites_target_F0),"ID")),"_Z")
X <- paste0(make.names(setdiff(colnames(Metabolites_target_F0),"ID")),"_Z")
Y <- grep("BMD.*(AUC.*_Z$)|(AUC.*BMD.*_Z$)", colnames(data_analyse3_all), value = TRUE, perl = TRUE)
forest_data2_female <- process_glm2(X, Y, data_analyse3_female,  Covariates_female, 0)
forest_data2_male <- process_glm2(X,Y, data_analyse3_male,  Covariates_male, 1)
#forest_data2_all <- process_glm2(X,Y, data_analyse3_all,  Covariates_all, 2)
forest_data2_all <- process_glm3(X,Y, data_analyse3_all,  Covariates_all, 2)
forest_data2_all_sex <- forest_data2_all[grep(":Sex_F01", forest_data2_all$Predictor, fixed = TRUE),]

e1 <- forest_data2_female[forest_data2_female$P_value <0.05,]
e2 <- forest_data2_male[forest_data2_male$P_value < 0.05,]
e3 <- forest_data2_all[forest_data2_all$P_value < 0.05,]
####*****************F234血清脂肪酸&体成分*****************####
####Lmer：合并数据集+数据前处理####
colnames(Metabolites_target_long) <- make.names(colnames(Metabolites_target_long))
data_list <- list(Age_long, DXA, Metabolites_target_long,Oil_long)
DXA_age_met <- data_list %>% purrr::reduce(dplyr::inner_join, by = c("ID","Times"))
DXA_age_met <- DXA_age_met[complete.cases(DXA_age_met),]
data_analyse7 <- merge(Cov_merge, DXA_age_met, by = "ID")
# 计算每一个ID都有四次随访的观测量
id_complete <- data_analyse7 %>%
  distinct(ID, Times) %>%
  group_by(ID) %>%
  summarise(
    n_times = n_distinct(Times),
    .groups = "drop"
  ) %>%
  filter(n_times == 3)
#*************************Log2转换
Var <- make.names(setdiff(colnames(Metabolites_target_long),c("ID","Times")))
data_analyse7_all <- data_analyse7 %>%
  mutate(across(all_of(Var), ~ log2(.+ 1e-6)))
data_analyse7_female <- data_analyse7[data_analyse7$Sex_F0 == 0,] %>%
  mutate(across(all_of(Var), ~ log2(.+ 1e-6)))
data_analyse7_male <- data_analyse7[data_analyse7$Sex_F0 == 1,] %>%
  mutate(across(all_of(Var), ~ log2(.+ 1e-6)))
#*************************标准化
var <- c(make.names(setdiff(colnames(Metabolites_target_long),c("ID","Times"))),"WBTOT_BMD","HTOT_BMD","NECK_BMD","TOT_BMD")
data_analyse7_all <- scale_selected_columns(data_analyse7_all, var)
a <- unique(data_analyse7_all$ID)
data_analyse7_female <- scale_selected_columns(data_analyse7_female, var)
data_analyse7_male <- scale_selected_columns(data_analyse7_male, var)
####Lmer####
#X <- make.names(setdiff(colnames(Metabolites_target_long),c("ID","Times")))
X <- paste0(make.names(setdiff(colnames(Metabolites_target_long),c("ID","Times"))),"_Z")
Y <- c("WBTOT_BMD_Z","HTOT_BMD_Z","NECK_BMD_Z","TOT_BMD_Z")
Lmer_data4_female <- process_lmer(data_analyse7_female, X,Y, Covariates_female_lmer, 0)
Lmer_data4_male <- process_lmer(data_analyse7_male, X, Y, Covariates_male_lmer, 1)
#Lmer_data4_all <- process_lmer(data_analyse7_all,X,Y, Covariates_all_lmer, 2)
Lmer_data4_all <- process_lmer3(data_analyse7_all,X,Y, Covariates_all_lmer, 2)
Lmer_data4_all_sex <- Lmer_data4_all[grep(":Sex_F01", Lmer_data4_all$Predictor, fixed = TRUE),]

warnings()
f1 <- Lmer_data4_female[Lmer_data4_female$P_value < 0.05,]
f2 <- Lmer_data4_male[Lmer_data4_male$P_value < 0.05,]
f3 <- Lmer_data4_all[Lmer_data4_all$P_value < 0.05,]
####Lmer验证####
# x <- "Adrenic.acid"
# y <- "TOT_BMD"
# covariates <- Covariates_male_lmer
# data <- data_analyse7_male
# formula <- as.formula(paste(y, "~", x, "+", paste(covariates, collapse = "+"), "+ (1|ID)"))
# lme_model <- lmer(formula, data = data)
# summary(lme_model)
####性别交互效应可视化####
Lmer_data4_all_sex$Model <- "LMM"
forest_data2_all_sex$Model <- "GLM"

Sex_intersect_plotS <- rbind(Lmer_data4_all_sex, forest_data2_all_sex)
Sex_intersect_plotS1 <- recode_fatty_target2(Sex_intersect_plotS, "Predictor")
Sex_intersect_plotS1$Predictor <- paste0(Sex_intersect_plotS1$Predictor, ": Sex")
Sex_intersect_plotS1 <- recode_AUC_Bone_enhanced(Sex_intersect_plotS1, "Outcome")

pdf("GNHS_Serum.pdf", width = 8, height = 5)
fa_levels <- rev(c(
  "C14:0: Sex", "C16:1 n-7: Sex", "C18:1 n-9: Sex", "C20:5 n-3: Sex", "C22:6 n-3: Sex","C18:2 n-6: Sex", "C20:4 n-6: Sex"
))

plot_sex_interaction_heatmap(
  data = Sex_intersect_plotS1,
  predictor_levels = fa_levels,
  title = "GNHS (Serum Fatty Acids)"
)

dev.off()
####********************血清脂肪酸GLM和Lmer一起可视化********************####
forest_data2 <- rbind(forest_data2_female,forest_data2_male)
forest_data2$Model <- "GLM-Fatty acid (F0) & BMD AUC (F1-4)"

Lmer_data4 <- rbind(Lmer_data4_female, Lmer_data4_male)
Lmer_data4$Model <- "LMM-Fatty acid (F2-4) & BMD (F2-4)"

GLM_Lmer_plot2 <- rbind(forest_data2, Lmer_data4)
GLM_Lmer_plot2 <- recode_fatty_target2(GLM_Lmer_plot2, "Predictor")
GLM_Lmer_plot2 <- recode_AUC_Bone_enhanced(GLM_Lmer_plot2, "Outcome")
####可视化
pdf("Figure 4.pdf", width = 9, height = 10)
plot_forest4_enhanced2(
  data = GLM_Lmer_plot2,
  x_title = "Serum",
  title = "GNHS",
  model1 = "GLM-Fatty acid (F0) & BMD AUC (F1-4)",
  model2 = "LMM-Fatty acid (F2-4) & BMD (F2-4)",
  colors = c("#FF7F00", "#3C5488FF"),
  facet_nrow = 2 
)
dev.off()
####正文写作####
a <- GLM_Lmer_plot2[GLM_Lmer_plot2$P_value < 0.05,]
a$Estimate <- round(a$Estimate,3)
a$P_value <- round(a$P_value,3)
a1 <- a[a$Gender == "Female",]
max(a1$Estimate)
min(a1$Estimate)
max(a1$P_value)
min(a1$P_value)
a2 <- a[a$Gender == "Male",]
####*****************其他附加材料*****************####
####计算随访时间####
Age_all <-  left_join(Cov_F0_all[,c("ID","Age_F0")], Age_wide, by = "ID")
Follow_up_time <- Age_all[Age_all$ID %in% data_analyse6B_all$ID,c("ID","Age_F0","F1age","F2age","F3age","F4age")]
Follow_up_time_filtered <- Follow_up_time %>%
  filter(!grepl("^NL4", ID))
Follow_up_time_filtered$F01 <- Follow_up_time_filtered$F1age-Follow_up_time_filtered$Age_F0
Follow_up_time$F12 <- Follow_up_time$F2age-Follow_up_time$F1age
Follow_up_time$F23 <- Follow_up_time$F3age-Follow_up_time$F2age
Follow_up_time$F34 <- Follow_up_time$F4age-Follow_up_time$F3age

round(mean(Follow_up_time_filtered$F01, na.rm = TRUE),1)
round(mean(Follow_up_time$F12, na.rm = TRUE),1)
round(mean(Follow_up_time$F23, na.rm = TRUE),1)
round(mean(Follow_up_time$F34, na.rm = TRUE),1)
####Table1a####
Table1a_data <- merge(Cov_merge, DXA_wide[,c("ID","F1WBTOT_FAT","F2WBTOT_FAT","F3WBTOT_FAT","F4WBTOT_FAT")], by = "ID")
Table1a_data <- dplyr::left_join(Table1a_data,Age_wide[,c("ID","F1age","F2age","F3age","F4age")], by = "ID")
Table1a_data <- Table1a_data[Table1a_data$ID %in% unique(data_analyse6B_all$ID),]
Table1a_data$WBTOT_FAT_mean <- rowMeans(Table1a_data[, c("F1WBTOT_FAT","F2WBTOT_FAT","F3WBTOT_FAT","F4WBTOT_FAT")], na.rm = TRUE)
Table1a_data <- Table1a_data %>%
  mutate(across(c(WBTOT_FAT_mean, F1WBTOT_FAT, F2WBTOT_FAT, F3WBTOT_FAT, F4WBTOT_FAT),
                ~ .x * 0.001))
Table1a <- descrTable(Sex_F0 ~ Age_F0+F1age+F2age+F3age+F4age+
                        WBTOT_FAT_mean+F1WBTOT_FAT+F2WBTOT_FAT+F3WBTOT_FAT+F4WBTOT_FAT+
                        Met_mean+Energy_mean+
                        Smoke_F0+Alcohol_F0+Tea_F0+Calcium_F0+Vitamin_F0+Oil_sup_follow+Disease_F0+Fracture_F0+Estrogen_F0+Menopause_F0,
                      data = Table1a_data ,method =NA,show.all = TRUE)#show.all = TRUE :显示all
export2word(Table1a, file='Table1a.docx')
####RBC脂肪酸可视化####
#****************************************F0
# 转换为长数据画图
FA_F0_sta <-  F0_FAs[F0_FAs$ID %in% unique(data_analyse6B_all$ID),]
FA_F0_sta <- merge(Cov_merge[,c("ID","Sex_F0")], FA_F0_sta, by = "ID")
FA_long_F0 <- FA_F0_sta %>% 
  dplyr::select(Sex_F0, setdiff(colnames(F0_FAs),c("ID"))) %>% # 选择性别列和第 3 到 23 列
  pivot_longer(cols = -Sex_F0, # 转换，保持性别列不变
               names_to = "Fatty_Acid", 
               values_to = "Value")
unique(FA_long_F0$Fatty_Acid)
#,"F0n6n3ratio","F0LA_ALA","F0AA_EPA_DHA"
FA_long_F0 <- FA_long_F0[ !FA_long_F0$Fatty_Acid %in% c("F0RBC22_1","F0RBC22_5", "F0RBC22_4"),]

# 修改变量名
FA_long_F0_combined <- recode_F0F3_FA2(FA_long_F0, predictor_col = "Fatty_Acid")

#****************************************F3
FA_F0_sta <-  F3_FAs[F3_FAs$ID %in% unique(data_analyse6B_all$ID),]
FA_F0_sta <- merge(Cov_merge[,c("ID","Sex_F0")], FA_F0_sta, by = "ID")

FA_long_F3 <- FA_F0_sta %>% 
  dplyr::select(Sex_F0, setdiff(colnames(F3_FAs),c("ID"))) %>% 
  tidyr::pivot_longer(cols = -Sex_F0, 
                      names_to = "Fatty_Acid", 
                      values_to = "Value")
unique(FA_long_F3$Fatty_Acid)
# 只选择需要展示的脂肪酸
#,"F3n6n3ratio","F3LA_ALA","F3AA_EPA_DHA"
FA_long_F3 <- FA_long_F3[ !FA_long_F3$Fatty_Acid %in% c("F3RBC15_0", "F3RBC17_0","F3RBC23_0"),]
# 修改变量名
FA_long_F3_combined <- recode_F0F3_FA2(FA_long_F3, predictor_col = "Fatty_Acid")
#****************************************F0F3平均值
FA_F03_sta <- data_analyse5 %>%
  dplyr::select(ID, Sex_F0, 
         # 包含所有F0和F3的脂肪酸相关列
         matches("^(F0|F3)(RBC|SFA|MUFA|PUFA|n3PUFA|n6PUFA|n6n3ratio|LA_ALA|AA_EPA_DHA)"))

FA_mean <- FA_F03_sta %>%
  mutate(
    across(
      # 匹配所有F0开头的脂肪酸相关列
      matches("^F0(RBC|SFA|MUFA|PUFA|n3PUFA|n6PUFA|n6n3ratio|LA_ALA|AA_EPA_DHA)"),
      .fns = function(x) {
        # 提取列名后缀，例如 F0SFA → SFA
        suffix <- str_replace(cur_column(), "^F0", "")
        f3_col <- paste0("F3", suffix)
        
        # 如果对应的F3列存在，则计算均值
        if (f3_col %in% colnames(FA_F03_sta)) {
          return((x + FA_F03_sta[[f3_col]]) / 2)
        } else {
          return(NA)
        }
      },
      .names = "Mean_{str_replace(.col, '^F0', '')}"
    )
  )


FA_long_F03 <- FA_mean %>%
  dplyr::select(Sex_F0, starts_with("Mean")) %>% 
  tidyr::pivot_longer(
    cols = -c(Sex_F0),
    names_to = "Fatty_Acid",
    values_to = "Value"
  )
FA_long_F03 <- recode_F0F3_FA2(FA_long_F03, predictor_col = "Fatty_Acid")

#****************************************F03脂肪酸一起可视化
FA_long_F0_combined$Times <- "F0"
FA_long_F3_combined$Times <- "F3"
FA_long_F03$Times <- "Mean"
FA_long_F03_combined <- rbind(FA_long_F0_combined, FA_long_F3_combined, FA_long_F03)

pdf("RBC_Fatty_acids_GNHS.pdf", width = 12, height = 12)
plot_fatty_acid2(FA_long_F03_combined, ncol_facet = 5, plot_title = "","Fatty Acids Level (%)")
dev.off()
####血清脂肪酸可视化####
Fatty_acid_serum <- Metabolites_target_long[Metabolites_target_long$ID %in% data_analyse7_all$ID,]
Fatty_acid_serum <- merge(Cov_merge[,c("ID","Sex_F0")], Fatty_acid_serum, by = "ID")
# 转换成长数据
FA_long_serum <- Fatty_acid_serum %>%
  pivot_longer(
    cols = Myristic.acid:Oleic.acid,  # 所有脂肪酸列
    names_to = "Fatty_Acid",
    values_to = "Value"
  )

FA_long_serum <- recode_fatty_target1(FA_long_serum, "Fatty_Acid")

pdf("Serum_Fatty_acids_GNHS.pdf", width = 10, height = 6)
plot_fatty_acid2(FA_long_serum, ncol_facet = 4, plot_title = "","Fatty Acids Level (µmol/L)")
dev.off()
####骨量可视化####
Bone_sta <- DXA[DXA$ID %in% Table1a_data$ID,]
Bone_sta <- merge(Bone_sta,Age_long,by = c("ID","Times"))
Bone_sta <- merge(Cov_merge[,c("ID","Sex_F0")], Bone_sta, by = "ID")
Bone_sta <- Bone_sta[complete.cases(Bone_sta),]

a1 <- Bone_sta[Bone_sta$Times == "F1",]
a2 <- Bone_sta[Bone_sta$Times == "F2",]
a3 <- Bone_sta[Bone_sta$Times == "F3",]
a4 <- Bone_sta[Bone_sta$Times == "F4",]
setdiff(a1$ID, a3$ID)
#***************************可视化
bone_long <- Bone_sta %>%
  dplyr::select(ID, Sex_F0, Times, Age, WBTOT_BMD, TOT_BMD, HTOT_BMD, NECK_BMD) %>%
  pivot_longer(
    cols = c(WBTOT_BMD, TOT_BMD, HTOT_BMD, NECK_BMD),
    names_to = "BMD_Type",
    values_to = "BMD_Value"
  ) %>%
  mutate(
    BMD_Type = factor(BMD_Type,
                      levels = c("WBTOT_BMD", "TOT_BMD", "HTOT_BMD", "NECK_BMD"),
                      labels = c("Whole Body BMD", "Lumbar Spine BMD", "Total Hip BMD", "Femur Neck BMD")),
    Times = factor(Times,
                   levels = c("F1", "F2", "F3", "F4"),
                   labels = c("F1", "F2", "F3", "F4")),
    Sex_F0 = factor(Sex_F0, levels = c(0,1), labels = c("Female","Male"))
  )

bone_long_clean <- bone_long %>%
  group_by(BMD_Type, Times, Sex_F0) %>%
  mutate(
    Q1 = quantile(BMD_Value, 0.25, na.rm = TRUE),
    Q3 = quantile(BMD_Value, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower_bound = Q1 - 3*IQR,
    upper_bound = Q3 + 3*IQR
  ) %>%
  filter(BMD_Value >= lower_bound & BMD_Value <= upper_bound) %>%
  dplyr::select(-Q1, -Q3, -IQR, -lower_bound, -upper_bound) %>%
  ungroup()

stats_data <- bone_long_clean %>%
  group_by(BMD_Type, Times, Sex_F0) %>%
  summarise(
    median_bmd = median(BMD_Value, na.rm = TRUE),
    mean_age = sprintf("%.1f", mean(Age, na.rm = TRUE)),
    y_max = max(BMD_Value, na.rm = TRUE),
    y_min = min(BMD_Value, na.rm = TRUE),
    .groups = "drop"
  )


p <- ggplot(bone_long_clean, aes(x = Times, y = BMD_Value, fill = Sex_F0)) +
  
  # 小提琴图：男女分开
  geom_violin(aes(group = interaction(Times, Sex_F0)),  # 按性别分组
              position = position_dodge(width = 0.8), 
              alpha = 0.5, color = "black") +
  
  # 箱线图：男女分开
  geom_boxplot(aes(group = interaction(Times, Sex_F0)),
               width = 0.25, 
               position = position_dodge(width = 0.8), 
               outlier.shape = NA, color = "black", fill = "white") +
  
  # 中位数折线：按性别分开
  geom_line(data = stats_data,
            aes(x = Times, y = median_bmd, group = Sex_F0, color = Sex_F0),
            position = position_dodge(width = 0.8), linewidth = 1.2) +
  geom_point(data = stats_data,
             aes(x = Times, y = median_bmd, color = Sex_F0),
             position = position_dodge(width = 0.8), size = 3, shape = 18) +
  
  facet_wrap(~ BMD_Type, ncol = 2, scales = "free_y") +
  
  # 只保留平均年龄标签，移除样本量标签
  geom_text(data = stats_data,
            aes(x = Times, y = y_min - 0.05, label = paste0("(", mean_age,"y)")),
            position = position_dodge(width = 0.8),
            size = 3.2, fontface = "bold") +
  
  scale_fill_manual(values = c("Female"="#FFBE7A", "Male"="#1E6FBA")) +
  scale_color_manual(values = c("Female"="#FFBE7A", "Male"="#1E6FBA")) +
  
  labs(title = "",
       x = "Follow-up Visits (with mean age)",
       y = "BMD (g/cm²)",
       fill = "Sex",
       color = "Sex") +
  theme_shared +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "#C8E6C9", color = "black", linewidth = 0.5)) 

pdf("Bone_GNHS.pdf", width = 10, height = 9)
print(p)
dev.off()
####BMD AUC可视化####
BMD_AUC_stat <- DXA_wide_age_AUC_slope[DXA_wide_age_AUC_slope$ID %in% data_analyse5$ID,]
BMD_AUC_stat <- merge(Cov_merge[,c("ID","Sex_F0")], BMD_AUC_stat, by = "ID")
# 添加Sex_F0因子转换
BMD_AUC_stat$Sex_F0 <- factor(BMD_AUC_stat$Sex_F0,
                              levels = c(0, 1),
                              labels = c("Female", "Male"))

auc_long <- BMD_AUC_stat %>%
  dplyr::select(ID, Sex_F0, WBTOT_BMD_AUC, TOT_BMD_AUC, HTOT_BMD_AUC, NECK_BMD_AUC) %>%
  pivot_longer(
    cols = c(WBTOT_BMD_AUC, TOT_BMD_AUC, HTOT_BMD_AUC, NECK_BMD_AUC),
    names_to = "BMD_Type",
    values_to = "AUC_Value"
  ) %>%
  mutate(
    BMD_Type = factor(BMD_Type,
                      levels = c("WBTOT_BMD_AUC", "TOT_BMD_AUC", "HTOT_BMD_AUC", "NECK_BMD_AUC"),
                      labels = c("Whole Body BMD", "Spine BMD", "Total Hip BMD", "Femur Neck BMD"))
  )

p <- ggplot(auc_long, aes(x = AUC_Value, fill = Sex_F0)) +
  # 直方图 - 按性别分色
  geom_histogram(aes(y = after_stat(density)), 
                 position = "identity", alpha = 0.5, bins = 30, color = "black") +
  # 密度曲线 - 按性别分色
  geom_density(aes(color = Sex_F0), linewidth = 0.8, fill = NA, alpha = 0.8) +
  
  # 双分面：BMD类型 × 性别
  facet_grid(Sex_F0 ~ BMD_Type, scales = "free") +
  
  scale_fill_manual(values = c("Female" = "#FFBE7A", "Male" = "#B0D4FF")) +
  scale_color_manual(values = c("Female" = "#FF4500", "Male" = "blue")) +
  
  labs(
    title = "",
    x = "BMD AUC (g·year/cm²)",
    y = "Density",
    fill = "Sex",
    color = "Sex"
  ) +
  theme_shared +
  theme(
    legend.position = "none",  
    strip.background = element_rect(fill = "#C8E6C9", color = "black", linewidth = 0.5),
  )

pdf("BMD_AUC_Distribution_Plots.pdf", width = 12, height = 8)
print(p)
dev.off()
####流程图####
data_list <- list(Cov_merge, F0_FAs[,c("ID",F0_common_cols)] , F3_FAs[,c("ID",F3_common_cols)])
a <- data_list %>% purrr::reduce(dplyr::inner_join, by = "ID")
a <- a[complete.cases(a),]

a1 <- DXA[DXA$Times == "F1",]
a2 <- DXA[DXA$Times == "F2",]
a3 <- DXA[DXA$Times == "F3",]
a4 <- DXA[DXA$Times == "F4",]
####**********************************ALSPAC可视化**********************************####
####主图####
recode_fatty_ALSPAC <- function(data, var) {
  if (!var %in% colnames(data)) {
    stop(paste("Column", var, "not found in the data."))
  }
  
  # 定义脂肪酸名称映射
  fa_mapping <- list(
    "DHAFA" = "C22:6 n-3",
    "LAFA" = "C18:2 n-6",
    "FAw3FA" = "n3PUFA",
    "FAw6FA" = "n6PUFA",
    "PUFAFA" = "PUFA",
    "MUFAFA" = "MUFA",
    "SFAFA" = "SFA",
    "FAw6w3FA" = "n6PUFA/n3PUFA"
  )
  
  for (fa_name in names(fa_mapping)) {
    data[[var]] <- ifelse(grepl(fa_name, data[[var]]), 
                          fa_mapping[[fa_name]], 
                          data[[var]])
  }
  
  # 将 Predictor 列转换为因子，并指定顺序
  levels_original <- c("C22:6 n-3","C18:2 n-6","n3PUFA","n6PUFA","SFA","MUFA","PUFA","n6PUFA/n3PUFA")
  
  data[[var]] <- factor(data[[var]],  levels = rev(levels_original))
  
  return(data)
}
recode_BMD_ALSPAC <- function(data, var) {
  if (!var %in% colnames(data)) {
    stop(paste("Column", var, "not found in the data."))
  }
  
  # 定义脂肪酸名称映射
  fa_mapping <- list(
    "MTotalBMD" = "Whole Body BMD",
    "MSpineBMD" = "Spine BMD",
    "MTotalHipBMD" = "Total Hip BMD",
    "MHipNeckBMD" = "Femur Neck BMD"
  )
  
  for (fa_name in names(fa_mapping)) {
    data[[var]] <- ifelse(grepl(fa_name, data[[var]]), 
                          fa_mapping[[fa_name]], 
                          data[[var]])
  }
  
  # 将 Predictor 列转换为因子，并指定顺序
  levels_original <- c("Whole Body BMD","Spine BMD","Total Hip BMD","Femur Neck BMD")
  
  data[[var]] <- factor(data[[var]],  levels = levels_original)
  
  return(data)
}

ALSPAC_results_all <- read_excel("/Users/hozan/Library/CloudStorage/OneDrive-Personal/Papers/2.1 中老年人骨量肌肉和脂肪酸/ALSPAC/Output/ALSPAC_results.xlsx",sheet=1)
ALSPAC_results_all <- recode_fatty_ALSPAC(ALSPAC_results_all, "Predictor")
ALSPAC_results_all <- recode_BMD_ALSPAC(ALSPAC_results_all, "Outcome")
colnames(ALSPAC_results_all) <- recode(colnames(ALSPAC_results_all),"Std. Error" = "Std.Error")

ALSPAC_results <- ALSPAC_results_all[ALSPAC_results_all$Model %in% c("GLM","LMM_FOM1"),]


ALSPAC_results$Model <- recode(ALSPAC_results$Model,
                               "GLM" = "GLM-Fatty acid (FOM1&FOM2) & BMD AUC (FOM1−4)",
                               "LMM_FOM1" = "LMM-Fatty acid (FOM1) & BMD (FOM1−4)")

pdf("Figure 2.pdf", width = 10, height = 5)
plot_forest4_enhanced(
  data = ALSPAC_results,
  x_title = "Plasma",
  title = "ALSPAC",
  model1 = "GLM-Fatty acid (FOM1&FOM2) & BMD AUC (FOM1−4)",
  model2 = "LMM-Fatty acid (FOM1) & BMD (FOM1−4)",
  colors = c("#00A087FF", "#C51B7D")
)
dev.off()
####附加材料-FOM1&FOM2均值####
ALSPAC_results2 <- ALSPAC_results_all[ALSPAC_results_all$Model %in% c("LMM_FOM1&FOM2"),]

ALSPAC_results2$Model <- recode(ALSPAC_results2$Model,
                               "LMM_FOM1&FOM2" = "LMM-Fatty acid (FOM1&FOM2) & BMD (FOM1−4)")
pdf("Figure 3_sup.pdf", width = 12, height = 4)
plot_forest4_enhanced3(
  data = ALSPAC_results2,
  x_title = "Plasma",
  y_title = "LMM Estimate (95% CI)-Fatty acid (FOM1&FOM2) & BMD (FOM1−4) ",
  title = "ALSPAC",
  model1 = "LMM-Fatty acid (Instance 0) & BMD (Instance 2-3)",
  colors = c("purple"),
  facet_nrow = 1
)
dev.off()
####敏感性分析比较####
a1 <- ALSPAC_results[ALSPAC_results$Model == "LMM-Fatty acid (FOM1) & BMD (FOM1−4)",]
a1 <- a1[a1$P_value < 0.05,]
# 均值
a2 <- ALSPAC_results2
a2 <- a2[a2$P_value < 0.05,]
####**********************************UKB Biobank**********************************####
####修改变量名函数####
recode_boneF2 <- function(data, var) {
  
  if (!var %in% colnames(data)) {
    stop(paste("Column", var, "not found in the data."))
  }
  
  # 使用stringr进行正则表达式替换
  data[[var]] <- stringr::str_replace(data[[var]], "WBTOT_BMD.*", "Whole Body BMD")
  data[[var]] <- stringr::str_replace(data[[var]], "PELVIS_BMD.*", "Pelvis BMD")
  data[[var]] <- stringr::str_replace(data[[var]], "TOT_BMD.*", "Lumbar Spine BMD")
  data[[var]] <- stringr::str_replace(data[[var]], "NECK_BMD.*", "Femur Neck BMD")
  
  data[[var]] <- factor(data[[var]], 
                        levels = c("Whole Body BMD",
                                   "Lumbar Spine BMD", 
                                   "Pelvis BMD", 
                                   "Femur Neck BMD"),
                        ordered = TRUE)
  
  return(data)
}

recode_fatty <- function(data, var) {
  
  if (!var %in% colnames(data)) {
    stop(paste("Column", var, "not found in the data."))
  }
  
  library(dplyr)
  library(stringr)
  
  data[[var]] <- case_when(
    str_detect(data[[var]], "LA_percentage_F0_Z")          ~ "C18:2 n-6",
    str_detect(data[[var]], "DHA_percentage_F0_Z")         ~ "C22:6 n-3",
    str_detect(data[[var]], "n3PUFA_percentage_F0_Z")      ~ "n3PUFA",
    str_detect(data[[var]], "n6PUFA_percentage_F0_Z")      ~ "n6PUFA",
    str_detect(data[[var]], "SFA_percentage_F0_Z")         ~ "SFA",
    str_detect(data[[var]], "MUFA_percentage_F0_Z")        ~ "MUFA",
    str_detect(data[[var]], "PUFA_percentage_F0_Z")        ~ "PUFA",
    str_detect(data[[var]], "n6PUFA_n3PUFA_F0_Z")           ~ "n6PUFA/n3PUFA",
    TRUE ~ as.character(data[[var]])  # 兜底
  )
  
  # 如果你还要固定排序
  data[[var]] <- factor(
    data[[var]],
    levels = rev(c(
      "C22:6 n-3",
      "C18:2 n-6",
      "n3PUFA",
      "n6PUFA",
      "SFA",
      "MUFA",
      "PUFA",
      "n6PUFA/n3PUFA"
    )),
    ordered = TRUE
  )
  
  return(data)
}


recode_fatty_noZ <- function(data, var) {
  
  if (!var %in% colnames(data)) {
    stop(paste("Column", var, "not found in the data."))
  }
  
  data[[var]] <- dplyr::recode(data[[var]],
                               "LA_percentage_F0" = "C18:2 n-6",
                               "DHA_percentage_F0" = "C22:6 n-3",
                               "n3PUFA_percentage_F0" = "n3PUFA",
                               "n6PUFA_n3PUFA_F0" = "n6PUFA/n3PUFA",
                               "SFA_percentage_F0" = "SFA",
                               "MUFA_percentage_F0" = "MUFA",
                               "PUFA_percentage_F0" = "PUFA",
                               "n6PUFA_percentage_F0" = "n6PUFA",
  )
  
  
  data[[var]] <- factor(data[[var]], 
                        levels = c("C22:6 n-3","C18:2 n-6", 
                                   "n3PUFA", "n6PUFA","SFA", "MUFA","PUFA", 
                                    "n6PUFA/n3PUFA"),
                        ordered = TRUE)
  
  return(data)
}
####数据前处理####
data1 <- read.csv("/Users/hozan/Library/CloudStorage/OneDrive-Personal/Papers/2.1 中老年人骨量肌肉和脂肪酸/Database/Biobank_fatty_acid_bone_database_2025.06.07_participant.csv",header = TRUE)
data2 <- read.csv("/Users/hozan/Library/CloudStorage/OneDrive-Personal/Papers/2.1 中老年人骨量肌肉和脂肪酸/Database/Biobank_age_met_2025.06.07_participant.csv",header = TRUE)
a <- data2[as.numeric(data2$"Summed.MET.minutes.per.week.for.all.activity...Instance.0") == 0,]
data3 <- read.csv("/Users/hozan/Library/CloudStorage/OneDrive-Personal/Papers/2.1 中老年人骨量肌肉和脂肪酸/Database/Data_2025.11.14_participant.csv",header = TRUE)
data3 <- data3[,c("Participant.ID","Mineral.and.other.dietary.supplements...Instance.0")]
data3$Fish_oil_sup_F0 <- ifelse(
  grepl("Fish oil", data3$Mineral.and.other.dietary.supplements...Instance.0, ignore.case = TRUE),
  1,
  0
)
data_list <- list(data3[,c("Participant.ID","Fish_oil_sup_F0")],data2, data1)
data_all <- data_list %>% purrr::reduce(dplyr::inner_join, by = "Participant.ID")

colnames(data_all) <- recode(colnames(data_all),
                             "Participant.ID" = "ID",
                             "Date.of.attending.assessment.centre...Instance.0" = "Date_F0",
                             "Date.of.attending.assessment.centre...Instance.1" = "Date_F1",
                             "Date.of.attending.assessment.centre...Instance.2" = "Date_F2",
                             "Date.of.attending.assessment.centre...Instance.3" = "Date_F3",
                             "Age.when.attended.assessment.centre...Instance.0" = "Age_F0",
                             "Age.when.attended.assessment.centre...Instance.1" = "Age_F1",
                             "Age.when.attended.assessment.centre...Instance.2" = "Age_F2",
                             "Age.when.attended.assessment.centre...Instance.3" = "Age_F3",
                             "Summed.MET.minutes.per.week.for.all.activity...Instance.0" = "Met_F0",
                             "Summed.MET.minutes.per.week.for.all.activity...Instance.1" = "Met_F1",
                             "Summed.MET.minutes.per.week.for.all.activity...Instance.2" = "Met_F2",
                             "Summed.MET.minutes.per.week.for.all.activity...Instance.3" = "Met_F3",
                             "Smoking.status...Instance.0"  = "Smoking_F0",
                             "Alcohol.drinker.status...Instance.0" = "Alcohol_F0",
                             "Tea.intake...Instance.0" = "Tea_F0",
                             "Fractured.broken.bones.in.last.5.years...Instance.0" = "Fractured_F0",
                             "Vascular.heart.problems.diagnosed.by.doctor...Instance.0" = "Vascular_F0",
                             "Blood.clot..DVT..bronchitis..emphysema..asthma..rhinitis..eczema..allergy.diagnosed.by.doctor...Instance.0" = "Blood_clot_F0",
                             "Diabetes.diagnosed.by.doctor...Instance.0" = "Diabetes_F0",
                             "Cancer.diagnosed.by.doctor...Instance.0" = "Cancer_F0",
                             "Other.serious.medical.condition.disability.diagnosed.by.doctor...Instance.0" = "Other_diseases_F0",
                             "Vitamin.and.mineral.supplements...Instance.0" = "Vitamin_mineral_F0",
                             "Had.menopause...Instance.0" = "Menopause_F0",
                             "Ever.used.hormone.replacement.therapy..HRT....Instance.0" = "Hormone_F0",
                             "Energy....Instance.0" = "Energy_F0",
                             "Energy....Instance.1" = "Energy_F1",
                             "Energy....Instance.2" = "Energy_F2",
                             "Energy....Instance.3" = "Energy_F3",
                             "Total.fat.mass...Instance.2" = "WBTOT_FAT_F2",
                             "Total.fat.mass...Instance.3" = "WBTOT_FAT_F3",
                             "Femur.neck.BMC..bone.mineral.content...left....Instance.2"  = "NECK_BMC_left_F2",
                             "Femur.neck.BMC..bone.mineral.content...right....Instance.2" = "NECK_BMC_right_F2",
                             "Femur.neck.BMC..bone.mineral.content...left....Instance.3"  = "NECK_BMC_left_F3",
                             "Femur.neck.BMC..bone.mineral.content...right....Instance.3" = "NECK_BMC_right_F3",
                             "Femur.neck.BMD..bone.mineral.density...left....Instance.2"  = "NECK_BMD_left_F2",
                             "Femur.neck.BMD..bone.mineral.density...left....Instance.3"  = "NECK_BMD_left_F3",
                             "Femur.neck.BMD..bone.mineral.density...right....Instance.2" = "NECK_BMD_right_F2",
                             "Femur.neck.BMD..bone.mineral.density...right....Instance.3" = "NECK_BMD_right_F3",
                             "L1.L4.BMC..bone.mineral.content....Instance.2" = "TOT_BMC_F2",
                             "L1.L4.BMC..bone.mineral.content....Instance.3" = "TOT_BMC_F3",
                             "L1.L4.BMD..bone.mineral.density....Instance.2" = "TOT_BMD_F2",
                             "L1.L4.BMD..bone.mineral.density....Instance.3" = "TOT_BMD_F3",
                             "L1.L4.BMD..bone.mineral.density..T.score...Instance.2" = "TOT_BMD_Tscore_F2",
                             "L1.L4.BMD..bone.mineral.density..T.score...Instance.3" = "TOT_BMD_Tscore_F3",
                             "Femur.neck.BMD..bone.mineral.density..T.score..left....Instance.2" = "NECK_BMD_Tscore_left_F2",
                             "Femur.neck.BMD..bone.mineral.density..T.score..left....Instance.3" = "NECK_BMD_Tscore_left_F3",
                             "Femur.neck.BMD..bone.mineral.density..T.score..right....Instance.2"  = "NECK_BMD_Tscore_right_F2",
                             "Femur.neck.BMD..bone.mineral.density..T.score..right....Instance.3"  = "NECK_BMD_Tscore_right_F3",
                             "Pelvis.BMC..bone.mineral.content....Instance.2" = "PELVIS_BMC_F2",
                             "Pelvis.BMC..bone.mineral.content....Instance.3" = "PELVIS_BMC_F3",
                             "Pelvis.BMD..bone.mineral.density....Instance.2" = "PELVIS_BMD_F2",
                             "Pelvis.BMD..bone.mineral.density....Instance.3" = "PELVIS_BMD_F3",
                             "Total.BMC..bone.mineral.content....Instance.2" = "WBTOT_BMC_F2",
                             "Total.BMC..bone.mineral.content....Instance.3" = "WBTOT_BMC_F3",
                             "Total.BMD..bone.mineral.density....Instance.2" = "WBTOT_BMD_F2",
                             "Total.BMD..bone.mineral.density....Instance.3" = "WBTOT_BMD_F3",
                             "Total.BMD..bone.mineral.density..T.score...Instance.2" = "WBTOT_BMD_Tscore_F2",
                             "Total.BMD..bone.mineral.density..T.score...Instance.3" = "WBTOT_BMD_Tscore_F3",
                             "Linoleic.Acid.to.Total.Fatty.Acids.percentage...Instance.0" = "LA_percentage_F0",
                             "Linoleic.Acid.to.Total.Fatty.Acids.percentage...Instance.1" = "LA_percentage_F1",
                             "Docosahexaenoic.Acid.to.Total.Fatty.Acids.percentage...Instance.0" = "DHA_percentage_F0",
                             "Docosahexaenoic.Acid.to.Total.Fatty.Acids.percentage...Instance.1" = "DHA_percentage_F1",
                             "Omega.3.Fatty.Acids.to.Total.Fatty.Acids.percentage...Instance.0" = "n3PUFA_percentage_F0",
                             "Omega.3.Fatty.Acids.to.Total.Fatty.Acids.percentage...Instance.1" = "n3PUFA_percentage_F1",
                             "Omega.6.Fatty.Acids.to.Omega.3.Fatty.Acids.ratio...Instance.0" = "n6PUFA_n3PUFA_F0",
                             "Omega.6.Fatty.Acids.to.Omega.3.Fatty.Acids.ratio...Instance.1" = "n6PUFA_n3PUFA_F1",
                             "Omega.6.Fatty.Acids.to.Total.Fatty.Acids.percentage...Instance.0" = "n6PUFA_percentage_F0",
                             "Omega.6.Fatty.Acids.to.Total.Fatty.Acids.percentage...Instance.1" = "n6PUFA_percentage_F1",
                             "Saturated.Fatty.Acids.to.Total.Fatty.Acids.percentage...Instance.0" = "SFA_percentage_F0",
                             "Saturated.Fatty.Acids.to.Total.Fatty.Acids.percentage...Instance.1" = "SFA_percentage_F1",
                             "Monounsaturated.Fatty.Acids.to.Total.Fatty.Acids.percentage...Instance.0" = "MUFA_percentage_F0",
                             "Monounsaturated.Fatty.Acids.to.Total.Fatty.Acids.percentage...Instance.1" = "MUFA_percentage_F1",
                             "Polyunsaturated.Fatty.Acids.to.Total.Fatty.Acids.percentage...Instance.0" = "PUFA_percentage_F0",
                             "Polyunsaturated.Fatty.Acids.to.Total.Fatty.Acids.percentage...Instance.1" = "PUFA_percentage_F1"
)
#********************剔除无应答的观测
data <- data_all[
  !data_all$Smoking_F0 %in% c("Prefer not to answer") &
    !data_all$Alcohol_F0 %in% c("Prefer not to answer") &
    !data_all$Tea_F0 %in% c("Do not know", "Prefer not to answer") &
    !data_all$Vitamin_mineral_F0 %in% c("Prefer not to answer") &
    !data_all$Fractured_F0 %in% c("Do not know", "Prefer not to answer") &
    !data_all$Menopause_F0 %in% c("Do not know", "Prefer not to answer") &
    !data_all$Hormone_F0 %in% c("Do not know", "Prefer not to answer"),
]
#********************计算均值的混杂
# a <- data[, c("ID","Met_F0", "Met_F1", "Met_F2", "Met_F3")]
# a$Met_F0123mean <- rowMeans(a[, c("Met_F0", "Met_F1", "Met_F2", "Met_F3")], na.rm = TRUE)

data$Met_F0123mean <- apply(data[, c("Met_F0", "Met_F1", "Met_F2", "Met_F3")], 1, function(x) {
  if (all(is.na(x))) {
    return(NA)
  } else {
    return(mean(x, na.rm = TRUE))
  }
})

data$Energy_F0123mean <- apply(data[, c("Energy_F0", "Energy_F1", "Energy_F2", "Energy_F3")], 1, function(x) {
  if (all(is.na(x))) {
    return(NA)
  } else {
    return(mean(x, na.rm = TRUE))
  }
})
#********************混杂重新赋值
# 性别
data$Sex_F0 <- case_when(
  data$Sex == "Male" ~ 1,
  data$Sex == "Female" ~ 0
)

# 疾病
data$Disease_F0 <- ifelse(
  !(data$Vascular_F0 %in% c("None of the above", "Prefer not to answer", "High blood pressure")) |
    data$Diabetes_F0 == "Yes" |
    data$Cancer_F0 == "Yes - you will be asked about this later by an interviewer" |
    data$Other_diseases_F0 == "Yes - you will be asked about this later by an interviewer",
  1, 0
)
table(data$Disease_F0)
# 吸烟
data$Smoking_F0_recode <- recode(data$Smoking_F0,
                                 "Never" = 0,
                                 "Previous" = 1,
                                 "Current" = 2)
table(data$Smoking_F0)
table(data$Smoking_F0_recode)
# 喝酒
data$Alcohol_F0_recode <- case_when(
  data$Alcohol_F0 == "Never" ~ 0,
  data$Alcohol_F0 == "Previous" ~ 1,
  data$Alcohol_F0 == "Current" ~ 2
)
table(data$Alcohol_F0)
table(data$Alcohol_F0_recode)
# 喝茶
data$Tea_F0_recode <- case_when(
  data$Tea_F0 == "Less than one" ~ 1,
  as.numeric(data$Tea_F0) == 0 ~ 0,
  as.numeric(data$Tea_F0) >= 1 & as.numeric(data$Tea_F0) <= 5 ~ 1,
  as.numeric(data$Tea_F0) >= 6 ~ 2,
  TRUE ~ NA_real_
)
table(data$Tea_F0)
table(data$Tea_F0_recode)
# 维生素和矿物质补充剂
data$Vitamin_mineral_F0_recode <- case_when(
  data$Vitamin_mineral_F0 == "None of the above" ~ 0,
  TRUE ~ 1
)
table(data$Vitamin_mineral_F0)
table(data$Vitamin_mineral_F0_recode)
# 骨折
data$Fractured_F0_recode <- case_when(
  data$Fractured_F0 == "No" ~ 0,
  data$Fractured_F0 == "Yes" ~ 1
)
table(data$Fractured_F0)
table(data$Fractured_F0_recode)
# 绝经
data$Menopause_F0_recode <- ifelse(data$Sex == "Male", 0,
                                   case_when(data$Menopause_F0 == "No" ~ 0,
                                             data$Menopause_F0 == "Yes" ~ 1,
                                             data$Menopause_F0 %in% c("Not sure - had a hysterectomy","Not sure - other reason") ~ 2))
table(data$Menopause_F0_recode)
table(data$Menopause_F0)
# 激素治疗
data$Hormone_F0_recode <- ifelse(data$Sex == "Male", 0,
                                 case_when(data$Hormone_F0 == "No" ~ 0,
                                           data$Hormone_F0 == "Yes" ~ 1)) 
table(data$Hormone_F0)
table(data$Hormone_F0_recode)
# 股骨颈
data$NECK_BMC_F2 <- data$NECK_BMC_left_F2 + data$NECK_BMC_right_F2
data$NECK_BMC_F3 <- data$NECK_BMC_left_F3 + data$NECK_BMC_right_F3
data$NECK_BMD_F2 <- data$NECK_BMD_left_F2 + data$NECK_BMD_right_F2
data$NECK_BMD_F3 <- data$NECK_BMD_left_F3 + data$NECK_BMD_right_F3

#a <- data[,c("ID","NECK_BMC_left_F2","NECK_BMC_right_F2","NECK_BMD_left_F2","NECK_BMD_right_F2","NECK_BMC_F2","NECK_BMD_F2")]

# 转换摄入能量单位
data$Energy_F0_kcal <- ifelse(
  !is.na(data$Energy_F0),
  data$Energy_F0 / 4.184,
  NA
)
# 计算Instance 2和3之间隔的时间
data$F2F3TimeInterval <- as.numeric(difftime(as.Date(data$Date_F3), as.Date(data$Date_F2), units = "days")) / 365
#********************剔除Met为0的观测
#data <- data[data$Met_F0 !=0 & data$Energy_F0_kcal>=600 & data$Energy_F0_kcal<= 5000,]
data <- data[data$Met_F0 !=0 ,]
####******************最终分析的数据集******************####
####GLM-只取F2体成分####
data_norepeat_F2 <- data[, c("ID","Age_F0","Sex_F0","Energy_F0_kcal","Met_F0","Disease_F0","WBTOT_FAT_F2",
                             "Smoking_F0_recode","Alcohol_F0_recode","Tea_F0_recode","Vitamin_mineral_F0_recode",
                             "Menopause_F0_recode","Hormone_F0_recode","Fractured_F0_recode","Fish_oil_sup_F0",
                             
                             "LA_percentage_F0","DHA_percentage_F0","n3PUFA_percentage_F0","n6PUFA_n3PUFA_F0",
                             "SFA_percentage_F0","MUFA_percentage_F0", "PUFA_percentage_F0","n6PUFA_percentage_F0",

                             "NECK_BMD_F2",  "PELVIS_BMD_F2","TOT_BMD_F2","WBTOT_BMD_F2"#,
)]
data_norepeat_F2  <- data_norepeat_F2[complete.cases(data_norepeat_F2),]
####已撤回的知情同意名单####
Delete_list1 <- read.csv("/Users/hozan/Library/CloudStorage/OneDrive-Personal/Papers/2.1 中老年人骨量肌肉和脂肪酸/Database/UKB_w406475_20250818.csv",header = TRUE)
Delete_list2 <- read.csv("/Users/hozan/Library/CloudStorage/OneDrive-Personal/Papers/2.1 中老年人骨量肌肉和脂肪酸/Database/UKB_delete.csv",header = TRUE)

data_norepeat_F2 <- data_norepeat_F2[!data_norepeat_F2$ID %in% c(Delete_list1$ID, Delete_list2$X1016304),]
####混合效应模型数据框####
data_norepeat_F2_LMM <- data[, c("ID","Age_F2","Sex_F0","Energy_F0_kcal","Met_F0","Disease_F0","WBTOT_FAT_F2",
                             "Smoking_F0_recode","Alcohol_F0_recode","Tea_F0_recode","Vitamin_mineral_F0_recode",
                             "Menopause_F0_recode","Hormone_F0_recode","Fractured_F0_recode","Fish_oil_sup_F0",
                             
                             "LA_percentage_F0","DHA_percentage_F0","n3PUFA_percentage_F0","n6PUFA_n3PUFA_F0",
                             "SFA_percentage_F0","MUFA_percentage_F0", "PUFA_percentage_F0","n6PUFA_percentage_F0",
                             
                            "NECK_BMD_F2", "PELVIS_BMD_F2","TOT_BMD_F2","WBTOT_BMD_F2"#,
)]
data_norepeat_F2_LMM  <- data_norepeat_F2_LMM[complete.cases(data_norepeat_F2_LMM),]
colnames(data_norepeat_F2_LMM) <- recode(colnames(data_norepeat_F2_LMM),
                                         "Age_F2" = "Age",
                                         "WBTOT_FAT_F2" = "WBTOT_FAT",
                                         "NECK_BMD_F2" =  "NECK_BMD",
                                         "PELVIS_BMD_F2" = "PELVIS_BMD",
                                         "TOT_BMD_F2" = "TOT_BMD",
                                         "WBTOT_BMD_F2" = "WBTOT_BMD"
)
data_norepeat_F2_LMM$Times <- "Instance2"

data_norepeat_F3_LMM <- data[, c("ID","Age_F3","Sex_F0","Energy_F0_kcal","Met_F0","Disease_F0","WBTOT_FAT_F3",
                              "Smoking_F0_recode","Alcohol_F0_recode","Tea_F0_recode","Vitamin_mineral_F0_recode",
                              "Menopause_F0_recode","Hormone_F0_recode","Fractured_F0_recode","Fish_oil_sup_F0",
                              
                              "LA_percentage_F0","DHA_percentage_F0","n3PUFA_percentage_F0","n6PUFA_n3PUFA_F0",
                              "SFA_percentage_F0","MUFA_percentage_F0", "PUFA_percentage_F0","n6PUFA_percentage_F0",

                              "NECK_BMD_F3", "PELVIS_BMD_F3","TOT_BMD_F3","WBTOT_BMD_F3"
)]
a <- data[, c("ID", "NECK_BMD_F3", "PELVIS_BMD_F3","TOT_BMD_F3","WBTOT_BMD_F3")]
a <- a[complete.cases(a),]
data_norepeat_F3_LMM  <- data_norepeat_F3_LMM[complete.cases(data_norepeat_F3_LMM),]
colnames(data_norepeat_F3_LMM) <- recode(colnames(data_norepeat_F3_LMM),
                                         "Age_F3" = "Age",
                                         "WBTOT_FAT_F3" = "WBTOT_FAT",
                                         "NECK_BMD_F3" =  "NECK_BMD",
                                         "PELVIS_BMD_F3" = "PELVIS_BMD",
                                         "TOT_BMD_F3" = "TOT_BMD",
                                         "WBTOT_BMD_F3" = "WBTOT_BMD"
                                         )
data_norepeat_F3_LMM$Times <- "Instance3"

data_LMM <- rbind(data_norepeat_F2_LMM, data_norepeat_F3_LMM)
a <- unique(data_LMM$ID)
# 计算每一个ID都有四次随访的观测量
id_complete <- data_LMM %>%
  distinct(ID, Times) %>%
  group_by(ID) %>%
  summarise(
    n_times = n_distinct(Times),
    .groups = "drop"
  ) %>%
  filter(n_times == 2)
#******************已撤回的知情同意名单
data_LMM <- data_LMM[!data_LMM$ID %in% c(Delete_list1$ID, Delete_list2$X1016304),]
a <- unique(data_LMM$ID)
####定性定量变量定义好####
#**************************data_norepeat_clean_F2
numeric_name<-c("Age_F0","Energy_F0_kcal","Met_F0","WBTOT_FAT_F2")
ndx <- which(names(data_norepeat_F2)%in% numeric_name)
for(n in ndx ){
  data_norepeat_F2[,n]  <-  as.numeric(data_norepeat_F2[,n])
}

factor_name<-c("Sex_F0","Disease_F0","Smoking_F0_recode","Alcohol_F0_recode","Tea_F0_recode","Vitamin_mineral_F0_recode",
               "Menopause_F0_recode","Hormone_F0_recode","Fractured_F0_recode","Fish_oil_sup_F0")
idx <- which(names(data_norepeat_F2)%in% factor_name)
for(i in idx ){
  data_norepeat_F2[,i]  <-  as.factor(data_norepeat_F2[,i])
}
colnames(data_norepeat_F2)
#**************************data_LMM
numeric_name<-c("Age","Energy_F0_kcal","Met_F0","WBTOT_FAT_F2")
ndx <- which(names(data_LMM)%in% numeric_name)
for(n in ndx ){
  data_LMM[,n]  <-  as.numeric(data_LMM[,n])
}

factor_name<-c("Sex_F0","Disease_F0","Smoking_F0_recode","Alcohol_F0_recode","Tea_F0_recode","Vitamin_mineral_F0_recode",
               "Menopause_F0_recode","Hormone_F0_recode","Fractured_F0_recode","Fish_oil_sup_F0")
idx <- which(names(data_LMM)%in% factor_name)
for(i in idx ){
  data_LMM[,i]  <-  as.factor(data_LMM[,i])
}
####混杂定义####
Covariates_all_F2 <- c("Age_F0","Sex_F0","WBTOT_FAT_F2","Energy_F0_kcal","Met_F0","Disease_F0","Smoking_F0_recode","Alcohol_F0_recode","Tea_F0_recode",
                       "Vitamin_mineral_F0_recode","Menopause_F0_recode","Hormone_F0_recode","Fractured_F0_recode","Fish_oil_sup_F0")

Covariates_female_LMM_UKB <- c("Age","WBTOT_FAT","Energy_F0_kcal","Met_F0","Disease_F0","Smoking_F0_recode","Alcohol_F0_recode","Tea_F0_recode",
                       "Vitamin_mineral_F0_recode","Menopause_F0_recode","Hormone_F0_recode","Fractured_F0_recode","Fish_oil_sup_F0")

Covariates_male_LMM_UKB <- c("Age","WBTOT_FAT","Energy_F0_kcal","Met_F0","Disease_F0","Smoking_F0_recode","Alcohol_F0_recode","Tea_F0_recode",
                       "Vitamin_mineral_F0_recode","Fractured_F0_recode","Fish_oil_sup_F0")

Covariates_all_LMM_UKB <- c("Age","Sex_F0","WBTOT_FAT","Energy_F0_kcal","Met_F0","Disease_F0","Smoking_F0_recode","Alcohol_F0_recode","Tea_F0_recode",
                       "Vitamin_mineral_F0_recode","Menopause_F0_recode","Hormone_F0_recode","Fractured_F0_recode","Fish_oil_sup_F0")
####*************************GLM*************************####
####Z分数转换####
#*************************Z分数转换
data_norepeat_clean_F2 <- scale_selected_columns(data_norepeat_F2, 
                                                 c("LA_percentage_F0","DHA_percentage_F0","n3PUFA_percentage_F0","n6PUFA_n3PUFA_F0",
                                                   "SFA_percentage_F0","MUFA_percentage_F0", "PUFA_percentage_F0","n6PUFA_percentage_F0",
                                                   "NECK_BMD_F2", "PELVIS_BMD_F2", "TOT_BMD_F2", "WBTOT_BMD_F2"))
data_norepeat_clean_F2_female <- scale_selected_columns(data_norepeat_F2[data_norepeat_F2$Sex_F0 == 0,], 
                                                        c("LA_percentage_F0","DHA_percentage_F0","n3PUFA_percentage_F0","n6PUFA_n3PUFA_F0",
                                                          "SFA_percentage_F0","MUFA_percentage_F0", "PUFA_percentage_F0","n6PUFA_percentage_F0",
                                                          "NECK_BMD_F2", "PELVIS_BMD_F2", "TOT_BMD_F2", "WBTOT_BMD_F2"))
data_norepeat_clean_F2_male <- scale_selected_columns(data_norepeat_F2[data_norepeat_F2$Sex_F0 == 1,], 
                                                      c("LA_percentage_F0","DHA_percentage_F0","n3PUFA_percentage_F0","n6PUFA_n3PUFA_F0",
                                                        "SFA_percentage_F0","MUFA_percentage_F0", "PUFA_percentage_F0","n6PUFA_percentage_F0",
                                                        "NECK_BMD_F2", "PELVIS_BMD_F2", "TOT_BMD_F2", "WBTOT_BMD_F2"))
####GLM####
X <- c("LA_percentage_F0_Z","DHA_percentage_F0_Z","n3PUFA_percentage_F0_Z","n6PUFA_n3PUFA_F0_Z",
               "SFA_percentage_F0_Z","MUFA_percentage_F0_Z", "PUFA_percentage_F0_Z","n6PUFA_percentage_F0_Z")
Y <- c("NECK_BMD_F2_Z", "PELVIS_BMD_F2_Z",
       "TOT_BMD_F2_Z","WBTOT_BMD_F2_Z")

Results_F2_all <- process_glm3(X, Y, data_norepeat_clean_F2, Covariates_all_F2, 2)
Results_F2_all_sex <- Results_F2_all[grep(":Sex_F01", Results_F2_all$Predictor, fixed = TRUE),]

Results_F2_female <- process_glm2(X, Y, data_norepeat_clean_F2_female, 
                                 setdiff(Covariates_all_F2, "Sex_F0"),
                                 0)

Results_F2_male <- process_glm2(X, Y, data_norepeat_clean_F2_male, 
                                setdiff(Covariates_all_F2, c("Sex_F0","Menopause_F0_recode","Hormone_F0_recode")),
                               1)

####GLM验证####
# y <- "WBTOT_BMD_F2"
# x <- "n6PUFA_n3PUFA_F0_Z"
# covariates <- setdiff(Covariates_all_F2, c("Sex_F0","Menopause_F0_recode","Hormone_F0_recode"))
# data <- data_norepeat_clean_F2_male
# model <- glm(as.formula(paste(y, "~", x, "+", paste(covariates, collapse = "+"))),
#              data = data, family = gaussian())
# summary(model)$coefficients
# table(data_norepeat_clean$Tea_F0_recode)
####正文F2结果描述####
a11 <- rbind(Results_F2_female, Results_F2_male)

a11a <- a11[a11$P_value<0.05,]
a11a$Estimate <- round(a11a$Estimate,3)
a11a$P_value <- round(a11a$P_value,3)

a11a_female <- a11a[a11a$Gender == "Female",]
a11a_female_1 <- a11a_female[a11a_female$Estimate >0,]
a11a_female_2 <- a11a_female[a11a_female$Estimate <0,]
max(a11a_female_2$Estimate)
min(a11a_female_2$Estimate)
max(a11a_female_2$P_value)
min(a11a_female_2$P_value)


a11a_male <- a11a[a11a$Gender == "Male",]
a11a_male_1 <- a11a_male[a11a_male$Estimate >0,]
max(a11a_male_1$Estimate)
min(a11a_male_1$Estimate)
max(a11a_male_1$P_value)
min(a11a_male_1$P_value)
a11a_male_2 <- a11a_male[a11a_male$Estimate <0,]
a11a_male_21 <- a11a_male_2[a11a_male_2$Predictor != "n6PUFA_percentage_F0_Z",]
max(a11a_male_21$Estimate)
min(a11a_male_21$Estimate)
max(a11a_male_21$P_value)
min(a11a_male_21$P_value)

a11a_male_22 <- a11a_male_2[a11a_male_2$Predictor == "n6PUFA_percentage_F0_Z",]
####F2体成分GLM可视化####
GLM_Results_F2_merge <- rbind(Results_F2_female, Results_F2_male)
a3 <- GLM_Results_F2_merge[GLM_Results_F2_merge$P_value<0.05,]
#修改变量名
GLM_Results_F2_merge <- recode_boneF2(GLM_Results_F2_merge, "Outcome")
GLM_Results_F2_merge <- recode_fatty(GLM_Results_F2_merge, "Predictor")
# 数据准备
GLM_Results_F2_merge <- GLM_Results_F2_merge %>%
  mutate(Gender = factor(Gender, levels = c("Female", "Male", "All")),
         Label = ifelse(Significance == "Significant", "*", ""))

Results_F2_plot <- ggplot(GLM_Results_F2_merge, aes(x = Predictor, y = Estimate, fill = Gender)) +
  
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) +
  
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.25, position = position_dodge(0.7)) +
  
  geom_text(aes(
    label = ifelse(Significance == "Significant", "*", ""),
    y = ifelse(Estimate > 0, Estimate + Std.Error, Estimate - Std.Error - 0.0015)  # 根据Estimate的符号调整y的位置
  ),
  position = position_dodge(0.7),
  vjust = 0.5,  # 垂直居中对齐
  size = 6, color = "red") +
  
  facet_wrap(~ Outcome, ncol = 2, scales = "free_y") +  # Facet by Outcome
  scale_fill_manual(values = c("Female" = "#E89F00", "Male" = "#7AA8D1")) +  
  theme_minimal() +
  theme(
    text = element_text(size = 13),  
    axis.title = element_text(face = "bold", size = 14),  
    plot.title = element_text(face = "bold", size = 16, hjust = 0),  
    axis.text.x = element_text(angle = 45, hjust = 1),  
    strip.text = element_text(size = 14, face = "bold"),  
    legend.position = "bottom",
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey50", fill = NA, size = 1),  
    strip.background = element_rect(fill = "#EAF0F6", color = "grey50",size = 0.5),
    legend.title = element_text(face = "bold")
  ) +
  labs(
    title = "UKB",
    x = "Plasma",
    y = "Estimate (95% CI)",
    fill = "Sex"
  ) 

ggsave("Figure 3-1.pdf", Results_F2_plot, width=10, height=10)
####*************************LMM************************####
####Z分数转换####
data_LMM_Z_all <- scale_selected_columns(data_LMM,
                                            c("LA_percentage_F0","DHA_percentage_F0","n3PUFA_percentage_F0","n6PUFA_n3PUFA_F0",
                                              "SFA_percentage_F0","MUFA_percentage_F0", "PUFA_percentage_F0","n6PUFA_percentage_F0",
                                              "NECK_BMD", "PELVIS_BMD", "TOT_BMD", "WBTOT_BMD"))

data_LMM_Z_female <- scale_selected_columns(data_LMM[data_LMM$Sex_F0 == 0,],
                                            c("LA_percentage_F0","DHA_percentage_F0","n3PUFA_percentage_F0","n6PUFA_n3PUFA_F0",
                                              "SFA_percentage_F0","MUFA_percentage_F0", "PUFA_percentage_F0","n6PUFA_percentage_F0",
                                              "NECK_BMD", "PELVIS_BMD", "TOT_BMD", "WBTOT_BMD"))

data_LMM_Z_male <- scale_selected_columns(data_LMM[data_LMM$Sex_F0 == 1,], 
                                          c("LA_percentage_F0","DHA_percentage_F0","n3PUFA_percentage_F0","n6PUFA_n3PUFA_F0",
                                            "SFA_percentage_F0","MUFA_percentage_F0", "PUFA_percentage_F0","n6PUFA_percentage_F0",
                                            "NECK_BMD", "PELVIS_BMD", "TOT_BMD", "WBTOT_BMD"))
####LMM####
X <- c("LA_percentage_F0_Z","DHA_percentage_F0_Z","n3PUFA_percentage_F0_Z","n6PUFA_n3PUFA_F0_Z",
       "SFA_percentage_F0_Z","MUFA_percentage_F0_Z", "PUFA_percentage_F0_Z","n6PUFA_percentage_F0_Z")
Y <- c("WBTOT_BMD_Z","PELVIS_BMD_Z","NECK_BMD_Z","TOT_BMD_Z")

Lmer_UKB_female <- process_lmer(data_LMM_Z_female, 
                                  X,Y, Covariates_female_LMM_UKB, 0)
Lmer_UKB_female_sig <- Lmer_UKB_female[Lmer_UKB_female$P_value < 0.05,]
Lmer_UKB_male <- process_lmer(data_LMM_Z_male, 
                                X, Y, Covariates_male_LMM_UKB, 1)
Lmer_UKB_male_sig <- Lmer_UKB_male[Lmer_UKB_male$P_value < 0.05,]
warnings()

Lmer_UKB_all <- process_lmer3(data_LMM_Z_all, 
                              X, Y, Covariates_all_LMM_UKB, 2)
Lmer_UKB_all_sex <- Lmer_UKB_all[grep(":Sex_F01", Lmer_UKB_all$Predictor, fixed = TRUE),]
####正文结果描述####
a1 <- Lmer_UKB_female_sig
a11 <- a1[a1$Estimate <0,]
round(max(a11$Estimate),3)
round(min(a11$Estimate),3)
round(max(a11$P_value),3)
round(min(a11$P_value),3)
a12 <- a1[a1$Estimate >0,]
round(max(a12$Estimate),3)
round(min(a12$Estimate),3)
round(max(a12$P_value),3)
round(min(a12$P_value),3)

a2 <- Lmer_UKB_male_sig
a21 <- a2[a2$Estimate <0,]
round(max(a21$Estimate),3)
round(min(a21$Estimate),3)
round(max(a21$P_value),3)
round(min(a21$P_value),3)
a22 <- a2[a2$Estimate >0,]
round(max(a22$Estimate),3)
round(min(a22$Estimate),3)
round(max(a22$P_value),3)
round(min(a22$P_value),3)
####验证LMM/性别交互效应####
# x <- "n6PUFA_n3PUFA_F0_Z"
# y <- "WBTOT_BMD_Z"
# covariates <- Covariates_all_LMM_UKB
# data <- data_LMM_Z_all
# formula_str <- paste0(y, " ~ ", x, " * Sex_F0 + ", paste(covariates, collapse = " + "), " + (1|ID)")
# lme_model <- lmer(formula_str, data = data)
# summary(lme_model)
####性别交互可视化####
Lmer_UKB_all_sex$Model <- "LMM"
Results_F2_all_sex$Model <- "GLM"

Sex_intersect_plot_UKB <- rbind(Lmer_UKB_all_sex, Results_F2_all_sex)
Sex_intersect_plot_UKB1 <- recode_fatty(Sex_intersect_plot_UKB, "Predictor")
Sex_intersect_plot_UKB1$Predictor <- paste0(Sex_intersect_plot_UKB1$Predictor, ": Sex")
Sex_intersect_plot_UKB1 <- recode_boneF2(Sex_intersect_plot_UKB1, "Outcome")
Sex_intersect_plot_UKB1 <- Sex_intersect_plot_UKB1[Sex_intersect_plot_UKB1$Model == "LMM",]

pdf("UKB_sex.pdf", width = 5, height = 5)
fa_levels <- rev(c(
  "C22:6 n-3: Sex", "C18:2 n-6: Sex", "n3PUFA: Sex", "n6PUFA: Sex", "SFA: Sex","MUFA: Sex", "PUFA: Sex","n6PUFA/n3PUFA: Sex"
))

plot_sex_interaction_heatmap(
  data = Sex_intersect_plot_UKB1,
  predictor_levels = fa_levels,
  title = "UKB (Plasma Fatty Acids)"
)

dev.off()
####UKB:GLM和LMM一起可视化####
forest_data_UKB <- rbind(Results_F2_female,Results_F2_male)
forest_data_UKB$Model <- "GLM-Fatty acid (Instance 0) & BMD (Instance 2)"

Lmer_data_UKB <- rbind(Lmer_UKB_female, Lmer_UKB_male)
Lmer_data_UKB$Model <- "LMM-Fatty acid (Instance 0) & BMD (Instance 2-3)"

GLM_Lmer_UKB <- rbind(forest_data_UKB, Lmer_data_UKB)
GLM_Lmer_UKB <- recode_fatty(GLM_Lmer_UKB, "Predictor")
GLM_Lmer_UKB <- recode_boneF2(GLM_Lmer_UKB, "Outcome")
GLM_Lmer_UKB <- GLM_Lmer_UKB[GLM_Lmer_UKB$Model == "LMM-Fatty acid (Instance 0) & BMD (Instance 2-3)",]
unique(GLM_Lmer_UKB$Model)
#**********************可视化
pdf("Figure 3.pdf", width = 10, height = 10)
plot_forest4_enhanced3(
  data = GLM_Lmer_UKB,
  x_title = "Plasma",
  title = "UKB",
  model1 = "LMM-Fatty acid (Instance 0) & BMD (Instance 2-3)",
  colors = c("#1f78b4"),
  facet_nrow = 2
)
dev.off()
# pdf("Figure 3.pdf", width = 9, height = 10)
# plot_forest4_enhanced2(
#   data = GLM_Lmer_UKB,
#   x_title = "Plasma",
#   title = "UKB",
#   model1 = "GLM-Fatty acid (Instance 0) & BMD (Instance 2)",
#   model2 = "LMM-Fatty acid (Instance 0) & BMD (Instance 2-3)",
#   colors = c("#1f78b4", "#1b9e77"),
#   facet_nrow = 2 
# )
# dev.off()
####***************Table1-UKB****************####
# Table1b_data <- data_norepeat_clean_F2
# colnames(Table1b_data)
# Table1b_data$WBTOT_FAT_F2 <- Table1b_data$WBTOT_FAT_F2*0.001
# Table1b <- descrTable(Sex_F0 ~ Age_F0+Met_F0+Energy_F0_kcal+WBTOT_FAT_F2+
#                         Smoking_F0_recode+Alcohol_F0_recode+Tea_F0_recode+Vitamin_mineral_F0_recode+Fish_oil_sup_F0+Disease_F0+Fractured_F0_recode+Hormone_F0_recode+Menopause_F0_recode,
#                       data = Table1b_data ,method =NA,show.all = TRUE)#show.all = TRUE :显示all
# export2word(Table1b, file='/Users/hozan/Library/CloudStorage/OneDrive-Personal/Papers/2.1 中老年人骨量肌肉和脂肪酸/Output/Table1b.docx')

a <- unique(data_LMM$ID)
Table1_data <- data[data$ID %in% data_LMM$ID,]

factor_name<-c("Sex_F0","Disease_F0","Smoking_F0_recode","Alcohol_F0_recode","Tea_F0_recode","Vitamin_mineral_F0_recode",
               "Menopause_F0_recode","Hormone_F0_recode","Fractured_F0_recode","Fish_oil_sup_F0")
idx <- which(names(Table1_data)%in% factor_name)
for(i in idx ){
  Table1_data[,i]  <-  as.factor(Table1_data[,i])
}

Table1_data <- Table1_data[,c("ID","Age_F2","Age_F3","WBTOT_FAT_F3",Covariates_all_F2)]
Table1_data$WBTOT_FAT_F2 <- Table1_data$WBTOT_FAT_F2*0.001
Table1_data$WBTOT_FAT_F3 <- Table1_data$WBTOT_FAT_F3*0.001
Table1b <- descrTable(Sex_F0 ~ Age_F0+Age_F2+Age_F3+Met_F0+Energy_F0_kcal+WBTOT_FAT_F2+WBTOT_FAT_F3+
                        Smoking_F0_recode+Alcohol_F0_recode+Tea_F0_recode+Vitamin_mineral_F0_recode+Fish_oil_sup_F0+Disease_F0+Fractured_F0_recode+Hormone_F0_recode+Menopause_F0_recode,
                      data = Table1_data ,method =NA,show.all = TRUE)#show.all = TRUE :显示all
export2word(Table1b, file='/Users/hozan/Library/CloudStorage/OneDrive-Personal/Papers/2.1 中老年人骨量肌肉和脂肪酸/Output/Table1b.docx')

a <- Table1_data[,c("ID","WBTOT_FAT_F2")]
a <- a[complete.cases(a),]
median(a$Age_F3)

Instance3 <- data[data$ID %in% data_LMM[data_LMM$Times == "Instance3",]$ID,]
Instance3$WBTOT_FAT_F3 <- Instance3$WBTOT_FAT_F3*0.001
Table1c <- descrTable(Sex_F0 ~ Age_F3+WBTOT_FAT_F3,
                      data = Instance3 ,method =NA,show.all = TRUE)#show.all = TRUE :显示all
export2word(Table1c, file='/Users/hozan/Library/CloudStorage/OneDrive-Personal/Papers/2.1 中老年人骨量肌肉和脂肪酸/Output/Table1c.docx')


a <- Table1_data[Table1_data$ID %in% Instance3$ID ,c("ID","Age_F2","Age_F3")]
a <- a[complete.cases(a),]
a$Age_gap <- a$Age_F3-a$Age_F2
mean(a$Age_gap)
####计算随访时间####
colnames(data_all)[1:6]
Follow_time <- data_all[data_all$ID %in% data_norepeat_clean_F2$ID, c("ID","Date_F0","Date_F1","Date_F2","Date_F3")]

Follow_time$Date_F3 <- as.Date(Follow_time$Date_F3)
Follow_time$Date_F2 <- as.Date(Follow_time$Date_F2)
Follow_time$Date_F1 <- as.Date(Follow_time$Date_F1)
Follow_time$Date_F0 <- as.Date(Follow_time$Date_F0)

# 计算时间间隔
Follow_time$F0F1TimeInterval <- ifelse(!is.na(Follow_time$Date_F0) & !is.na(Follow_time$Date_F1),
                                       as.numeric(difftime(Follow_time$Date_F1, Follow_time$Date_F0, units = "days")) / 365, NA)

Follow_time$F0F2TimeInterval <- ifelse(!is.na(Follow_time$Date_F0) & !is.na(Follow_time$Date_F2),
                                       as.numeric(difftime(Follow_time$Date_F2, Follow_time$Date_F0, units = "days")) / 365, NA)
mean(Follow_time$F0F2TimeInterval)
Follow_time$F2F3TimeInterval <- ifelse(!is.na(Follow_time$Date_F2) & !is.na(Follow_time$Date_F3),
                                       as.numeric(difftime(Follow_time$Date_F3, Follow_time$Date_F2, units = "days")) / 365, NA)
mean(Follow_time$F2F3TimeInterval, na.rm = TRUE)
# 计算最大/最小的年份
Follow_time$Year_F0 <- format(Follow_time$Date_F0, "%Y")
min(Follow_time$Year_F0, na.rm = TRUE)
max(Follow_time$Year_F0, na.rm = TRUE)

Follow_time$Year_F2 <- format(Follow_time$Date_F2, "%Y")
min(Follow_time$Year_F2, na.rm = TRUE)
max(Follow_time$Year_F2, na.rm = TRUE)

Follow_time$Year_F3 <- format(Follow_time$Date_F3, "%Y")
min(Follow_time$Year_F3, na.rm = TRUE)
max(Follow_time$Year_F3, na.rm = TRUE)
####流程图####
a <- data_norepeat_F2 <- data[, c("ID","LA_percentage_F0","DHA_percentage_F0","n3PUFA_percentage_F0","n6PUFA_n3PUFA_F0",
                                  "SFA_percentage_F0","MUFA_percentage_F0", "PUFA_percentage_F0","n6PUFA_percentage_F0"
                                  
)]
a  <- a[complete.cases(a),]

a <- data[, c("ID","NECK_BMC_F2","NECK_BMD_F2", "PELVIS_BMC_F2", "PELVIS_BMD_F2",
              "TOT_BMC_F2","TOT_BMD_F2","WBTOT_BMC_F2","WBTOT_BMD_F2"
)]
a  <- a[complete.cases(a),]
####脂肪酸可视化####
UKB_fattyacid <- data[data$ID %in% data_norepeat_clean_F2$ID, c("ID","Sex_F0",
                                                                "LA_percentage_F0","DHA_percentage_F0","n3PUFA_percentage_F0","n6PUFA_n3PUFA_F0",
                                                                "SFA_percentage_F0","MUFA_percentage_F0", "PUFA_percentage_F0","n6PUFA_percentage_F0")]

UKB_fattyacid_long <- UKB_fattyacid %>% 
  tidyr::pivot_longer(cols = -c(Sex_F0,ID), 
                      names_to = "Fatty_Acid", 
                      values_to = "Value")

#修改变量名
UKB_fattyacid_long <- recode_fatty_noZ(UKB_fattyacid_long, "Fatty_Acid")
UKB_fattyacid_long$Sex_F0 <- as.factor(UKB_fattyacid_long$Sex_F0)
colnames(UKB_fattyacid_long)
#**********************************************可视化
# 确保Sex_F0为因子
UKB_fattyacid_long$Sex_F0 <- factor(
  UKB_fattyacid_long$Sex_F0,
  levels = c(0, 1),
  labels = c("Female", "Male")
)

sex_colors <- c("Female" = "#FF8C69", "Male" = "#3C9D9B")

p <- ggplot(UKB_fattyacid_long, aes(x = Sex_F0, y = Value, fill = Sex_F0)) +
  
  geom_violin(alpha = 0.7, color = "black", linewidth = 0.3, trim = TRUE) +
  
  # 黑点：中位数
  stat_summary(fun = median, geom = "point", shape = 16, size = 2, color = "black") +
  
  # 红点：均值
  stat_summary(fun = mean, geom = "point", shape = 16, size = 2, color = "red") +
  
  facet_wrap(~ Fatty_Acid, scales = "free_y", nrow = 2, ncol = 4) +
  
  scale_fill_manual(values = sex_colors) +
  labs(
    x = "",
    y = "Fatty Acids Level (%)",
    title = ""
  ) +
  
  theme_shared +
  theme(
    legend.position="none",
    strip.background = element_rect(fill = "#C2DEFF", color = "black", linewidth = 0.5)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))

pdf("UKB_fattyacid_violin_by_sex.pdf", width = 11, height = 7)
print(p)
dev.off()
####骨量可视化####
UKB_BoneF2 <- data[data$ID %in% data_norepeat_clean_F2$ID, c("ID","Sex_F0","Age_F0",
                                                             "NECK_BMD_F2", "PELVIS_BMD_F2",
                                                             "TOT_BMD_F2","WBTOT_BMD_F2")]
UKB_BoneF2_long <- UKB_BoneF2 %>%
  pivot_longer(cols = -c(Sex_F0, Age_F0, ID), 
               names_to = "Bone", 
               values_to = "Value")
UKB_BoneF2_long_age <- merge(UKB_BoneF2_long, Follow_time[,c("ID","F0F2TimeInterval")],by = "ID")
UKB_BoneF2_long_age$Age_F2 <- UKB_BoneF2_long_age$Age_F0 + UKB_BoneF2_long_age$F0F2TimeInterval
colnames(UKB_BoneF2_long_age)
UKB_BoneF2_long_age <- recode_boneF2(UKB_BoneF2_long_age,"Bone")
#*************************可视化
# Sex_F0 因子
UKB_BoneF2_long_age$Sex_F0 <- factor(
  UKB_BoneF2_long_age$Sex_F0,
  levels = c(0,1),
  labels = c("Female","Male")
)

# 去除离群值（每个 Bone × Sex）
UKB_clean <- UKB_BoneF2_long_age %>%
  group_by(Bone, Sex_F0) %>%
  mutate(
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower = Q1 - 3*IQR,
    upper = Q3 + 3*IQR
  ) %>%
  filter(Value >= lower & Value <= upper) %>%
  dplyr::select(-Q1, -Q3, -IQR, -lower, -upper) %>%
  ungroup()

# 统计量（mean age, median BMD）
stats_data <- UKB_clean %>%
  group_by(Bone, Sex_F0) %>%
  summarise(
    median_bmd = median(Value, na.rm = TRUE),
    mean_age = sprintf("%.1f", mean(Age_F2, na.rm = TRUE)),
    y_max = max(Value, na.rm = TRUE),
    y_min = min(Value, na.rm = TRUE),
    .groups = "drop"
  )

# 构建 x 轴标签（性别 + 年龄）
age_labels_vec <- stats_data %>%
  dplyr::select(Sex_F0, mean_age) %>%
  distinct() %>%
  mutate(label = paste0(Sex_F0, "\n(", mean_age, "y)")) %>%
  { setNames(.$label, .$Sex_F0) }


p <- ggplot(UKB_clean, aes(x = Sex_F0, y = Value, fill = Sex_F0)) +
  
  # 小提琴图
  geom_violin(
    aes(group = Sex_F0),
    position = position_dodge(width = 0.7),
    alpha = 0.5,
    color = "black"
  ) +
  
  # 箱线图
  geom_boxplot(
    aes(group = Sex_F0),
    width = 0.25,
    position = position_dodge(width = 0.7),
    outlier.shape = NA,
    fill = "white",
    color = "black"
  ) +
  
  # 中位数点
  geom_point(
    data = stats_data,
    aes(x = Sex_F0, y = median_bmd, color = Sex_F0),
    position = position_dodge(width = 0.7),
    size = 3,
    shape = 18
  ) +
  
  facet_wrap(~ Bone, ncol = 2, scales = "free_y") +
  
  scale_x_discrete(labels = age_labels_vec) +
  
  scale_fill_manual(values = c("Female"="#FFBE7A", "Male"="#C8E6C9")) +
  scale_color_manual(values = c("Female"="#FFBE7A", "Male"="#C8E6C9")) +
  
  labs(
    x = "Sex (with mean age)",
    y = "BMD (g/cm²)",
    fill = "Sex",
    color = "Sex"
  ) +
  
  theme_shared +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "#C2DEFF", color = "black", linewidth = 0.6),
    axis.text.x = element_text(size = 12, face = "bold", lineheight = 0.9)
  )


pdf("UKB_BONE_F2_plot.pdf", width = 11, height = 9)
print(p)
dev.off()
####******************meta分析******************####
####数据整理####
#*************************ALSPAC
ALSPAC_results_meta <- ALSPAC_results
colnames(ALSPAC_results_meta) <- paste0(colnames(ALSPAC_results_meta),"_A")
ALSPAC_results_meta <- ALSPAC_results_meta %>%
  mutate(
    Model_A = case_when(
      Model_A == "GLM-Fatty acid (FOM1&FOM2) & BMD AUC (FOM1−4)" ~ "GLM",
      Model_A == "LMM-Fatty acid (FOM1) & BMD (FOM1−4)" ~ "LMM",
      TRUE ~ Model_A
    )
  )
#*************************UKB
UKB_results_meta <- GLM_Lmer_UKB
colnames(UKB_results_meta) <- paste0(colnames(UKB_results_meta),"_U")
UKB_results_meta <- UKB_results_meta %>%
  mutate(
    Model_U = case_when(
      Model_U == "GLM-Fatty acid (Instance 0) & BMD (Instance 2)" ~ "GLM_noAUC",
      Model_U == "LMM-Fatty acid (Instance 0) & BMD (Instance 2-3)" ~ "LMM",
      TRUE ~ Model_U
    )
  )
#*************************GNHS
GNHS_results_meta <- GLM_Lmer_plotB
colnames(GNHS_results_meta) <- paste0(colnames(GNHS_results_meta),"_G")
unique(GNHS_results_meta$Model_G)
GNHS_results_meta <- GNHS_results_meta %>%
  mutate(
    Model_G = case_when(
      Model_G == "GLM-Fatty acid (F0&F3) & BMD AUC (F1-4)" ~ "GLM",
      Model_G == "LMM-Fatty acid (F0) & BMD (F1-4)" ~ "LMM",
      TRUE ~ Model_G
    )
  )


Meta_merge <- merge(ALSPAC_results_meta, GNHS_results_meta, by.x = c("Predictor_A", "Outcome_A","Gender_A","Model_A"), by.y = c("Predictor_G", "Outcome_G","Gender_G","Model_G"))
colnames(Meta_merge) <- recode(colnames(Meta_merge),
                               "Predictor_A" = "Predictor",
                               "Outcome_A" = "Outcome",
                               "Gender_A" = "Gender",
                               "Model_A" = "Model")
Meta_merge1 <- Meta_merge[Meta_merge$Model == "GLM",]
Meta_merge2 <- merge(Meta_merge[Meta_merge$Model == "LMM",],UKB_results_meta, by.x = c("Predictor", "Outcome","Gender","Model"), by.y = c("Predictor_U", "Outcome_U","Gender_U","Model_U"))
Meta_merge3 <- Meta_merge[Meta_merge$Model == "LMM" & Meta_merge$Outcome == "Total Hip BMD",]
Meta_merge4 <- merge(GNHS_results_meta, UKB_results_meta, by.x = c("Predictor_G", "Outcome_G","Gender_G","Model_G"), by.y = c("Predictor_U", "Outcome_U","Gender_U","Model_U"))
colnames(Meta_merge4) <- recode(colnames(Meta_merge4),
                               "Predictor_G" = "Predictor",
                               "Outcome_G" = "Outcome",
                               "Gender_G" = "Gender",
                               "Model_G" = "Model")
Meta_merge4_LS_female <- Meta_merge4[Meta_merge4$Model == "LMM" & Meta_merge4$Outcome == "Lumbar Spine BMD" & Meta_merge4$Gender == "Female",]
Meta_merge4_LS_male <- Meta_merge4[Meta_merge4$Model == "LMM" & Meta_merge4$Outcome == "Lumbar Spine BMD" & Meta_merge4$Gender == "Male",]
Meta_merge4_WB_male <- Meta_merge4[Meta_merge4$Model == "LMM" & Meta_merge4$Outcome == "Whole Body BMD" & Meta_merge4$Gender == "Male",]
Meta_merge4_FN_male <- Meta_merge4[Meta_merge4$Model == "LMM" & Meta_merge4$Outcome == "Femur Neck BMD" & Meta_merge4$Gender == "Male",]

# 算出ALSPAC的标准误
Meta_merge1$Std.Error_A <- (Meta_merge1$CI_high_A - Meta_merge1$CI_low_A) / (2 * 1.96)
Meta_merge2$Std.Error_A <- (Meta_merge2$CI_high_A - Meta_merge2$CI_low_A) / (2 * 1.96)
Meta_merge3$Std.Error_A <- (Meta_merge3$CI_high_A - Meta_merge3$CI_low_A) / (2 * 1.96)

####meta分析####
auto_meta <- function(data, 
                      estimate_prefix = "Estimate_", 
                      se_prefix = "Std.Error_",
                      I2_cutoff = 25) {
  
  meta_results <- list()
  
  # 自动识别 estimate 和 se 列名
  est_cols <- grep(paste0("^", estimate_prefix), colnames(data), value = TRUE)
  se_cols  <- grep(paste0("^", se_prefix), colnames(data), value = TRUE)
  
  # 确保两个列表的顺序匹配
  cohort_names <- gsub(estimate_prefix, "", est_cols)
  se_cols <- se_cols[match(cohort_names, gsub(se_prefix, "", se_cols))]
  
  if (any(is.na(se_cols))) stop("SE columns do not match estimate columns.")
  
  for (i in 1:nrow(data)) {
    
    # 按严格匹配顺序提取 yi 和 sei
    yi  <- as.numeric(data[i, est_cols])
    sei <- as.numeric(data[i, se_cols])
    
    # 1.随机效应模型计算 I2 
    res_re <- rma.uni(yi = yi, sei = sei, method = "REML")
    I2_val <- res_re$I2
    
    # 2. 根据 I2 自动选择 FE / RE 
    if (I2_val <= I2_cutoff) {
      method_used <- "FE"
      res <- rma.uni(yi = yi, sei = sei, method = "FE")
    } else {
      method_used <- "RE"
      res <- res_re
    }
    
    # 3. 权重计算（严格按队列顺序）
    if (method_used == "FE") {
      weights <- 1 / (sei^2)
    } else {
      weights <- 1 / (sei^2 + res$tau2)
    }
    
    weights_norm <- weights / sum(weights)
    
    # 4. 按队列名给权重命名
    weight_df <- setNames(as.data.frame(t(weights_norm)),
                          paste0("Weight_", cohort_names))
    
    meta_results[[i]] <- cbind(
      data.frame(
        Predictor  = data$Predictor[i],
        Outcome    = data$Outcome[i],
        Gender     = data$Gender[i],
        Model      = data$Model[i],
        Meta_Est   = res$b,
        Meta_LCL   = res$ci.lb,
        Meta_UCL   = res$ci.ub,
        Meta_p     = res$pval,
        I2         = I2_val,
        Model_used = method_used
      ),
      weight_df
    )
  }
  
  Meta_final <- do.call(rbind, meta_results)
  return(Meta_final)
}

Meta_GLM <- auto_meta(Meta_merge1, "Estimate_", "Std.Error_", 25)
Meta_LMM <- auto_meta(Meta_merge2, "Estimate_", "Std.Error_", 25)
Meta_LMM_TH <- auto_meta(Meta_merge3, "Estimate_", "Std.Error_", 25)
Meta_LMM_LS_female <- auto_meta(Meta_merge4_LS_female, "Estimate_", "Std.Error_", 25)
Meta_LMM_LS_male <- auto_meta(Meta_merge4_LS_male, "Estimate_", "Std.Error_", 25)
Meta_LMM_WB_male <- auto_meta(Meta_merge4_WB_male, "Estimate_", "Std.Error_", 25)
Meta_LMM_FN_male <- auto_meta(Meta_merge4_FN_male, "Estimate_", "Std.Error_", 25)
####正文写作####
Var <- c("Predictor","Outcome","Gender","Model","Meta_Est","Meta_LCL","Meta_UCL","Meta_p","I2","Model_used")
#***********女性
a1 <- rbind(Meta_GLM[Meta_GLM$Meta_p < 0.05,Var],Meta_LMM[Meta_LMM$Meta_p < 0.05,Var],Meta_LMM_TH[Meta_LMM_TH$Meta_p < 0.05,Var],Meta_LMM_LS_female[Meta_LMM_LS_female$Meta_p < 0.05,Var])
a1$Meta_Est <- round(a1$Meta_Est,3)
a1$Meta_p <- round(a1$Meta_p,3)
# GLM
a11_pos <- a1[a1$Meta_Est>0 & a1$Model == "GLM",]
a11_neg <- a1[a1$Meta_Est<0 & a1$Model == "GLM",]
round(max(a11_neg$Meta_Est),3)
round(min(a11_neg$Meta_Est),3)
round(max(a11_neg$Meta_p),3)
round(min(a11_neg$Meta_p),3)
# LMM
a12_pos <- a1[a1$Meta_Est>0 & a1$Model == "LMM",]
a12_neg <- a1[a1$Meta_Est<0 & a1$Model == "LMM",]
round(max(a12_neg$Meta_Est),3)
round(min(a12_neg$Meta_Est),3)
round(max(a12_neg$Meta_p),3)
round(min(a12_neg$Meta_p),3)
#***********男性
a2 <- rbind(Meta_LMM_LS_male[Meta_LMM_LS_male$Meta_p < 0.05,Var],Meta_LMM_WB_male[Meta_LMM_WB_male$Meta_p < 0.05, Var],Meta_LMM_FN_male[Meta_LMM_FN_male$Meta_p < 0.05, Var])
a2$Meta_Est <- round(a2$Meta_Est,3)
a2$Meta_p <- round(a2$Meta_p,3)
unique(a2$Predictor)
a21 <- a2[a2$Predictor %in% c("n3PUFA","C22:6 n-3"),]
max(a21$Meta_Est)
min(a21$Meta_Est)
max(a21$Meta_p)
min(a21$Meta_p)

a22 <- a2[a2$Predictor %in% c("n6PUFA","n6PUFA/n3PUFA","C18:2 n-6"),]
max(a22$Meta_Est)
min(a22$Meta_Est)
max(a22$Meta_p)
min(a22$Meta_p)

a23 <- a2[a2$Predictor %in% c("SFA"),]
####meta可视化####
plot_forest_BMD <- function(
    Meta_raw,
    Meta_meta = NULL,
    interval = 4,
    outcome_keep = "Whole Body BMD",
    predictor_order = c(
      "C22:6 n-3", "C18:2 n-6", "n3PUFA", "n6PUFA",
      "SFA","MUFA","PUFA","n6PUFA/n3PUFA"
    ),
    title = "LMM-Whole Body BMD-Female",
    pdf_file = "Meta.pdf",
    width = 7,
    height = 11
) {
  
  library(dplyr)
  library(tidyr)
  library(grid)
  library(gridExtra)
  library(forestploter)
  
  #  原始队列（自动识别 1 / 2 / 3 / N 个队列）
  df_left <- Meta_raw %>%
    dplyr::select(
      Predictor, Outcome, Gender, Model,
      matches("^Estimate_"),
      matches("^CI_low_"),
      matches("^CI_high_"),
      matches("^P_value_")
    ) %>%
    pivot_longer(
      cols = matches("^Estimate_"),
      names_to = "Cohort",
      values_to = "Estimate"
    ) %>%
    # 第一次 mutate：只生成 Cohort_code
    mutate(
      Cohort_code = sub("Estimate_", "", Cohort)
    ) %>%
    # 第二次 mutate：安全索引其他列
    mutate(
      CI_low  = mapply(function(code, row) row[[paste0("CI_low_", code)]],
                       Cohort_code,
                       split(cur_data(), seq_len(n()))),
      CI_high = mapply(function(code, row) row[[paste0("CI_high_", code)]],
                       Cohort_code,
                       split(cur_data(), seq_len(n()))),
      P_value = mapply(function(code, row) row[[paste0("P_value_", code)]],
                       Cohort_code,
                       split(cur_data(), seq_len(n()))),
      Panel = "Original",
      Cohort = Cohort_code
    ) %>%
    dplyr::select(
      Predictor, Outcome, Gender, Model,
      Cohort, Panel, Estimate, CI_low, CI_high, P_value
    )
  
  
  #  Meta-analysis 队列
  if (!is.null(Meta_meta)) {
    
    df_right <- Meta_meta %>%
      mutate(
        Cohort  = paste0("Meta (", Model_used, ")"),
        Panel   = "Meta",
        Estimate = Meta_Est,
        CI_low   = Meta_LCL,
        CI_high  = Meta_UCL,
        P_value  = Meta_p
      ) %>%
      dplyr::select(
        Predictor, Outcome, Gender, Model,
        Cohort, Panel, Estimate, CI_low, CI_high, P_value
      )
    
    df_plot <- bind_rows(df_left, df_right)
    
  } else {
    df_plot <- df_left
  }
  
  #   格式化数值
  df_plot <- df_plot %>%
    mutate(
      `Estimate (95% CI)` = paste0(
        sprintf("%.3f", Estimate),
        " (",
        sprintf("%.3f", CI_low),
        " - ",
        sprintf("%.3f", CI_high),
        ")"
      ),
      P_value_fmt = ifelse(P_value < 0.001, "<0.001", sprintf("%.3f", P_value)),
      Estimate_fmt = sprintf("%.3f", Estimate),
      CI_low_fmt   = sprintf("%.3f", CI_low),
      CI_high_fmt  = sprintf("%.3f", CI_high)
    )
  
  #  Predictor 顺序 & 队列名映射
  df_plot$Predictor <- factor(df_plot$Predictor, levels = predictor_order)
  df_plot <- df_plot[order(df_plot$Predictor, df_plot$Outcome), ]
  
  cohort_map <- c(
    "A" = "ALSPAC",
    "G" = "GNHS",
    "U" = "UKB"
  )
  
  df_plot <- df_plot %>%
    mutate(
      Cohort = ifelse(Cohort %in% names(cohort_map),
                      cohort_map[Cohort],
                      Cohort)
    )
  
  #  只保留指定 Outcome
  df <- df_plot %>% filter(Outcome == outcome_keep)
  n <- nrow(df)
  
  #  插入 Predictor summary 行
  insert_pos <- seq(1, n + length(seq(1, n, by = interval)), by = interval + 1)
  new_n <- n + length(insert_pos)
  
  new_df <- df[rep(1, new_n), ]
  new_df[,] <- NA
  colnames(new_df) <- colnames(df)
  
  j <- 1
  for (i in seq_len(new_n)) {
    if (i %in% insert_pos) {
      if (j <= n) new_df$Cohort[i] <- as.character(df$Predictor[j])
    } else {
      new_df[i, ] <- df[j, ]
      j <- j + 1
    }
  }
  
  #  forestploter 输入数据
  new_df <- new_df %>%
    mutate(
      Is_Summary = is.na(`Estimate (95% CI)`),
      Models = ifelse(Is_Summary, Cohort, paste0("    ", Cohort)),
      `Estimate (95% CI)` = ifelse(Is_Summary, "", `Estimate (95% CI)`),
      P_value_fmt = ifelse(Is_Summary, "", P_value_fmt),
      Estimate_fmt = ifelse(Is_Summary, "", Estimate_fmt),
      CI_low_fmt = ifelse(Is_Summary, "", CI_low_fmt),
      CI_high_fmt = ifelse(Is_Summary, "", CI_high_fmt)
    )
  
  kb <- paste(rep(" ", 24), collapse = " ")
  
  dt1 <- cbind(
    new_df[,"Models"],
    kb,
    new_df[,c("Estimate_fmt","CI_low_fmt","CI_high_fmt","P_value_fmt")]
  )
  
  colnames(dt1) <- c("Group", "", "Estimate","CI_low","CI_high", "P")
  
  #  生成森林图
  Meta_plot <- forestploter::forest(
    dt1,
    est = as.numeric(new_df$Estimate),
    lower = as.numeric(new_df$CI_low),
    upper = as.numeric(new_df$CI_high),
    sizes = 0.5,
    ci_column = 2,
    ref_line = 0,
    is.summary = new_df$Is_Summary,
    footnote = "",
    paginate = FALSE
  )
  
  title_text <- textGrob(
    title,
    gp = gpar(fontsize = 16, fontface = "bold")
  )
  
  final_plot <- arrangeGrob(
    title_text,
    Meta_plot,
    ncol = 1,
    heights = c(0.05, 0.95)
  )
  
  #  输出 PDF
  pdf(pdf_file, width = width, height = height)
  grid.newpage()
  grid.draw(final_plot)
  dev.off()
  
  message("Forest plot saved to: ", pdf_file)
  
  return(list(
    df_plot = df_plot,
    new_df  = new_df,
    dt1     = dt1,
    plot_obj = final_plot
  ))
}


LMM_WB <- plot_forest_BMD(Meta_raw = Meta_merge2, 
                          Meta_meta = Meta_LMM, 
                          interval = 4, 
                          outcome_keep = "Whole Body BMD",
                          predictor_order = c("C22:6 n-3", "C18:2 n-6", "n3PUFA", "n6PUFA",
                                              "SFA","MUFA","PUFA","n6PUFA/n3PUFA"),
                          title = "LMM (female): Whole Body BMD",
                          pdf_file = "LMM(female)-Whole Body BMD.pdf",
                          width = 7,
                          height = 11)

LMM_FN <- plot_forest_BMD(Meta_raw = Meta_merge2, 
                          Meta_meta = Meta_LMM, 
                          interval = 4, 
                          outcome_keep = "Femur Neck BMD",
                          predictor_order = c("C22:6 n-3", "C18:2 n-6", "n3PUFA", "n6PUFA",
                                              "SFA","MUFA","PUFA","n6PUFA/n3PUFA"),
                          title = "LMM (female): Femur Neck BMD",
                          pdf_file = "LMM(female)-Femur Neck BMD.pdf",
                          width = 7,
                          height = 11)

LMM_TH <- plot_forest_BMD(Meta_raw = Meta_merge3, 
                          Meta_meta = Meta_LMM_TH, 
                          interval = 3, 
                          outcome_keep = "Total Hip BMD",
                          predictor_order = c("C22:6 n-3", "C18:2 n-6", "n3PUFA", "n6PUFA",
                                              "SFA","MUFA","PUFA","n6PUFA/n3PUFA"),
                          title = "LMM (female): Total Hip BMD",
                          pdf_file = "LMM(female)-Total Hip BMD.pdf",
                          width = 7,
                          height = 9)

LMM_LS_female <- plot_forest_BMD(Meta_raw = Meta_merge4_LS_female, 
                          Meta_meta = Meta_LMM_LS_female, 
                          interval = 3, 
                          outcome_keep = "Lumbar Spine BMD",
                          predictor_order = c("C22:6 n-3", "C18:2 n-6", "n3PUFA", "n6PUFA",
                                              "SFA","MUFA","PUFA","n6PUFA/n3PUFA"),
                          title = "LMM (female): Lumbar Spine BMD",
                          pdf_file = "LMM(female)-Lumbar Spine BMD.pdf",
                          width = 7,
                          height = 9)

LMM_LS_male <- plot_forest_BMD(Meta_raw = Meta_merge4_LS_male, 
                                 Meta_meta = Meta_LMM_LS_male, 
                                 interval = 3, 
                                 outcome_keep = "Lumbar Spine BMD",
                                 predictor_order = c("C22:6 n-3", "C18:2 n-6", "n3PUFA", "n6PUFA",
                                                     "SFA","MUFA","PUFA","n6PUFA/n3PUFA"),
                                 title = "LMM (male): Lumbar Spine BMD",
                                 pdf_file = "LMM(male)-Lumbar Spine BMD.pdf",
                                 width = 7,
                                 height = 9)

LMM_WB_male <- plot_forest_BMD(Meta_raw = Meta_merge4_WB_male, 
                               Meta_meta = Meta_LMM_WB_male, 
                               interval = 3, 
                               outcome_keep = "Whole Body BMD",
                               predictor_order = c("C22:6 n-3", "C18:2 n-6", "n3PUFA", "n6PUFA",
                                                   "SFA","MUFA","PUFA","n6PUFA/n3PUFA"),
                               title = "LMM (male): Whole Body BMD",
                               pdf_file = "LMM(male)-Whole Body BMD.pdf",
                               width = 7,
                               height = 9)

LMM_FN_male <- plot_forest_BMD(Meta_raw = Meta_merge4_FN_male, 
                               Meta_meta = Meta_LMM_FN_male, 
                               interval = 3, 
                               outcome_keep = "Femur Neck BMD",
                               predictor_order = c("C22:6 n-3", "C18:2 n-6", "n3PUFA", "n6PUFA",
                                                   "SFA","MUFA","PUFA","n6PUFA/n3PUFA"),
                               title = "LMM (male): Femur Neck BMD",
                               pdf_file = "LMM(male)-Femur Neck BMD.pdf",
                               width = 7,
                               height = 9)

GLM_WB <- plot_forest_BMD(Meta_raw = Meta_merge1, 
                          Meta_meta = Meta_GLM, 
                          interval = 3, 
                          outcome_keep = "Whole Body BMD",
                          predictor_order = c("C22:6 n-3", "C18:2 n-6", "n3PUFA", "n6PUFA",
                                              "SFA","MUFA","PUFA","n6PUFA/n3PUFA"),
                          title = "GLM (female): Whole Body BMD AUC",
                          pdf_file = "GLM(female)-Whole Body BMD.pdf",
                          width = 7,
                          height = 9)

unique(Meta_merge1$Outcome)
GLM_FN <- plot_forest_BMD(Meta_raw = Meta_merge1, 
                          Meta_meta = Meta_GLM, 
                          interval = 3, 
                          outcome_keep = "Femur Neck BMD",
                          predictor_order = c("C22:6 n-3", "C18:2 n-6", "n3PUFA", "n6PUFA",
                                              "SFA","MUFA","PUFA","n6PUFA/n3PUFA"),
                          title = "GLM (female): Femur Neck BMD AUC",
                          pdf_file = "GLM(female)-Femur Neck BMD.pdf",
                          width = 7,
                          height = 9)

GLM_TH <- plot_forest_BMD(Meta_raw = Meta_merge1, 
                          Meta_meta = Meta_GLM, 
                          interval = 3, 
                          outcome_keep = "Total Hip BMD",
                          predictor_order = c("C22:6 n-3", "C18:2 n-6", "n3PUFA", "n6PUFA",
                                              "SFA","MUFA","PUFA","n6PUFA/n3PUFA"),
                          title = "GLM (female): Total Hip BMD AUC",
                          pdf_file = "GLM(female)-Total Hip BMD.pdf",
                          width = 7,
                          height = 9)
####验证meta函数####
# 设置参数
estimate_prefix <- "Estimate_"
se_prefix <- "Std.Error_"
I2_cutoff <- 25

# 自动识别列名
est_cols <- grep(paste0("^", estimate_prefix), colnames(Meta_merge2), value = TRUE)
se_cols  <- grep(paste0("^", se_prefix), colnames(Meta_merge2), value = TRUE)

# 核心：确保队列名称在 Estimate 和 SE 之间是一一对应的
cohort_names <- gsub(estimate_prefix, "", est_cols)
se_cols <- se_cols[match(cohort_names, gsub(se_prefix, "", se_cols))]

# 打印一下看看对齐没
print(data.frame(Estimate_Col = est_cols, SE_Col = se_cols))

i <- 2 # 验证第一行

# 提取当前行的 yi (效应值) 和 sei (标准误)
yi  <- as.numeric(Meta_merge2[i, est_cols])
sei <- as.numeric(Meta_merge2[i, se_cols])

# 步骤 A: 运行随机效应模型 (REML) 以获取 I2
res_re <- rma.uni(yi = yi, sei = sei, method = "REML")
I2_val <- res_re$I2

# 步骤 B: 根据 I2 阈值选择模型
if (I2_val <= I2_cutoff) {
  method_used <- "FE"
  res <- rma.uni(yi = yi, sei = sei, method = "FE")
} else {
  method_used <- "RE"
  res <- res_re
}

# 打印中间结果
cat("第", i, "行结果：I2 =", I2_val, "，采用模型 =", method_used, "\n")
res$beta
res$pval
####******************图形摘要用**********************####
####UKB####
pdf("Graphical_Abstract1.pdf",width = 6, height = 4)

ggplot(data_norepeat_clean_F2_male, aes(x = DHA_percentage_F0, y = WBTOT_BMD_F2)) +
  # 仅绘制线性拟合线（含置信区间）
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#e31a1c",       
    fill = "#e31a1c",        
    alpha = 0.25,             
    size = 1.4
  ) +
  # 坐标轴与标题
  labs(
    x = expression(paste(bold("Plasma Fatty Acid (%)"))),
    y = expression(bold("BMD (g/cm"^2*")")),
    title = "Association between plasma fatty acid (F0) and BMD (F2)",
    subtitle = ""
  ) +
  # 优雅主题
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 12, color = "black"),
    #plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title = element_text(face = "bold", size = 13, color = "black"),
    #axis.text = element_text(face = "bold", size = 11, color = "black"),
    axis.text = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.line = element_line(color = "black", linewidth = 0.3)
  )
dev.off()
####GNHS:AUC####
# 构建数据框
a <- DXA_wide_age[DXA_wide_age$ID == "NL0014", c("F1age","F2age","F3age","F4age","F1WBTOT_BMD","F2WBTOT_BMD","F3WBTOT_BMD","F4WBTOT_BMD")]
ages <- unlist(a[1, c("F1age","F2age","F3age","F4age")])
values <- unlist(a[1, c("F1WBTOT_BMD","F2WBTOT_BMD","F3WBTOT_BMD","F4WBTOT_BMD")])

df <- data.frame(Age = ages, BMD = values)

auc_value <- trapz(df$Age, df$BMD)

# 先对年龄取整
df$Age_floor <- floor(df$Age)

pdf("Graphical_Abstract2.pdf",width = 6, height = 4)
ggplot(df, aes(x = Age, y = BMD)) +
  # 阴影区表示 AUC 面积
  geom_area(fill = "#a6cee3", alpha = 0.5) +
  # 折线连接点
  geom_line(color = "#1f78b4", size = 1.2) +
  geom_point(color = "#e31a1c", size = 3) +
  # 标注 AUC 数值
  # annotate(
  #   "text",
  #   x = mean(df$Age),
  #   y = min(df$BMD) + 0.08,
  #   label = paste0("AUC = ", round(auc_value, 3)),
  #   color = "black", size = 5, fontface = "bold"
  # ) +
  # 坐标轴和主题美化
  labs(
    x = "Age (years)",
    y = "BMD (g/cm²)",
    title = "Area Under the BMD-Age Curve",
    subtitle = ""
  ) +
  # 只显示3个刻度，并向下取整
  scale_x_continuous(
    breaks = df$Age,        # 原始三个年龄点
    labels = floor(df$Age)  # 向下取整后的标签
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "black"),
    plot.subtitle = element_text(size = 13, color = "gray40"),
    axis.title = element_text(face = "bold", size = 13),
    #axis.text = element_text(size = 11, color = "black"),
    axis.text = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.3)
  )
dev.off()
####GNHS：AUC&FA####
pdf("Graphical_Abstract3.pdf",width = 6, height = 4)
ggplot(data_analyse5_female, aes(x = MeanRBC20_5_Z, y =WBTOT_BMD_AUC)) +
  # 仅绘制线性拟合线（含置信区间）
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#1f78b4",        
    fill = "#1f78b4",        
    alpha = 0.25,             
    size = 1.4
  ) +
  # 坐标轴与标题
  labs(
    x = "Plasma (%)/Serum (µmol/L)/Erythrocyte (%) Fatty Acids",
    y = "BMD AUC (g·years/cm²)",
    title = "Associations between Fatty Acids and BMD AUC (GLM)",
    subtitle = ""
  ) +
  # 优雅主题
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "black"),
    #plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title = element_text(face = "bold", size = 13, color = "black"),
    #axis.text = element_text(face = "bold", size = 11, color = "black"),
    axis.text = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.line = element_line(color = "black", linewidth = 0.3)
  )
dev.off()
####GNHS：重复测量####
x <- "MeanRBC20_5_Z"
y <- "WBTOT_BMD"
covariates <- Covariates_female_lmer
data <- data_analyse6_female
formula <- as.formula(paste(y, "~", x, "+", paste(covariates, collapse = "+"), "+ (1|ID)"))
lme_model <- lmer(formula, data = data)
summary(lme_model)
# 提取模型预测值
pred <- ggeffects::ggpredict(lme_model, terms = x)

pdf("Graphical_Abstract4.pdf",width = 6, height = 4)
ggplot(pred, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              fill = "#A9DFBF", alpha = 0.4) +      # 浅绿色阴影
  geom_line(color = "#1E8449", linewidth = 1.2) + # 深绿色主趋势线
  theme_minimal(base_size = 13) +
  theme(
    #axis.text = element_text(face = "bold", size = 11, color = "black"),
    axis.text = element_blank(),
    axis.title = element_text(face = "bold", size = 13, color = "black"),
    plot.title = element_markdown(face = "bold", color = "black", size = 13),
    plot.subtitle = element_markdown(face = "bold", color = "black", size = 11),
    axis.line = element_line(color = "black", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = "Effects of Fatty Acids on BMD (LMM)",
    x = "Plasma (%)/Serum (µmol/L)/Erythrocyte (%) Fatty Acids",
    y = "BMD (g/cm²)",
    subtitle = "Fixed effects &/ Random effects (1|ID)"
  )
dev.off()
####摘要数值####
#***********摘要数值
# 三个队列的女性,n6PUFA,n6/n3
Var <- c("Predictor","Outcome","Gender","Model","Meta_Est","Meta_LCL","Meta_UCL","Meta_p","I2","Model_used")
b <- rbind(rbind(Meta_GLM[Meta_GLM$Meta_p < 0.05,Var],Meta_LMM[Meta_LMM$Meta_p < 0.05,Var],Meta_LMM_TH[Meta_LMM_TH$Meta_p < 0.05,Var],Meta_LMM_LS_female[Meta_LMM_LS_female$Meta_p < 0.05,Var]), 
           rbind(Meta_LMM_LS_male[Meta_LMM_LS_male$Meta_p < 0.05,Var],Meta_LMM_WB_male[Meta_LMM_WB_male$Meta_p < 0.05, Var],Meta_LMM_FN_male[Meta_LMM_FN_male$Meta_p < 0.05, Var]))
unique(b$Predictor)
# 女性的SFA
# b3 <- b[b$Predictor == "SFA" & b$Gender == "Female",]
# round(max(b3$Meta_Est),3)
# round(min(b3$Meta_Est),3)
# round(max(b3$Meta_p),3)
# round(min(b3$Meta_p),3)

b1 <- b[b$Predictor %in% c("n6PUFA","n6PUFA/n3PUFA"),]
round(max(b1$Meta_Est),3)
round(min(b1$Meta_Est),3)
round(max(b1$Meta_p),3)
round(min(b1$Meta_p),3)
# 两个队列的男性n3PUFA
b2 <- b[b$Predictor %in% c("n3PUFA"),]
#**********UKB和ALSPAC的女性SFA
c1 <- GLM_Lmer_UKB[GLM_Lmer_UKB$Gender == "Female" & GLM_Lmer_UKB$P_value < 0.05 & GLM_Lmer_UKB$Predictor == "SFA",]
c2 <- ALSPAC_results[ALSPAC_results$P_value < 0.05 & ALSPAC_results$Predictor == "SFA" & ALSPAC_results$Model == "LMM-Fatty acid (FOM1) & BMD (FOM1−4)",]
c2 <- dplyr::select(c2,-"Level")
c12 <- rbind(c1,c2)
round(max(c12$Estimate),3)
round(min(c12$Estimate),3)
round(max(c12$P_value),3)
round(min(c12$P_value),3)
#**********GNHS女性个别饱和脂肪酸
d1 <- GLM_Lmer_plotB[GLM_Lmer_plotB$P_value < 0.05,]
unique(d1$Model)
d2 <- d1[d1$Predictor %in% c("C22:0", "C20:0") & d1$Gender == "Female" & d1$Model == "LMM-Fatty acid (F0) & BMD (F1-4)",]
round(max(d2$Estimate),3)
round(min(d2$Estimate),3)
round(max(d2$P_value),3)
round(min(d2$P_value),3)
#**********GNHS C16:1 n-7
d3 <- d1[d1$Predictor == "C16:1 n-7",]
d4 <- GLM_Lmer_plot2[GLM_Lmer_plot2$P_value < 0.05 & GLM_Lmer_plot2$Predictor == "C16:1 n-7",]
d34 <- rbind(d3, d4)
round(max(d34$Estimate),3)
round(min(d34$Estimate),3)
round(max(d34$P_value),3)
round(min(d34$P_value),3)