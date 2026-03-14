library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(openxlsx)
library(janitor)
library(readr)
library(haven)
library(Hmisc)
# set working directory
setwd("~/Desktop/alspac")
dataalspac <- read_sav("~/Desktop/alspac/Li_B4535_22Oct25.sav")
dataalspac_sum<-dataalspac
#*************maternal fatty acids 处理**************#
## 1. 提取每列标签
labels <- sapply(dataalspac_sum, function(x) attr(x, "label"))

## 2. 定义要匹配的 label 关键句 和 对应前缀
patterns <- c(
  "Ratio of 22:6 docosahexaenoic acid to total fatty acids",      # DHA
  "Ratio of omega-3 fatty acids to total fatty acids",            # n-3
  "Ratio of 18:2 linoleic acid to total fatty acids",             # LA
  "Ratio of omega-6 fatty acids to total fatty acids",            # n-6
  "Ratio of polyunsaturated fatty acids to total fatty acids",    # PUFA
  "Ratio of monounsaturated fatty acids to total fatty acids",    # MUFA
  "Ratio of saturated fatty acids to total fatty acids"           # SFA
)

prefixes <- c(
  "MDHA",   # DHA
  "MN3",    # omega-3
  "MLA",    # 18:2 linoleic
  "MN6",    # omega-6
  "MPUFA",  # PUFA
  "MMUFA",  # MUFA
  "MSFA"    # SFA
)

## 3. 循环每一类 ratio，生成新列 + 均值列
for (i in seq_along(patterns)) {
  pat <- patterns[i]
  pre <- prefixes[i]
  
  # 同时满足：label 包含该句子 + 含 FOM
  idx <- grepl(pat, labels, ignore.case = TRUE) &
    grepl("FOM", labels, ignore.case = TRUE)
  
  # 如果这一类一个变量都没有，就跳过
  if (!any(idx)) next
  
  # 原变量名
  vars <- names(labels)[idx]
  
  # 从标签中提取 FOM 编号（假设形如 "FOM1"、"FOM 2" 等）
  fom_num <- sub(".*FOM\\s*([0-9]+).*", "\\1", labels[idx])
  
  # 新列名，如 MDHAFOM1, MN3FOM2, ...
  new_names <- paste0(pre, "FOM", fom_num)
  
  # 复制成新列（不改原始列）
  for (j in seq_along(vars)) {
    dataalspac_sum[[ new_names[j] ]] <- dataalspac_sum[[ vars[j] ]]
  }
  
  # 计算该类指标在各 FOM 时点的均值
  mean_name <- paste0(pre, "mean")
  dataalspac_sum[[ mean_name ]] <- rowMeans(dataalspac_sum[, new_names], 
                                            na.rm = FALSE)
}

## 可以检查看看新增了哪些列：
names(dataalspac_sum)[grepl("MDHA|MN3|MLA|MN6|MPUFA|MMUFA|MSFA", names(dataalspac_sum))]



#***************BMD变量处理**********#
## 1. 提取每列标签
labels <- sapply(dataalspac_sum, function(x) attr(x, "label"))

## 2. 定义 BMD 模式和前缀
bmd_patterns <- c(
  "Total BMD",      # 总体 BMD（非髋部）
  "Spine BMD",      # 脊柱 BMD
  "Hip Total BMD",  # 全髋 BMD
  "Hip Neck BMD"    # 髋颈 BMD
)

bmd_prefixes <- c(
  "MtotalbodyBMD",
  "MSpineBMD",
  "MtotalhipBMD",
  "MHipNeckBMD"
)

## 仅保留 Age
extra_patterns <- c(
  "Age at attendance"   # 年龄
)

extra_prefixes <- c(
  "Mage"
)

## 合并（此时只有 BMD + Age）
all_patterns  <- c(bmd_patterns, extra_patterns)
all_prefixes  <- c(bmd_prefixes, extra_prefixes)

## 3. 循环所有类型，按 FOM1–4 生成新列
for (i in seq_along(all_patterns)) {
  pat <- all_patterns[i]
  pre <- all_prefixes[i]
  
  idx <- grepl(pat, labels, ignore.case = TRUE) &
    grepl("FOM\\s*[1-4]", labels, ignore.case = TRUE)
  
  if (!any(idx)) next
  
  vars <- names(labels)[idx]
  fom_num <- sub(".*FOM\\s*([1-4]).*", "\\1", labels[idx])
  new_names <- paste0(pre, "FOM", fom_num)
  
  for (j in seq_along(vars)) {
    dataalspac_sum[[ new_names[j] ]] <- dataalspac_sum[[ vars[j] ]]
  }
}

## 检查生成的列：只应剩下 BMD 和 Age
names(dataalspac_sum)[grepl(
  "MtotalbodyBMD|MSpineBMD|MtotalhipBMD|MHipNeckBMD|MageFOM",
  names(dataalspac_sum)
)]

#************其他变量处理***************#

## 0. 先有这一句（你已经有了）-----------------------
labels <- sapply(dataalspac_sum, function(x) attr(x, "label"))

## 1️⃣ Fat mass: 生成 MtotalfatmassFOM1–4 ----------------
idx_fat <- grepl("Total Fat Mass \\(g\\)", labels, ignore.case = TRUE) &
  grepl("FOM[1-4]", labels, ignore.case = TRUE)

if (any(idx_fat)) {
  vars_fat <- names(labels)[idx_fat]              # 原变量名
  lab_fat  <- labels[idx_fat]                     # 对应标签
  
  # 从标签里抓出 FOM 编号（支持 FOM1 / FOM 1）
  fom_num_fat <- sub(".*FOM\\s*([1-4]).*", "\\1", lab_fat)
  
  for (j in seq_along(vars_fat)) {
    new_name <- paste0("MtotalfatmassFOM", fom_num_fat[j])
    dataalspac_sum[[new_name]] <- dataalspac_sum[[vars_fat[j]]]
  }
}

## 2️⃣ Weight: 生成 MweightFOM1–4 ------------------------
idx_wt <- grepl("Weight \\(kg\\)", labels, ignore.case = TRUE) &
  grepl("FOM[1-4]", labels, ignore.case = TRUE)

if (any(idx_wt)) {
  vars_wt <- names(labels)[idx_wt]
  lab_wt  <- labels[idx_wt]
  
  fom_num_wt <- sub(".*FOM\\s*([1-4]).*", "\\1", lab_wt)
  
  for (j in seq_along(vars_wt)) {
    new_name <- paste0("MweightFOM", fom_num_wt[j])
    dataalspac_sum[[new_name]] <- dataalspac_sum[[vars_wt[j]]]
  }
}

## 3️⃣ Height: 生成 MheightFOM1–4（排除 sitting height）--
idx_ht <- grepl("Height \\(cm\\)", labels, ignore.case = TRUE) &
  grepl("FOM[1-4]", labels, ignore.case = TRUE) &
  !grepl("sitting", labels, ignore.case = TRUE)

if (any(idx_ht)) {
  vars_ht <- names(labels)[idx_ht]
  lab_ht  <- labels[idx_ht]
  
  fom_num_ht <- sub(".*FOM\\s*([1-4]).*", "\\1", lab_ht)
  
  for (j in seq_along(vars_ht)) {
    new_name <- paste0("MheightFOM", fom_num_ht[j])
    dataalspac_sum[[new_name]] <- dataalspac_sum[[vars_ht[j]]]
  }
}

## 4️⃣ 验证一下到底生成了哪些新列 -----------------------
names(dataalspac_sum)[grepl(
  "MtotalfatmassFOM|MweightFOM|MheightFOM",
  names(dataalspac_sum)
)]


#**************fatty acids covatiates definition************#
dataalspac_sum$MvitaminD<-dataalspac_sum$fm1sa202
dataalspac_sum$Mcalcium<-dataalspac_sum$fm1sa203
dataalspac_sum$Mbisphosphonate<-dataalspac_sum$fm1sa205
## 提取所有变量的 label
labels <- sapply(dataalspac_sum, function(x) attr(x, "label"))

## 要识别的标签关键词
patterns <- c(
  "Currently taking oral contraceptives",
  "Currently using contraceptive injection",
  "Had a period/menstrual bleeding in the previous 12 month"
)

## 新变量名前缀
new_prefix <- c(
  "Moralcontraceptives",
  "Mcontraceptiveinjection",
  "Mmenopause"
)

## 循环三类变量
for (i in seq_along(patterns)) {
  
  pat <- patterns[i]
  pre <- new_prefix[i]
  
  ## 找出 label 中含该信息 AND 含 FOM1–4 的变量
  idx <- grepl(pat, labels, ignore.case = TRUE) &
    grepl("FOM\\s*[1-4]", labels, ignore.case = TRUE)
  
  vars <- names(labels)[idx]
  
  if (length(vars) == 0) next
  
  ## 从标签中提取 FOM 编号
  fom_num <- sub(".*FOM\\s*([1-4]).*", "\\1", labels[idx])
  
  ## 新列名称
  new_names <- paste0(pre, "FOM", fom_num)
  
  ## 创建新变量
  for (j in seq_along(vars)) {
    dataalspac_sum[[ new_names[j] ]] <- dataalspac_sum[[ vars[j] ]]
  }
}

## 查看新变量
names(dataalspac_sum)[grepl("Moralcontraceptives|Mcontraceptiveinjection|Mmenopause",
                            names(dataalspac_sum))]

#**************处理HRT*************#
## 1. 提取每列的 label
labels <- sapply(dataalspac_sum, function(x) attr(x, "label"))

## 2. 定义 HRT 合并函数
collapse_hrt <- function(df, vars) {
  apply(df[, vars, drop = FALSE], 1, function(x) {
    non_na <- x[!is.na(x)]
    
    if (length(non_na) == 0) return(NA)      # 全 NA → NA
    if (any(non_na == 1)) return(1)          # 只要出现 1 → 1
    if (any(non_na == 2)) return(2)          # 否则如果出现 2 → 2
    
    return(NA)
  })
}

## 3. FOM1：只有一个变量
idx_FOM1 <- grepl("Currently taking hormone replacement therapy: FOM1",
                  labels, ignore.case = TRUE)
var_FOM1 <- names(labels)[idx_FOM1]

if (length(var_FOM1) == 1) {
  dataalspac_sum$MCurrentlyHRTFOM1 <- dataalspac_sum[[var_FOM1]]
}

## 4. FOM2：tablets / patches / creams 合并
idx_FOM2 <- grepl("Hormone Replacement Therapy", labels, ignore.case = TRUE) &
  grepl("FOM2", labels, ignore.case = TRUE)
vars_FOM2 <- names(labels)[idx_FOM2]

if (length(vars_FOM2) > 0) {
  dataalspac_sum$MCurrentlyHRTFOM2 <- collapse_hrt(dataalspac_sum, vars_FOM2)
}

## 5. FOM3：tablets / patches / cream 合并
idx_FOM3 <- grepl("Hormone Replacement Therapy", labels, ignore.case = TRUE) &
  grepl("FOM3", labels, ignore.case = TRUE)
vars_FOM3 <- names(labels)[idx_FOM3]

if (length(vars_FOM3) > 0) {
  dataalspac_sum$MCurrentlyHRTFOM3 <- collapse_hrt(dataalspac_sum, vars_FOM3)
}

## 6. FOM4：tablets / patches / cream 合并
idx_FOM4 <- grepl("Hormone Replacement Therapy", labels, ignore.case = TRUE) &
  grepl("FOM4", labels, ignore.case = TRUE)
vars_FOM4 <- names(labels)[idx_FOM4]

if (length(vars_FOM4) > 0) {
  dataalspac_sum$MCurrentlyHRTFOM4 <- collapse_hrt(dataalspac_sum, vars_FOM4)
}

## 7. 查看新变量
names(dataalspac_sum)[grepl("MCurrentlyHRTFOM", names(dataalspac_sum))]

#*****************GLM/LMM************#
## 0. 加载 R 包
library(pracma)
library(dplyr)
library(tidyr)
library(broom)
library(broom.mixed)
library(ggplot2)
library(tibble)
library(lmerTest)
library(grid)

## 0. 脂肪酸变量 + n6PUFA/n3PUFA + 标准化（不覆盖原值）

## GLM 用：脂肪酸均值（原始变量名）
fa_means <- c(
  "MDHAmean",   # C22:6 n-3
  "MLAmean",    # C18:2 n-6
  "MN3mean",    # n3PUFA
  "MN6mean",    # n6PUFA
  "MSFAmean",   # SFA
  "MMUFAmean",  # MUFA
  "MPUFAmean"   # PUFA
)

## LMM 用：FOM1 脂肪酸（原始变量名）
fa_fom1 <- c(
  "MDHAFOM1",
  "MLAFOM1",
  "MN3FOM1",
  "MN6FOM1",
  "MSFAFOM1",
  "MMUFAFOM1",
  "MPUFAFOM1"
)

## 1️⃣ 计算 n6PUFA/n3PUFA（FOM1、FOM2 先求比值再求均值）
dataalspac_sum <- dataalspac_sum %>%
  mutate(
    ## 每个时间点的比值（单独计算，不改）
    Mn6n3_F1 = ifelse(!is.na(MN6FOM1) & !is.na(MN3FOM1) & MN3FOM1 != 0,
                      MN6FOM1 / MN3FOM1, NA_real_),
    Mn6n3_F2 = ifelse(!is.na(MN6FOM2) & !is.na(MN3FOM2) & MN3FOM2 != 0,
                      MN6FOM2 / MN3FOM2, NA_real_),
    
    ## ⭐ 两次都存在，才计算均值，否则 NA
    Mn6n3mean = ifelse(
      !is.na(Mn6n3_F1) & !is.na(Mn6n3_F2),
      (Mn6n3_F1 + Mn6n3_F2) / 2,
      NA_real_
    ),
    
    ## LMM 用 FOM1（不变）
    Mn6n3FOM1 = Mn6n3_F1
  )

## 把 n6/n3 加入脂肪酸列表（原始名）
fa_means_all <- c(fa_means, "Mn6n3mean")
fa_fom1_all  <- c(fa_fom1,  "Mn6n3FOM1")

## 2️⃣ 生成 z-score 版本（不覆盖原始变量）
fa_all <- c(fa_means_all, fa_fom1_all)

dataalspac_sum <- dataalspac_sum %>%
  mutate(
    across(
      all_of(fa_all),
      ~ as.numeric(scale(.)),
      .names = "{.col}_z"   # 例如 MDHAmean_z
    )
  )

## 对应的 z 变量名称向量
fa_means_all_z <- paste0(fa_means_all, "_z")
fa_fom1_all_z  <- paste0(fa_fom1_all,  "_z")

## y 轴标签顺序
fa_labels <- c(
  "C22:6 n-3",
  "C18:2 n-6",
  "n3PUFA",
  "n6PUFA",
  "SFA",
  "MUFA",
  "PUFA",
  "n6PUFA/n3PUFA"
)

## 2. 计算 4 次 BMD 的 AUC（GLM 用）

bone_prefix <- c("MtotalbodyBMD","MSpineBMD","MtotalhipBMD","MHipNeckBMD")
age_vars    <- paste0("MageFOM", 1:4)

for (pre in bone_prefix) {
  bmd_vars <- paste0(pre,"FOM",1:4)
  auc_name <- paste0(pre,"AUC")
  
  dataalspac_sum[[auc_name]] <- sapply(seq_len(nrow(dataalspac_sum)), function(i){
    ages   <- as.numeric(dataalspac_sum[i, age_vars])
    values <- as.numeric(dataalspac_sum[i, bmd_vars])
    if (any(is.na(ages)) || any(is.na(values))) return(NA_real_)
    trapz(ages, values)
  })
}

## 3. GLM：BMD AUC ~ 标准化脂肪酸均值_z + 协变量
##    fat mass 用 4 次均值

dataalspac_sum <- dataalspac_sum %>%
  mutate(
    Mtotalfatmass_mean = {
      tmp <- rowMeans(dplyr::select(., starts_with("MtotalfatmassFOM")), na.rm = TRUE)
      ifelse(is.nan(tmp), NA_real_, tmp)
    }
  )

bmd_aucs   <- paste0(bone_prefix, "AUC")
bone_names <- c("Total body BMD","Spine BMD","Total hip BMD","Hip neck BMD")

global_confounders <- c("MvitaminD","Mcalcium","Mbisphosphonate")

glm_results <- list()

for (i in seq_along(fa_means_all)) {
  fa_orig <- fa_means_all[i]      # 原始名，如 MDHAmean
  fa_z    <- fa_means_all_z[i]    # 标准化变量名，如 MDHAmean_z
  
  for (k in seq_along(bmd_aucs)) {
    outcome  <- bmd_aucs[k]
    bone_lab <- bone_names[k]
    
    timevary_conf <- c(
      paste0("MoralcontraceptivesFOM",1:4),
      paste0("McontraceptiveinjectionFOM",1:4),
      paste0("MmenopauseFOM",1:4),
      paste0("MCurrentlyHRTFOM",1:4)
    )
    
    all_conf <- c(timevary_conf, "MageFOM1", "Mtotalfatmass_mean", global_confounders)
    all_conf <- all_conf[all_conf %in% names(dataalspac_sum)]
    if (! outcome %in% names(dataalspac_sum)) next
    
    rhs  <- paste(c(fa_z, all_conf), collapse = " + ")
    form <- as.formula(paste0(outcome, " ~ ", rhs))
    
    fit <- glm(form, data = dataalspac_sum)
    
    tmp <- tidy(fit, conf.int = TRUE) %>%
      filter(term == fa_z) %>%        # 只保留该脂肪酸_z 那一行
      mutate(
        FA        = fa_orig,          # 记录原始名，方便后面映射标签
        bone_site = bone_lab,
        model_type = "AUC"
      )
    
    glm_results[[paste(fa_orig, outcome, sep = "_")]] <- tmp
  }
}

glm_res <- bind_rows(glm_results) %>%
  rename(
    beta    = estimate,
    ci_low  = conf.low,
    ci_high = conf.high,
    p_value = p.value
  ) %>%
  mutate(
    fa_name = case_when(
      FA == "MDHAmean"   ~ "C22:6 n-3",
      FA == "MLAmean"    ~ "C18:2 n-6",
      FA == "MN3mean"    ~ "n3PUFA",
      FA == "MN6mean"    ~ "n6PUFA",
      FA == "MSFAmean"   ~ "SFA",
      FA == "MMUFAmean"  ~ "MUFA",
      FA == "MPUFAmean"  ~ "PUFA",
      FA == "Mn6n3mean"  ~ "n6PUFA/n3PUFA",
      TRUE               ~ FA
    )
  )

## 4. LMM：BMD ~ 标准化 FOM1 脂肪酸_z + 时变协变量
##    fat mass 用每次测量

bone_map <- tibble(Bone = bone_prefix, bone_site = bone_names)

fa_map_lmm <- tibble(
  FA = fa_fom1_all,
  fa_name = c(
    "C22:6 n-3",
    "C18:2 n-6",
    "n3PUFA",
    "n6PUFA",
    "SFA",
    "MUFA",
    "PUFA",
    "n6PUFA/n3PUFA"
  )
)

## 宽→长
tv_vars <- c(
  "MtotalbodyBMD","MSpineBMD","MtotalhipBMD","MHipNeckBMD",
  "Moralcontraceptives","Mcontraceptiveinjection",
  "Mmenopause","MCurrentlyHRT",
  "Mtotalfatmass","Mage"
)

tv_regex <- paste0("^(", paste(tv_vars, collapse="|"), ")FOM[1-4]$")

long_data <- dataalspac_sum %>%
  mutate(ID = factor(cidB4535)) %>%
  pivot_longer(
    cols = matches(tv_regex),
    names_to = c(".value","FOM"),
    names_pattern = "(.*)FOM([1-4])"
  ) %>%
  mutate(FOM = as.numeric(FOM))

timevary_conf <- c("Moralcontraceptives","Mcontraceptiveinjection",
                   "Mmenopause","MCurrentlyHRT","Mtotalfatmass","Mage")

lmm_results <- list()

for (i in seq_along(fa_fom1_all)) {
  fa_orig <- fa_fom1_all[i]      # 原始名，例如 MDHAFOM1
  fa_z    <- fa_fom1_all_z[i]    # 标准化变量名，例如 MDHAFOM1_z
  
  for (pre in bone_prefix) {
    
    rhs  <- paste(c(fa_z, timevary_conf, global_confounders, "FOM"), collapse = " + ")
    form <- as.formula(paste0(pre, " ~ ", rhs, " + (1|ID)"))
    
    fit <- lmer(form, data = long_data, REML = FALSE)
    
    tmp <- tidy(fit, effects = "fixed", conf.int = TRUE) %>%
      filter(term == fa_z) %>%      # 只保留该脂肪酸_z
      mutate(
        FA   = fa_orig,
        Bone = pre
      )
    
    lmm_results[[paste(fa_orig, pre, sep = "_")]] <- tmp
  }
}

lmm_res <- bind_rows(lmm_results) %>%
  left_join(fa_map_lmm, by = "FA") %>%
  left_join(bone_map,   by = c("Bone")) %>%
  mutate(
    model_type = "LMM",
    beta       = estimate,
    ci_low     = conf.low,
    ci_high    = conf.high,
    p_value    = p.value
  )

## 5. 合并 GLM + LMM 结果（供后面 res_all_plot + 作图使用）

res_all <- bind_rows(
  glm_res %>% select(model_type, fa_name, bone_site, beta, ci_low, ci_high, p_value),
  lmm_res %>% select(model_type, fa_name, bone_site, beta, ci_low, ci_high, p_value)
) %>%
  mutate(
    fa_name  = factor(fa_name, levels = rev(fa_labels))
  )

write.xlsx(res_all,"alspacfattybone.xlsx")

## 5. 合并 GLM + LMM → 用于双坐标森林图

res_all <- bind_rows(
  glm_res %>% select(model_type, fa_name, bone_site, beta, ci_low, ci_high, p_value),
  lmm_res %>% select(model_type, fa_name, bone_site, beta, ci_low, ci_high, p_value)
)

res_all <- res_all %>%
  mutate(
    fa_name  = factor(fa_name, levels = rev(fa_labels)),
    bone_site = factor(
      bone_site,
      levels = c("Total body BMD","Spine BMD","Total hip BMD","Hip neck BMD")
    )
  )

## 6. 双坐标 GLM + LMM 森林图（无箭头，p<0.05 标星）

res_all_plot <- res_all %>%
  mutate(
    Model = dplyr::recode(
      model_type,
      "AUC" = "GLM",
      "LMM" = "LMM"
    ),
    # 修改骨指标名称
    bone_site = dplyr::recode(
      as.character(bone_site),
      "Total body BMD" = "Whole Body BMD",
      "Spine BMD"      = "Spine BMD",
      "Total hip BMD"  = "Total Hip BMD",
      "Hip neck BMD"   = "Femur Neck BMD"
    )
  ) %>%
  mutate(
    bone_site = factor(
      bone_site,
      levels = c("Whole Body BMD",
                 "Spine BMD",
                 "Total Hip BMD",
                 "Femur Neck BMD")
    )
  )

## 拆分 GLM / LMM，确定 x 轴范围
glm_dat <- res_all_plot %>% filter(Model == "GLM")
lmm_dat <- res_all_plot %>% filter(Model == "LMM")

gmin0 <- min(glm_dat$ci_low,  na.rm = TRUE)
gmax0 <- max(glm_dat$ci_high, na.rm = TRUE)
lmin0 <- min(lmm_dat$ci_low,  na.rm = TRUE)
lmax0 <- max(lmm_dat$ci_high, na.rm = TRUE)

g_range   <- gmax0 - gmin0
l_range   <- lmax0 - lmin0

gmin_plot <- gmin0 - 0.1 * g_range
gmax_plot <- gmax0 + 0.1 * g_range
lmin_plot <- lmin0 - 0.1 * l_range
lmax_plot <- lmax0 + 0.1 * l_range

## GLM ↔ LMM 线性映射 + 反函数（给 sec.axis 用）
scale_factor <- (gmax_plot - gmin_plot) / (lmax_plot - lmin_plot)
offset       <- gmin_plot - scale_factor * lmin_plot
inv_lmm      <- function(x) (x - offset) / scale_factor

## 把 LMM 的 β/CI 映射到 GLM 轴上
df_plot <- res_all_plot %>%
  mutate(
    beta_scaled    = if_else(Model == "GLM", beta,
                             offset + scale_factor * beta),
    ci_low_scaled  = if_else(Model == "GLM", ci_low,
                             offset + scale_factor * ci_low),
    ci_high_scaled = if_else(Model == "GLM", ci_high,
                             offset + scale_factor * ci_high)
  )

x_range <- gmax_plot - gmin_plot

## ⭐ 先在 df_plot 里加 y_pos（GLM 在上，LMM 在下）
df_plot <- df_plot %>%
  mutate(
    y_pos = as.numeric(fa_name) + ifelse(Model == "GLM", 0.25, -0.25)  # 0.25 再拉开一点
  )

## ⭐ 星号也用同样的 y_pos
star_data <- df_plot %>%
  mutate(
    star_x_raw = ci_high_scaled + 0.02 * x_range,
    star_x = dplyr::if_else(
      p_value < 0.05,
      pmin(star_x_raw, gmax_plot - 0.01 * x_range),
      NA_real_
    )
  )

col_glm <- "#D55E00"
col_lmm <- "#009E73"

p_final <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4) +
  
  ## CI + 点
  geom_pointrange(
    data = df_plot,
    aes(
      x    = beta_scaled,
      y    = y_pos,
      xmin = ci_low_scaled,
      xmax = ci_high_scaled,
      color = Model
    ),
    linewidth = 0.7,
    size      = 1.2
  ) +
  
  ## 星号（p < 0.05）
  geom_text(
    data = star_data,
    aes(
      x     = star_x,
      y     = y_pos,
      group = Model
    ),
    label    = "★",
    color    = "red",
    size     = 4.5,
    fontface = "bold",
    na.rm    = TRUE
  ) +
  
  facet_wrap(~ bone_site, nrow = 1) +
  
  scale_color_manual(
    name   = "Model",
    values = c("GLM" = col_glm, "LMM" = col_lmm),
    labels = c("GLM(FOM1&FOM2) & BMD AUC(FOM1-4)",
               "LMM(FOM1) & BMD (FOM1-4)")
  ) +
  
  ## y 轴：用整数 1:8 作为“中线”，标签用 fa_labels
  scale_y_continuous(
    breaks = 1:length(fa_labels),
    labels = rev(fa_labels)    # 因为之前 fa_name 用的是 rev(fa_labels) 做 levels
  ) +
  
  ## x 轴：左 GLM，右 LMM
  scale_x_continuous(
    name   = "GLM β (95% CI)",
    limits = c(gmin_plot, gmax_plot),
    sec.axis = sec_axis(
      ~ inv_lmm(.),
      name = "LMM β (95% CI)"
    )
  ) +
  
  labs(title = NULL, subtitle = NULL, y = NULL) +
  theme_bw(base_size = 11) +
  theme(
    text = element_text(face = "bold"),
    
    strip.background   = element_rect(fill = "#ddeeff"),
    strip.text         = element_text(face = "bold", size = 12),
    
    panel.grid.major.y = element_line(linetype = "dotted", linewidth = 0.2),
    panel.grid.minor.y = element_blank(),
    
    panel.spacing.y = unit(1.2, "lines"),
    panel.spacing   = unit(1.5, "lines"),
    
    legend.position    = "bottom",
    legend.key.size    = unit(0.6, "cm"),
    legend.text        = element_text(size = 10, face = "bold"),
    legend.title       = element_text(size = 10, face = "bold"),
    
    axis.text.x.top    = element_text(angle = 45, hjust = 0, size = 10, face = "bold"),
    axis.text.x.bottom = element_text(size = 10, face = "bold"),
    axis.text.y        = element_text(size = 12, face = "bold"),
    
    axis.title.x       = element_text(size = 11, face = "bold"),
    axis.title.y       = element_text(size = 11, face = "bold")
  )

## 先在 R 里看一眼
p_final

## 保存 PDF
ggsave("alspacfattyacidsbone.pdf", p_final, width = 16, height = 6)
#================骨指标描述===================

library(broom)

## 1. 骨部位信息
bone_prefix <- c("MtotalbodyBMD","MSpineBMD","MtotalhipBMD","MHipNeckBMD")
bone_labels <- c("Whole Body BMD","Spine BMD","Total Hip BMD","Femur Neck BMD")

bone_map <- tibble(
  Bone      = bone_prefix,
  bone_site = bone_labels
)

## 2. 长格式 BMD 数据（未过滤极端值）

bmd_vars <- unlist(lapply(bone_prefix, function(pre) paste0(pre, "FOM", 1:4)))

bmd_long_raw <- dataalspac_sum %>%
  select(all_of(bmd_vars)) %>%
  mutate(ID = row_number()) %>%
  pivot_longer(
    cols = all_of(bmd_vars),
    names_to = c("Bone","FOM"),
    names_pattern = "(M[^F]+BMD)FOM([1-4])"
  ) %>%
  mutate(
    BMD = as.numeric(value),
    FOM = factor(FOM, levels = c("1","2","3","4")),
  ) %>%
  select(-value) %>%
  left_join(bone_map, by = "Bone") %>%
  mutate(
    bone_site = factor(bone_site, levels = bone_labels)
  )

## 3. ⭐ 去极端值（按 bone_site × FOM 单独去）
bmd_long <- bmd_long_raw %>%
  group_by(bone_site, FOM) %>%
  mutate(
    Q1  = quantile(BMD, 0.25, na.rm = TRUE),
    Q3  = quantile(BMD, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower = Q1 - 3 * IQR,
    upper = Q3 + 3 * IQR,
    is_outlier = (BMD < lower | BMD > upper)
  ) %>%
  ungroup() %>%
  filter(!is_outlier) %>%       # ⭐ 去掉极端值
  select(-Q1, -Q3, -IQR, -lower, -upper, -is_outlier)

## 4. 计算每次随访的平均年龄（用于横坐标标签）
age_means <- dataalspac_sum %>%
  summarise(across(starts_with("MageFOM"), ~mean(.x, na.rm=TRUE)))

age_vec <- as.numeric(age_means[1, ])
fom_levels <- c("1","2","3","4")
fom_labels_pretty <- paste0("FOM", fom_levels, "\n(", sprintf("%.1f", age_vec), "y)")

## 转换标签
bmd_long <- bmd_long %>%
  mutate(
    FOM_pretty = factor(FOM, labels = fom_labels_pretty)
  )

## 5. 均值 / SD / 样本量
bmd_summary <- bmd_long %>%
  group_by(bone_site, FOM_pretty) %>%
  summarise(
    mean = mean(BMD, na.rm = TRUE),
    sd   = sd(BMD, na.rm = TRUE),
    .groups = "drop"
  )

bmd_n <- bmd_long %>%
  group_by(bone_site, FOM_pretty) %>%
  summarise(
    n = sum(!is.na(BMD)),
    maxBMD = max(BMD, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(label_y = maxBMD * 1.05)

## 7. ⭐ 组合图：小提琴 + 箱线 + 均值 ± SD + n
##      —— 删除趋势 P 值，趋势线改颜色

# 自定义趋势线颜色
trend_col <- "#1f77b4"   # 深蓝色（可换）

# Y 轴刻度
y_breaks_all <- pretty(range(bmd_long$BMD, na.rm = TRUE), n = 8)

p_bmd_combined <- ggplot() +
  geom_violin(
    data = bmd_long,
    aes(x = FOM_pretty, y = BMD),
    fill = "#e6f2ff",
    color = "black",
    alpha = 0.85,
    linewidth = 0.6
  ) +
  geom_boxplot(
    data = bmd_long,
    aes(x = FOM_pretty, y = BMD),
    width = 0.18,
    outlier.shape = NA,
    linewidth = 0.6
  ) +
  geom_line(
    data = bmd_summary,
    aes(x = FOM_pretty, y = mean, group = 1),
    linewidth = 1.0,
    color = trend_col                    # ★ 改成深蓝趋势线
  ) +
  geom_point(
    data = bmd_summary,
    aes(x = FOM_pretty, y = mean),
    size = 2.4,
    color = trend_col                    # ★ 同色点
  ) +
  geom_errorbar(
    data = bmd_summary,
    aes(
      x = FOM_pretty,
      ymin = mean - sd,
      ymax = mean + sd
    ),
    width = 0.10,
    linewidth = 0.8,
    color = trend_col                    # ★ 同色误差线
  )  +
  facet_wrap(~ bone_site, ncol = 2) +
  scale_y_continuous(
    breaks = y_breaks_all,
    name = "BMD (g/cm²)"
  ) +
  labs(
    x = "Follow-up visits (with mean age)",
  ) +
  theme_bw(base_size = 11) +
  theme(
    text = element_text(face = "bold"),
    strip.background = element_rect(fill = "#bcd9ff"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 9, face = "bold"),
    axis.text.y = element_text(size = 9, face = "bold"),
    panel.spacing = unit(1.2, "lines")
  )

p_bmd_combined

ggsave("alspac_BMD_violin_trend_outlier_removed.pdf",
       p_bmd_combined, width = 10, height = 7)


#**************骨指标 AUC统计描述**********#

## 1. 长格式 AUC 数据
bone_labels_ordered <- c("Whole Body BMD",
                         "Spine BMD",
                         "Total Hip BMD",
                         "Femur Neck BMD")

auc_long_raw <- dataalspac_sum %>%
  select(
    MtotalbodyBMDAUC,
    MSpineBMDAUC,
    MtotalhipBMDAUC,
    MHipNeckBMDAUC
  ) %>%
  mutate(ID = row_number()) %>%
  pivot_longer(
    cols = c(MtotalbodyBMDAUC, MSpineBMDAUC, MtotalhipBMDAUC, MHipNeckBMDAUC),
    names_to = "Bone",
    values_to = "AUC"
  ) %>%
  mutate(
    bone_site = dplyr::recode(
      Bone,
      "MtotalbodyBMDAUC" = "Whole Body BMD",
      "MSpineBMDAUC"      = "Spine BMD",
      "MtotalhipBMDAUC"   = "Total Hip BMD",
      "MHipNeckBMDAUC"    = "Femur Neck BMD"
    ),
    bone_site = factor(bone_site, levels = bone_labels_ordered)
  )

## 2. 去极端值（按骨部位：3×IQR）
auc_long <- auc_long_raw %>%
  group_by(bone_site) %>%
  mutate(
    Q1  = quantile(AUC, 0.25, na.rm = TRUE),
    Q3  = quantile(AUC, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower = Q1 - 3 * IQR,
    upper = Q3 + 3 * IQR,
    is_outlier = (AUC < lower | AUC > upper)
  ) %>%
  ungroup() %>%
  filter(!is_outlier) %>%
  select(ID, bone_site, AUC)

## 3. 计算每个部位的样本量（用于标在图上）
auc_n <- auc_long %>%
  group_by(bone_site) %>%
  summarise(
    n = sum(!is.na(AUC)),
    .groups = "drop"
  ) %>%
  mutate(
    x = -Inf,   # 放在左上角
    y = Inf
  )

## 4. AUC 分布图（直方图 + 密度曲线，四个部位一张图）
p_auc_dist <- ggplot(auc_long, aes(x = AUC)) +
  geom_histogram(
    aes(y = ..density..),
    bins = 30,
    fill = "#ddeeff",
    color = "grey40",
    alpha = 0.9
  ) +
  geom_density(
    linewidth = 0.9
  ) +
  facet_wrap(~ bone_site, ncol = 2, scales = "free") +
  labs(
    x = "BMD AUC (unit × years)",   # 按你的实际单位可以再改
    y = "Density"
  ) +
  theme_bw(base_size = 12) +
  theme(
    text            = element_text(face = "bold"),
    strip.background= element_rect(fill = "#bcd9ff"),
    strip.text      = element_text(size = 11, face = "bold"),
    axis.text       = element_text(size = 10, face = "bold"),
    axis.title      = element_text(size = 11, face = "bold"),
    plot.title      = element_text(size = 12, face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

p_auc_dist

## 保存为 PDF
ggsave("alspac_BMDAUC_distribution.pdf",
       p_auc_dist, width = 8, height = 6)

#*************脂肪酸描述**************#

## 1. 变量名与标签
fa_fom1 <- c("MDHAFOM1","MLAFOM1","MN3FOM1","MN6FOM1",
             "MSFAFOM1","MMUFAFOM1","MPUFAFOM1","Mn6n3_F1")

fa_fom2 <- c("MDHAFOM2","MLAFOM2","MN3FOM2","MN6FOM2",
             "MSFAFOM2","MMUFAFOM2","MPUFAFOM2","Mn6n3_F2")

fa_mean <- c("MDHAmean","MLAmean","MN3mean","MN6mean",
             "MSFAmean","MMUFAmean","MPUFAmean","Mn6n3mean")

fa_labels <- c(
  "C22:6 n-3",
  "C18:2 n-6",
  "n3PUFA",
  "n6PUFA",
  "SFA",
  "MUFA",
  "PUFA",
  "n6PUFA/n3PUFA"
)

## 2. FOM1/FOM2 → 长格式
fa_long_12 <- dataalspac_sum %>%
  select(all_of(c(fa_fom1, fa_fom2))) %>%
  mutate(ID = row_number()) %>%
  pivot_longer(
    cols = -ID,
    names_to  = "var",
    values_to = "value"
  ) %>%
  mutate(
    FOM = case_when(
      grepl("FOM1$|_F1$", var) ~ "FOM1",
      grepl("FOM2$|_F2$", var) ~ "FOM2",
      TRUE ~ NA_character_
    ),
    fa_name = case_when(
      grepl("^MDHA", var) ~ "C22:6 n-3",
      grepl("^MLA",  var) ~ "C18:2 n-6",
      grepl("^MN3",  var) ~ "n3PUFA",
      grepl("^MN6",  var) ~ "n6PUFA",
      grepl("^MSFA", var) ~ "SFA",
      grepl("^MMUFA",var) ~ "MUFA",
      grepl("^MPUFA",var) ~ "PUFA",
      grepl("^Mn6n3",var,ignore.case=TRUE) ~ "n6PUFA/n3PUFA",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(FOM), !is.na(fa_name), !is.na(value))

## 3. “两个时点均值” → 长格式
fa_long_mean <- dataalspac_sum %>%
  select(all_of(fa_mean)) %>%
  mutate(ID = row_number()) %>%
  pivot_longer(
    cols = -ID,
    names_to  = "var",
    values_to = "value"
  ) %>%
  mutate(
    FOM = "Mean",
    fa_name = case_when(
      var == "MDHAmean"   ~ "C22:6 n-3",
      var == "MLAmean"    ~ "C18:2 n-6",
      var == "MN3mean"    ~ "n3PUFA",
      var == "MN6mean"    ~ "n6PUFA",
      var == "MSFAmean"   ~ "SFA",
      var == "MMUFAmean"  ~ "MUFA",
      var == "MPUFAmean"  ~ "PUFA",
      var == "Mn6n3mean"  ~ "n6PUFA/n3PUFA",
      TRUE                ~ NA_character_
    )
  ) %>%
  filter(!is.na(fa_name), !is.na(value))

## 4. 合并三个“时间点”：FOM1 / FOM2 / Mean
fa_long_all <- bind_rows(fa_long_12, fa_long_mean) %>%
  mutate(
    fa_name = factor(fa_name, levels = fa_labels),
    FOM     = factor(FOM, levels = c("FOM1","FOM2","Mean"))
  )

## 5. 计算 n、均值、median、顶部位置
fa_stats <- fa_long_all %>%
  group_by(fa_name, FOM) %>%
  summarise(
    n      = sum(!is.na(value)),
    mean   = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    ymax   = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(label_y = ymax * 1.05)

## 6. 小提琴图（三把）：FOM1 / FOM2 / Mean
p_fa_violin3 <- ggplot(fa_long_all, aes(x = FOM, y = value, fill = FOM)) +
  
  geom_violin(alpha = 0.55, trim = TRUE, linewidth = 0.3) +
  
  ## 中位数（黑点）
  geom_point(
    data = fa_stats,
    aes(x = FOM, y = median),
    color = "black", size = 2.0, shape = 16
  ) +
  
  ## 均值（红菱形）
  geom_point(
    data = fa_stats,
    aes(x = FOM, y = mean),
    color = "red", size = 2.6, shape = 18
  ) +
  
  facet_wrap(~ fa_name, scales = "free_y", ncol = 4) +
  
  scale_fill_manual(
    values = c(
      "FOM1" = "#1f77b4",
      "FOM2" = "#D55E00",
      "Mean" = "#009E73"
    )
  ) +
  
  labs(
    x = NULL,
    y = "Fatty acids level (%)"
  ) +
  
  theme_bw(base_size = 11) +
  theme(
    text            = element_text(face = "bold"),
    strip.background= element_rect(fill = "#ddeeff"),
    strip.text      = element_text(size = 11),
    axis.text       = element_text(size = 9),
    axis.title      = element_text(size = 10),
    legend.position = "none",
    panel.spacing   = unit(1.2, "lines")
  )

p_fa_violin3

ggsave("fa_FOM1_FOM2_Mean_violin.pdf",
       p_fa_violin3, width = 11, height = 6)


#****************基本表**************#
## 1. 定义变量

# 连续变量前缀（有 FOM1–4）
continuous_vars <- c("Mage", "Mheight", "Mweight", "Mtotalfatmass")

# 随时间变化的二分类变量（有 FOM1–4，值 1/2）
binary_timevary_vars <- c(
  "MCurrentlyHRT",
  "Mcontraceptiveinjection",
  "Moralcontraceptives",
  "Mmenopause"
)

# 只在基线的二分类变量（无 FOM 后缀，值 1/2）
baseline_bin_vars <- c("MvitaminD", "Mcalcium", "Mbisphosphonate")

foms <- paste0("FOM", 1:4)

## 2. 复制数据并转换单位
##    total fat mass: g → kg

dat <- dataalspac_sum %>%
  mutate(
    MtotalfatmassFOM1 = MtotalfatmassFOM1 / 1000,
    MtotalfatmassFOM2 = MtotalfatmassFOM2 / 1000,
    MtotalfatmassFOM3 = MtotalfatmassFOM3 / 1000,
    MtotalfatmassFOM4 = MtotalfatmassFOM4 / 1000
  )

## 3. 连续变量：长格式

continuous_long <- lapply(continuous_vars, function(v) {
  df <- dat %>%
    select(matches(paste0("^", v, "FOM[1-4]$")))
  
  if (ncol(df) == 0) return(NULL)
  
  df %>%
    rename_with(~ paste0("FOM", seq_along(.)), everything()) %>%
    mutate(Variable = v) %>%
    pivot_longer(
      cols      = starts_with("FOM"),
      names_to  = "FOM",
      values_to = "Value"
    )
}) %>% bind_rows()

## 4. 二分类：随时间变化的变量（有 FOM1–4，1/2→yes/no）

binary_long_timevary <- lapply(binary_timevary_vars, function(v) {
  df <- dat %>%
    select(matches(paste0("^", v, "FOM[1-4]$")))
  
  if (ncol(df) == 0) return(NULL)
  
  df %>%
    rename_with(~ paste0("FOM", seq_along(.)), everything()) %>%
    mutate(Variable = v) %>%
    pivot_longer(
      cols      = starts_with("FOM"),
      names_to  = "FOM",
      values_to = "Value"
    ) %>%
    mutate(
      Value = dplyr::case_when(
        Value == 1 ~ "yes",
        Value == 2 ~ "no",
        TRUE       ~ NA_character_
      )
    )
}) %>% bind_rows()
## 5. 二分类：基线变量（视为 FOM1）
binary_long_baseline <- dat %>%
  mutate(ID = dplyr::row_number()) %>%
  select(ID, all_of(baseline_bin_vars)) %>%
  pivot_longer(
    cols      = all_of(baseline_bin_vars),
    names_to  = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    FOM = "FOM1",
    Value = dplyr::case_when(
      Value == 1 ~ "yes",
      Value == 2 ~ "no",
      TRUE       ~ NA_character_
    )
  )

## 合并所有二分类的长格式
binary_long <- bind_rows(
  binary_long_timevary,
  binary_long_baseline
)

## 6. 统计描述

## 6.1 连续变量：只要 Mean ± SD（样本量不输出）
cont_summary <- continuous_long %>%
  group_by(Variable, FOM) %>%
  summarise(
    mean    = mean(Value, na.rm = TRUE),
    sd      = sd(Value, na.rm = TRUE),
    Mean_SD = sprintf("%.2f ± %.2f", mean, sd),
    .groups = "drop"
  ) %>%
  select(Variable, FOM, Mean_SD)

## 宽表：每个变量一行，FOM1–4 四列
cont_wide <- cont_summary %>%
  tidyr::pivot_wider(
    names_from  = FOM,
    values_from = Mean_SD
  )

## 6.2 二分类变量：各访视点 n(%)（按该访视的总样本量）
binary_summary <- binary_long %>%
  filter(!is.na(Value)) %>%
  group_by(Variable, FOM, Value) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Variable, FOM) %>%
  mutate(
    total_n = sum(n),
    pct     = sprintf("%.1f%%", 100 * n / total_n),
    cell    = paste0(n, " (", pct, ")")
  ) %>%
  ungroup()

## ⭐ 按 1/2（yes/no）两行排列：每个 Variable × Value 一行
binary_table <- binary_summary %>%
  mutate(
    row_label = paste0(Variable, " = ", Value)
  ) %>%
  select(row_label, FOM, cell) %>%
  tidyr::pivot_wider(
    names_from  = FOM,
    values_from = cell
  )

## 连续变量也加上 row_label（方便后面统一改名字）
cont_table <- cont_wide %>%
  mutate(row_label = Variable) %>%
  select(row_label, everything(), -Variable)

## 合并连续 + 二分类
table_final <- bind_rows(
  cont_table,
  binary_table
)

## 7. 替换输出 table 里的变量名（不改原数据）
rename_map <- c(
  "Mage"             = "Age (years)",
  "Mheight"          = "Height (cm)",
  "Mweight"          = "Weight (kg)",
  "Mtotalfatmass"    = "Total fat mass (kg)",
  "MCurrentlyHRT"    = "Currently taking hormone replacement therapy",
  "Mcontraceptiveinjection" = "Currently using contraceptive injection",
  "Moralcontraceptives"     = "Currently taking oral contraceptives",
  "Mmenopause"       = "Had a period/menstrual bleeding in the previous 12 months",
  "MvitaminD"        = "Vitamin D supplement usage",
  "Mcalcium"         = "Calcium supplement usage",
  "Mbisphosphonate"  = "Bisphosphonate usage"
)

# 对 row_label 进行前缀替换，例如：
# "MCurrentlyHRT = yes" -> "Currently taking hormone replacement therapy = yes"
table_final$Variables <- table_final$row_label
for (old in names(rename_map)) {
  table_final$Variables <- gsub(
    pattern = paste0("^", old),
    replacement = rename_map[[old]],
    x = table_final$Variables
  )
}

table_final <- table_final %>%
  select(Variables, FOM1, FOM2, FOM3, FOM4)

## 8. 导出 Excel

write_xlsx(table_final, "ALSPAC_covariates_FOM1to4_final_basic.xlsx")

cat("✔ 已完成：ALSPAC_covariates_FOM1to4_final_basic.xlsx 已生成。\n")

#**************样本量统计************#
# BMD 四个时点变量名
bmd_vars <- c("MtotalbodyBMDFOM1",
              "MtotalbodyBMDFOM2",
              "MtotalbodyBMDFOM3",
              "MtotalbodyBMDFOM4")
# 1. 各 FOM 的非 NA 样本量
count_each <- sapply(bmd_vars, function(v){
  sum(!is.na(dataalspac_sum[[v]]))
})
# 2. 任意一次 BMD 非 NA 的总样本量
count_any <- sum(rowSums(!is.na(dataalspac_sum[, bmd_vars])) > 0)
# 输出
cat("📌 各时点 BMD 非 NA 样本量：\n")
print(count_each)
cat("\n📌 测过至少一次 BMD 的样本量（任意 FOM 非 NA）：", count_any, "\n")

# 定义变量名
v1 <- "Mn6n3_F1"
v2 <- "Mn6n3_F2"

# 各自非 NA 的样本量
n1 <- sum(!is.na(dataalspac_sum[[v1]]))
n2 <- sum(!is.na(dataalspac_sum[[v2]]))

# 两者共同非 NA 的样本量
n_both <- sum(!is.na(dataalspac_sum[[v1]]) & !is.na(dataalspac_sum[[v2]]))
# 3. 任意一次非 NA 的样本量（F1 或 F2）
n_any <- sum(!is.na(dataalspac_sum[[v1]]) | !is.na(dataalspac_sum[[v2]]))

cat("📌", v1, "非 NA 样本量：", n1, "\n")
cat("📌", v2, "非 NA 样本量：", n2, "\n")
cat("📌 两者共同非 NA 样本量：", n_both, "\n")
cat("📌 任意一次非 NA 的样本量（F1 或 F2）：", n_any, "\n")



#============结局变量也Z分(按随访点)================#
## 0.1 自定义安全 scale 函数 ----
scale2 <- function(x) {
  if (all(is.na(x))) return(x)
  as.numeric(scale(x))
}

## 0.2 脂肪酸变量 + n6PUFA/n3PUFA（原始变量名） ----

## GLM 用：脂肪酸均值（原始变量名）
fa_means <- c(
  "MDHAmean",   # C22:6 n-3
  "MLAmean",    # C18:2 n-6
  "MN3mean",    # n3PUFA
  "MN6mean",    # n6PUFA
  "MSFAmean",   # SFA
  "MMUFAmean",  # MUFA
  "MPUFAmean"   # PUFA
)

## LMM 用：FOM1 脂肪酸（原始变量名）
fa_fom1 <- c(
  "MDHAFOM1",
  "MLAFOM1",
  "MN3FOM1",
  "MN6FOM1",
  "MSFAFOM1",
  "MMUFAFOM1",
  "MPUFAFOM1"
)

## 1️⃣ 计算 n6PUFA/n3PUFA（FOM1、FOM2 先求比值再求均值） ----
dataalspac_sum <- dataalspac_sum %>%
  mutate(
    Mn6n3_F1 = ifelse(!is.na(MN6FOM1) & !is.na(MN3FOM1) & MN3FOM1 != 0,
                      MN6FOM1 / MN3FOM1, NA_real_),
    Mn6n3_F2 = ifelse(!is.na(MN6FOM2) & !is.na(MN3FOM2) & MN3FOM2 != 0,
                      MN6FOM2 / MN3FOM2, NA_real_),
    Mn6n3mean = ifelse(
      !is.na(Mn6n3_F1) & !is.na(Mn6n3_F2),
      (Mn6n3_F1 + Mn6n3_F2) / 2,
      NA_real_
    ),
    Mn6n3FOM1 = Mn6n3_F1
  )

fa_means_all <- c(fa_means, "Mn6n3mean")
fa_fom1_all  <- c(fa_fom1,  "Mn6n3FOM1")

## 2️⃣ 生成脂肪酸 z-score（不覆盖原始变量） ----
fa_all <- c(fa_means_all, fa_fom1_all)

dataalspac_sum <- dataalspac_sum %>%
  mutate(
    across(
      all_of(fa_all),
      ~ scale2(.),
      .names = "{.col}_z"
    )
  )

fa_means_all_z <- paste0(fa_means_all, "_z")
fa_fom1_all_z  <- paste0(fa_fom1_all,  "_z")

fa_labels <- c(
  "C22:6 n-3",
  "C18:2 n-6",
  "n3PUFA",
  "n6PUFA",
  "SFA",
  "MUFA",
  "PUFA",
  "n6PUFA/n3PUFA"
)

## 2. 计算 4 次 BMD 的 AUC（原始） ----
bone_prefix <- c("MtotalbodyBMD","MSpineBMD","MtotalhipBMD","MHipNeckBMD")
age_vars    <- paste0("MageFOM", 1:4)

for (pre in bone_prefix) {
  bmd_vars <- paste0(pre,"FOM",1:4)
  auc_name <- paste0(pre,"AUC")
  
  dataalspac_sum[[auc_name]] <- sapply(seq_len(nrow(dataalspac_sum)), function(i){
    ages   <- as.numeric(dataalspac_sum[i, age_vars])
    values <- as.numeric(dataalspac_sum[i, bmd_vars])
    if (any(is.na(ages)) || any(is.na(values))) return(NA_real_)
    trapz(ages, values)
  })
}

## 2.1 对 BMD AUC 做 z-score（整体样本） ----
bmd_aucs   <- paste0(bone_prefix, "AUC")
bmd_aucs_z <- paste0(bmd_aucs, "_z")

dataalspac_sum <- dataalspac_sum %>%
  mutate(
    across(
      all_of(bmd_aucs),
      ~ scale2(.),
      .names = "{.col}_z"
    )
  )

## 3. GLM：BMD AUC_z ~ FAmean_z + 协变量（fat mass 用均值） ----
bone_names <- c("Total body BMD","Spine BMD","Total hip BMD","Hip neck BMD")
global_confounders <- c("MvitaminD","Mcalcium","Mbisphosphonate")

dataalspac_sum <- dataalspac_sum %>%
  mutate(
    Mtotalfatmass_mean = {
      tmp <- rowMeans(dplyr::select(., starts_with("MtotalfatmassFOM")), na.rm = TRUE)
      ifelse(is.nan(tmp), NA_real_, tmp)
    }
  )

glm_results <- list()

for (i in seq_along(fa_means_all)) {
  fa_orig <- fa_means_all[i]
  fa_z    <- fa_means_all_z[i]
  
  for (k in seq_along(bone_prefix)) {
    outcome  <- paste0(bone_prefix[k], "AUC_z")   # 用 AUC_z
    bone_lab <- bone_names[k]
    
    timevary_conf <- c(
      paste0("MoralcontraceptivesFOM",1:4),
      paste0("McontraceptiveinjectionFOM",1:4),
      paste0("MmenopauseFOM",1:4),
      paste0("MCurrentlyHRTFOM",1:4)
    )
    
    all_conf <- c(timevary_conf, "MageFOM1", "Mtotalfatmass_mean", global_confounders)
    all_conf <- all_conf[all_conf %in% names(dataalspac_sum)]
    if (! outcome %in% names(dataalspac_sum)) next
    
    rhs  <- paste(c(fa_z, all_conf), collapse = " + ")
    form <- as.formula(paste0(outcome, " ~ ", rhs))
    
    fit <- glm(form, data = dataalspac_sum)
    
    tmp <- tidy(fit, conf.int = TRUE) %>%
      filter(term == fa_z) %>%
      mutate(
        FA         = fa_orig,
        bone_site  = bone_lab,
        model_type = "AUC"
      )
    
    glm_results[[paste(fa_orig, outcome, sep = "_")]] <- tmp
  }
}

glm_res <- bind_rows(glm_results) %>%
  rename(
    beta    = estimate,
    ci_low  = conf.low,
    ci_high = conf.high,
    p_value = p.value
  ) %>%
  mutate(
    fa_name = case_when(
      FA == "MDHAmean"   ~ "C22:6 n-3",
      FA == "MLAmean"    ~ "C18:2 n-6",
      FA == "MN3mean"    ~ "n3PUFA",
      FA == "MN6mean"    ~ "n6PUFA",
      FA == "MSFAmean"   ~ "SFA",
      FA == "MMUFAmean"  ~ "MUFA",
      FA == "MPUFAmean"  ~ "PUFA",
      FA == "Mn6n3mean"  ~ "n6PUFA/n3PUFA",
      TRUE               ~ FA
    )
  )

## 4. LMM：BMD_z（按 FOM 年龄别 z） ~ FOM1 FA_z + 时变协变量 ----

bone_map <- tibble(Bone = bone_prefix, bone_site = bone_names)

fa_map_lmm <- tibble(
  FA = fa_fom1_all,
  fa_name = c(
    "C22:6 n-3",
    "C18:2 n-6",
    "n3PUFA",
    "n6PUFA",
    "SFA",
    "MUFA",
    "PUFA",
    "n6PUFA/n3PUFA"
  )
)

## 宽→长（包含 BMD、time-varying 协变量、fatmass、Mage） ----
tv_vars <- c(
  "MtotalbodyBMD","MSpineBMD","MtotalhipBMD","MHipNeckBMD",
  "Moralcontraceptives","Mcontraceptiveinjection",
  "Mmenopause","MCurrentlyHRT",
  "Mtotalfatmass","Mage"
)

tv_regex <- paste0("^(", paste(tv_vars, collapse="|"), ")FOM[1-4]$")

long_data <- dataalspac_sum %>%
  mutate(ID = factor(cidB4535)) %>%
  pivot_longer(
    cols = matches(tv_regex),
    names_to = c(".value","FOM"),
    names_pattern = "(.*)FOM([1-4])"
  ) %>%
  mutate(FOM = as.numeric(FOM))

## 4.1 ⭐ 按 FOM 做“年龄别 z 分”：每个随访点内对 BMD z 标准化 ----
long_data <- long_data %>%
  group_by(FOM) %>%
  mutate(
    across(
      all_of(bone_prefix),
      ~ scale2(.),
      .names = "{.col}_z"   # 比如 MtotalbodyBMD_z：在该 FOM 内 z
    )
  ) %>%
  ungroup()

timevary_conf <- c("Moralcontraceptives","Mcontraceptiveinjection",
                   "Mmenopause","MCurrentlyHRT","Mtotalfatmass","Mage")

lmm_results <- list()

for (i in seq_along(fa_fom1_all)) {
  fa_orig <- fa_fom1_all[i]
  fa_z    <- fa_fom1_all_z[i]
  
  for (pre in bone_prefix) {
    
    outcome_lmm <- paste0(pre, "_z")   # ⭐ 用“按 FOM 年龄别 z 后”的 BMD_z
    
    rhs  <- paste(c(fa_z, timevary_conf, global_confounders, "FOM"), collapse = " + ")
    form <- as.formula(paste0(outcome_lmm, " ~ ", rhs, " + (1|ID)"))
    
    fit <- lmer(form, data = long_data, REML = FALSE)
    
    tmp <- tidy(fit, effects = "fixed", conf.int = TRUE) %>%
      filter(term == fa_z) %>%
      mutate(
        FA   = fa_orig,
        Bone = pre
      )
    
    lmm_results[[paste(fa_orig, pre, sep = "_")]] <- tmp
  }
}

lmm_res <- bind_rows(lmm_results) %>%
  left_join(fa_map_lmm, by = "FA") %>%
  left_join(bone_map,   by = c("Bone")) %>%
  mutate(
    model_type = "LMM",
    beta       = estimate,
    ci_low     = conf.low,
    ci_high    = conf.high,
    p_value    = p.value
  )

## 5. 合并 GLM + LMM 结果 ----
res_all <- bind_rows(
  glm_res %>% select(model_type, fa_name, bone_site, beta, ci_low, ci_high, p_value),
  lmm_res %>% select(model_type, fa_name, bone_site, beta, ci_low, ci_high, p_value)
) %>%
  mutate(
    fa_name  = factor(fa_name, levels = rev(fa_labels))
  )

write.xlsx(res_all,"alspacfattybone_ageZ_LMM.xlsx")

## 5bis. 为双坐标森林图准备 ----
res_all <- res_all %>%
  mutate(
    fa_name  = factor(fa_name, levels = rev(fa_labels)),
    bone_site = factor(
      bone_site,
      levels = c("Total body BMD","Spine BMD","Total hip BMD","Hip neck BMD")
    )
  )

res_all_plot <- res_all %>%
  mutate(
    Model = dplyr::recode(
      model_type,
      "AUC" = "GLM",
      "LMM" = "LMM"
    ),
    bone_site = dplyr::recode(
      as.character(bone_site),
      "Total body BMD" = "Whole Body BMD",
      "Spine BMD"      = "Spine BMD",
      "Total hip BMD"  = "Total Hip BMD",
      "Hip neck BMD"   = "Femur Neck BMD"
    )
  ) %>%
  mutate(
    bone_site = factor(
      bone_site,
      levels = c("Whole Body BMD",
                 "Spine BMD",
                 "Total Hip BMD",
                 "Femur Neck BMD")
    )
  )

## 6. 双坐标 GLM + LMM 森林图 ----
glm_dat <- res_all_plot %>% filter(Model == "GLM")
lmm_dat <- res_all_plot %>% filter(Model == "LMM")

gmin0 <- min(glm_dat$ci_low,  na.rm = TRUE)
gmax0 <- max(glm_dat$ci_high, na.rm = TRUE)
lmin0 <- min(lmm_dat$ci_low,  na.rm = TRUE)
lmax0 <- max(lmm_dat$ci_high, na.rm = TRUE)

g_range   <- gmax0 - gmin0
l_range   <- lmax0 - lmin0

gmin_plot <- gmin0 - 0.1 * g_range
gmax_plot <- gmax0 + 0.1 * g_range
lmin_plot <- lmin0 - 0.1 * l_range
lmax_plot <- lmax0 + 0.1 * l_range

scale_factor <- (gmax_plot - gmin_plot) / (lmax_plot - lmin_plot)
offset       <- gmin_plot - scale_factor * lmin_plot
inv_lmm      <- function(x) (x - offset) / scale_factor

df_plot <- res_all_plot %>%
  mutate(
    beta_scaled    = if_else(Model == "GLM", beta,
                             offset + scale_factor * beta),
    ci_low_scaled  = if_else(Model == "GLM", ci_low,
                             offset + scale_factor * ci_low),
    ci_high_scaled = if_else(Model == "GLM", ci_high,
                             offset + scale_factor * ci_high)
  )

x_range <- gmax_plot - gmin_plot

df_plot <- df_plot %>%
  mutate(
    y_pos = as.numeric(fa_name) + ifelse(Model == "GLM", 0.25, -0.25)
  )

star_data <- df_plot %>%
  mutate(
    star_x_raw = ci_high_scaled + 0.02 * x_range,
    star_x = dplyr::if_else(
      p_value < 0.05,
      pmin(star_x_raw, gmax_plot - 0.01 * x_range),
      NA_real_
    )
  )

col_glm <- "#D55E00"
col_lmm <- "#009E73"

p_final <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_pointrange(
    data = df_plot,
    aes(
      x    = beta_scaled,
      y    = y_pos,
      xmin = ci_low_scaled,
      xmax = ci_high_scaled,
      color = Model
    ),
    linewidth = 0.7,
    size      = 1.2
  ) +
  geom_text(
    data = star_data,
    aes(
      x     = star_x,
      y     = y_pos,
      group = Model
    ),
    label    = "★",
    color    = "red",
    size     = 4.5,
    fontface = "bold",
    na.rm    = TRUE
  ) +
  facet_wrap(~ bone_site, nrow = 1) +
  scale_color_manual(
    name   = "Model",
    values = c("GLM" = col_glm, "LMM" = col_lmm),
    labels = c("GLM(FOM1&FOM2) & BMD AUC_z(FOM1-4)",
               "LMM(FOM1) & BMD_z (age-specific by FOM)")
  ) +
  scale_y_continuous(
    breaks = 1:length(fa_labels),
    labels = rev(fa_labels)
  ) +
  scale_x_continuous(
    name   = "GLM β (95% CI)",
    limits = c(gmin_plot, gmax_plot),
    sec.axis = sec_axis(
      ~ inv_lmm(.),
      name = "LMM β (95% CI)"
    )
  ) +
  labs(title = NULL, subtitle = NULL, y = NULL) +
  theme_bw(base_size = 11) +
  theme(
    text = element_text(face = "bold"),
    strip.background   = element_rect(fill = "#ddeeff"),
    strip.text         = element_text(face = "bold", size = 12),
    panel.grid.major.y = element_line(linetype = "dotted", linewidth = 0.2),
    panel.grid.minor.y = element_blank(),
    panel.spacing.y = unit(1.2, "lines"),
    panel.spacing   = unit(1.5, "lines"),
    legend.position    = "bottom",
    legend.key.size    = unit(0.6, "cm"),
    legend.text        = element_text(size = 10, face = "bold"),
    legend.title       = element_text(size = 10, face = "bold"),
    axis.text.x.top    = element_text(angle = 45, hjust = 0, size = 10, face = "bold"),
    axis.text.x.bottom = element_text(size = 10, face = "bold"),
    axis.text.y        = element_text(size = 12, face = "bold"),
    axis.title.x       = element_text(size = 11, face = "bold"),
    axis.title.y       = element_text(size = 11, face = "bold")
  )

p_final
ggsave("alspacfattyacidsbone_ageZ_LMM.pdf", p_final, width = 16, height = 6)
####***************** GLM / LMM：脂肪酸 vs BMD（整体 Z 分版） ************####

## 0. 加载 R 包 ----
library(pracma)
library(dplyr)
library(tidyr)
library(broom)
library(broom.mixed)
library(ggplot2)
library(tibble)
library(lmerTest)
library(grid)
library(openxlsx)   # 用于 write.xlsx

## 0.1 自定义安全 scale 函数（防止全 NA 出错） ----
scale2 <- function(x) {
  if (all(is.na(x))) return(x)
  as.numeric(scale(x))
}

## 0. 脂肪酸变量 + n6PUFA/n3PUFA + 标准化（不覆盖原值） ----

## GLM 用：脂肪酸均值（原始变量名）
fa_means <- c(
  "MDHAmean",   # C22:6 n-3
  "MLAmean",    # C18:2 n-6
  "MN3mean",    # n3PUFA
  "MN6mean",    # n6PUFA
  "MSFAmean",   # SFA
  "MMUFAmean",  # MUFA
  "MPUFAmean"   # PUFA
)

## LMM 用：FOM1 脂肪酸（原始变量名）
fa_fom1 <- c(
  "MDHAFOM1",
  "MLAFOM1",
  "MN3FOM1",
  "MN6FOM1",
  "MSFAFOM1",
  "MMUFAFOM1",
  "MPUFAFOM1"
)

## 1️⃣ 计算 n6PUFA/n3PUFA（FOM1、FOM2 先求比值再求均值） ----
dataalspac_sum <- dataalspac_sum %>%
  mutate(
    ## 每个时间点的比值（单独计算，不改原始变量）
    Mn6n3_F1 = ifelse(!is.na(MN6FOM1) & !is.na(MN3FOM1) & MN3FOM1 != 0,
                      MN6FOM1 / MN3FOM1, NA_real_),
    Mn6n3_F2 = ifelse(!is.na(MN6FOM2) & !is.na(MN3FOM2) & MN3FOM2 != 0,
                      MN6FOM2 / MN3FOM2, NA_real_),
    
    ## 两次都存在，才计算均值，否则 NA
    Mn6n3mean = ifelse(
      !is.na(Mn6n3_F1) & !is.na(Mn6n3_F2),
      (Mn6n3_F1 + Mn6n3_F2) / 2,
      NA_real_
    ),
    
    ## LMM 用 FOM1
    Mn6n3FOM1 = Mn6n3_F1
  )

## 把 n6/n3 加入脂肪酸列表（原始名） ----
fa_means_all <- c(fa_means, "Mn6n3mean")
fa_fom1_all  <- c(fa_fom1,  "Mn6n3FOM1")

## 2️⃣ 生成脂肪酸 z-score（不覆盖原始变量） ----
fa_all <- c(fa_means_all, fa_fom1_all)

dataalspac_sum <- dataalspac_sum %>%
  mutate(
    across(
      all_of(fa_all),
      ~ scale2(.),               # 整体 Z 分
      .names = "{.col}_z"        # 例如 MDHAmean_z、MDHAFOM1_z
    )
  )

fa_means_all_z <- paste0(fa_means_all, "_z")
fa_fom1_all_z  <- paste0(fa_fom1_all,  "_z")

## y 轴标签顺序 ----
fa_labels <- c(
  "C22:6 n-3",
  "C18:2 n-6",
  "n3PUFA",
  "n6PUFA",
  "SFA",
  "MUFA",
  "PUFA",
  "n6PUFA/n3PUFA"
)

## 2. 计算 4 次 BMD 的 AUC（原始） ----
bone_prefix <- c("MtotalbodyBMD","MSpineBMD","MtotalhipBMD","MHipNeckBMD")
age_vars    <- paste0("MageFOM", 1:4)

for (pre in bone_prefix) {
  bmd_vars <- paste0(pre,"FOM",1:4)
  auc_name <- paste0(pre,"AUC")
  
  dataalspac_sum[[auc_name]] <- sapply(seq_len(nrow(dataalspac_sum)), function(i){
    ages   <- as.numeric(dataalspac_sum[i, age_vars])
    values <- as.numeric(dataalspac_sum[i, bmd_vars])
    if (any(is.na(ages)) || any(is.na(values))) return(NA_real_)
    trapz(ages, values)
  })
}

## 2.1 ⭐ 对 BMD AUC 做整体 z-score ----
bmd_aucs   <- paste0(bone_prefix, "AUC")
bmd_aucs_z <- paste0(bmd_aucs, "_z")

dataalspac_sum <- dataalspac_sum %>%
  mutate(
    across(
      all_of(bmd_aucs),
      ~ scale2(.),                 # 整体样本 z 分
      .names = "{.col}_z"          # 例如 MtotalbodyBMDAUC_z
    )
  )

## 3. GLM：BMD AUC_z ~ 标准化脂肪酸均值_z + 协变量（fat mass 用 4 次均值） ----
bone_names <- c("Total body BMD","Spine BMD","Total hip BMD","Hip neck BMD")
global_confounders <- c("MvitaminD","Mcalcium","Mbisphosphonate")

dataalspac_sum <- dataalspac_sum %>%
  mutate(
    Mtotalfatmass_mean = {
      tmp <- rowMeans(dplyr::select(., starts_with("MtotalfatmassFOM")), na.rm = TRUE)
      ifelse(is.nan(tmp), NA_real_, tmp)
    }
  )

glm_results <- list()

for (i in seq_along(fa_means_all)) {
  fa_orig <- fa_means_all[i]      # 原始名，如 MDHAmean
  fa_z    <- fa_means_all_z[i]    # 标准化变量名，如 MDHAmean_z
  
  for (k in seq_along(bone_prefix)) {
    outcome  <- paste0(bone_prefix[k], "AUC_z")  # ⭐ 用 AUC_z
    bone_lab <- bone_names[k]
    
    timevary_conf <- c(
      paste0("MoralcontraceptivesFOM",1:4),
      paste0("McontraceptiveinjectionFOM",1:4),
      paste0("MmenopauseFOM",1:4),
      paste0("MCurrentlyHRTFOM",1:4)
    )
    
    all_conf <- c(timevary_conf, "MageFOM1", "Mtotalfatmass_mean", global_confounders)
    all_conf <- all_conf[all_conf %in% names(dataalspac_sum)]
    if (! outcome %in% names(dataalspac_sum)) next
    
    rhs  <- paste(c(fa_z, all_conf), collapse = " + ")
    form <- as.formula(paste0(outcome, " ~ ", rhs))
    
    fit <- glm(form, data = dataalspac_sum)
    
    tmp <- tidy(fit, conf.int = TRUE) %>%
      filter(term == fa_z) %>%
      mutate(
        FA         = fa_orig,
        bone_site  = bone_lab,
        model_type = "AUC"          # 这里代表 AUC_z 模型
      )
    
    glm_results[[paste(fa_orig, outcome, sep = "_")]] <- tmp
  }
}

glm_res <- bind_rows(glm_results) %>%
  rename(
    beta    = estimate,
    ci_low  = conf.low,
    ci_high = conf.high,
    p_value = p.value
  ) %>%
  mutate(
    fa_name = case_when(
      FA == "MDHAmean"   ~ "C22:6 n-3",
      FA == "MLAmean"    ~ "C18:2 n-6",
      FA == "MN3mean"    ~ "n3PUFA",
      FA == "MN6mean"    ~ "n6PUFA",
      FA == "MSFAmean"   ~ "SFA",
      FA == "MMUFAmean"  ~ "MUFA",
      FA == "MPUFAmean"  ~ "PUFA",
      FA == "Mn6n3mean"  ~ "n6PUFA/n3PUFA",
      TRUE               ~ FA
    )
  )

## 4. LMM：BMD_z（整体 Z 分） ~ 标准化 FOM1 脂肪酸_z + 时变协变量 ----
bone_map <- tibble(Bone = bone_prefix, bone_site = bone_names)

fa_map_lmm <- tibble(
  FA = fa_fom1_all,
  fa_name = c(
    "C22:6 n-3",
    "C18:2 n-6",
    "n3PUFA",
    "n6PUFA",
    "SFA",
    "MUFA",
    "PUFA",
    "n6PUFA/n3PUFA"
  )
)

## 宽→长 ----
tv_vars <- c(
  "MtotalbodyBMD","MSpineBMD","MtotalhipBMD","MHipNeckBMD",
  "Moralcontraceptives","Mcontraceptiveinjection",
  "Mmenopause","MCurrentlyHRT",
  "Mtotalfatmass","Mage"
)

tv_regex <- paste0("^(", paste(tv_vars, collapse="|"), ")FOM[1-4]$")

long_data <- dataalspac_sum %>%
  mutate(ID = factor(cidB4535)) %>%
  pivot_longer(
    cols = matches(tv_regex),
    names_to = c(".value","FOM"),
    names_pattern = "(.*)FOM([1-4])"
  ) %>%
  mutate(FOM = as.numeric(FOM))

## 4.1 ⭐ 整体 Z 分：不分 FOM，直接对所有时间点的 BMD 一起 z 分 ----
long_data <- long_data %>%
  mutate(
    across(
      all_of(bone_prefix),
      ~ scale2(.),               # 所有 FOM1–4 一起 z
      .names = "{.col}_z"        # 例如 MtotalbodyBMD_z
    )
  )

timevary_conf <- c("Moralcontraceptives","Mcontraceptiveinjection",
                   "Mmenopause","MCurrentlyHRT","Mtotalfatmass","Mage")

lmm_results <- list()

for (i in seq_along(fa_fom1_all)) {
  fa_orig <- fa_fom1_all[i]      # 原始名，例如 MDHAFOM1
  fa_z    <- fa_fom1_all_z[i]    # 标准化变量名，例如 MDHAFOM1_z
  
  for (pre in bone_prefix) {
    
    outcome_lmm <- paste0(pre, "_z")   # ⭐ 用整体 z 后的 BMD_z
    
    rhs  <- paste(c(fa_z, timevary_conf, global_confounders, "FOM"), collapse = " + ")
    form <- as.formula(paste0(outcome_lmm, " ~ ", rhs, " + (1|ID)"))
    
    fit <- lmer(form, data = long_data, REML = FALSE)
    
    tmp <- tidy(fit, effects = "fixed", conf.int = TRUE) %>%
      filter(term == fa_z) %>%
      mutate(
        FA   = fa_orig,
        Bone = pre
      )
    
    lmm_results[[paste(fa_orig, pre, sep = "_")]] <- tmp
  }
}

lmm_res <- bind_rows(lmm_results) %>%
  left_join(fa_map_lmm, by = "FA") %>%
  left_join(bone_map,   by = c("Bone")) %>%
  mutate(
    model_type = "LMM",
    beta       = estimate,
    ci_low     = conf.low,
    ci_high    = conf.high,
    p_value    = p.value
  )

## 5. 合并 GLM + LMM 结果 ----
res_all <- bind_rows(
  glm_res %>% select(model_type, fa_name, bone_site, beta, ci_low, ci_high, p_value),
  lmm_res %>% select(model_type, fa_name, bone_site, beta, ci_low, ci_high, p_value)
) %>%
  mutate(
    fa_name  = factor(fa_name, levels = rev(fa_labels))
  )

write.xlsx(res_all,"alspacfattybone_overallZ_LMM.xlsx")

## 5bis. 为双坐标森林图准备 ----
res_all <- res_all %>%
  mutate(
    fa_name  = factor(fa_name, levels = rev(fa_labels)),
    bone_site = factor(
      bone_site,
      levels = c("Total body BMD","Spine BMD","Total hip BMD","Hip neck BMD")
    )
  )

res_all_plot <- res_all %>%
  mutate(
    Model = dplyr::recode(
      model_type,
      "AUC" = "GLM",
      "LMM" = "LMM"
    ),
    bone_site = dplyr::recode(
      as.character(bone_site),
      "Total body BMD" = "Whole Body BMD",
      "Spine BMD"      = "Spine BMD",
      "Total hip BMD"  = "Total Hip BMD",
      "Hip neck BMD"   = "Femur Neck BMD"
    )
  ) %>%
  mutate(
    bone_site = factor(
      bone_site,
      levels = c("Whole Body BMD",
                 "Spine BMD",
                 "Total Hip BMD",
                 "Femur Neck BMD")
    )
  )

## 6. 双坐标 GLM + LMM 森林图 ----
glm_dat <- res_all_plot %>% filter(Model == "GLM")
lmm_dat <- res_all_plot %>% filter(Model == "LMM")

gmin0 <- min(glm_dat$ci_low,  na.rm = TRUE)
gmax0 <- max(glm_dat$ci_high, na.rm = TRUE)
lmin0 <- min(lmm_dat$ci_low,  na.rm = TRUE)
lmax0 <- max(lmm_dat$ci_high, na.rm = TRUE)

g_range   <- gmax0 - gmin0
l_range   <- lmax0 - lmin0

gmin_plot <- gmin0 - 0.1 * g_range
gmax_plot <- gmax0 + 0.1 * g_range
lmin_plot <- lmin0 - 0.1 * l_range
lmax_plot <- lmax0 + 0.1 * l_range

## GLM ↔ LMM 线性映射 + 反函数（给 sec.axis 用）----
scale_factor <- (gmax_plot - gmin_plot) / (lmax_plot - lmin_plot)
offset       <- gmin_plot - scale_factor * lmin_plot
inv_lmm      <- function(x) (x - offset) / scale_factor

## 把 LMM 的 β/CI 映射到 GLM 轴上 ----
df_plot <- res_all_plot %>%
  mutate(
    beta_scaled    = if_else(Model == "GLM", beta,
                             offset + scale_factor * beta),
    ci_low_scaled  = if_else(Model == "GLM", ci_low,
                             offset + scale_factor * ci_low),
    ci_high_scaled = if_else(Model == "GLM", ci_high,
                             offset + scale_factor * ci_high)
  )

x_range <- gmax_plot - gmin_plot

## y 轴错位（GLM 在上，LMM 在下）----
df_plot <- df_plot %>%
  mutate(
    y_pos = as.numeric(fa_name) + ifelse(Model == "GLM", 0.25, -0.25)
  )

## 星号位置 ----
star_data <- df_plot %>%
  mutate(
    star_x_raw = ci_high_scaled + 0.02 * x_range,
    star_x = dplyr::if_else(
      p_value < 0.05,
      pmin(star_x_raw, gmax_plot - 0.01 * x_range),
      NA_real_
    )
  )

col_glm <- "#D55E00"
col_lmm <- "#009E73"

p_final <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_pointrange(
    data = df_plot,
    aes(
      x    = beta_scaled,
      y    = y_pos,
      xmin = ci_low_scaled,
      xmax = ci_high_scaled,
      color = Model
    ),
    linewidth = 0.7,
    size      = 1.2
  ) +
  geom_text(
    data = star_data,
    aes(
      x     = star_x,
      y     = y_pos,
      group = Model
    ),
    label    = "★",
    color    = "red",
    size     = 4.5,
    fontface = "bold",
    na.rm    = TRUE
  ) +
  facet_wrap(~ bone_site, nrow = 1) +
  scale_color_manual(
    name   = "Model",
    values = c("GLM" = col_glm, "LMM" = col_lmm),
    labels = c("GLM(FOM1&FOM2) & BMD AUC_z(FOM1-4)",
               "LMM(FOM1) & BMD_z (overall)")
  ) +
  scale_y_continuous(
    breaks = 1:length(fa_labels),
    labels = rev(fa_labels)
  ) +
  scale_x_continuous(
    name   = "GLM β (95% CI)",
    limits = c(gmin_plot, gmax_plot),
    sec.axis = sec_axis(
      ~ inv_lmm(.),
      name = "LMM β (95% CI)"
    )
  ) +
  labs(title = NULL, subtitle = NULL, y = NULL) +
  theme_bw(base_size = 11) +
  theme(
    text = element_text(face = "bold"),
    strip.background   = element_rect(fill = "#ddeeff"),
    strip.text         = element_text(face = "bold", size = 12),
    panel.grid.major.y = element_line(linetype = "dotted", linewidth = 0.2),
    panel.grid.minor.y = element_blank(),
    panel.spacing.y = unit(1.2, "lines"),
    panel.spacing   = unit(1.5, "lines"),
    legend.position    = "bottom",
    legend.key.size    = unit(0.6, "cm"),
    legend.text        = element_text(size = 10, face = "bold"),
    legend.title       = element_text(size = 10, face = "bold"),
    axis.text.x.top    = element_text(angle = 45, hjust = 0, size = 10, face = "bold"),
    axis.text.x.bottom = element_text(size = 10, face = "bold"),
    axis.text.y        = element_text(size = 12, face = "bold"),
    axis.title.x       = element_text(size = 11, face = "bold"),
    axis.title.y       = element_text(size = 11, face = "bold")
  )

p_final
ggsave("alspacfattyacidsbone_overallZ_LMM.pdf", p_final, width = 16, height = 6)

