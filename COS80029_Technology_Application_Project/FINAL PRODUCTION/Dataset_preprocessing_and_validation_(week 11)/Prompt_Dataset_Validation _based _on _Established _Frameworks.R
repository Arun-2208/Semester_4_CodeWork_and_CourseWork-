# validate_prompts_views.R
# ------------------------------------------------------------------
# Prompt dataset validator with clear, colourful plots (cleaned scope).
#
# What it shows (only):
#  1) Technique balance (combined techniques -> "Other (Combined)") [BAR]
#  2) Missingness by column (strict: NA/blank/whitespace) [BAR]
#     + missing PROMPT rows with row indexes [TABLE]
#  3) Length distribution for representative prompt + outlier cutoff [HIST + Normal curve + shaded outlier region]
#     + outlier rows [TABLE]
#  4) Direct vs Paraphrased agreement per model (Grok/GPT-5/Gemini) [BAR]
#     + each model's summary & confusion tables [TABLES]
#     + inter-model agreement on PARAPHRASED results [BAR]
#  5) Rows involved in duplicates by technique (exact OR near-dup) [BAR + TABLE]
#  6) Schema validation [TABLE]
#  7) Split-leakage status printed to console
#
# NEW: Every plot is also saved to PNG in a timestamped folder.
# ------------------------------------------------------------------

# ---- Package setup ----
required_pkgs <- c(
  "readxl","readr","tools","dplyr","stringr",
  "tibble","tidyr","text2vec","Matrix","irr",
  "ggplot2","scales"
)
to_install <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(to_install) > 0) {
  message("Installing missing packages: ", paste(to_install, collapse = ", "))
  install.packages(to_install, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(readxl); library(readr); library(tools)
  library(dplyr);  library(stringr); library(tibble); library(tidyr)
  library(text2vec); library(Matrix); library(irr)
  library(ggplot2); library(scales)
})

# ---- Small helpers ----
is_text_series <- function(x) {
  if (!is.character(x)) return(FALSE)
  non_null <- x[!is.na(x)]
  if (length(non_null) == 0) return(FALSE)
  mean_len <- mean(nchar(non_null))
  whitespace_rate <- mean(stringr::str_detect(non_null, "\\s"))
  (mean_len > 20) || (whitespace_rate > 0.2)
}

read_dataset_auto <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx","xls")) {
    sh <- readxl::excel_sheets(path)[1]
    df <- readxl::read_excel(path, sheet = sh, .name_repair = "minimal")
    df <- as.data.frame(df, stringsAsFactors = FALSE)
  } else if (ext %in% c("csv","txt")) {
    df <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
    df <- as.data.frame(df, stringsAsFactors = FALSE)
  } else {
    stop("Unsupported file type: ", ext, ". Please select .xlsx, .xls, or .csv")
  }
  names(df) <- trimws(names(df))
  df[] <- lapply(df, function(col) if (is.factor(col)) as.character(col) else col)
  df
}

.num_or_na <- function(x) { if (is.null(x) || length(x) == 0) return(NA_real_) else as.numeric(x) }

compute_agreement <- function(a, b) {
  a <- as.character(a); b <- as.character(b)
  keep <- !(is.na(a) | is.na(b) | a == "" | b == "")
  a <- a[keep]; b <- b[keep]
  if (length(a) < 2) return(list(summary=NULL, confusion=NULL))
  labs <- sort(unique(c(a, b)))
  ta <- factor(a, levels = labs); tb <- factor(b, levels = labs)
  tab <- as.data.frame.matrix(table(ta, tb))
  agree <- sum(diag(as.matrix(tab)))/sum(as.matrix(tab))
  k <- tryCatch({ irr::kappa2(cbind(as.character(ta), as.character(tb)), weight = "unweighted") }, error = function(e) NULL)
  out <- data.frame(
    metric = c("n_pairs","percent_agreement","cohens_kappa","kappa_se","kappa_p_value"),
    value  = c(length(a),
               round(100*agree,2),
               round(if (!is.null(k)) .num_or_na(k$value) else NA_real_,4),
               round(if (!is.null(k)) .num_or_na(k$se)    else NA_real_,4),
               if (!is.null(k)) .num_or_na(k$p.value)     else NA_real_),
    stringsAsFactors = FALSE
  )
  list(summary = out, confusion = tab)
}

guess_result_columns <- function(df) {
  cols <- names(df)
  res_pat <- "(label|result|decision|outcome|class|verdict|judg(e)?ment|response|policy)"
  dir_pat <- "(^|[_\\-\\s])(direct|orig(inal)?)([_\\-\\s]|$)"
  par_pat <- "(^|[_\\-\\s])(para(phrase|phrased)?|paraphrased|rephrase(d)?)([_\\-\\s]|$)"
  res_cols <- cols[str_detect(tolower(cols), res_pat)]
  dir_cols <- res_cols[str_detect(tolower(res_cols), dir_pat)]
  par_cols <- res_cols[str_detect(tolower(res_cols), par_pat)]
  if (length(dir_cols) == 0 || length(par_cols) == 0) {
    if (length(res_cols) >= 2) {
      if (length(dir_cols) == 0) dir_cols <- res_cols[1]
      if (length(par_cols) == 0 && length(res_cols) >= 2) par_cols <- res_cols[2]
    }
  }
  list(direct = unique(dir_cols)[1], paraphrase = unique(par_cols)[1])
}

# Collapse combined techniques (comma, slash, ampersand, or ' and ')
collapse_technique <- function(x) {
  ifelse(str_detect(tolower(x), ",|/|\\&|\\band\\b"),
         "Other (Combined)", x)
}

# ---- Main validator ----
validate_prompts_view <- function(df,
                                  rep_text_col = NULL,
                                  near_dup_threshold = 0.90,
                                  split_leak_threshold = 0.92,
                                  output_dir = "validation_plots") {
  
  # Ensure output directory exists
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Helper to save a plot (PNG 10x6, 150 dpi) with numbered filename
  save_plot <- function(plot_obj, fname_prefix, idx) {
    if (inherits(plot_obj, "ggplot")) {
      fn <- file.path(output_dir, sprintf("%02d_%s.png", idx, fname_prefix))
      ggplot2::ggsave(filename = fn, plot = plot_obj, width = 10, height = 6, dpi = 150)
    }
  }
  
  # Identify prompt-like columns & representative text col
  text_cols <- names(df)[vapply(df, is_text_series, logical(1))]
  priority_cols <- names(df)[str_detect(names(df), regex("prompt|text|instruction|input", ignore_case = TRUE))]
  prompt_like_cols <- intersect(priority_cols, text_cols)
  if (length(prompt_like_cols) == 0) prompt_like_cols <- head(text_cols, 2)
  if (is.null(rep_text_col)) {
    if (length(prompt_like_cols) >= 1) rep_text_col <- prompt_like_cols[1]
    else stop("No text-like columns found. Please ensure at least one prompt/text column is character type.")
  }
  
  # ---------------- Schema & Missingness ----------------
  schema <- data.frame(
    column   = names(df),
    dtype    = vapply(df, function(x) class(x)[1], character(1)),
    non_null = vapply(df, function(x) sum(!is.na(x)), integer(1)),
    nulls    = vapply(df, function(x) sum(is.na(x)), integer(1)),
    stringsAsFactors = FALSE
  )
  schema$null_pct <- round(100 * schema$nulls / pmax(1, (schema$non_null + schema$nulls)), 2)
  
  # Strict missingness by column
  is_missing_strict <- function(v) { v <- as.character(v); v[is.na(v)] <- ""; str_trim(v) == "" }
  missingness_strict <- data.frame(
    column = names(df),
    missing_pct = vapply(names(df), function(col) mean(is_missing_strict(df[[col]])), numeric(1))
  ) %>% mutate(missing_pct = round(100*missing_pct, 2)) %>% arrange(desc(missing_pct))
  
  # Missing PROMPT rows with indexes (for all prompt-like columns)
  missing_rows_prompts <- bind_rows(lapply(prompt_like_cols, function(col) {
    v <- df[[col]]; miss <- is_missing_strict(v)
    if (!any(miss)) return(NULL)
    data.frame(prompt_column = col, row_index = which(miss) - 1, stringsAsFactors = FALSE)
  }))
  if (is.null(missing_rows_prompts)) missing_rows_prompts <- data.frame(prompt_column=character(), row_index=integer(), stringsAsFactors = FALSE)
  
  # ---------------- Duplicates ----------------
  # Exact duplicates across main text fields
  exact_duplicates <- data.frame()
  if (length(prompt_like_cols) > 0) {
    key <- apply(df[prompt_like_cols], 1, function(row) paste(row, collapse = " || "))
    dup_mask <- duplicated(key) | duplicated(key, fromLast = TRUE)
    if (any(dup_mask, na.rm = TRUE)) {
      exact_duplicates <- data.frame(row_index = which(dup_mask) - 1, stringsAsFactors = FALSE)
    }
  }
  
  # Near-duplicates on representative column
  near_duplicates <- data.frame()
  rep_text <- df[[rep_text_col]]; rep_text <- if (is.character(rep_text)) rep_text else as.character(rep_text); rep_text[is.na(rep_text)] <- ""
  norm <- tolower(str_squish(rep_text))
  if (length(norm) > 1 && any(nchar(norm) > 0)) {
    it <- itoken(norm, progressbar = FALSE)
    vocab <- create_vocabulary(it, ngram = c(3L, 6L), stopwords = character(0))
    dtm <- create_dtm(it, vocab_vectorizer(vocab))
    if (nrow(dtm) > 1) {
      sim <- text2vec::sim2(dtm, method = "cosine", norm = "l2")
      coords <- which(sim >= near_dup_threshold, arr.ind = TRUE)
      coords <- coords[coords[,1] < coords[,2], , drop = FALSE]
      if (nrow(coords) > 0) {
        near_duplicates <- data.frame(i = coords[,1] - 1, j = coords[,2] - 1, stringsAsFactors = FALSE)
      }
    }
  }
  
  # Rows involved in ANY duplicate (exact or near)
  duplicate_row_idx <- unique(c(exact_duplicates$row_index, near_duplicates$i, near_duplicates$j))
  duplicate_row_idx <- duplicate_row_idx[!is.na(duplicate_row_idx)]
  
  # ---------------- Technique balance (collapse combined) ----------------
  technique_cols <- names(df)[str_detect(names(df), regex("\\btechnique\\b", ignore_case = TRUE))]
  technique_counts_collapsed <- data.frame()
  duplicates_by_technique <- data.frame()
  
  if (length(technique_cols) > 0) {
    tech_col <- technique_cols[1]
    techs_raw <- as.character(df[[tech_col]])
    techs_plot <- collapse_technique(techs_raw)
    
    technique_counts_collapsed <- as.data.frame(table(techs_plot), stringsAsFactors = FALSE) %>%
      rename(Technique = techs_plot, count = Freq) %>% arrange(desc(count))
    
    # Duplicate rows per technique (collapsed)
    if (length(duplicate_row_idx) > 0) {
      duplicates_by_technique <- data.frame(
        Technique = collapse_technique(techs_raw[duplicate_row_idx + 1])  # convert 0-based to 1-based
      ) %>% count(Technique, name = "dup_rows_involved") %>% arrange(desc(dup_rows_involved))
    } else {
      duplicates_by_technique <- data.frame(Technique = character(), dup_rows_involved = integer())
    }
  }
  
  # ---------------- Length outliers on representative prompt ----------------
  length_outliers <- data.frame(); outlier_cut <- NA_real_
  prompt_lengths <- nchar(rep_text)
  if (length(prompt_lengths) > 1) {
    mu <- mean(prompt_lengths); sdv <- stats::sd(prompt_lengths)
    z <- if (sdv > 0) (prompt_lengths - mu) / sdv else rep(0, length(prompt_lengths))
    idx <- which(z > 3)
    if (length(idx) > 0) {
      length_outliers <- data.frame(row_index = idx - 1, char_len = prompt_lengths[idx], stringsAsFactors = FALSE)
    }
    outlier_cut <- if (sdv > 0) mu + 3*sdv else NA_real_
  }
  
  # ---------------- Split leakage ----------------
  cross_split_near_dups <- data.frame()
  split_cols <- names(df)[str_detect(names(df), regex("\\b(split|fold|partition|set)\\b", ignore_case = TRUE))]
  split_status_msg <- ""
  if (!is.null(rep_text_col) && length(split_cols) > 0) {
    split_col <- split_cols[1]
    texts <- tolower(str_squish(replace(rep_text, is.na(rep_text), "")))
    if (length(texts) > 1 && any(nchar(texts) > 0)) {
      it <- itoken(texts, progressbar = FALSE)
      vocab <- create_vocabulary(it, ngram = c(3L, 6L), stopwords = character(0))
      dtm <- create_dtm(it, vocab_vectorizer(vocab))
      if (nrow(dtm) > 1) {
        sim <- text2vec::sim2(dtm, method = "cosine", norm = "l2")
        coords <- which(sim >= split_leak_threshold, arr.ind = TRUE)
        coords <- coords[coords[,1] < coords[,2], , drop = FALSE]
        if (nrow(coords) > 0) {
          splits <- as.character(df[[split_col]])
          keep <- splits[coords[,1]] != splits[coords[,2]]
          coords <- coords[keep, , drop = FALSE]
          if (nrow(coords) > 0) {
            cross_split_near_dups <- data.frame(
              i = coords[,1] - 1, i_split = splits[coords[,1]],
              j = coords[,2] - 1, j_split = splits[coords[,2]],
              cosine_sim = sim[coords], stringsAsFactors = FALSE
            ) %>% arrange(desc(cosine_sim))
          }
        }
      }
    }
    split_status_msg <- paste0("Split leakage check: split column = '", split_col,
                               "', threshold = ", split_leak_threshold,
                               ", pairs found = ", nrow(cross_split_near_dups))
  } else {
    split_status_msg <- "Split leakage check: NO split/fold/set column found (check skipped)."
  }
  
  # ---------------- Direct vs Paraphrased per model (IRA) ----------------
  ira_summary_Grok <- ira_confusion_Grok <- ira_summary_GPT5 <- ira_confusion_GPT5 <- ira_summary_Gemini <- ira_confusion_Gemini <- data.frame()
  model_result_pairs <- list(
    Grok   = list(direct = "Test Result Grok - Direct",    paraphrase = "Test Result Grok - paraphrased"),
    GPT5   = list(direct = "Test Result (GPT-5) - Direct", paraphrase = "Test Result (GPT-5) - paraphrased"),
    Gemini = list(direct = "Test Result Gemini - Direct",  paraphrase = "Test Result Gemini - paraphrased")
  )
  ira_bar <- data.frame(model=character(), percent_agreement=numeric(), cohens_kappa=numeric(), n_pairs=integer())
  
  for (m in names(model_result_pairs)) {
    cols <- model_result_pairs[[m]]
    if (all(c(cols$direct, cols$paraphrase) %in% names(df))) {
      res <- compute_agreement(df[[cols$direct]], df[[cols$paraphrase]])
      if (!is.null(res$summary)) {
        summ <- res$summary
        ira_bar <- bind_rows(ira_bar, data.frame(
          model = m,
          n_pairs = as.numeric(summ$value[summ$metric=="n_pairs"]),
          percent_agreement = as.numeric(summ$value[summ$metric=="percent_agreement"]),
          cohens_kappa = as.numeric(summ$value[summ$metric=="cohens_kappa"])
        ))
        # per-model tables
        summ_tbl <- data.frame(metric = c("model", as.character(summ$metric)),
                               value  = c(m, as.character(summ$value)), stringsAsFactors = FALSE)
        conf_tbl <- if (!is.null(res$confusion)) tibble::as_tibble(
          tibble::rownames_to_column(as.data.frame(res$confusion), var = "direct\\paraphrase")
        ) else data.frame()
        if (m == "Grok")   { ira_summary_Grok <- summ_tbl;   ira_confusion_Grok <- conf_tbl }
        if (m == "GPT5")   { ira_summary_GPT5 <- summ_tbl;   ira_confusion_GPT5 <- conf_tbl }
        if (m == "Gemini") { ira_summary_Gemini <- summ_tbl; ira_confusion_Gemini <- conf_tbl }
      }
    }
  }
  
  # Inter-model agreement on PARAPHRASED results (bar)
  inter_pairs <- list(
    "Grok vs GPT-5"   = c("Test Result Grok - paraphrased",   "Test Result (GPT-5) - paraphrased"),
    "Grok vs Gemini"  = c("Test Result Grok - paraphrased",   "Test Result Gemini - paraphrased"),
    "GPT-5 vs Gemini" = c("Test Result (GPT-5) - paraphrased","Test Result Gemini - paraphrased")
  )
  inter_bar <- data.frame(pair=character(), percent_agreement=numeric(), n_pairs=integer(), stringsAsFactors = FALSE)
  for (nm in names(inter_pairs)) {
    pair <- inter_pairs[[nm]]
    if (all(pair %in% names(df))) {
      res <- compute_agreement(df[[pair[1]]], df[[pair[2]]])
      if (!is.null(res$summary)) {
        n <- as.numeric(res$summary$value[res$summary$metric=="n_pairs"])
        pa <- as.numeric(res$summary$value[res$summary$metric=="percent_agreement"])
        inter_bar <- bind_rows(inter_bar, data.frame(pair = nm, percent_agreement = pa, n_pairs = n))
      }
    }
  }
  
  # ---------------- Console summary (spaced) ----------------
  cat("Rows:", nrow(df), "  Columns:", ncol(df), "\n\n")
  cat(split_status_msg, "\n\n")
  
  # ---------------- COLORFUL PLOTS (SAVE EACH) ----------------
  # Dynamic palettes that handle many categories
  make_hue_palette <- function(n, seed = 123) { set.seed(seed); scales::hue_pal()(n) }
  
  plot_idx <- 1
  
  # 1) Technique balance (collapsed)
  if (nrow(technique_counts_collapsed) > 0) {
    tech_palette <- setNames(make_hue_palette(nrow(technique_counts_collapsed)),
                             technique_counts_collapsed$Technique)
    p_tech <- ggplot(technique_counts_collapsed, aes(x = Technique, y = count, fill = Technique)) +
      geom_col(width = 0.75) +
      scale_y_continuous(labels = comma) +
      scale_fill_manual(values = tech_palette, guide = "none") +
      labs(title = "Technique Balance (combined → Other (Combined))",
           x = "Technique", y = "Count") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p_tech); save_plot(p_tech, "01_technique_balance", plot_idx); plot_idx <- plot_idx + 1
  }
  
  # 2) Missingness (strict) by column
  if (nrow(missingness_strict) > 0) {
    p_miss <- ggplot(missingness_strict, aes(x = column, y = missing_pct, fill = missing_pct)) +
      geom_col(width = 0.75) +
      scale_fill_gradient(low = "#6EE7B7", high = "#EF4444") +
      labs(title = "Missingness by Column (strict: NA / blank / whitespace)",
           x = "Column", y = "Missing (%)") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p_miss); save_plot(p_miss, "02_missingness_by_column", plot_idx); plot_idx <- plot_idx + 1
  }
  
  # 3) Length distribution + normal curve + shaded outlier region (x > cutoff)
  if (length(prompt_lengths) > 1 && is.finite(outlier_cut)) {
    df_len <- data.frame(len = prompt_lengths)
    mu <- mean(prompt_lengths); sdv <- stats::sd(prompt_lengths)
    
    # Freedman–Diaconis rule for binwidth
    bw <- IQR(prompt_lengths, na.rm = TRUE) / (length(prompt_lengths)^(1/3))
    bw <- ifelse(is.finite(bw) && bw > 0, bw, max(1, round(diff(range(prompt_lengths))/30)))
    
    # Normal curve scaled to histogram counts
    xseq <- seq(min(prompt_lengths), max(prompt_lengths), length.out = 200)
    y_norm <- dnorm(xseq, mean = mu, sd = sdv)
    scale_factor <- nrow(df_len) * bw
    df_norm <- data.frame(x = xseq, y = y_norm * scale_factor)
    
    p_len <- ggplot(df_len, aes(x = len)) +
      annotate("rect", xmin = outlier_cut, xmax = Inf, ymin = 0, ymax = Inf,
               fill = "#FB7185", alpha = 0.25) +
      geom_histogram(binwidth = bw, fill = "#60A5FA", color = "white", alpha = 0.9) +
      geom_line(data = df_norm, aes(x = x, y = y), color = "#0EA5E9", linewidth = 1) +
      geom_vline(xintercept = outlier_cut, linetype = "dashed", linewidth = 1, color = "#DC2626") +
      labs(title = "Representative Prompt Lengths (normal curve + shaded outliers)",
           x = "Characters", y = "Count") +
      theme_minimal(base_size = 12)
    print(p_len); save_plot(p_len, "03_prompt_length_distribution", plot_idx); plot_idx <- plot_idx + 1
  }
  
  # 4a) IRA % agreement by model (Direct vs Paraphrased)
  if (nrow(ira_bar) > 0) {
    p_ira <- ggplot(ira_bar, aes(x = model, y = percent_agreement, fill = model)) +
      geom_col(width = 0.7) +
      geom_text(aes(label = paste0(round(percent_agreement,1), "%")), vjust = -0.5, size = 4) +
      scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 100)) +
      scale_fill_manual(values = make_hue_palette(nrow(ira_bar)), guide = "none") +
      labs(title = "Direct vs Paraphrased Agreement (by Model)",
           x = "Model", y = "Percent Agreement") +
      theme_minimal(base_size = 12)
    print(p_ira); save_plot(p_ira, "04_ira_by_model", plot_idx); plot_idx <- plot_idx + 1
  }
  
  # 4b) Inter-model agreement (PARAPHRASED)
  if (nrow(inter_bar) > 0) {
    p_inter <- ggplot(inter_bar, aes(x = pair, y = percent_agreement, fill = pair)) +
      geom_col(width = 0.7) +
      geom_text(aes(label = paste0(round(percent_agreement,1), "%")), vjust = -0.5, size = 4) +
      scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 100)) +
      scale_fill_manual(values = make_hue_palette(nrow(inter_bar)), guide = "none") +
      labs(title = "Inter-Model Agreement on PARAPHRASED Results",
           x = "Model Pair", y = "Percent Agreement") +
      theme_minimal(base_size = 12)
    print(p_inter); save_plot(p_inter, "05_inter_model_agreement", plot_idx); plot_idx <- plot_idx + 1
  }
  
  # 5) Duplicates per Technique — colourful bars
  if (nrow(duplicates_by_technique) > 0) {
    dup_palette <- setNames(make_hue_palette(nrow(duplicates_by_technique)),
                            duplicates_by_technique$Technique)
    p_dup <- ggplot(duplicates_by_technique, aes(x = Technique, y = dup_rows_involved, fill = Technique)) +
      geom_col(width = 0.75) +
      scale_y_continuous(labels = comma) +
      scale_fill_manual(values = dup_palette, guide = "none") +
      labs(title = "Rows Involved in Duplicates by Technique (exact OR near-dup)",
           x = "Technique", y = "Duplicate Rows") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p_dup); save_plot(p_dup, "06_duplicates_by_technique", plot_idx); plot_idx <- plot_idx + 1
  }
  
  # ---------------- MINIMAL TABLES ----------------
  View(schema)                            # 6) schema validation [TABLE]
  View(missing_rows_prompts)              # 2) missing prompt rows + indexes [TABLE]
  if (nrow(technique_counts_collapsed) > 0) View(technique_counts_collapsed)   # 1) technique balance [TABLE]
  if (nrow(length_outliers) > 0) View(length_outliers)                         # 3) outlier rows [TABLE]
  if (nrow(duplicates_by_technique) > 0) View(duplicates_by_technique)         # 5) dup rows per technique [TABLE]
  
  # 4) IRA tables
  if (nrow(ira_summary_Grok) > 0)   View(ira_summary_Grok)
  if (nrow(ira_confusion_Grok) > 0) View(ira_confusion_Grok)
  if (nrow(ira_summary_GPT5) > 0)   View(ira_summary_GPT5)
  if (nrow(ira_confusion_GPT5) > 0) View(ira_confusion_GPT5)
  if (nrow(ira_summary_Gemini) > 0) View(ira_summary_Gemini)
  if (nrow(ira_confusion_Gemini) > 0) View(ira_confusion_Gemini)
  
  # Optional: show split leakage pairs table (if any)
  if (nrow(cross_split_near_dups) > 0) View(cross_split_near_dups)
  
  # Return for programmatic reuse
  invisible(list(
    schema = schema,
    missingness_strict = missingness_strict,
    missing_rows_prompts = missing_rows_prompts,
    technique_counts_collapsed = technique_counts_collapsed,
    length_outliers = length_outliers,
    duplicates_by_technique = duplicates_by_technique,
    ira_bar = ira_bar,
    inter_bar = inter_bar,
    ira_summary_Grok = ira_summary_Grok,   ira_confusion_Grok = ira_confusion_Grok,
    ira_summary_GPT5 = ira_summary_GPT5,   ira_confusion_GPT5 = ira_confusion_GPT5,
    ira_summary_Gemini = ira_summary_Gemini, ira_confusion_Gemini = ira_confusion_Gemini,
    cross_split_near_dups = cross_split_near_dups,
    prompt_like_cols = prompt_like_cols,
    rep_text_col = rep_text_col,
    output_dir = output_dir
  ))
}

# ---- Interactive runner ----
try({
  message("Select your dataset (.xlsx/.xls or .csv) ...")
  path <- file.choose()
  message("Reading: ", path)
  df <- read_dataset_auto(path)
  # make a timestamped output folder using dataset base name
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  base  <- tools::file_path_sans_ext(basename(path))
  outdir <- paste0(base, "_plots_", stamp)
  message("Saving plots to: ", normalizePath(outdir, winslash = "/",
                                             mustWork = FALSE))
  results <- validate_prompts_view(
    df,
    rep_text_col = NULL,       # auto-pick representative prompt column
    near_dup_threshold = 0.90, # near-duplicate similarity
    split_leak_threshold = 0.92,
    output_dir = outdir        # <--- all plots saved here
  )
}, silent = FALSE)
