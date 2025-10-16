# validate_prompts_views.R
# ------------------------------------------------------------------
# One-file validator for prompt datasets.
# - Prompts you to select a dataset file (.xlsx/.xls or .csv) when sourced.
# - Reads the file, validates, and opens result tables with View().
# - Computes agreement between DIRECT vs PARAPHRASED *result* labels:
#     - Auto-detects result columns, AND
#     - Runs hard-wired checks for Grok, GPT-5, and Gemini using your exact headers.
# - Writes NOTHING to disk.
# - Works on larger files with the same schema.
#
# Usage:
#   source("validate_prompts_views.R")   # a file chooser will open
# ------------------------------------------------------------------

# ---- Package setup ----
required_pkgs <- c(
  "readxl","dplyr","stringr","text2vec","Matrix","readr","tools",
  "irr","tibble"
)
to_install <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(to_install) > 0) {
  message("Installing missing packages: ", paste(to_install, collapse = ", "))
  install.packages(to_install, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(text2vec)
  library(Matrix)
  library(readr)
  library(tools)
  library(irr)     # Cohen's kappa
  library(tibble)  # rownames_to_column, as_tibble
})

# ---- Helpers ----

# Heuristic: identify likely text columns for prompts
is_text_series <- function(x) {
  if (!is.character(x)) return(FALSE)
  non_null <- x[!is.na(x)]
  if (length(non_null) == 0) return(FALSE)
  mean_len <- mean(nchar(non_null))
  whitespace_rate <- mean(str_detect(non_null, "\\s"))
  return(mean_len > 20 || whitespace_rate > 0.2)
}

# Robust reader for .xlsx/.xls or .csv (first sheet for Excel)
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
  # Convert factor -> character if any
  df[] <- lapply(df, function(col) if (is.factor(col)) as.character(col) else col)
  return(df)
}

# Safe numeric extractor for possibly NULL/length-0 objects
.num_or_na <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  as.numeric(x)
}

# Compute Cohen's kappa + basic agreement stats between two categorical vectors
compute_agreement <- function(a, b) {
  a <- as.character(a); b <- as.character(b)
  keep <- !(is.na(a) | is.na(b) | a == "" | b == "")
  a <- a[keep]; b <- b[keep]
  if (length(a) < 2) return(list(summary=NULL, confusion=NULL))
  # Harmonize label sets
  labs <- sort(unique(c(a, b)))
  a <- factor(a, levels = labs)
  b <- factor(b, levels = labs)
  tab <- as.data.frame.matrix(table(a, b))
  # Agreement rate
  agree <- sum(diag(as.matrix(tab))) / sum(as.matrix(tab))
  # Cohen's kappa (unweighted)
  k <- tryCatch({
    irr::kappa2(cbind(as.character(a), as.character(b)), weight = "unweighted")
  }, error = function(e) NULL)
  
  kappa_val <- if (!is.null(k)) .num_or_na(k$value) else NA_real_
  p_value   <- if (!is.null(k)) .num_or_na(k$p.value) else NA_real_
  se_val    <- if (!is.null(k)) .num_or_na(k$se) else NA_real_
  
  summary_df <- data.frame(
    metric = c("n_pairs","percent_agreement","cohens_kappa","kappa_se","kappa_p_value"),
    value  = c(length(a),
               round(100*agree,2),
               round(kappa_val,4),
               round(se_val,4),
               p_value),
    stringsAsFactors = FALSE
  )
  rownames(summary_df) <- NULL
  list(summary = summary_df, confusion = tab)
}

# Try to auto-detect "result" columns for direct vs paraphrase
# Looks for column names containing label/result/decision/outcome/class and direct vs para keywords.
guess_result_columns <- function(df) {
  cols <- names(df)
  res_pat <- "(label|result|decision|outcome|class|verdict|judg(e)?ment|response|policy)"
  dir_pat <- "(^|[_\\-\\s])(direct|orig(inal)?)([_\\-\\s]|$)"
  par_pat <- "(^|[_\\-\\s])(para(phrase|phrased)?|paraphrased|rephrase(d)?)([_\\-\\s]|$)"
  
  res_cols <- cols[str_detect(tolower(cols), res_pat)]
  dir_cols <- res_cols[str_detect(tolower(res_cols), dir_pat)]
  par_cols <- res_cols[str_detect(tolower(res_cols), par_pat)]
  
  # Fallback: choose first two result-like columns
  if (length(dir_cols) == 0 || length(par_cols) == 0) {
    if (length(res_cols) >= 2) {
      if (length(dir_cols) == 0) dir_cols <- res_cols[1]
      if (length(par_cols) == 0 && length(res_cols) >= 2) par_cols <- res_cols[2]
    }
  }
  list(direct = unique(dir_cols)[1], paraphrase = unique(par_cols)[1])
}

# Main validator (returns a named list and opens View() tables)
validate_prompts_view <- function(df,
                                  rep_text_col = NULL,
                                  near_dup_threshold = 0.90,
                                  split_leak_threshold = 0.92) {
  # ---- Identify prompt-like columns ----
  text_cols <- names(df)[vapply(df, is_text_series, logical(1))]
  priority_cols <- names(df)[str_detect(names(df), regex("prompt|text|instruction|input", ignore_case = TRUE))]
  prompt_like_cols <- intersect(priority_cols, text_cols)
  if (length(prompt_like_cols) == 0) {
    prompt_like_cols <- head(text_cols, 2)
  }
  if (is.null(rep_text_col)) {
    if (length(prompt_like_cols) >= 1) {
      rep_text_col <- prompt_like_cols[1]
    } else {
      stop("No text-like columns found. Please ensure at least one prompt/text column is character type.")
    }
  }
  
  # ---- Schema & Missingness ----
  schema <- data.frame(
    column = names(df),
    dtype = vapply(df, function(x) class(x)[1], character(1)),
    non_null = vapply(df, function(x) sum(!is.na(x)), integer(1)),
    nulls = vapply(df, function(x) sum(is.na(x)), integer(1)),
    stringsAsFactors = FALSE
  )
  schema$null_pct <- round(100 * schema$nulls / pmax(1, (schema$non_null + schema$nulls)), 2)
  missingness <- schema %>% select(column, null_pct) %>% arrange(desc(null_pct))
  
  # ---- Exact Duplicates across prompt-like columns ----
  exact_duplicates <- data.frame()
  if (length(prompt_like_cols) > 0) {
    key <- apply(df[prompt_like_cols], 1, function(row) paste(row, collapse = " || "))
    dup_mask <- duplicated(key) | duplicated(key, fromLast = TRUE)
    if (any(dup_mask, na.rm = TRUE)) {
      exact_duplicates <- data.frame(
        row_index = which(dup_mask) - 1,  # zero-based display
        group_id = as.integer(factor(key[dup_mask])) - 1,
        preview = substr(key[dup_mask], 1, 200),
        stringsAsFactors = FALSE
      ) %>% arrange(group_id, row_index)
    }
  }
  
  # ---- Near-Duplicates in representative prompt column ----
  near_duplicates <- data.frame()
  if (!is.null(rep_text_col)) {
    texts <- df[[rep_text_col]]
    texts <- if (is.character(texts)) texts else as.character(texts)
    texts[is.na(texts)] <- ""
    norm <- tolower(stringr::str_squish(texts))
    if (length(norm) > 1 && any(nchar(norm) > 0)) {
      it <- itoken(norm, progressbar = FALSE)
      vocab <- create_vocabulary(it, ngram = c(3L, 6L), stopwords = character(0))
      vectorizer <- vocab_vectorizer(vocab)
      dtm <- create_dtm(it, vectorizer)
      if (nrow(dtm) > 1) {
        sim <- text2vec::sim2(dtm, method = "cosine", norm = "l2")
        coords <- which(sim >= near_dup_threshold, arr.ind = TRUE)
        coords <- coords[coords[,1] < coords[,2], , drop = FALSE]
        if (nrow(coords) > 0) {
          near_duplicates <- data.frame(
            i = coords[,1] - 1,
            j = coords[,2] - 1,
            cosine_sim = sim[coords],
            i_preview = substr(texts[coords[,1]], 1, 200),
            j_preview = substr(texts[coords[,2]], 1, 200),
            stringsAsFactors = FALSE
          ) %>% arrange(desc(cosine_sim))
        }
      }
    }
  }
  
  # ---- Length Outliers (z > 3) across prompt-like columns ----
  length_outliers <- data.frame()
  if (length(prompt_like_cols) > 0) {
    for (c in prompt_like_cols) {
      s <- df[[c]]
      s <- if (is.character(s)) s else as.character(s)
      s[is.na(s)] <- ""
      nchar_vec <- nchar(s)
      sdv <- stats::sd(nchar_vec)
      if (sdv > 0) {
        z <- (nchar_vec - mean(nchar_vec)) / sdv
        idx <- which(z > 3)
        if (length(idx) > 0) {
          tmp <- data.frame(
            column = c,
            row_index = idx - 1,
            char_len = nchar_vec[idx],
            preview = substr(s[idx], 1, 200),
            stringsAsFactors = FALSE
          )
          length_outliers <- dplyr::bind_rows(length_outliers, tmp)
        }
      }
    }
  }
  
  # ---- Category Coverage (first category-like column) ----
  category_counts <- data.frame()
  category_cols <- names(df)[str_detect(names(df), regex("category|attack[_\\s-]*type", ignore_case = TRUE))]
  if (length(category_cols) > 0) {
    cat_col <- category_cols[1]
    cats <- as.character(df[[cat_col]])
    category_counts <- as.data.frame(table(cats), stringsAsFactors = FALSE) %>%
      arrange(desc(Freq)) %>%
      rename(!!cat_col := cats, count = Freq)
  }
  
  # ---- Paired-field identical check (first two prompt-like columns) ----
  paired_field_check <- data.frame()
  if (length(prompt_like_cols) >= 2) {
    a <- prompt_like_cols[1]; b <- prompt_like_cols[2]
    A <- df[[a]]; B <- df[[b]]
    A <- if (is.character(A)) A else as.character(A)
    B <- if (is.character(B)) B else as.character(B)
    A <- tolower(stringr::str_trim(replace(A, is.na(A), "")))
    B <- tolower(stringr::str_trim(replace(B, is.na(B), "")))
    eq <- sum(A == B)
    paired_field_check <- data.frame(
      field_A = a, field_B = b,
      identical_pairs = eq,
      total_pairs_checked = nrow(df),
      identical_pct = round(100 * eq / max(1, nrow(df)), 2),
      stringsAsFactors = FALSE
    )
  }
  
  # ---- Split Leakage (if split-like column exists) ----
  cross_split_near_dups <- data.frame()
  split_cols <- names(df)[str_detect(names(df), regex("\\b(split|fold|partition|set)\\b", ignore_case = TRUE))]
  if (!is.null(rep_text_col) && length(split_cols) > 0) {
    split_col <- split_cols[1]
    texts <- df[[rep_text_col]]
    texts <- if (is.character(texts)) texts else as.character(texts)
    texts <- tolower(stringr::str_squish(replace(texts, is.na(texts), "")))
    if (length(texts) > 1 && any(nchar(texts) > 0)) {
      it <- itoken(texts, progressbar = FALSE)
      vocab <- create_vocabulary(it, ngram = c(3L, 6L), stopwords = character(0))
      vectorizer <- vocab_vectorizer(vocab)
      dtm <- create_dtm(it, vectorizer)
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
              i = coords[,1] - 1,
              i_split = splits[coords[,1]],
              j = coords[,2] - 1,
              j_split = splits[coords[,2]],
              cosine_sim = sim[coords],
              stringsAsFactors = FALSE
            ) %>% arrange(desc(cosine_sim))
          }
        }
      }
    }
  }
  
  # ---- Auto-detected agreement (fallback) ----
  ira_summary <- data.frame()
  ira_confusion <- data.frame()
  ira_cols <- guess_result_columns(df)
  direct_col <- ira_cols$direct
  para_col   <- ira_cols$paraphrase
  if (!is.null(direct_col) && !is.null(para_col) &&
      direct_col %in% names(df) && para_col %in% names(df)) {
    A <- df[[direct_col]]; B <- df[[para_col]]
    res <- compute_agreement(A, B)
    if (!is.null(res$summary)) ira_summary <- res$summary
    if (!is.null(res$confusion)) {
      ira_confusion <- tibble::as_tibble(rownames_to_column(as.data.frame(res$confusion), var = "direct\\paraphrase"))
    }
  }
  
  # ---- Hard-wired agreement checks per model (your exact headers) ----
  ira_summary_Grok <- ira_confusion_Grok <-
    ira_summary_GPT5 <- ira_confusion_GPT5 <-
    ira_summary_Gemini <- ira_confusion_Gemini <- data.frame()
  
  model_result_pairs <- list(
    Grok = list(
      direct = "Test Result Grok - Direct",
      paraphrase = "Test Result Grok - paraphrased"
    ),
    GPT5 = list(
      direct = "Test Result (GPT-5) - Direct",
      paraphrase = "Test Result (GPT-5) - paraphrased"
    ),
    Gemini = list(
      direct = "Test Result Gemini - Direct",
      paraphrase = "Test Result Gemini - paraphrased"
    )
  )
  for (m in names(model_result_pairs)) {
    cols <- model_result_pairs[[m]]
    if (all(c(cols$direct, cols$paraphrase) %in% names(df))) {
      a <- as.character(df[[cols$direct]])
      b <- as.character(df[[cols$paraphrase]])
      keep <- !(is.na(a) | is.na(b) | a == "" | b == "")
      a <- a[keep]; b <- b[keep]
      if (length(a) >= 2) {
        labs <- sort(unique(c(a,b)))
        ta <- factor(a, levels = labs)
        tb <- factor(b, levels = labs)
        tab <- as.data.frame.matrix(table(ta, tb))
        agree <- sum(diag(as.matrix(tab))) / sum(as.matrix(tab))
        k <- tryCatch({
          irr::kappa2(cbind(as.character(ta), as.character(tb)), weight = "unweighted")
        }, error = function(e) NULL)
        
        k_val <- if (!is.null(k)) .num_or_na(k$value)   else NA_real_
        k_se  <- if (!is.null(k)) .num_or_na(k$se)      else NA_real_
        k_p   <- if (!is.null(k)) .num_or_na(k$p.value) else NA_real_
        
        summ <- data.frame(
          metric = c("model","n_pairs","percent_agreement","cohens_kappa","kappa_se","kappa_p_value"),
          value  = c(m,
                     length(a),
                     round(100*agree,2),
                     round(k_val,4),
                     round(k_se,4),
                     k_p),
          stringsAsFactors = FALSE
        )
        conf <- tibble::as_tibble(rownames_to_column(as.data.frame(tab), var = "direct\\paraphrase"))
        if (m == "Grok")   { ira_summary_Grok <- summ;   ira_confusion_Grok <- conf }
        if (m == "GPT5")   { ira_summary_GPT5 <- summ;   ira_confusion_GPT5 <- conf }
        if (m == "Gemini") { ira_summary_Gemini <- summ; ira_confusion_Gemini <- conf }
      }
    }
  }
  
  # ---- Console summary ----
  cat("Rows:", nrow(df), " Columns:", ncol(df), "\n")
  if (nrow(exact_duplicates) > 0) cat("Exact duplicate rows:", length(unique(exact_duplicates$row_index)), "\n") else cat("Exact duplicate rows: 0\n")
  if (nrow(near_duplicates) > 0) cat("Near-duplicate pairs:", nrow(near_duplicates), "\n") else cat("Near-duplicate pairs: 0\n")
  if (nrow(length_outliers) > 0) cat("Length outlier rows:", length(unique(length_outliers$row_index)), "\n") else cat("Length outlier rows: 0\n")
  if (nrow(cross_split_near_dups) > 0) cat("Cross-split near-dup pairs:", nrow(cross_split_near_dups), "\n") else if (length(split_cols) > 0) cat("Cross-split near-dup pairs: 0\n")
  if (nrow(ira_summary) > 0) cat("Auto-detected direct vs paraphrase agreement pairs:", ira_summary$value[ira_summary$metric=="n_pairs"], "\n")
  if (nrow(ira_summary_Grok) > 0)   cat("Grok agreement pairs:", ira_summary_Grok$value[ira_summary_Grok$metric=="n_pairs"], "\n")
  if (nrow(ira_summary_GPT5) > 0)   cat("GPT-5 agreement pairs:", ira_summary_GPT5$value[ira_summary_GPT5$metric=="n_pairs"], "\n")
  if (nrow(ira_summary_Gemini) > 0) cat("Gemini agreement pairs:", ira_summary_Gemini$value[ira_summary_Gemini$metric=="n_pairs"], "\n")
  
  # ---- Open tables in Viewer ----
  View(schema); View(missingness)
  if (nrow(category_counts) > 0) View(category_counts)
  if (nrow(exact_duplicates) > 0) View(exact_duplicates)
  if (nrow(near_duplicates) > 0) View(near_duplicates)
  if (nrow(length_outliers) > 0) View(length_outliers)
  if (nrow(paired_field_check) > 0) View(paired_field_check)
  if (nrow(cross_split_near_dups) > 0) View(cross_split_near_dups)
  if (nrow(ira_summary) > 0) { View(ira_summary); if (nrow(ira_confusion) > 0) View(ira_confusion) }
  if (nrow(ira_summary_Grok) > 0)   { View(ira_summary_Grok);   if (nrow(ira_confusion_Grok) > 0)   View(ira_confusion_Grok) }
  if (nrow(ira_summary_GPT5) > 0)   { View(ira_summary_GPT5);   if (nrow(ira_confusion_GPT5) > 0)   View(ira_confusion_GPT5) }
  if (nrow(ira_summary_Gemini) > 0) { View(ira_summary_Gemini); if (nrow(ira_confusion_Gemini) > 0) View(ira_confusion_Gemini) }
  
  invisible(list(
    schema = schema,
    missingness = missingness,
    category_counts = category_counts,
    exact_duplicates = exact_duplicates,
    near_duplicates = near_duplicates,
    length_outliers = length_outliers,
    paired_field_check = paired_field_check,
    cross_split_near_dups = cross_split_near_dups,
    ira_summary_auto = ira_summary,
    ira_confusion_auto = ira_confusion,
    ira_summary_Grok = ira_summary_Grok,
    ira_confusion_Grok = ira_confusion_Grok,
    ira_summary_GPT5 = ira_summary_GPT5,
    ira_confusion_GPT5 = ira_confusion_GPT5,
    ira_summary_Gemini = ira_summary_Gemini,
    ira_confusion_Gemini = ira_confusion_Gemini,
    prompt_like_cols = prompt_like_cols,
    rep_text_col = rep_text_col
  ))
}

# ---- Interactive runner (auto-opens file chooser) ----
try({
  message("Select your dataset (.xlsx/.xls or .csv) ...")
  path <- file.choose()
  message("Reading: ", path)
  df <- read_dataset_auto(path)
  results <- validate_prompts_view(df,
                                   rep_text_col = NULL,
                                   near_dup_threshold = 0.90,
                                   split_leak_threshold = 0.92)
}, silent = FALSE)

