# load required librarie
library("tcltk2")
library("Matrix")
# function definitions


Content_Words <- c("ADJ", "ADV", "NOUN", "NUM", "PROPN", "VERB", "X") # decie if "X" is 
Function_Words <- c("ADP", "AUX", "CCONJ", "DET", "INTJ", "PART", "PRON", "SCONJ")
Non_Words <- c("PUNCT", "SYM")
Punctuation_Marks = c("?", "!", ":", ";", "-", "(", ")", "[", "]", "'", "\"",
                      "/", ",", ".", "—", "…", "―", "«", "»")
Cons <- "[qwrtzpsdfghjklxcvbnmśłżźćńj]"
Vow <- "[aeiouáéűąęąęåäáæøèõéêëîïôœùûüåéíóúyůýěöüóóäöüypаеиоуъюαεηήιουωя]"

#' Partiotions the annotation file into chunks of 2000 tokens while preserving sentence boundaries. Chunks can have slightly
#' over 2000 tokens in order to preserve sentence boundaries.
#' Credits go to the Stackoverflow community for providing help with the code for this function 
#' (https://stackoverflow.com/questions/51291288/grouping-n-or-more-observations-in-data-table-without-interrputing-sequences-of)
#' TODO: Add parameter to choose on which column to perform partitioning
#' @param vec The vector along which partitions are made (e.g. doc_sent_id)
#' @param size The size of chunks (integer)
#' @return Vector of running ID numbers for each partition
#' @export
PartitionCorpus <- function(vec, part.size = 2000, shuffle = TRUE) { # min.text.len = 100, 
  if (length(vec) < part.size) {
    op <- rep(1, length(vec))
  } else {
    x <- data.table(doc_sent_id = vec)
    x[, grp := .GRP, by = .(doc_sent_id)]
    i <- 1
    
    x[1:part.size, partition := i] # set chunk = i for first five rows
    x[grp == last(grp[!is.na(partition)]), partition := i] # make chunk = i for any rows with same type
    
    while((last.I <- x[, last(.I[!is.na(partition)])]) < nrow(x)) {
      i <- i + 1
      x[last.I + seq(min(c(part.size, nrow(x) - last.I))), partition := i] # set chunk = i for next five rows
      x[grp == last(grp[!is.na(partition)]), partition := i] # make chunk = i for any rows with same type
    }
    op <- x$partition
  }
  op <- as.character(op)
  return (op)
}


ShowSize <- function(x) {
  print(object.size(x), units = "auto")
}

# This function keeps the row containing the original unsplitted multiword token (e.g. "chciałbym") and deletes
# subsequent row containing parts of the multiword token (e.g. "chciał", "by", "m"), completes missing
# information for unsplitted token and renumbers token IDs
JoinMultiwordTokens <- function(x) {
  # Obtain vector of positions (i.e. row numbers) of all multiword tokens in annotation data.table
  #positions <- which(is.na(annotation$lemma))
  positions <- which(grepl("-", x$token_id))
  #positions <- which(grepl("-", txt1$token_id))
  
  
  if (length(positions) > 0) {
    # Obtain vector of start positions of rows to be deleted (i.e. rows containing splitted tokens)
    remove_from <- positions + 1
    
    # Obtain vector of ranges of multiword tokens from token_id (e.g. 12-14) and calculate number
    # of rows to be deleted from token ranges
    ranges <- x$token_id[positions]
    ranges <- sapply(ranges, function(z) { abs(eval(parse(text = z))) + 1 }, simplify = TRUE, USE.NAMES = FALSE)
    
    # Obtain vector of end positions of rows to be deleted
    remove_to <- positions + ranges
    
    # Obtain vector of all row indices to be deleted by adding mising values in range between index pairs at same
    # positions in vectors remove_from and remove_to
    remove_indices <- unlist(do.call(c, list(mapply( seq, remove_from, remove_to))))
    
    # Add missing information to multiword token from next row
    missing_lemmas <- txt_next(x$lemma)[positions]
    missing_upos <- txt_next(x$upos)[positions]
    missing_xpos <- txt_next(x$xpos)[positions]
    missing_feats <- txt_next(x$feats)[positions]
    x[positions, c("lemma", "upos", "xpos", "feats")] <- data.frame(missing_lemmas,
                                                                    missing_upos,
                                                                    missing_xpos,
                                                                    missing_feats)
    # Drop rows containing splitted parts of multiword tokens
    x <- x[-remove_indices]
    
    # Renumber token IDs
    x$token_id <- NULL
    x[, token_id := sequence(.N), by = .(doc_id, sentence_id)]
    #.annotation[, token_id:=seq(.N), by=list(cumsum(c(0,abs(diff(sentence_id)))))] # ORIGINAL
    #.annotation[, yID := seq(.N), by=list(cumsum(c(0,abs(diff(sentence_id)))))] # ORIGINAL
    
    
    #p <- .annotation[, .(x =seq(.N)), by=list(cumsum(c(0,abs(diff(sentence_id)))))] ### DEBUG ONLY!!!!
    #
    #  re-establish initial column order
    x <- subset(x, select=c(1:3, token_id, 5:which(colnames(x)=="token_id")-1))
    
    #if (Keep_Text == TRUE) {
    #  x <- x[ , c(1:4, 14, 5:13)] 
    #} else {
    #  x <- x[ , c(1:3, 9, 4:8)] #  re-establish initial column order
    #  
    #}
    #annotation$token_id <- toString(annotation$token_id)
  }

  return(x)
}


SimpleToComplex <- function(x, by = c("partition", "doc_id")) {
  by <- match.arg(by)
  x <- x[!is.na(token_id), ]
  sent_ratios <- x[, .(verbcount = sum(upos == "VERB")), by = .(get(by), doc_sent_id)]
  colnames(sent_ratios)[1] <- "id"
  sent_ratios <- sent_ratios[, .(simp_compl = sum(verbcount <= 1) / (sum(verbcount > 1) + 1), # add one to avoind division by zero
                                 simp_tot = sum(verbcount <= 1) / .N,
                                 compl_tot = sum(verbcount > 1) / .N),
                             by = .(id)] # consider adding 0-verb sentences to > change simple to == 1 rather than <= 1
  return(sent_ratios)
}


ExplicitNaming <- function(x, by = c("partition", "doc_id")) {
  by <- match.arg(by)
  en <- x[, .(pron_to_prop = CountPOS(upos, c("PRON")) / (CountPOS(upos, c("PROPN")) + 1)),
          by = .(get(by))]
  colnames(en)[1] <- "id"
  return(en)
}


SingleNaming <- function(x, by = c("partition", "doc_id")) {
  by <- match.arg(by)
  sn <- x[, .(single_names_ratio = NROW(upos[upos == "PROPN" &
                                               txt_next(upos) != "PROPN" &
                                               txt_previous(upos) != "PROPN"]) / NROW(upos[upos != "PUNCT"])),
          by = .(get(by))]
  colnames(sn)[1] <- "id"
  return(sn)
}

SpecifyParameters <- function() { # To set extraction parameters interactively, change FALSE to TRUE in next line
  lang_choices <- c("BG", "CS", "DA", "DE", "EL", "EN", "ES", "ET", "FI",
                    "FR", "HU", "IT", "LT", "LV", "NL", "PL", "PT", "RO",
                    "SK", "SL", "SV")
  indir <- tk_choose.dir(caption = "Select INPUT Folder")
  outdir <- tk_choose.dir(caption = "Select OUTPUT Folder")
  
  
  langs <- readline(prompt = paste0("One or more languages from {",
                                    paste(lang_choices, collapse="|"),
                                    "}, comma-separated without blanks: "))
  langs <- unlist(strsplit(langs, ","))
  
  # Repeat input if unsupported language specified
  while (length(langs) < 1 | any(!(langs %in% lang_choices))) { # if any of specified languages not in lang.choices
    langs <- unlist(strsplit(readline(prompt = "One or more unsupported languages selected! Please revise your choice: "),
                             ","))
  }
  
  
  # Specify method for calculation of Most Frequent Words
  # and terminate program if unsupported method specified
  cat("\n")
  #mfw.method <- readline(prompt = paste0("Method to Calculate MFWs, choose from [",
  #                                       paste(mfw.choices, collapse = "|"),
  #                                       "]: "))
  
  ## Repeat input if unsupported method specified
  #while (length(mfw.method) < 1 | !(mfw.method %in% mfw.choices)) { # if specified method not in mfw.choices
  #  mfw.method <- (readline(prompt = "Unsupported MFW method selected! Please revise your choice: "))
  #}
  #cat("\n")
  
  #if (mfw.method == "wordlist") {
  #  mfw.parameter <- tk_choose.dir(caption = "Select Folder with Wordlists (wordlist_xy.txt)")
  #} else if (mfw.method == "relative") {
  #  mfw.parameter <- as.numeric(readline(prompt = "Threshold for mean relative frequency for MFW selection [recommended value: 0.001]: "))
  #  if (is.na(mfw.parameter) | grepl("[:alpha:]", mfw.parameter) | as.numeric(mfw.parameter >= 1)) {
  #    mfw.parameter <- 0.001
  #  }
  #} else {
  #  mfw.parameter <- as.numeric(readline(prompt = "Number of MFWs: "))
  #  if (is.na(mfw.parameter) | grepl("[:alpha:]", mfw.parameter) | as.numeric(mfw.parameter < 1)) {
  #    mfw.parameter <- 100
  #  }
  #}
  
  # Parameter supplied to selected MFW extraction method, corresponding to
  #  1) threshold for the mean relative frequency of words across whole corpus if selected method is 'relative', or
  #  2) n most frequent words in sorted frequency list across whole corpus if selected method is 'absolute', or
  #  3) path to an external wordlist in plain text (one word per line) if
  #     selected method is 'wordlist'.
  cat("\n")
  use_tfidf <- readline("Use TFIDF weighting for DTM of MFWs [y/n]? ")
  
  if (use_tfidf == "y") {
    use_tfidf <- TRUE
  } else {
    use_tfidf <- FALSE
  }
  
  # Decide whether to store word list and texts as compressed .RData binaries
  cat("\n")
  save_binaries <- readline("Save supplementary data as binaries [y/n]? ")
  if (save_binaries == "y") {
    save_binaries <- TRUE
    } else {
      save_binaries <- FALSE
    }
  
  # Decide whether to retain multiword tokens rather than splitting them (postprocesing function)
  cat("\n")
  retain_multiwordtokens <- readline("Retain multiword tokens [y/n]? ")
  if (retain_multiwordtokens == "y") {
    retain_multiwordtokens <- TRUE
  } else {
    retain_multiwordtokens <- FALSE
  }
  
  # Decide whether to lowercase tokens in document-level feature extraction
  cat("\n")
  lowercase_tokens <- readline("Lowercase tokens in document-level features [y/n]? ")
  if (lowercase_tokens == "y") {
    lowercase_tokens <- TRUE
  } else {
    lowercase_tokens <- FALSE
  }
  
  # Decide whether to retain ignore punctuation in document-level feature extraction
  cat("\n")
  ignore_punctuation <- readline("Ignore punctuation in document-level features [y/n]? ")
  if (ignore_punctuation == "y") {
    ignore_punctuation <- TRUE
  } else {
    ignore_punctuation <- FALSE
  }
  
  # Decide whether to add sentence beginning/end markers to annotation
  cat("\n")
  mark_sent_bord <- readline("Mark sentence beginning/end [y/n]? ")
  if (ignore_punctuation == "y") {
    mark_sent_bord <- TRUE
  } else {
    mark_sent_bord <- FALSE
  }
  
  
  
  
  return(list(indir, outdir, langs, use_tfidf, save_binaries, retain_multiwordtokens,
              lowercase_tokens, ignore_punctuation, mark_sent_bord))
}


CreateOutputDirs <- function(outdir) {
  path_texts <- paste0(outdir, "/texts")
  path_frequencytables <- paste0(outdir, "/frequencies")
  if (!dir.exists(path_texts)) {
    dir.create(path_texts)
  }
  if (!dir.exists(path_frequencytables)) {
    dir.create(path_frequencytables)
  }
}

ReadTextfile <- function(filename) {
  current_text <- readLines(filename, n = -1, encoding = "UTF-8", warn = FALSE)
  current_text <- paste(current_text, collapse = "\n\n") # paragraphs indicated by \n\n, c.f. https://github.com/bnosac/udpipe/issues/15
  return(current_text)
}

RemoveEmptyTexts <- function(texts.l) {
  nonemty_texts.v <- sapply(texts.l, function(x) { length(x) > 0 })
  files.v <- files.v[nonemty_texts.v]
  texts.l <- texts.l[nonemty_texts.v]
  
}

GetCorpusTypeFromFilename <- function(filenames) {
  corpustypes.v <- grepl("non-translated", filenames)
  corpustypes.v <- gsub("TRUE", "ORIG", corpustypes.v)
  corpustypes.v <- gsub("FALSE", "TRANS", corpustypes.v)
  return(corpustypes.v)
}

GetMetadataFromIDs <- function(ids) {
  meta <- data.table(id = ids)
  meta[, sl := substr(id, 1,2)]
  if (grepl(".txt", ids[1])) { # if document name (e.g. FR-DE/97-02-19_115-de.txt)
    meta[, tl := toupper(regmatches(id, regexpr("[[:lower:]]{2}", id)))]
  } else { # if partition rather than doc name (e.g. FR-DE_2000_0237)
    meta[, tl := sub("[[:upper:]]{2}-", "", sub("_.*", "", id))]
  }
  meta[, year := GetYearFromID(id)]
  meta[, type := ifelse(sl == tl, "ORIG", "TRANS")]
  meta[, type_adv := ifelse(sl == tl, "ORIG",
                                 ifelse(as.integer(year) > 2003, "T_ID", "T_DIR" ))]
  meta[, phase := ifelse(as.integer(year) > 2003, "AFTER", "BEFORE" )]
  return(meta)
}

# Get source language SL from filename
GetSLFromFilename <- function(filenames) {
  # First, extract 2 caps before optional - optional cap optional cap followed by /digit (digit marks beginning of base file name)
  sls.c <- regmatches(filenames, regexpr("([A-Z]{2}-?[A-Z]?[A-Z]?/[0-9])", filenames))
  # Second, use substr() to strip  string after - and before /digit
  sls.c <- substr(regmatches(filenames, regexpr("([A-Z]{2}-?[A-Z]?[A-Z]?/[0-9])", filenames)),1, 2)
  return(sls.c)
  
}

GetYearFromID <- function(x) {
  if (grepl(".txt", x[1])) {
    year <- regmatches(x, regexpr("[[:digit:]]{2}", x))
    year <- ifelse(substr(year, 1, 1) == "9",
                   paste("19", year, sep=""),
                   paste("20", year, sep=""))
  } else {
    year <- regmatches(x, regexpr("[[:digit:]]{4}", x))
  }
  return(year)
}


GetDatesFromFilename <- function(filenames) {
  date.c <- regmatches(filenames, regexpr("(/[0-9]{2})-[0-9]{2}-[0-9]{2}", filenames))
  year.c <- substr(date.c, 2,3)
  month.c <- substr(date.c, 5,6)
  day.c <- substr(date.c, 8,9)
  year.c <- sapply(year.c, function(year) {
    if (grepl("^9", year)) { paste("19", year, sep="") }
    else { paste("20", year, sep="") }
  })
  dates.l <- list(year = year.c,
                month = month.c,
                day = day.c)
  return(dates.l)
}



CalculateTTR <- function (tokens, lowercase = FALSE) {
  if (lowercase == TRUE) {
    tokens <- tolower(tokens)
  }
  return(length(unique(tokens))/length(tokens))
}


CalculateYulesK <- function(tokens, lowercase = FALSE) {
  if (lowercase == TRUE) {
    tokens <- tolower(tokens)
  }
  m1 = length(tokens)
  freq_tab <- data.table(table(tokens))[, .(count=.N), by=N]
  m2 <- freq_tab[, sum((N^2)*count)]
  k <- 10000 * ((m2 - m1) / (m1 * m1))
  #i <- (m1 * m1) / (m2 - m1)
  #yules <- list("k" = k, "i" = i)
  yules <- list("k" = k)
  return(yules)
  # K = 10000 * (M2-M1) / (M1*M1)
  # M1 = all words in text
  # M2 = Sum of products of each observed frequency² * that frequency
}

CalculateMTLD <- function(tokens, lowercase = FALSE, threshold = 0.72) {
  # Function to calculate Measure of Textual Lexical Diversity (cf. McCarthy, P.M. & Jarvis, S. (2010): MTLD, vocd-D, and HD-D: A validation study of sophisticated approaches to lexical diversity assessment. In: Behavior Research Methods 42/2, pp. 381-392. doi:10.3758/BRM.42.2.381 )
  # To Do: Documentation
  if (lowercase == TRUE) {
    tokens <- tolower(tokens)
  }
  #print(tokens)
  mtld.score <- 0.0 # Initializing variable to be returned
  RunMTLD <- function(tokens, threshold) {
    # Inner function called within CalculateMTLD(tokens, threshold)
    # To Do: Documentation
    factors <- 0
    start.token.position <- 1 # Start calculations from 1st token in text
    for (i in 1:length(tokens)) {
      # Calculation from start token to current token
      ttr <- length(unique(tokens[start.token.position : i])) / length(tokens[start.token.position : i])
      if (ttr < threshold) {
        factors <- factors +1 # increase factor by 1 if ttr below threshold
        ttr <- 1
        start.token.position <- i + 1 # setting new start token to token after current one
      }
    }
    remainder = 1 - ttr
    excess.range = 1 - threshold
    partial.factor = remainder / excess.range
    factors <- factors + partial.factor
    return(factors)
  } # End of inner function definition
  
  mtld.forward <- RunMTLD(tokens, threshold) # MTLD calculated twice: from start to end and vice versa
  mtld.reverse <- RunMTLD(rev(tokens), threshold)
  val1 <- length(tokens) / mtld.forward
  val2 <- length(tokens) / mtld.reverse
  mtld.score <- mean(c(val1, val2))
  if (mtld.forward == 0 | mtld.reverse == 0) {
    mtld.score <- 0
  }
  return(mtld.score)
}


### AVERAGE NUMBER OF TOKENS PER SENTENCE IN A DOCUMENT
CalculateMeanTokensPerSentence <- function(x, by = c("partition", "doc_id")) {
  # Get first line of each (doc_id, sentence_id) combination by subsetting
  # and on this subset, aggregate means on each document for columns containing
  # sentence counts with and without punctuation tokens
  by = match.arg(by)
  mean_toks_per_sent.dt <- x[, .SD[1], by = list(get(by), doc_sent_id)]
  mean_toks_per_sent.dt <- mean_toks_per_sent.dt[,
                                         .(mean_tok_nr = mean(nr_tokens),
                                           mean_tok_nr_noPunct = mean(nr_tokens_noPunct)),
                                         by=get(by)]
  colnames(mean_toks_per_sent.dt)[1] <- "id"
  return(mean_toks_per_sent.dt)
  # Alternatively:
  # 1) calculate sentence length for each unique sentence:
  # sent_lengths.dt <- annotation.dt[, .(nr_of_tokens=NROW(token)), by = list(doc_id, sentence_id)] # to arrange length counts by doc_sent_id rather than document name, replace list(doc_sent_id)
  # mean_sentence_lengths.dt <- sent_lengths.dt[, .(mean_sentence_length = mean(nr_of_tokens)), by=doc_id]
  # 2) aggregate those counts grouped by document name
  # 3) Repeat previous two steps for non-punctuation tokens only, too
}

CalculateMeanSentenceLength <- function(x, by = c("partition", "doc_id")) {
  # Get first line of each (doc_id, sentence_id) combination by subsetting
  # and on this subset, aggregate means on each document for columns containing
  # sentence charactercounts with and without punctuation tokens
  by <- match.arg(by)
  mean_sent_len.dt <- x[, .SD[1], by = .(get(by), doc_sent_id)]
  mean_sent_len.dt <- mean_sent_len.dt[,
                                       .(mean_sent_len = mean(sent_len),
                                         mean_sent_len_noPunct = mean(sent_len_noPunct)),
                                       by=get(by)]
  colnames(mean_sent_len.dt)[1] <- "id"
  return(mean_sent_len.dt)
}

CalculateMeanTokenLength <- function(x, by = c("partition", "doc_id")) {
  by = match.arg(by)
  mean_tok_len.dt <- x[, .(mean_tok_len = mean(token_len),
                           mean_tok_len_noPunct = mean(token_len[upos != "PUNCT"])),
                       by=get(by)]
  colnames(mean_tok_len.dt)[1] <- "id"
  return(mean_tok_len.dt)
}

CalculateLexDens <- function(x, by = c("partition", "doc_id")) {
  # Calculates Lexical Density, i.e. ratio content words to all words
  by = match.arg(by)
  ld <- x[, .(lex_dens = CountPOS(upos, Content_Words) / NROW(upos),
              lex_dens_noPunct = CountPOS(upos, Content_Words) / NROW(upos[upos != "PUNCT"])),
          by = get(by)]
  colnames(ld)[1] <- "id"
  return(ld)
}



CalculateLexDivMeasures <- function(x, lower = FALSE, ignore.punct = FALSE,
                                    by = c("partition", "doc_id")) {
  by <- match.arg(by)
  t_lexdiv <- proc.time()
  cat ("Calculating lexical diversity measures...")
  if (ignore.punct == TRUE) {
    lexdv.dt <- x[upos != "PUNCT", .(ttr = CalculateTTR(token, lower),
                                     mtld = CalculateMTLD(token, lower, 0.72),
                                     yules_k = CalculateYulesK(token, lower)$k),
                                    #yules_i = CalculateYulesK(token, lower)$i
                  by = .(get(by))]
    } else {
      lexdv.dt <- x[, .(ttr = CalculateTTR(token, lower),
                        mtld = CalculateMTLD(token, lower, 0.72),
                        yules_k = CalculateYulesK(token, lower)$k),
                        #yules_i = CalculateYulesK(token, lower)$i
                    by = .(get(by))]
      }
  colnames(lexdv.dt)[1] <- "id"
  
  t_lexdiv = (proc.time() - t_lexdiv)[3]/60
  cat("  --> DONE in ", t_lexdiv, "minutes!\n\n")
  return(lexdv.dt)
  }


LoadReferenceCorpus <- function(lang, model) {
  ptm_ref <- proc.time()
  fn.out.refcorp <- gsub("//", "/",
                         paste0(Dir_ReferenceCorpora, "/", tolower(lang), "_refcorp.rds"))
  
  #ref_corp <- paste0(Dir_ReferenceCorpora, tolower(lang), "_refcorp.txt")
  ref_corp <- file.path(Dir_ReferenceCorpora,
                        paste0(tolower(lang), "_refcorp.txt"))
  
  # Load reference corpus
  cat("\nReading and annotating reference corpus ", ref_corp, " ...  ", sep = "")
  #fnames.v <- dir(Dir_In, recursive = T,  full.names = T)
  #fnames.v <- gsub("//", "/", fnames.v)
  ref_corp <- as.list(readLines(ref_corp, n = -1, encoding = "UTF-8"))
  names(ref_corp) <- seq(1:length(ref_corp))
  cat("\n  Created ref_corp of length ", length(ref_corp), " ...", sep = "")
  # Tokenise and POS-tag it
  cat("\n  ~~~ Parsing with model ", length(model), "\n")
  #cat("~", model$file, "\n")
  
  #ref_corp <- mclapply(ref_corp, FUN = ParseSplits2, t = "dt", mc.cores = 4)
  ref_corp <- mclapply(ref_corp, FUN = ParseSplits, t = "lst", model, mc.cores = 4)
  cat("\n  Merging Parsed Splitted Reference Corpus ...\n")
  ref_corp <- rbindlist(ref_corp)
  #ref_corp <- as.data.table(udpipe_annotate(model, x = ref_corp,
  #                                          tagger = "default", parser = "none"))
  
  # Unsplit multi-word-tokens if parameter set
  #if (Retain_MultiWordTokens == TRUE) {
  #  ref_corp <- JoinMultiwordTokens(ref_corp)
  #}
  
  # Remove not needed columns from reference corpus
  #ref_corp[, c("sentence", "xpos", "feats", "head_token_id", "dep_rel", "deps", "misc") := NULL]
  ref_corp <- ref_corp[!grepl("(http:)|(www\\.)", ref_corp$token)]
  t_ref = (proc.time() - ptm_ref)[3]/60
  saveRDS(ref_corp, file = fn.out.refcorp)
  cat("  --> DONE in ", t_ref, " minutes, ", nrow(ref_corp),
      " reference tokens loaded and saved to ", fn.out.refcorp, "!\n\n",
      sep = "")
  return(ref_corp)
}

ParseSplits <- function(txt, t = c("lst", "dt"), model) {
  if (t == "lst") {
    x <- as.data.table(udpipe_annotate(model, x = txt,
                                       tagger = "default",
                                       parser = "none"))
    x[, c("sentence", "head_token_id", "dep_rel", "deps", "misc") := NULL]
  } else {
    x <- as.data.table(udpipe_annotate(model, x = txt$text,
                                       doc_id = txt$doc_id,
                                       tagger = "default",
                                       parser = "none"))
    if (Keep_Text == FALSE) {
      x[, sentence := NULL]
    }
    # Remove unneeded columns if no dependency parsing features used
    if (Dependency_Parsing == FALSE) {
      x[, c("head_token_id", "dep_rel", "deps", "misc") := NULL]
    }
  }
  if (Retain_MultiWordTokens == TRUE) {
    x <- JoinMultiwordTokens(x)
  }
  return(x)
}


LongWordRatio <- function(x, threshold = 8, fixed.threshold = TRUE,
                          by = c("partition", "doc_id")) {
  by <- match.arg(by)
  lwr <- x[!is.na(token_id) & upos != "PUNCT",
           .(long_word_ratio = sum(nchar(token) > threshold) / NROW(token)),
           by = .(get(by))]
  colnames(lwr)[1] <- "id"
  if (fixed.threshold == TRUE) {
    colnames(lwr)[2] <- "long_word_ratio_fixed"
    }
  return(lwr)
}


CalculateMeanWordRank <- function(x, ref_corp, by = c("partition", "doc_id")) {
  by <- match.arg(by)
  cat("\nCalculating frequencies and word ranks for all reference tokens ...  ")
  # Calculate frequency for each token in reference corpus
  ref_freqs <- ref_corp[upos != "PUNCT", .(freq_abs=.N), by = .(lemma = tolower(lemma))]
  # Sort frequency list by frequency
  ref_freqs <- ref_freqs[order(-freq_abs)]
  # Add column with rank
  ref_freqs[, rank := frankv(ref_freqs, order = -1, cols="freq_abs",
                             ties.method = "dense")]
  
  # Create column word_rank that indicated the frequency rank of each lemma in annotated corpus:
  # Select colunns of reffrequencies that have same value as corpus_annotated in column "lemma"
  #x$word_rank <- ref_freqs[x[,tolower(lemma)], .(lemma, rank, freq_abs),
  #                         on="lemma"][, rank]
  #x$word_rank <- ref_freqs[x[,tolower(lemma)], .(lemma, rank, freq_abs),
  #                         on="lemma"][, rank]
  
  x[, word_rank := ref_freqs[x[,tolower(lemma)], .(lemma, rank, freq_abs),
                             on="lemma"][, rank]]
  
  #x[upos == "PUNCT", word_rank := NA]
  x[upos == "PUNCT", word_rank := NA]
  
  max_rank <- max(x[, word_rank], na.rm = T)
  mean_w_ranks <- x[upos != "PUNCT",
                    .(mean_word_rank = mean(word_rank, na.rm = TRUE),
                      mean_word_rank_noPunct = mean(replace(word_rank, is.na(word_rank), max_rank + 1))),
                    by = get(by)]
  colnames(mean_w_ranks)[1] <- "id"
  cat("  --> DONE!\n\n")
  return(mean_w_ranks)
}




CountPOS <- function(tags, wordclass) {
  relevant_tags <- wordclass
  count <- sum(tags %in% relevant_tags)
  return(count)
}

GetMeanSequenceLength <- function(vec, pat) {
  # Calculate average length of sequences that consist of consecutive occurrences
  # of the same token (e.g. "PROPN" "PROPN" is a sequence of length 2)
  # Any character string can be passed as vector, e.g. "PROPN" or "PUNCT".
  # The function finds all uninterrputed sequences of consecutive token occurences within
  # a vector and returns the average length (in tokens) of all sequences
  # Applies greedy matching, i.e. looks for longest possible sequences of consecutive tokens
  vec <- paste0(vec, collapse = "#") # vector to string
  vec <- paste0(vec, "#") # append separator
  regex <- paste0("(", pat, "#)*") # create regex pattern
  matches <- unlist(regmatches(vec, gregexpr(regex, vec))) # grep pattern
  matches <- matches[matches != ""] #remove non-matches
  counts <- nchar(gsub(paste0(pat, "#"), "x", matches))
  if (length(counts) > 0) {
    men_seq_len <- mean(counts)
  } else {
    men_seq_len <- 0
  }
  return(men_seq_len)
}



GetContextualFctWords <- function(x, normalise = FALSE, by = "partition") {
  cat("Calculating contextual function words ... ")
  by = match.arg(arg = by, choices = c("partition", "doc_id", "doc_sent_id"),
                 several.ok = FALSE)
  ptm <- proc.time()
  if (normalise == TRUE) {
    token_count <- x[upos != "PUNCT" & !is.na(token_id), .(toks = NROW(token)),
                     by = .(get(by))]
    colnames(token_count)[1] <- "id"
    token_count <- token_count[order(id)]
  }
  #### CONTEXTUAL FUNCTION WORDS:
  # Calculate frequencies of trigrams that consist of at least two function words.
  # If the trigram consists of two function words, the third word is replaced by
  # its POS tag, e.g. "jest NOUN aby"
  # TO DO: Check whether it makes sense to keep NUM and PUNCT
  x <- x[upos != "#START#" & upos != "#END#", ]
  cfw <- x[ , .(w1 = ifelse(upos %in% Function_Words, tolower(token), upos),
                w2 = ifelse(txt_next(upos) %in% Function_Words, tolower(txt_next(token)), txt_next(upos)),
                w3 = ifelse(txt_next(upos, 2) %in% Function_Words, tolower(txt_next(token, 2)), txt_next(upos, 2))),
            by = .(get(by), doc_sent_id)]
  colnames(cfw)[1] <- "id"
  cfw[, nr_non_fct_word := rowSums(sapply(.SD, function(x) {x %in% c(Content_Words, Non_Words) })),
      .SDcols=c("w1", "w2", "w3")]
  cfw <- cfw[complete.cases(cfw) & nr_non_fct_word < 2, ]
  cfw[, contextFctW := paste(w1, w2, w3)]
  cfw <- document_term_frequencies(cfw, document = "id", term = c("contextFctW"))
  cfw <- cfw[order(doc_id)]
  dtm <- document_term_matrix(cfw)
  
  if (normalise == TRUE) {
    dtm <- dtm / token_count[id %in% rownames(dtm), toks]
  }
  t_elapsed = (proc.time() - ptm)[3]/60
  cat("  --> DONE in ", t_elapsed, "minutes!\n\n")
  return(dtm)
}


GetPositionalFrequencies <- function(x, ignore.punct = FALSE, min.len = 5,
                                     normalise = TRUE, by = "partition") {
  # Counts of tokens in specific positions: 1st, 2nd, last, penultimate and antepenultimate position
  # i.e. per-text counts of e.g. "t1_this", "t2_is" ... "t-2_done", ... "t1_however"
  by = match.arg(arg = by, choices = c("partition", "doc_id", "doc_sent_id"),
                 several.ok = FALSE)
  cat("Calculating positional token frequencies ... ")
  ptm <- proc.time()
  if (normalise == TRUE) {
    token_count <- x[upos != "PUNCT" & !is.na(token_id), .(toks = NROW(token)),
                     by = .(get(by))]
    colnames(token_count)[1] <- "id"
    token_count <- token_count[order(id)]
  }
  if (ignore.punct == TRUE) {
    x <- x[!is.na(token_id) & upos != "PUNCT" & nr_tokens >= min.len, ]
    position_dict <- x[ , rbind(head(.SD, 2), tail(.SD, 2)),
                        by = .(get(by), doc_sent_id),
                        .SDcols = c("token_id", "token", "upos")]
    colnames(position_dict)[1] <- "id"
    position_names <- c("t1", "t2", "t_2", "t_1") 
  } else {
    x <- x[!is.na(token_id) & nr_tokens >= min.len, ]
    position_dict <- x[ , rbind(head(.SD, 2), tail(.SD, 3)),
                        by = .(get(by), doc_sent_id),
                        .SDcols = c("token_id", "token", "upos")]
    colnames(position_dict)[1] <- "id"
    position_names <- c("t1", "t2", "t-3", "t-2", "t-1") 
  }
  position_dict[, cts := paste(position_names, tolower(token), sep = ".")]
  position_dict <- document_term_frequencies(position_dict, document = "id",
                                             term = "cts")
  dtm <- document_term_matrix(position_dict)
  if (normalise == TRUE) {
    dtm <-  dtm / token_count[id %in% rownames(dtm), toks]
  }
  t_elapsed = (proc.time() - ptm)[3]/60
  cat("  --> DONE in ", t_elapsed, "minutes!\n\n")
  return(dtm)
}


### PUNCTUATION COUNTS
# Counts of selected punctuation marks
# Three variants of counts: 1) raw frequency of given punctuation mark in document;
# 2) frequency of each mark divided by number of tokens in document;
# 3) frequency of each punctuation mark divided by total number of punctuation
# marks in document
GetPunctuationCounts <- function(x, by = "partition") {
  by = match.arg(arg = by, choices = c("partition", "doc_id", "doc_sent_id"),
                 several.ok = FALSE)
  cat("Calculating punctuation counts ...")
  ptm <- proc.time()
  x <- x[token %in% Punctuation_Marks, ]
  punct <- x[, .(term = token, total_pct = .N, total_tok = nr_tokens,
                 total_tok_sum = sum(unique(nr_tokens))), by = .(get(by))]
  colnames(punct)[1] <- "doc_id"
  punct <- punct[, .(freq = .N,
                     freq2 = .N / total_tok_sum,
                     freq3 = .N / total_pct), by = .(doc_id, term)]
  punct <- unique(punct)
  punct_v1 <- punct[, 1:3]
  punct_v2 <- punct[, c(1,2,4)]
  punct_v3 <- punct[, c(1,2,5)]
  colnames(punct_v2)[3] <- "freq"
  colnames(punct_v3)[3] <- "freq"
  punct_dtm_v1 <- document_term_matrix(punct_v1)
  punct_dtm_v2 <- document_term_matrix(punct_v2)
  punct_dtm_v3 <- document_term_matrix(punct_v3)
  #colnames(punct_dtm_v2) <- paste0(colnames(punct_dtm_v2), "_v2")
  #colnames(punct_dtm_v3) <- paste0(colnames(punct_dtm_v3), "_v3")
  #punct.dtm <- dtm_cbind(punct_dtm_v1, punct_dtm_v2)
  #punct.dtm <- dtm_cbind(punct.dtm, punct_dtm_v3)
  punct.l <- list(punct.raw = punct_dtm_v1,
                  punct.norm.tot.tokens = punct_dtm_v2,
                  punct.norm.tot.punct = punct_dtm_v3)
  t_elapsed = (proc.time() - ptm)[3]/60
  cat("  --> DONE in ", t_elapsed, "minutes!\n\n")
  return(punct.l)
}



### READABILITY INDICES
# 1)  COLEMAN LIAU READABILITY INDEX
# CLI = 0.0588 × L − 0.296 × S − 15.8
# where
# L = (Letters ÷ Words) × 100
# S = (Sentences ÷ Words) × 100
#
# 2) LIX
# https://support.siteimprove.com/hc/en-gb/articles/114094009592-Readability-tests
# LIX is suitable for Western-European languages, see Anderson, J. (1983): Lix and Rix: Variations on a Little-known Readability Index  (www.jstor.org/stable/40031755?seq=1#page_scan_tab_contents)
# LIX = (words / sentences) + (100 * long words / sentences)
# where
# long words = count of all tokens > 6 character length
ReadabilityMeasures <- function(x, by = c("partition", "doc_id")) {
  by <- match.arg(by)
  ptm <- proc.time()
  cat ("Calculating reasbility measures ... ")
  x <- x[!is.na(token_id) & upos != "PUNCT", ]
  
  cli <- x[ , .(letters = sum(nchar(gsub("[[:punct:]]", "", token))),
                words = NROW(token),
                sents = max(sentence_id),
                longwords = sum(nchar(token) > 6)),
            by = .(get(by))]
  colnames(cli)[1] <- "id"
  cli <- cli[, .(L = (letters / words) * 100,
                 S = (sents / words) * 100,
                 coleman_liau = 5.88 * (letters / words) - 29.6 * (sents / words) - 15.8,
                 lix = (words / sents) + (100 * longwords / words)),
             by = .(id)]
  t_elapsed = (proc.time() - ptm)[3]/60
  cat("  --> DONE in ", t_elapsed, "minutes!\n\n")
  return(cli[, c(1,4:5)])
}


TokenClass <- function(s) {
  tok_class <- NA
  if (grepl("^[[:lower:]]+$", s)) {
    tok_class <- "all_lower"
  } else if (s == "#START#" || s == "#END#") {
    tok_class <- s
  } else if (char_count(s, "[[:digit:]]") == 2) {
    tok_class <- "2d"
  } else if (char_count(s, "[[:digit:]]") == 4) {
    tok_class <- "4d"
  } else if (grepl("[[:digit:]]", s)) {
    if (grepl("[[:alpha:]]", s)) {
      tok_class <- "alnum"
    } else if (grepl("-", s)) {
      tok_class <- "dig_hyph"
    } else if (grepl("/", s)) {
      tok_class <- "dig_slash"
    } else if (grepl(",", s)) {
      tok_class <- "dig_comma"
    } else if (grepl("\\.", s)) {
      tok_class <- "dig_dot"
    } else {
      tok_class <- "num"
    }
  } else if (grepl("^[[:upper:]]$", s)) {
    tok_class <- "single_cap"
  } else if (grepl("^[[:upper:]]{2,}$", s)) {
    tok_class <- "all_cap"
  } else if (grepl("^[[:upper:]]\\.$", s)) {
    tok_class <- "cap_dot"
  } else if (grepl("^[[:upper:]]", s)) {
    tok_class <- "first_cap"
  } else {
    tok_class <- "other"
  }
  return(tok_class)
}


TokenShape <- function(s) {
  s <- gsub("#START#", "##<<##", s)
  s <- gsub("#END#", "##>>##", s)
  s <- gsub("[[:lower:]]", "x", s)
  s <- gsub("[[:upper:]]", "X", s)
  s <- gsub("[[:digit:]]", "d", s)
  s <- gsub("x{2, }", "x\\*", s)
  s <- gsub("X{2, }", "X\\*", s)
  s <- gsub("d{2, }", "d\\*", s)
  s <- gsub("##<<##", "#START#", s)
  s <- gsub("##>>##", "#END#", s)
  return(s)
}

SyllableStructure <- function(s) {
  s <- tolower(s)
  if (grepl("^[[:alpha:]\\-\\.]+$", s)) {
    s <- gsub(Vow, "V", s)
    s <- gsub(Cons, "C", s)
    s <- gsub("V{2,}", "V\\*", s)
    s <- gsub("C{2,}", "C\\*", s)
  } else {
    s <- NA
  }
  return(s)
}


char_count <- function(s, pat) {
  lengths(regmatches(s, gregexpr(pat, s)))
}


GetRepetitions <- function(x, normalise = FALSE, by = c("partition", "doc_id")) {
  # Count number of content words that occur >1 time per text, optionally
  # normalised by text length in tokens
  by = match.arg(by)
  text_lengths <- x[upos != "PUNCT", .(text_len = .N), by = get(by)]
  colnames(text_lengths)[1] <- "id"
  x <- x[upos %in% Content_Words, ]
  rpts <- x[, .(freq = .N), by = .(get(by), lemma)][freq  > 1]
  colnames(rpts)[1] <- "id"
  rpts <- rpts[, .(reps = .N), by = id]
  if (normalise == TRUE) {
    rpts <- merge(rpts, text_lengths, by = "id", all.y = TRUE)
    rpts[, c("reps", "text_len", "reps_norm") := list(NULL, NULL, (reps / text_len))] #normalise
    rpts[is.na(reps_norm), reps_norm := 0]
  }
  colnames(rpts)[1] <- "id"
  return(rpts)
}


MarkSentenceBorders <- function(x) {
  cat("Adding sentence border markers ... ")
  t.start <- proc.time()
  if ("partition" %in% colnames(x)) {
    x <- x[, rbind(.(paragraph_id = first(paragraph_id),
                     token = "#START#", lemma = "#START#", upos ="#START#",
                     doc_sent_id = first(doc_sent_id),
                     partition = first(partition)),
                   .SD, fill=TRUE),
           by = .(doc_id, sentence_id)]
    x <- x[, rbind(.SD,
                   .(paragraph_id = first(paragraph_id),
                     token = "#END#", lemma = "#END#", upos = "#END#",
                     doc_sent_id = last(doc_sent_id),
                     partition = first(partition)),
                   fill=TRUE),
           by = .(doc_id, sentence_id)]
  } else {
    x <- x[, rbind(.(paragraph_id = first(paragraph_id),
                     token = "#START#", lemma = "#START#", upos ="#START#",
                     doc_sent_id = first(doc_sent_id)),
                   .SD, fill=TRUE),
           by = .(doc_id, sentence_id)]
    x <- x[, rbind(.SD,
                   .(paragraph_id = first(paragraph_id),
                     token = "#END#", lemma = "#END#", upos = "#END#",
                     doc_sent_id = last(doc_sent_id)),
                   fill=TRUE),
           by = .(doc_id, sentence_id)]
  }
  t.elapsed = (proc.time() - t.start)[3]/60
  cat("  --> DONE in ", t.elapsed, "minutes!\n\n")
  return(x)
}

GetDocumentLengths <- function(x, by = c("partition", "doc_id")) {
  by <- match.arg(by)
  doclens <- x[, .(tokens = .N, tokens_noPct = NROW(upos[upos != "PUNCT"]),
                   sents = length(unique(doc_sent_id))),
               by = .(get(by))]
  colnames(doclens)[1] <- "id"
  return(doclens)
}

MeanMultipleNaming <- function(x, pos = "PROPN",
                               by = c("partition", "doc_id")) {
  by = match.arg(by)
  pos <- as.name(pos)
  mean_mult_name <- x[, .(mean_propn_seq = GetMeanSequenceLength(upos, pos)),
                      by = get(by)]
  colnames(mean_mult_name)[1] <- "id"
  return(mean_mult_name)
}


AveragePMI <- function(x, by = c("partition", "doc_id"),
                       repl.na = c("0", "na", "mean")) {
  # AVERAGE POINTWISE MUTUAL INFORMATION FOR BIGRAMS
  by = match.arg(by)
  repl.na = match.arg(repl.na)
  docs.all <- unique(x$doc_id)
  x <- x[upos != "PUNCT" & upos != "#START#" & upos != "#END#",]
  ngrams <- x[, .(w1 = tolower(token),
                  w2 = tolower(txt_next(token)),
                  bigram = txt_nextgram(tolower(token), n = 2)),
              by = .(get(by), doc_sent_id)]
  colnames(ngrams)[1] <- "id"
  ngrams[, words := .N, by = .(id)]
  ngrams <- ngrams[!is.na(bigram), ]
  ngrams[, freq_w1 := .N, by = .(id, w1)]
  ngrams[, freq_w2 := .N, by = .(id, w2)]
  ngrams[, freq_bigram := .N, by = .(id, bigram)]
  ngrams[, pmi := log(words * (freq_bigram / (freq_w1 * freq_w2)))]
  average_pmi <- ngrams[, .(pmi_mean = mean(pmi)), by = id]
  docs.missing <- docs.all[!(docs.all %in% average_pmi$id)]
  if (length(docs.missing) > 0) {
    if (repl.na == "0") {
      rpl = 0
    } else if (rpl == "NA") {
      rpl = NA
    } else {
      rpl = mean(average_pmi$pmi_mean)
    }
    rows.missing <- data.table(id = docs.missing,
                               pmi_mean = rpl,
                               stringsAsFactors = FALSE)
    average_pmi <- rbindlist(list(average_pmi, rows.missing))
  }
  rm(x)
  rm(ngrams)
  return(average_pmi)
}


CharacterRatio <- function(vec, pat) {
  # Counts percentage of characters in a string not matching a certain pattern,
  # e.g. r("ab.cd", "[[:punct:]]) =  0.8 because 4/5 characters are non-punctuation
  r <- nchar(gsub(pat, "", vec)) / nchar(vec)
  
  #r <- sapply(vec, function(x) { 1 - length(unlist(regmatches(x, gregexpr(pat, x))))  / nchar(x)})
  #r <- sapply(annotation.dt$token, function(x) { 1 - length(unlist(regmatches(x, gregexpr(pat, x))))  / nchar(x)})
  #t <- proc.time()
  #r <- mcmapply(function(x) { 1 - length(unlist(regmatches(x, gregexpr(pat, x))))  / nchar(x) },
  #              vec, SIMPLIFY = T, USE.NAMES = TRUE, mc.cores = 8)
  #r <- mcmapply(xyz, vec, SIMPLIFY = T, USE.NAMES = TRUE, mc.cores = 8)
  #proc.time() - t
  
  #xyz <- function(x) {
   # 1 - length(unlist(regmatches(x, gregexpr(pat, x))))  / nchar(x)
    #}
  return(r)
}



#### CHARACTER NGRAMS
# Calculate character n-gram counts for each text and convert counts
# to document-term-matrix
# First, de-select puncutation marks, sentence beginning/end markers and tokens
# consisting of less than 50% letters; second, split these lowercased tokens into letters
# and then count n-grams. Word beginnning (<) and end(>) markers added to each token
CharNgrams <- function(x, from = 2, to = 3, normalise = FALSE) {
  
  cat("Calculating character n-grams in range ", from, " to ", to, " ... ", sep = "")
  ptm <- proc.time()
  if (normalise == TRUE) {
    token_count <- x[upos != "PUNCT" & !is.na(token_id), .(toks = NROW(token)), by = doc_id ]
    token_count <- token_count[order(doc_id)]
  }
  cat("\n\tStarting with selection of tokens...")
  ptm_sel <- proc.time()
  #char_ngrams <- x[upos != "#START#" & upos != "#END#" & upos != "PUNCT" & 
  #                   CharacterRatio(token, "[[:punct:]]|[[:digit:]]") >= 0.5, ]
  char_ngrams <- x[upos != "#START#" & upos != "#END#" & upos != "PUNCT", .(doc_id, sentence_id, token_id, token)]
  char_ngrams <- char_ngrams[CharacterRatio(token, "[[:punct:]]|[[:digit:]]") > 0.5, ]
  #char_ngrams[, char_ratio := CharacterRatio(token, "[[:punct:]]|[[:digit:]]")]
  #char_ngrams <- x[upos != "#START#" & upos != "#END#" & upos != "PUNCT" &
  #                   CharacterRatio(token, "[[:punct:]]|[[:digit:]]") >= 0.5, ]
  t_sel <- (proc.time() - ptm_sel)[3]/60
  cat("  --> Selection completed in", t_sel, "minutes!\n")
  cat("\tStarting with token splitting...")
  ptm_spl <- proc.time()
  char_ngrams <- char_ngrams[, .(char = unlist(strsplit(paste0("<", tolower(token), ">"), ""))),
                             by = .(doc_id, sentence_id, token_id)]
  t_spl <- (proc.time() - ptm_spl)[3]/60
  cat("  --> Splitting completed in ", t_spl, "minutes!\n")
  
  # Define names of columns for list to be returned
  dtm_names <- NULL
  for (i in from : to) {
    colname <- paste0("char_", i, "gram")
    dtm_names <- c(dtm_names, colname)
  }
  dtms.l <- vector("list", length(dtm_names))
  dtms.l <- setNames(dtms.l, dtm_names)
  
  #bla <- CharacterRatio(annotation.dt$token, "[[:punct:]]|[[:digit:]]")
  for (i in from : to) {
    cat("\n\tCalculating ", i, "-grams ...", sep = "")
    ptm_ng <- proc.time()
    colname <- paste0("char_", i, "gram")
    ### Next line is longest subprocess, i.e. paralellise this
    char_ngrams[, eval(colname) := txt_nextgram(char, i, sep = " "),
                by = .(doc_id, sentence_id, token_id)]
    
    #z.s.ng3 <- mclapply(bla, FUN = xyz, cn = colname, i = i,
    #         mc.cores = 4)
    #
    #xyz <- function(fileanno, cn, i) {
    #  fileanno <- fileanno[, eval(cn) := txt_nextgram(char, i, sep = " "),
    #              by = .(doc_id, sentence_id, token_id)]
    #}
    
    
    #short <- char_ngrams[1:10000, ]
    #z_splitted <- split(z.s, ceiling(seq_along(z.s$doc_id)/100))
    #z.s.ng <- z.s[, eval(colname) := txt_nextgram(char, i, sep = " "),
     #                     by = .(doc_id, sentence_id, token_id)]
    #short_splitted <- split(z.s, z.s$doc_id)
    #short_splitted.ng <- mclapply(short_splitted, FUN = xyz, cn = colname, i = i,
    #                              mc.cores = 8)
    
    #char_ngrams_splitted <- split(char_ngrams, char_ngrams$doc_id)
    #char_ngrams <- mclapply(char_ngrams_splitted, FUN = function(a) {
    #  a[, eval(colname) := txt_nextgram(char, i, sep = " "),
    #         by = .(doc_id, sentence_id, token_id)]
    #  a
    #  #return(a)
    #  }, mc.cores = 4)
    #char_ngrams <- rbindlist(char_ngrams)
    
    #short_splitted.ng2 <- rbindlist(short_splitted.ng2)
    
    #short.ng <- short[, .(char, trg = txt_nextgram(char, i, sep = " ")),
    #                  by = .(doc_id, sentence_id, token_id)]
    ####################
    t_ng <- (proc.time() - ptm_ng)[3]/60
    cat("  --> N-gram calculation completed in ", t_ng, "minutes!\n")
    
    cat("\tStart calculating document-term frequencies ...")
    ptm_dtf <- proc.time()
    dtf <- document_term_frequencies(x = char_ngrams, document = "doc_id",
                                     term = colname)
    char_ngrams[, eval(colname) := NULL]
    t_dtf <- (proc.time() - ptm_dtf)[3]/60
    cat("  --> Completed calculating DTF in", t_dtf, "minutes!\n")
    dtm <- Matrix(0, nrow = length(unique(dtf$term)),
                  ncol = length(unique(dtf$term)), sparse = T)
    cat("\tEmpty sparse matrix created.\n")
    ptm_dtm <- proc.time()
    cat("\tStart building document-term matrix  ...")
    dtm <- document_term_matrix(dtf)
    rm(dtf)
    t_dtm <- (proc.time() - ptm_dtm)[3]/60
    cat("  --> Completed building DTM in", t_dtm, "minutes!\n")
    dtms.l[[colname]] <- dtm
    rm(dtm)
    ### TO DO: ADD NORMALISATION PROCEDURE
  }
  
  #dtm_names <- colnames(char_ngrams)[-c(1:4)]
  #dtms.l <- vector("list", length(dtm_names))
  #dtms.l <- setNames(dtms.l, dtm_names)

  #for (d_n in dtm_names) {
  #  cat("\tStart calculating document-term frequencies ...")
  #  ptm_dtf <- proc.time()
  #  dtf <- document_term_frequencies(x = char_ngrams, document = "doc_id",
  #                                   term = d_n)
  #  t_dtf <- (proc.time() - ptm_dtf)[3]/60
  #  cat("  --> Completed calculating DTF in", t_dtf, "minutes!\n")
  #  dtm <- Matrix(0, nrow = length(unique(dtf$term)),
  #                ncol = length(unique(dtf$term)), sparse = T)
  #  cat("\tEmpty sparse matrix created.\n")
  #  ptm_dtm <- proc.time()
  #  cat("\tStart building document-term matrix  ...")
  #  dtm <- document_term_matrix(dtf)
  #  rm(dtf)
  #  t_dtm <- (proc.time() - ptm_dtm)[3]/60
  #  cat("  --> Completed building DTM in", t_dtm, "minutes!\n")
  #  if (normalise == TRUE) {
  #    dtm <- dtm / token_count$toks
  #  }
  #  dtms.l[[d_n]] <- dtm
  #}
  
  t_elapsed = (proc.time() - ptm)[3]/60
  cat("\n--> DONE in ", t_elapsed, "minutes!\n\n", sep = "")
  return(dtms.l)
}

CharNgrams_parallel <- function(x, from = 2, to = 3, normalise = FALSE) {
  
  cat("Calculating character n-grams in range ", from, " to ", to, " ... ", sep = "")
  ptm <- proc.time()
  if (normalise == TRUE) {
    token_count <- x[upos != "PUNCT" & !is.na(token_id), .(toks = NROW(token)), by = doc_id ]
    token_count <- token_count[order(doc_id)]
  }
  cat("\n\tStarting with selection of tokens...")
  ptm_sel <- proc.time()
  #char_ngrams <- x[upos != "#START#" & upos != "#END#" & upos != "PUNCT" & 
  #                   CharacterRatio(token, "[[:punct:]]|[[:digit:]]") >= 0.5, ]
  char_ngrams <- x[upos != "#START#" & upos != "#END#" & upos != "PUNCT", .(doc_id, sentence_id, token_id, token)]
  char_ngrams <- char_ngrams[CharacterRatio(token, "[[:punct:]]|[[:digit:]]") > 0.5, ]
  #char_ngrams[, char_ratio := CharacterRatio(token, "[[:punct:]]|[[:digit:]]")]
  #char_ngrams <- x[upos != "#START#" & upos != "#END#" & upos != "PUNCT" &
  #                   CharacterRatio(token, "[[:punct:]]|[[:digit:]]") >= 0.5, ]
  t_sel <- (proc.time() - ptm_sel)[3]/60
  cat("  --> Selection completed in", t_sel, "minutes!\n")
  cat("\tStarting with token splitting...")
  ptm_spl <- proc.time()
  char_ngrams <- char_ngrams[, .(char = unlist(strsplit(paste0("<", tolower(token), ">"), ""))),
                             by = .(doc_id, sentence_id, token_id)]
  t_spl <- (proc.time() - ptm_spl)[3]/60
  cat("  --> Splitting completed in ", t_spl, "minutes!\n")
  
  # Define names of columns for list to be returned
  dtm_names <- NULL
  for (i in from : to) {
    colname <- paste0("char_", i, "gram")
    dtm_names <- c(dtm_names, colname)
  }
  dtms.l <- vector("list", length(dtm_names))
  dtms.l <- setNames(dtms.l, dtm_names)
  
  #bla <- CharacterRatio(annotation.dt$token, "[[:punct:]]|[[:digit:]]")
  for (i in from : to) {
    cat("\n\tCalculating ", i, "-grams ...", sep = "")
    ptm_ng <- proc.time()
    colname <- paste0("char_", i, "gram")
    ### Next line is longest subprocess, i.e. paralellise this
    #char_ngrams[, eval(colname) := txt_nextgram(char, i, sep = " "),
    #            by = .(doc_id, sentence_id, token_id)]
    
    #z.s.ng3 <- mclapply(bla, FUN = xyz, cn = colname, i = i,
    #         mc.cores = 4)
    #
    #xyz <- function(fileanno, cn, i) {
    #  fileanno <- fileanno[, eval(cn) := txt_nextgram(char, i, sep = " "),
    #              by = .(doc_id, sentence_id, token_id)]
    #}
    
    
    #short <- char_ngrams[1:10000, ]
    #z_splitted <- split(z.s, ceiling(seq_along(z.s$doc_id)/100))
    #z.s.ng <- z.s[, eval(colname) := txt_nextgram(char, i, sep = " "),
    #                     by = .(doc_id, sentence_id, token_id)]
    #short_splitted <- split(z.s, z.s$doc_id)
    #short_splitted.ng <- mclapply(short_splitted, FUN = xyz, cn = colname, i = i,
    #                              mc.cores = 8)
    
    char_ngrams_splitted <- split(char_ngrams, char_ngrams$doc_id)
    char_ngrams <- mclapply(char_ngrams_splitted, FUN = function(a) {
      a[, eval(colname) := txt_nextgram(char, i, sep = " "),
        by = .(doc_id, sentence_id, token_id)]
      a
      #return(a)
    }, mc.cores = 2)
    char_ngrams <- rbindlist(char_ngrams)
    
    #short_splitted.ng2 <- rbindlist(short_splitted.ng2)
    
    #short.ng <- short[, .(char, trg = txt_nextgram(char, i, sep = " ")),
    #                  by = .(doc_id, sentence_id, token_id)]
    ####################
    t_ng <- (proc.time() - ptm_ng)[3]/60
    cat("  --> N-gram calculation completed in ", t_ng, "minutes!\n")
    
    cat("\tStart calculating document-term frequencies ...")
    ptm_dtf <- proc.time()
    dtf <- document_term_frequencies(x = char_ngrams, document = "doc_id",
                                     term = colname)
    char_ngrams[, eval(colname) := NULL]
    t_dtf <- (proc.time() - ptm_dtf)[3]/60
    cat("  --> Completed calculating DTF in", t_dtf, "minutes!\n")
    dtm <- Matrix(0, nrow = length(unique(dtf$term)),
                  ncol = length(unique(dtf$term)), sparse = T)
    cat("\tEmpty sparse matrix created.\n")
    ptm_dtm <- proc.time()
    cat("\tStart building document-term matrix  ...")
    dtm <- document_term_matrix(dtf)
    rm(dtf)
    t_dtm <- (proc.time() - ptm_dtm)[3]/60
    cat("  --> Completed building DTM in", t_dtm, "minutes!\n")
    dtms.l[[colname]] <- dtm
    rm(dtm)
    ### TO DO: ADD NORMALISATION PROCEDURE
  }
  
  #dtm_names <- colnames(char_ngrams)[-c(1:4)]
  #dtms.l <- vector("list", length(dtm_names))
  #dtms.l <- setNames(dtms.l, dtm_names)
  
  #for (d_n in dtm_names) {
  #  cat("\tStart calculating document-term frequencies ...")
  #  ptm_dtf <- proc.time()
  #  dtf <- document_term_frequencies(x = char_ngrams, document = "doc_id",
  #                                   term = d_n)
  #  t_dtf <- (proc.time() - ptm_dtf)[3]/60
  #  cat("  --> Completed calculating DTF in", t_dtf, "minutes!\n")
  #  dtm <- Matrix(0, nrow = length(unique(dtf$term)),
  #                ncol = length(unique(dtf$term)), sparse = T)
  #  cat("\tEmpty sparse matrix created.\n")
  #  ptm_dtm <- proc.time()
  #  cat("\tStart building document-term matrix  ...")
  #  dtm <- document_term_matrix(dtf)
  #  rm(dtf)
  #  t_dtm <- (proc.time() - ptm_dtm)[3]/60
  #  cat("  --> Completed building DTM in", t_dtm, "minutes!\n")
  #  if (normalise == TRUE) {
  #    dtm <- dtm / token_count$toks
  #  }
  #  dtms.l[[d_n]] <- dtm
  #}
  
  t_elapsed = (proc.time() - ptm)[3]/60
  cat("\n--> DONE in ", t_elapsed, "minutes!\n\n", sep = "")
  return(dtms.l)
}


#' Generate individual document-term matrices with token-level n-gram counts for all specified variables
#'
#' N-grams in specified range are computed for all selected variables from
#' annotation data table arranged in tidy format (UD Pipe).
#' @param x The annotation data for n-gram computation (data.table)
#' @param terms The variables for which n-grams are to be computed (character vector)
#' @param from Size (n) of smallest n-gram to be computed (integer)
#' @param to Size (n) of largest n-gram to be computed (integer)
#' @param ignore.borders Should sentence border markers (#START#) be exluced? (logical)
#' @param ignore.punct Should punctuation marks be exluced from n-grams? (logical)
#' @param lowercase Should tokens be lowercased? (logical)
#' @param normalise Should ngram frequencies be normalised by text length or TF-IDF scores? (len, tfidf, none)
#' @param by Should ngram frequencies be calculated for documents, partitions or sentences? (partition, doc_id, doc_sent_id)
#' @return A list of all generated document-term-matrices of n-gram counts
#' @export
GetNgrams <- function(x, terms = c("token", "upos"), from = 1, to = 3,
                      ignore.borders = FALSE, ignore.punct = FALSE,
                      lowercase = TRUE, normalise = c("len", "tfidf", "none"),
                      by = c("partition", "doc_id")) {
  ptm <- proc.time()
  cat("Calculating n-grams (", paste0(terms, collapse = ", "), ") in range ", from, " to ", to, " ... ", sep = "")
  normalise = match.arg(arg = normalise, choices = c("len", "tfidf", "none"),
                        several.ok = FALSE)
  terms = match.arg(arg = terms, choices = c("token", "lemma", "upos"),
                    several.ok = TRUE)
  by = match.arg(arg = by, choices = c("partition", "doc_id", "doc_sent_id"),
                 several.ok = FALSE)
  docs.all <- unique(x$doc_id)
  ngrams <- x[, .SD, by = .(get(by), doc_sent_id), .SDcols = c("upos", terms)]
  colnames(ngrams)[1] <- "id"
  if (lowercase == TRUE && "token" %in% colnames(ngrams)) {
    ngrams[, token := tolower(token)]
  }
  
  for (t in terms) {
    for (i in from : to) {
      colname <- paste0(t, ".", i, "gram")
      
      if (ignore.borders == TRUE && ignore.punct == TRUE) {
        suffix <- ".noBorder.noPunct"
        ngrams[upos != "#START#" & upos != "#END#" & upos != "PUNCT",
               V1 := txt_nextgram(eval(as.name(t)), i),
               by = .(id, doc_sent_id)]
        # To avoid bigrams across omitted punctuation, remove "upos != PUNCT from previous call
        # and add the next two lines
        #ngrams[grepl("NA", V1), V1 := NA]
        #ngrams[grepl("NA", V1), V1 := NA]
      } else if (ignore.borders == TRUE) {
        suffix <- ".noBorder"
        ngrams[upos != "#START#" & upos != "#END#",
               V1 := txt_nextgram(eval(as.name(t)), i),
               by = .(id, doc_sent_id)]
      } else if (ignore.punct == TRUE) {
        suffix <- ".noPunct"
        if (i == 1) { # Avoid #START# or #END# unigrams
          ngrams[upos != "PUNCT" & upos != "#START#" & upos != "#END#",
                 V1 := txt_nextgram(eval(as.name(t)), i),
                 by = .(id, doc_sent_id)]
        } else {
          ngrams[upos != "PUNCT", V1 := txt_nextgram(eval(as.name(t)), i),
                 by = .(id, doc_sent_id)]
        }
      } else {
        suffix <- ""
        if (i == 1) { #Avoid #START# or #END# unigrams
          ngrams[upos != "#START#" & upos != "#END#",
                 V1 := txt_nextgram(eval(as.name(t)), i),
                 by = .(id, doc_sent_id)]
        } else {
          ngrams[, V1 := txt_nextgram(eval(as.name(t)), i),
                 by = .(id, doc_sent_id)]
        }
      }
      if (normalise != "none") {
        suffix <- paste0(suffix, ".norm.", normalise)
      }
      colnames(ngrams)[ncol(ngrams)] <- paste0(colname, suffix)
    }
  }
  
  # Delete base columns, from which ngrams have been generated
  ngrams[, which(colnames(ngrams) %in% c("upos", terms)) := NULL]
  
  # Add all extracted ngram features to a list to be returned
  dtm_names <- colnames(ngrams)[-c(1,2)]
  ngram_dtms <- vector("list", length(dtm_names))
  ngram_dtms <- setNames(ngram_dtms, dtm_names)
  for (n in dtm_names) {
    dtm <- document_term_frequencies(ngrams, document = "id", term = n) # resulting DTM has "doc_id" as first column name, no matter how it is called in reality --> udpipe does this so!
    if (normalise != "none") {
      dtm <- document_term_frequencies_statistics(dtm)
      if (normalise == "len") {
        dtm <- dtm[, .(term = term, freq = tf), by = doc_id] # tf equivalent to normalised frequency, i.e. frequency / total words
      } else {
        dtm <- dtm[, .(term = term, freq = tf_idf), by = doc_id] # tf equivalent to normalised frequency, i.e. frequency / total words
      }
    }
    dtm <- document_term_matrix(dtm)
    docs.missing <- docs.all[!(docs.all %in% rownames(dtm))]
    
    if (Unit != "partition") {
      if (length(docs.missing) > 0) {
        if (length(docs.missing) == 1) {
          docs.missing <- c(docs.missing, "dummyrow")
        }
        rows.missing <- Matrix(0, nrow = length(docs.missing),
                               ncol = ncol(dtm),
                               dimnames = list(docs.missing, colnames(dtm)),
                               sparse = TRUE)
        dtm <- dtm_rbind(dtm, rows.missing)
        if ("dummyrow" %in% docs.missing) {
          dtm <- dtm[-nrow(dtm), ]
        }
        dtm <- dtm[order(row.names(dtm)), ]
      }
    }
    
    colnames(dtm) <- gsub("[[:blank:]]", "_",
                           colnames(dtm))
    colnames(dtm) <- gsub("#", "",
                           colnames(dtm))
    
    ngram_dtms[[n]] <- dtm
    #ngram_dtms[[n]] <- document_term_matrix(document_term_frequencies(ngrams,document = "doc_id", term = n))
  }
  t_elapsed = (proc.time() - ptm)[3]/60
  cat("  --> DONE in ", t_elapsed, "minutes!\n\n")
  return(ngram_dtms)
}



GetMostFrequentWords <- function(x, n = 100, term = "token",
                                 ignore.punct = TRUE, lowercase = TRUE,
                                 normalise = "len",
                                 ignore.pos = "none", by = "partition") {
  # Parse Function Arguments
  term = match.arg(arg = term, choices = c("token", "lemma"),
                   several.ok = FALSE)
  normalise = match.arg(arg = normalise, choices = c("len", "tfidf", "none"),
                        several.ok = FALSE)
  ignore.pos = match.arg(arg = ignore.pos,
                         choices = c("none", "Content_Words", "Function_Words",
                                     "Non_Words", "ADJ", "ADV", "NOUN", "PROPN",
                                     "VERB", "X", "ADP", "AUX", "CCONJ", "DET",
                                     "INTJ", "NUM", "PART", "PRON", "SCONJ",
                                     "SYM"),
                         several.ok = TRUE)
  by = match.arg(arg = by, choices = c("partition", "doc_id", "doc_sent_id"),
                 several.ok = FALSE)
  cat("Calculating", n, "most frequent", term, " ... ")
  ptm <- proc.time()
  docs.all <- unique(x$doc_id)
  
  if ("none" %in% ignore.pos) {
    ignore.pos <- "none"
  } else if (sum(grepl("_", ignore.pos) > 0)) {
    pos_classes <- ignore.pos[grepl("_", ignore.pos)]
    ignore.pos <- ignore.pos[!grepl("_", ignore.pos)]
    for (pc in pos_classes) {
      ignore.pos <- c(ignore.pos, eval(as.name(pc)))
    }
  }
  
  # Get relevant fields from annotation data table
  if (ignore.punct == TRUE) {
    x <- x[upos != "#START#" & upos != "#END#" & upos != "PUNCT", ]
    x <- x[, .(upos, term = eval(as.name(term))), by = .(get(by), doc_sent_id)]
  } else {
    x <- x[upos != "#START#" & upos != "#END#", ]
    x <- x[, .(upos, term = eval(as.name(term))), by = .(get(by), doc_sent_id)]
  }
  colnames(x)[1] <- "id"
  
  if (lowercase == TRUE) {
    x[, term := tolower(term)]
  }
  
  if (normalise != "none") {
    text_lengths <- x[, .(len = .N), by = id]
    text_lengths <- text_lengths[order(id), ]
  }
  
  if (!("none" %in% ignore.pos)) {
    x <- x[!(upos %in% ignore.pos), ]
  }
  freqs <- document_term_frequencies(x[, c("id", "term")])
  dtm <- document_term_matrix(freqs)
  
  dtm <- dtm_remove_lowfreq(dtm, minfreq = 1, maxterms = n)
  
  if (normalise == "len") {
    dtm <- dtm / text_lengths[id %in% rownames(dtm), len] #dtm / text_lengths$len #dtm / token_count[doc_id %in% rownames(dtm), toks]
  } else if (normalise == "tfidf") {
    dtm <-  dtm / text_lengths[id %in% rownames(dtm), len] #dtm / text_lengths$len dtm / token_count[doc_id %in% rownames(dtm), toks]
    idfs <- log(nrow(dtm) / colSums(dtm != 0))
    dtm <- sweep(dtm, 2, idfs, "*")
  }
  
  
  if (Unit != "partition") {
    docs.missing <- docs.all[!(docs.all %in% rownames(dtm))] #setdiff(docs.all, rownames(dtm))
    if (length(docs.missing) > 0) {
      if (length(docs.missing) == 1) {
        docs.missing <- c(docs.missing, "dummyrow")
      }
      rows.missing <- Matrix(0, nrow = length(docs.missing),
                             ncol = ncol(dtm),
                             dimnames = list(docs.missing, colnames(dtm)),
                             sparse = TRUE)
      dtm <- dtm_rbind(dtm, rows.missing)
      if ("dummyrow" %in% docs.missing) {
        dtm <- dtm[-nrow(dtm), ]
      }
      dtm <- dtm[order(row.names(dtm)), ]
    }
  }
  t_elapsed = (proc.time() - ptm)[3]/60
  cat("  --> DONE in ", t_elapsed, "minutes!\n\n")
  return(dtm)
}




BindSparseToDense <- function(sparse, dense) {
  docs <- rownames(sparse)
  sparse <- data.table(as.matrix(sparse))
  sparse <- cbind(doc_id = docs, sparse)
  #rownames(mfw.dt) <- rownames(mfw.dtm)
  return(merge(dense, sparse))
  
  ## To combine DTM of MFWs with corpus.dt in sparse format (note: no metadata contained i nresulting DTM!):
  #dense <- Matrix(as.matrix(dense[, -c(1:8)]), dimnames = list(dense$doc_id, colnames(dense[, -c(1:8)])), sparse = TRUE)
  #return(dtm_cbind(dense, sparse)) # returns a sparse matrix
  #return()
}



#' Draws random sample from corpus data table such that number of translations
#' is equal to number of originals. Since in EuroParl for each language there
#' are much more translations than originals, the size of the sample is determined
#' by the number of originals.
#' @param x The annotated corpus (data.table) to sample from
#' @param sl.balance Should sampled translations be balances across source languages? (logical)
#' @param seed Seed value for randomisation (integer)
#' @return Data table of sampled corpus
#' @export
MakeBalancedSample <- function(x, sl.balance = TRUE, seed = 1234) {
  set.seed(seed)
  nr_origs <- nrow(x[type == "ORIG"])
  nr_langs <- length(unique(x$sl)) - 1
  if (nr_origs < nr_langs) {
    nr_per_sl <- 1
  } else {
    nr_per_sl <- as.integer(nr_origs / nr_langs)
  }
  if (nr_per_sl < 1) {
    nr_per_sl <- 1
  }
  samp.type <- x[,.SD[sample(.N,min(.N, nr_origs))], by = type]
  op <- samp.type
  if (sl.balance == TRUE) {
    samp.sl <- x[type != "ORIG", .SD[sample(.N,min(.N, nr_per_sl))], by = sl]
    if (nrow(samp.sl) > nr_origs) {
      samp.sl <- samp.sl[sample(.N, nr_origs)]
    }
    nr_missing_docs <- nr_origs - nrow(samp.sl)
    if (nr_missing_docs > 0) {
      samp.missing <- x[type != "ORIG" & !(doc_id %in% samp.sl$doc_id)]
      samp.missing <- samp.missing[sample(.N,nr_missing_docs)]
      samp.sl <- rbind(samp.sl, samp.missing)
    }
    samp.sl <- rbind(samp.type[type != "TRANS", ], samp.sl)
    op <- samp.sl
  }
  #  re-establish initial column order
  op <- op[, c(2:1, 3:ncol(op)), with = F]
  op <- op[order(op$doc_id), ]
  rownames(op) <- op$doc_id
  return(op)
}

#' Draws random sample from corpus data table such that sum of all translations
#' is equal to number of originals.
#' This function is currently very crude and lacks sophisticated parameter control
#' TODO, For nr.origs. choose MAX or spcific number
#' TODO: Set column by which sample is balanced
#' @param x The annotated corpus (data.table) to sample from
#' @param sl.balance Should sampled translations be balances across source languages? (logical)
#' @param seed Seed value for randomisation (integer)
#' @return Data table of sampled corpus
#' @export
MakeBalancedSample_Simple <- function(x, nr.origs, by = "id", seed = 1234) {
  # This function is currently very crude
  # It lacks sophisticated parameter control
  # For nr.origs. choose MAX or spcific number
  if (nr.origs != "max") {
    nr.origs <- as.integer(nr.origs)
  }
  set.seed(seed)
  origs.in.corp <- nrow(x[type == "ORIG"])
  if (nr.origs > origs.in.corp) {
    nr.origs <- origs.in.corp
    }
  nr.langs <- length(unique(x$sl)) - 1
  nr.per.sl <- floor(nr.origs / nr.langs)

  samp.origs <- x[type == "ORIG", .SD[sample(.N,min(.N, nr.origs))], by = sl]
  samp.trans <- x[type != "ORIG", .SD[sample(.N,min(.N, nr.per.sl))], by = sl]
  samp <- rbind(samp.origs, samp.trans)
  #  re-establish initial column order
  samp <- samp[, c(2:1, 3:ncol(samp)), with = F]
  samp <- samp[order(samp$id), ]
  #rownames(samp) <- samp$id
  indices <- which(x$id %in% samp$id)
  return(indices)
}

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

replaceNAs = function(x) {
  for (i in names(x))
    x[is.na(get(i)), (i):=0]
}

ExtractFeatures <- function(x, lang, save.output = TRUE, return.output = FALSE,
                            path.doclevel,
                            path.ngrams) {
  cat("Calculating Document-Level Features ...\n")
  feat.doclevel <- list(lang = lang)
  feat.doclevel$meta <- data.table(id = x[, unique(get(Unit))])
  feat.doclevel$meta <- merge(feat.doclevel$meta,
                              GetMetadataFromIDs(feat.doclevel$meta$id))
  feat.doclevel$meta <- merge(feat.doclevel$meta,
                              GetDocumentLengths(x, by = Unit))
  if (Keep_Text == FALSE) {
    feat.doclevel$text <- NA
  } else if (Keep_Text == TRUE & Unit == "partition") {
    feat.doclevel$texts <- data.table(id = feat.doclevel$meta$id)
    partitioned.texts <- x[, .(text = paste(token, collapse = " ")),
                                       by = .(partition, doc_id, paragraph_id)]
    partitioned.texts <- partitioned.texts[, .(text = paste(text, collapse = "\n")),
                                            by = .(partition, doc_id)]
    partitioned.texts <- partitioned.texts[, .(text = paste(text, collapse = "\n\n")),
                                           by = .(partition)]
    feat.doclevel$texts <- merge(feat.doclevel$texts, partitioned.texts,
                                 by.x = c("id"),
                                 by.y = c("partition"))
    rm(partitioned.texts)
  }
  feat.doclevel$data <- data.table(id = feat.doclevel$meta$id)
  feat.doclevel$data <- merge(feat.doclevel$data,
                              CalculateMeanTokensPerSentence(x, by = Unit))
  feat.doclevel$data <- merge(feat.doclevel$data,
                              CalculateMeanSentenceLength(x, by = Unit))
  feat.doclevel$data <- merge(feat.doclevel$data,
                              CalculateMeanTokenLength(x, by = Unit))
  feat.doclevel$data <- merge(feat.doclevel$data,
                              CalculateLexDens(x, by = Unit))
  udmodel = udpipe_load_model(file = file.path(Dir_Models,
                                               paste0(tolower(lang), "-ud-2.0-170801.udpipe")))
  #cat("  Loaded model ", udmodel$file, " to analyse refcorp in ", Dir_ReferenceCorpora, "\n", sep = "")
  fn.in.refcorp <- file.path(Dir_ReferenceCorpora,
                             paste0(tolower(lang), "_refcorp.rds"))
  if (file.exists(fn.in.refcorp)) {
    rc <- readRDS(fn.in.refcorp)
  } else {
    rc <- LoadReferenceCorpus(lang, udmodel)
  }
  feat.doclevel$data <- merge(feat.doclevel$data,
                              CalculateMeanWordRank(x, rc, by = Unit))
  threshold_len <- rc[upos != "PUNCT", quantile(nchar(token), 0.75)][[1]]
  rm(rc)
  invisible(gc(verbose = FALSE))
  feat.doclevel$data <- merge(feat.doclevel$data,
                              LongWordRatio(x, threshold = threshold_len,
                                            fixed.threshold = FALSE, by = Unit))
  feat.doclevel$data <- merge(feat.doclevel$data,
                              LongWordRatio(x, threshold = 8,
                                            fixed.threshold = TRUE, by = Unit))
  feat.doclevel$data <- merge(feat.doclevel$data,
                              SimpleToComplex(x, by = Unit))
  feat.doclevel$data <- merge(feat.doclevel$data,
                              CalculateLexDivMeasures(x,
                                                      lower = FALSE,
                                                      ignore.punct = TRUE,
                                                      by = Unit))
  feat.doclevel$data <- merge(feat.doclevel$data,
                              ExplicitNaming(x, by = Unit))
  feat.doclevel$data <- merge(feat.doclevel$data,
                              SingleNaming(x, by = Unit))
  feat.doclevel$data <- merge(feat.doclevel$data,
                              MeanMultipleNaming(x, pos = "PROPN", by = Unit))
  feat.doclevel$data <- merge(feat.doclevel$data,
                              GetRepetitions(x, normalise = TRUE,
                                             by = Unit))
  feat.doclevel$data <- merge(feat.doclevel$data,
                              AveragePMI(x, by = Unit))
  feat.doclevel$data <- merge(feat.doclevel$data,
                              ReadabilityMeasures(x, by = Unit))
  
  cat("Calculating N-Gram Features ...\n")
  x <- MarkSentenceBorders(x)
  
  feat.ngrams <- list(lang = lang)
  feat.ngrams$meta <- feat.doclevel$meta
  feat.ngrams <- append(feat.ngrams,
                        GetNgrams(x, terms = c("upos"),
                                  from = 1, to = 2, ignore.borders = F,
                                  ignore.punct = F, lowercase = T,
                                  normalise = "len", by = Unit))
  feat.ngrams <- append(feat.ngrams,
                        GetPunctuationCounts(x, by = Unit))
  feat.ngrams <- append(feat.ngrams,
                        list(mfws = GetMostFrequentWords(x, n = 100,
                                                         term = "token", ignore.punct = T,
                                                         lowercase = T, ignore.pos = c("Content_Words", "SYM"),
                                                         normalise = "len", by = "partition")))
  if (save.output == TRUE) {
    #cat("  Saving Output Files ... \n")
    saveRDS(feat.doclevel, file = path.doclevel)
    saveRDS(feat.ngrams, file = path.ngrams)
  }
  if (return.output == TRUE) {
    return(list(doclevel = feat.doclevel,
                ngrams = feat.ngrams))
  }
}