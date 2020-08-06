setwd("~/indirect-translation/")
invisible(gc(c))
rm(list=ls(all=TRUE))


library(pbapply)
library(udpipe)
library(data.table)
library(parallel)
source("R/functions.R")

#options(warn=1)

# Define Constants

Content_Words <- c("ADJ", "ADV", "NOUN", "NUM", "PROPN", "VERB", "X") # decie if "X" is 
Function_Words <- c("ADP", "AUX", "CCONJ", "DET", "INTJ", "PART", "PRON", "SCONJ")
Non_Words <- c("PUNCT", "SYM")
Punctuation_Marks = c("?", "!", ":", ";", "-", "(", ")", "[", "]", "'", "\"",
                      "/", ",", ".", "—", "…", "―", "«", "»")
Cons <- "[qwrtzpsdfghjklxcvbnmśłżźćńj]"
Vow <- "[aeiouáéűąęąęåäáæøèõéêëîïôœùûüåéíóúyůýěöüóóäöüypаеиоуъюαεηήιουωя]"

cat("\nCONFIGURATION OF FEATURE EXTRACTION PARAMETERS:\n\n")
Set_Parameters <- "hardcoded" # switch this to "interactive" or "hardcoded"

# consider loading external parameter file using library "properties")
# File is in format:
# val1=7
# > cfg <- read.properties("testparams.txt")
# cgf$val1

if (Set_Parameters == "hardcoded") {
  Dir_In <- "~/corpora/europarl-extract/data/comparable" # Path to plain text files
  # extracted with EuroparlExtract toolkit (https://github.com/mustaszewski/europarl-extract)
  Dir_Out <- "~/indirect-translation/data/"
  Dir_Models <- "models/"
  Dir_ReferenceCorpora <- "models/referencecorpora/"
  #Languages <- c("BG", "CS", "DA", "DE", "EL", "EN", "ES", "ET", "FI", "FR",
  #               "HU", "IT", "LT", "LV", "NL", "PL", "PT", "RO", "SK", "SL",
  #               "SV") #c("PL", "EN") #  c("EN") #c("PL", "EN")
  Languages <- c("DA", "DE")
  
  #mfw.method <- "wordlist"
  #mfw.parameter <- "./wordlists/"
  Use_Tfidf <- FALSE
  Save_Binaries <- TRUE
  Retain_MultiWordTokens <- TRUE
  Lowercase_Tokens <- FALSE
  Ignore_Punctuation <- TRUE
  Mark_Sentence_Borders <- TRUE
  Dependency_Parsing <- FALSE
  Keep_Text <- TRUE
  Keep_Xpos <- FALSE
  Normalise_Digits <- TRUE
  Normalise <- TRUE
  CalculateCharacterNGrams <- FALSE
  MinimumTextLength <- 1700 # 1700
  MinimumTextLengthForPartition <- 100
  Unit <- "partition" # options: c("partition", "doc_id)
  ShuffleDocsInPartitions <- TRUE
  
} else { 
  params <- SpecifyParameters()
  Dir_In <- params[[1]]
  Dir_Out <- params[[2]]
  Languages <- params[[3]]
  Use_Tfidf <- params[[4]]
  Save_Binaries <- params[[5]]
  Retain_MultiWordTokens <- params[[6]]
  Lowercase_Tokens <- params[[7]]
  Ignore_Punctuation <- params[[8]]
  Mark_Sentence_Borders <- params[[9]]
}


cat("Input folder:\t", Dir_In, "\n")
cat("Output folder:\t", Dir_Out, "\n")
cat("Languages:\t", Languages, "\n")
#cat("MFW Method:\t", mfw.method, "\n")
#cat("MFW Parameter:\t", mfw.parameter, "\n")
cat("TFIDF weighting:", Use_Tfidf, "\n")
cat("Save binaries:\t", Save_Binaries, "\n")
cat("Retain multiwordtokens:\t", Retain_MultiWordTokens, "\n")
cat("Lowercase tokens:\t", Lowercase_Tokens, "\n")
cat("Ignore punctuation:\t", Ignore_Punctuation, "\n")
cat("Mark Sentence Borders:\t", Mark_Sentence_Borders, "\n")
cat("Normalise:\t", Normalise, "\n")


if (Save_Binaries == TRUE) {
  CreateOutputDirs(Dir_Out)
}

cat("\nRetrieving list of EuroParl files in specified input folder ...  ")
fnames_all_langs.v <- dir(Dir_In, recursive = T,  full.names = T)
fnames_all_langs.v <- gsub("//", "/", fnames_all_langs.v)
cat("  --> DONE,", length(fnames_all_langs.v), "files in input folder\n\n")

if (Unit == "partition") {
  fn.out.debug <- gsub("//", "/",
                       paste0(Dir_Out, "/partitions/languages_base", "debug_", Sys.time(), ".csv"))
} else {
  fn.out.debug <- gsub("//", "/",
                       paste0(Dir_Out, "/documents/languages_base", "debug_", Sys.time(), ".csv"))
}

debug.dt <- data.table(Indicator = c("files_in_inputfolder",
                                     "docs_nonempty_into_memory",
                                     "docs_punct_normalised",
                                     "docs_in_split_corp",
                                     "docs_in_anno_after_parse",
                                     "docs_in_anno_after_parse_rbind",
                                     "docs_in_anno_nondigitrich",
                                     "docs_in_anno_for_partition",
                                     "docs_in_anno_after_partition",
                                     "partitions_in_anno",
                                     "items_in_meta_created",
                                     "items_in_meta_lengthfiltered",
                                     "items_in_ngrams",
                                     "items_in_mfws",
                                     "time_extraction"))

#lang = "DE"

for (lang in Languages) {
  ptm_lang <- proc.time()
  if (Unit == "partition") {
    fn.out.anno <- gsub("//", "/",
                        paste0(Dir_Out, "/partitions/languages_base/", tolower(lang), "_anno.rds"))
    fn.out.corp <- gsub("//", "/",
                        paste0(Dir_Out, "/partitions/languages_base/", tolower(lang), "_corp.rds"))
    fn.out.ng.tok <- gsub("//", "/",
                          paste0(Dir_Out, "/partitions/languages_base/", tolower(lang), "_ngrams-tok.rds"))
  } else {
    fn.out.anno <- gsub("//", "/",
                        paste0(Dir_Out, "/documents/languages_base/", tolower(lang), "_anno.rds"))
    fn.out.corp <- gsub("//", "/",
                        paste0(Dir_Out, "/documents/languages_base/", tolower(lang), "_corp.rds"))
    fn.out.ng.tok <- gsub("//", "/",
                          paste0(Dir_Out, "/documents/languages_base/", tolower(lang), "_ngrams-tok.rds"))}
  # From vector of filenames , select chosen language as specified by language code 
  files.v <- fnames_all_langs.v[grep(paste0("/", lang, "/"), fnames_all_langs.v)]
  #rm(fnames_all_langs.v)
  # files.v <- files.v[1:8000]
  
  corp <- list(lang = lang)
  corp$texts <- data.table(doc_id = files.v, stringsAsFactors = F, keep.rownames = T)
  cat("###########################   ", lang, "   ###########################\n\n")
  debug.infiles <- nrow(corp$texts)
  cat(debug.infiles, "documents in input folder for", lang, "\n\n")
  rm(files.v)
  if (nrow(corp$texts) < 1) {
    next
  }
  cat("Reading ", nrow(corp$texts), " text files into memory ...\n", sep="") ## Read text files into list of text vectors for each text
  corp$texts[, text := pbsapply(doc_id, function(x) ReadTextfile(x))]
  
  #corpus.dt[, textlen := nchar(text)]
  

  # Remove empty texts, if any
  corp$texts <- corp$texts[nchar(text) > 0, ]
  
  # Removing empty texts from texts.l
  #texts.l <- RemoveEmptyTexts(texts.l)
  #nonemty_texts.v <- sapply(texts.l, function(x) { length(x) > 0 })
  #files.v <- files.v[nonemty_texts.v]
  #texts.l <- texts.l[nonemty_texts.v]
  debug.docs.in.mem  <- nrow(corp$texts)
  cat("   --> DONE,", debug.docs.in.mem, " non-empty texts read into memory\n\n", sep="")
  
  ### SIMPLIFY DOCUMENT NAMES TO SAVE MEMORY
  corp$texts[, doc_id := sapply(doc_id, function(fn) {
    paste(tail(unlist(strsplit(fn, "/")), 2), collapse="/") }
  )]
  setorder(corp$texts, doc_id)
  
  ### REPLACE EACH SEQUENCE OF 1+ DIGITS BY "9"
  if (Normalise_Digits == TRUE) {
    corp$texts[, text := gsub("[[:digit:]]+", "9", text)]
    #gsub("([[:digit:]][[:blank:]]?)+", "#", x)
  }
  
  #corp$texts[, len := nchar(text), ]
  #Replace nonbreaking whitespace by regular whitespace # Alternative: gsub("\\intToUtf8(160)", "#", y2)
  corp$texts[, text := gsub("\xc2\xa0", " ", text)] 
  # Remove leading dots, dashes, hyphens or commas at beginning of texts
  corp$texts[, text := gsub("^([–,\\.-][[:space:]]?)+", "", text)]
  # Remove leading parentheses with some content in between at beginning of texts
  corp$texts[, text := gsub("^[[:space:]]?\\([^\\)]*\\)[[:space:]]?", "", text)]
  # Once again, remove leading dots, dashes, hyphens or commas at beginning of texts which may occur after removed parenthesis as wells
  corp$texts[, text := gsub("^([–,\\.-][[:space:]]?)+", "", text)]
  
  # Remove empty texts, if any
  corp$texts <- corp$texts[nchar(text) > 0, ]
  debug.docs.punctnormalised <- nrow(corp$texts)
  cat(debug.docs.punctnormalised,
      "\ttexts remaining texts after punctuation normalisation\n", sep="")
  
  #Only for Debgging
  # corp$texts <- corp$texts[, .SD[sample(.N, 3000)]]
  # setorder(corp$texts, doc_id)
  
  
  ### P A R S I N G
  ### TOKENISE, POS-TAG (AND DEPENDENCY PARSE) CORPUS
  model_path <- gsub("//", "/",
                     paste0(Dir_Models, tolower(lang), "-ud-2.0-170801.udpipe")
  )
  model <- udpipe_load_model(file = model_path)
  #annotation <- udpipe_annotate(model, x = corpus.dt$TEXT)
  
  corpus_splitted <- split(corp$texts, ceiling(seq_along(corp$texts$doc_id)/100)) # preserve original order
  debug.docs.splitted <- sum(sapply(corpus_splitted, function(x) length(unique(x$doc_id))))
  #debug.op4 <- sum(sapply(split(corp$texts, ceiling(seq_along(corp$texts$doc_id)/100)), function(x) length(unique(x$doc_id))))
  cat(debug.docs.splitted,
      "\ttexts in splitted corpus that are ready for parsing\n", sep="")
  gc(verbose = F)
  cat("Available memory: ",
      as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=T)),
      "\n", sep="")
  
  ### DEBUG ONLY:
  #corpus_splitted <- corpus_splitted[1:30]
  #cat("For debugging, subsetting first 50 elements of corpus_splitted, texts in total: ",
  #    sum(sapply(corpus_splitted, function(x) length(unique(x$doc_id)))),
  #    "\n", sep="")
  
  # At this stage of research, no dependency parsing is needed - only lemmatisation and POS tagging
  cat("\nParsing and annotating corpus with UDPipe ...  \n")
  
  ptm_anno <- proc.time()

  annotation.dt <- mclapply(corpus_splitted, FUN = ParseSplits, t = "dt",
                            model, mc.cores = 4)

  t_elapsed <- (proc.time() - ptm_anno)[3]/60
  cat("  --> DONE in ", t_elapsed, " minutes!\n\n", sep="")
  rm(corpus_splitted)
  
  
  cat(sum((sapply(annotation.dt, length)) == 1),
      "\tempty splits after parsing\n", sep="")
  debug.docs.parsed <- sum(sapply(annotation.dt, function(x) length(unique(x$doc_id))))
  cat(debug.docs.parsed,
      "\tdocs in annotation.dt after parsing and before rbinding: ",
       "\n", sep="")
  
  annotation.dt <- rbindlist(annotation.dt)
  #annotation.dt2 <- rbindlist(annotation.dt2)
  
  debug.docs.parsed.binded <- length(unique(annotation.dt$doc_id))
  cat(debug.docs.parsed.binded, "\tdocs in binded parsed annotation\n", sep="")
  
  if (Keep_Xpos == FALSE) {
    annotation.dt[, c("xpos", "feats") := NULL]
  }
  
  
  # Add unique identifier for each combination of document and sentence
  annotation.dt$doc_sent_id <- unique_identifier(annotation.dt,
                                                 fields = c("doc_id", "sentence_id"),
                                                 start_from = 1)
  
  annotation.dt[, sl := substr(doc_id, 1, 2)]
  
  # Draw Radom Sample from annotation.dt for debugging
  # View(annotation.dt[sample(.N, 200)])
  # If desired, remove chunks < 1800 tokens
  #annotation.dt[, .N < 500, by = .(partition)]$V1  
  
  ### REPLACE EACH SEQUENCE OF 1+ DIGITS BY "9"
  if (Normalise_Digits == TRUE) {
    annotation.dt[, token := gsub("[[:digit:]]+", "9", token)]
    annotation.dt[, lemma := gsub("[[:digit:]]+", "9", lemma)]
  }
  
  # Calculate length of each token in characters
  annotation.dt[, token_len := nchar(token)]
  
  # Calculate number of tokens for each sentence (with and w/o punctuation marks)
  annotation.dt[, nr_tokens := .N, by = list(doc_sent_id)]
  annotation.dt[, nr_tokens_noPunct := sum(upos != "PUNCT"),
                by = list(doc_sent_id)]
  annotation.dt[, digit_tokens_per_doc := sum(upos == "NUM"),
                by = list(doc_id)]
  annotation.dt[, text_len := .N,
                by = list(doc_id)]
  annotation.dt[, doc_len_noPunct := NROW(upos[upos != "PUNCT"]),
                by = list(doc_id)]
  
  # Remove all documents that contain >= 30% digit tokens (e.g. listings of EU resolution numbers)
  annotation.dt <- annotation.dt[(digit_tokens_per_doc / doc_len_noPunct) < 0.3, ]
  debug.docs.digitpoor <- length(unique(annotation.dt$doc_id))
  cat(debug.docs.digitpoor,
      "\ttexts remaining after removing digit-rich texts\n", sep="")  
  
  # PARTITION CORPUS INTO CHUNKS OF 2000 TOKENS
  if (Unit == "partition") {
    
    # Remove all documents shorter than specified minimum for inclusion into partitions
    annotation.dt <- annotation.dt[doc_len_noPunct >= MinimumTextLengthForPartition, ]
    debug.docs.filtered.for.part <- length(unique(annotation.dt$doc_id))
    cat(debug.docs.filtered.for.part,
        "\tdocs > ", MinimumTextLengthForPartition,
        " tokens selected for partitioning\n\n", sep="")
    
    cat("Partitioning corpus into chunks of 2000 tokens ... ")
    t_prt <- proc.time()
    
    annotation.dt[, year := GetYearFromID(doc_id)]
    
    # Shuffle order of documents within each year
    if (ShuffleDocsInPartitions == "TRUE") {
      annotation.dt[, TokenOrder := seq(.N), by = doc_id]
      doclengths <- annotation.dt[, .N, by = doc_id]$N
      set.seed(10)
      annotation.dt[, RandomDocID := rep(sample(c(1:length(unique(annotation.dt$doc_id))), replace = F), doclengths)]
      setorder(annotation.dt, sl, year, RandomDocID, TokenOrder)
      annotation.dt[, c("RandomDocID", "TokenOrder", "digit_tokens_per_doc") := NULL]
    }
    
    annotation.dt[, partition :=
                    PartitionCorpus(doc_sent_id, part.size = 2000), by = .(sl, year)]
    annotation.dt[, partition := paste(sl, "_", year, "_",
                                       formatC(partition,
                                               width = nchar(max(partition)) + 1,
                                               format = "d", flag = 0),
                                       sep = "")]
    t_prt <- (proc.time() - t_prt)[3]/60
    cat("  --> DONE in ", t_prt, " minutes!\n\n", sep = "")
    debug.partitions.in.anno <- length(unique(annotation.dt$partition))
    debug.docs.after.part <- length(unique(annotation.dt$doc_id))
    cat(debug.docs.after.part,
        "\tdocs remaining after partitioning - number of partitions: ",
        debug.partitions.in.anno,
        "\n", sep="")
  } else {
    debug.partitions.in.anno <- NA
    debug.docs.after.part <- length(unique(annotation.dt$doc_id))
    cat(debug.docs.after.part,
        "\tdocs remaining - NO partitioning was done\n", sep="")
  }
  
  # Calculate sentence lengths (in characters)
  annotation.dt[, sent_len := sum(nchar(token)), by=.(doc_sent_id)]
  annotation.dt[, sent_len_noPunct :=  sum(nchar(token[upos != "PUNCT"])),
                by=.(doc_sent_id)]
  
  
  
  ### M E T A D A T A 
  ### GET METADATA FROM FILE PATHS
  #corp$meta <- NULL
  #corp$data <- NULL
  #Unit == "doc_id"
  corp$meta <- data.table(id = annotation.dt[, unique(get(Unit))])
  
  # Add column that contains vector of all doc_ids for each partition
  
  #if (Unit == "partition") {
  #  docs_in_partition <- annotation.dt[, .(docs_in_partition = paste(unique(doc_id), collapse = ", ")),
  #                                     by = partition]
  #  corp$meta <- merge(corp$meta, docs_in_partition,
  #                     by.x = c("id"),
  #                     by.y = c("partition"))
  #  rm(docs_in_partition)
  #}
  debug.items.meta <- nrow(corp$meta)
  cat("\nCreated corp$meta of ", debug.items.meta, " items and ", ncol(corp$meta), " rows.\n", sep="")
  
  # Get translational status of text, i.e. original vs. translated subsection
  #corp$meta[, type := GetCorpusTypeFromFilename(doc_id)]
  corp$meta[, sl := substr(id, 1, 2)]
  corp$meta[, tl := lang]
  if (Unit == "doc_id") {
    #dates <- GetDatesFromFilename(corp$meta$id)
    #corp$meta[, year := dates$year]
    #corp$meta[, month := dates$month]
    #corp$meta[, day := dates$day]
    #rm(dates)
    corp$meta[, year := GetYearFromFilename(id)]
  } else {
    corp$meta[, year := substr(id, 4, 7)]
  }
  corp$meta[, type := ifelse(sl == lang, "ORIG", "TRANS")]
  corp$meta[, type_adv := ifelse(sl == lang, "ORIG",
                                 ifelse(as.integer(year) > 2003, "T_ID", "T_DIR" ))]
  corp$meta[, phase := ifelse(as.integer(year) > 2003, "AFTER", "BEFORE" )]
  
  
  ### DOCUMENT LENGTHS:
  ### In this step, order of corp$meta$id becomes alphabetical, whereas order of IDs in annotation.dt stays the same
  corp$meta <- merge(corp$meta,
                     GetDocumentLengths(annotation.dt, by = Unit))
  
  ### Subset corpus: keep only documents longer than certain threshold
  corp$meta <- corp$meta[tokens >= MinimumTextLength, ]
  annotation.dt <- annotation.dt[get(Unit) %in% corp$meta$id]
  debug.items.meta.lengthfiltered <- nrow(corp$meta)
  cat(debug.items.meta.lengthfiltered, 
      "\titems remain in corp$data after removing items < ",
      MinimumTextLength, " tokens - number of docs in annotation.dt: ",
      length(unique(annotation.dt$doc_id)), "\n", sep="")
  # Remove SENTENCE column from annotation.dt here?
  annotation.dt[, sentence := NULL]
  saveRDS(annotation.dt, file = fn.out.anno)

  
  if (Keep_Text == FALSE) {
    corp$texts <- NULL
  } else if (Keep_Text == TRUE & Unit == "partition") {
    corp$texts <- data.table(id = corp$meta$id)
    partitioned.texts <- annotation.dt[, .(text = paste(token, collapse = " ")),
                                       by = partition]
    corp$texts <- merge(corp$texts, partitioned.texts,
                        by.x = c("id"),
                        by.y = c("partition"))
    rm(partitioned.texts)
  }
  
  
  corp$data <- data.table(id = corp$meta$id)
  rownames(corp$meta) <- corp$meta$id
  rownames(corp$data) <- corp$meta$id
  cat("Created corp$data of ", nrow(corp$data), " rows and ", ncol(corp$data), " columns\n", sep="")
  
  
  ###################
  ###################
  ###################
  ###################   START FEATURE EXTRACTION (DOCUMENT-LEVEL FEATURES)
  ###################
  ###################
  ###################
  ###################
  
  
  ### SENTENCE LENGTHS IN TOKENS
  # Calculate Mean Sentence Length (in tokens) for each document
  corp$data <- merge(corp$data,
                     CalculateMeanTokensPerSentence(annotation.dt, by = Unit))
  
  ### SENTENCE LENGTHS IN CHARACTERS
  # Calculate mean sentence lenght (in characters) for each document
  corp$data <- merge(corp$data,
                     CalculateMeanSentenceLength(annotation.dt, by = Unit))
  
  ### MEAN WORD LENGTHS (in characters)
  corp$data <- merge(corp$data,
                     CalculateMeanTokenLength(annotation.dt, by = Unit))
  
  ### CALCULATE LEXICAL DENSITY
  corp$data <- merge(corp$data,
                     CalculateLexDens(annotation.dt, by = Unit))
  
  ### REFERENCE CORPUS FEATURES
  # Load reference corpus
  # To do: specify path to reference corpus
  # To do: fallback option if no reference corpus provided or file missing
  fn.in.refcorp <- gsub("//", "/",
                        paste0(Dir_ReferenceCorpora, "/", tolower(sl), "_refcorp.rds"))
  if (file.exists(fn.in.refcorp)) {
    ref_corp.dt <- readRDS(fn.in.refcorp)
  } else {
    ref_corp.dt <- LoadReferenceCorpus(lang, model)
  }
  rm(model)
  # 1) MEAN WORD RANK
  corp$data <- merge(corp$data,
                     CalculateMeanWordRank(annotation.dt, ref_corp.dt, by = Unit))
  
  # 2) RATIO LONG WORDS
  # Proportion of above-average long words (i.e. word longer than certain threshold)
  # Meaningful threshold values can be: mean, median, 3rd quartile, 90th percentile, ...
  threshold_len <- ref_corp.dt[upos != "PUNCT", quantile(nchar(token), 0.75)][[1]]
  rm(ref_corp.dt)
  corp$data <- merge(corp$data,
                     LongWordRatio(annotation.dt, threshold = threshold_len,
                                   fixed.threshold = FALSE, by = Unit))
  ### RATIO OF LONG WORDS (> 8 characters)
  corp$data <- merge(corp$data,
                     LongWordRatio(annotation.dt, threshold = 8,
                                   fixed.threshold = TRUE, by = Unit))
  
  ### SHALLOW SYNTACTIC FEATURES
  # 1) Complex vs. Simple Sentences
  corp$data <- merge(corp$data,
                     SimpleToComplex(annotation.dt, by = Unit))
  
  ### CALCULATE LEXICAL DIVERSITY: MTLD & TTR & YULE'S K
  
  corp$data <- merge(corp$data,
                     CalculateLexDivMeasures(annotation.dt,
                                             lower = Lowercase_Tokens,
                                             ignore.punct = Ignore_Punctuation,
                                             by = Unit))

  ### EXPLICITATION FEATURES
  # 1) EXPLICIT NAMING:
  # RATIO OF PERSONAL PRONOUNS TO PROPER NOUNS
  cat("Calculating explicit naming feature ... ")
  corp$data <- merge(corp$data,
                     ExplicitNaming(annotation.dt, by = Unit))
  
  # 2) SINGLE NAMING:
  # FREQ OF PROPN WITHOUT ADJACENT PROPER NAMES
  # Number of PROPN tags not preceded/followed by another PROPN tag
  # divided by number of total non-punctuation tokens
  # gouped by documents
  cat("Calculating single naming feature ... ")
  corp$data <- merge(corp$data,
                     SingleNaming(annotation.dt, by = Unit))
  cat("  --> DONE!\n\n")
  
  
  # 3) MEAN MULTIPLE NAMING
  corp$data <- merge(corp$data,
                     MeanMultipleNaming(annotation.dt, pos = "PROPN", by = Unit))  
  
  ### NORMALIZATION FEATURES
  # 1) Repetitions
  # Count number of content words that occur >1 time per text, normalised by text length in tokens
  corp$data <- merge(corp$data,
                     GetRepetitions(x = annotation.dt, normalise = TRUE,
                                    by = Unit))
  
  # AVERAGE PMI
  ### AVERAGE POINTWISE MUTUAL INFORMATION FOR BIGRAMS
  corp$data <- merge(corp$data,
                     AveragePMI(x = annotation.dt, by = Unit, repl.na = "0"))
  
  
  ### READABILITY INDICES (COLEMAN LIAU & LIX)
  #corpus.dt <- merge(corpus.dt, ReadabilityMeasures())
  corp$data <- merge(corp$data,
                     ReadabilityMeasures(x = annotation.dt, by = Unit))
  # Consider additional readability measures: LIX 68 revisited & OVIX
  
  saveRDS(corp, file = fn.out.corp)
  #rm(corp)
  annotation.dt[, c("sl", "year", "text_len", "doc_len_noPunct") := NULL]
  
  ###################
  ################### CONTINUE FEATURE EXTRACTION (N-GRAM & TOKEN FEATURES)
  ###################
  
  
  #Add sentence begining/end markers if flag is set
  
  ### TOKEN CLASS AND SHAPE REPRESENTATIONS
  # Barre'a > Xx*'x
  # Hypothesis: token shape feature high accuracy because captures foreign names in genitive, at least for PL
  # also captures contracted forms, e.g. I'll > X'x*
  invisible(gc(verbose = F))
  
  
  if (Mark_Sentence_Borders == TRUE) {
    annotation.dt <- MarkSentenceBorders(annotation.dt)
    #saveRDS(annotation.dt, file = fn.out.anno)
  }
  
  
  #annotation.dt[upos != "PUNCT",
  #              c("tok_shape", "tok_class") := .(TokenShape(token), sapply(token, TokenClass))]
  
  # SYLLABLE STRUCTURE REPRESENTATION
  # Syllable Structure omitted for proper nouns in order to avoid effects of
  # foreign language proper names biasing results (cf. exclusion of content word ngrams)
  # Not yet perfected, consonant/vowel definitions for languages other than Polish
  # missing/incomplete.
  
  #annotation.dt[!is.na(token_id) & upos != "PUNCT" & upos != "PROPN",
  #              syll_struct := sapply(token, SyllableStructure)]
  
  
  # WORD-LEVEL N-GRAMS
  ngrams_tok <- list(lang = lang)
  ngrams_tok$meta <- corp$meta
  rm(corp)
  #ngrams_tok <- append(ngrams_tok,
  #                     GetNgrams(x = annotation.dt, terms = c("upos", "token"),
  #                               from = 1, to = 3, ignore.borders = F,
  #                               ignore.punct = F, lowercase = T,
  #                               normalise = "len", by = Unit))
  
  ngrams_tok <- append(ngrams_tok,
                       GetNgrams(x = annotation.dt, terms = c("upos"),
                                 from = 1, to = 2, ignore.borders = F,
                                 ignore.punct = F, lowercase = T,
                                 normalise = "len", by = Unit))
  
  
  debug.items.ngrams <- nrow(ngrams_tok$upos.1gram.norm.len)
  cat(debug.items.ngrams,
      "\titems in n-grams counts\n\n", sep="")
  
  ### CONTECXTUAL FUNCTION WORDS
  ContextFctWord <- FALSE
  if (ContextFctWord == TRUE) {
    ngrams_tok <- append(ngrams_tok,
                         list(contextFctWords = GetContextualFctWords(x = annotation.dt,
                                                                      normalise = Normalise, by = Unit)))
  }
  
  ### POSITIONAL TOKEN FREQUENCIES
  PosTokFreq <- FALSE
  if (PosTokFreq == TRUE) {
    ngrams_tok <- append(ngrams_tok,
                         list(position.tok.freq = GetPositionalFrequencies(annotation.dt,
                                                                           min.len = 5,
                                                                           ignore.punct = TRUE,
                                                                           normalise = Normalise, by = Unit)))
  }
  
  ### PUNCTUATION:
  
  ngrams_tok <- append(ngrams_tok,
                       GetPunctuationCounts(annotation.dt, by = Unit))
  
  # MOST FREQUENT WORDS
  ngrams_tok <- append(ngrams_tok,
                       list(mfws = GetMostFrequentWords(x = annotation.dt, n = 100,
                                                        term = "token", ignore.punct = T,
                                                        lowercase = T, ignore.pos = c("Content_Words", "SYM"),
                                                        normalise = "len", by = Unit)))
  debug.items.mfws <- nrow(ngrams_tok$mfws)
  cat(debug.items.mfws,
      "\titems in MFW counts\n\n", sep="")
  
  
  saveRDS(ngrams_tok, file = fn.out.ng.tok)
  rm(ngrams_tok)
  
  # CHARACTER-LEVEL N-GRAMS
  if (CalculateCharacterNGrams == TRUE) {
    fn_out_ng_char <- gsub("//", "/",
                           paste0(Dir_Out, "/", tolower(lang), "_ngrams-char.RData"))
    char_ngrams.l <- CharNgrams(x = annotation.dt, from = 2, to = 3, normalise = Normalise)
    texts_chargrams <- nrow(char_ngrams.l[[1]])
    cat("Char n-grams calculated for ", texts_chargrams, " texts.\n\n", sep="")
    ptm_save <- proc.time()
    cat("Saving output file ...")
    saveRDS(char_ngrams.l, file = fn_out_ng_char)
    t_save <- (proc.time() - ptm_save)[3]/60
    cat("  --> DONE in ", t_save, " minutes!", sep = "")
    rm(char_ngrams.l)
  } else {
    texts_chargrams <- NA
  }
  
  rm(annotation.dt)
  
  ### FUNCTION WORDS:
  # Normalised frequencies of all words from external list of function words,
  # including content words that are indispensable to organise text.
  # Currently not implemented because no manually crafted language-specific
  # knowledge is to be used at this stage of research
  
  # Normalised frequencies of all words from external list of pronouns.
  # Currently not implemented because no manually crafted language-specific
  # knowledge is to be used at this stage of research
  
  ############### SAVE BINARY OUTPUT FILES #####################################
  
  invisible(gc(c))
  t_lang <- (proc.time() - ptm_lang)[3]/60
  cat("EXTRACTION FOR ", lang, " COMPLETED in ", t_lang, " minutes!\n\n", sep ="")
  #debug.dt <- rbind(debug.dt, list(lang, nr_texts, debug.op6, texts_wordgrams, texts_mfws, texts_chargrams, t_lang))
  
  debug.langsummary <- c(debug.infiles,
                         debug.docs.in.mem,
                         debug.docs.punctnormalised,
                         debug.docs.splitted,
                         debug.docs.parsed,
                         debug.docs.parsed.binded,
                         debug.docs.digitpoor,
                         debug.docs.filtered.for.part,
                         debug.docs.after.part,
                         debug.partitions.in.anno,
                         debug.items.meta,
                         debug.items.meta.lengthfiltered,
                         debug.items.ngrams,
                         debug.items.mfws,
                         floor(t_lang))
  debug.dt <- cbind(debug.dt, debug.langsummary)
  colnames(debug.dt)[which(lang == Languages)+1] <- lang
  
  
  write.table(debug.dt, file = fn.out.debug, sep = "\t", row.names = F)
}

