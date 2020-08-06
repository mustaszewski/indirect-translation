ShowSize <- function(x) {
  print(object.size(x), units = "auto")
}

CreateOutputDir <- function(outdir) {
  if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = T)
  }
}


#' DEPRECATED FUNCTION, USE Sample_IDs_Two_SourceLang(...) INSTEAD
SampleSL_TraDir_TraIndir <- function(x.d, tlang, slang, seed = 10) {
  x.d <- x.d[tl == eval(tlang) & sl == eval(slang), ]
  # For selected SL-TL pair, draw balanced sample of T_DIR vs T_ID
  npt <- min(x.d[, .N, by = .(type_adv)]$N)
  set.seed(seed)
  op <- x.d[, ][,.SD[sample(.N, min(.N, npt))], , by = type_adv]$id
  return(op)
}

#' DEPRECATED FUNCTION, USE Sample_IDs_One_SourceLang(...) INSTEAD
SampleTL_Before_After <- function(x.d, tlang, slang, seed = 10) {
  # For selected TL pair, draw balanced sample of translations before and after 2004
  x.d <- x.d[tl == eval(tlang) & sl == eval(slang), ]
  npt <- min(x.d[, .N, by = .(phase)]$N)
  set.seed(seed)
  op <- x.d[, ][,.SD[sample(.N, min(.N, npt))], , by = phase]$id
  return(op)
}

#' Returns a randomly sampled vector of IDs such that the number of items
#' is balanced across all levels of selected variable for given language pair
#' Example: Equal number of direct vs indirect translations for FI-DE or
#' equal number of Italian originals before vs after 2004 for IT-IT.
#' @param x.d (data.table) The data frame from which sample of IDs is to be drawn
#' @param tlang (character) The target language (e.g. 'DE')
#' @param slang (character) The first source language (e.g. 'FI')
#' @param var.meta (charcter) The variable (= column of x.d) across whose levels
#' balanced sample is to be drawn.
#' @param seed (int) Random seed. Default = 10
#' @export
Sample_IDs_One_SourceLang <- function(x.d, tlang, slang, var.meta, seed = 10) {
  x.d <- x.d[tl == eval(tlang) & sl == eval(slang), ]
  min.n <- min(x.d[, .N, by = var.meta]$N)
  set.seed(seed)
  ids <- x.d[, ][,.SD[sample(.N, min(.N, min.n))], , by = var.meta]$id
  return(ids)
}


#' Returns a randomly sampled vector of IDs such that the number of items
#' is balanced across both language pairs and across all levels of selected variable.
#' Example: Equal number of direct vs indirect translations for FI-DE and EN-DE
#' Default value for var.meta is 'type_adv'.
#' @param x.d (data.table) The data frame from which sample of IDs is to be drawn
#' @param tlang (character) The target language (e.g. 'DE')
#' @param slang1 (character) The first source language (e.g. 'FI')
#' @param slang2 (character) The second source language (e.g. 'EN')
#' @param var.meta (charcter) The variable (= column of x.d) across whose levels
#' balanced sample is to be drawn. Default is 'type_adv'
#' @param  aggregate.slang2 (bool) If True: Each level in SL2 has same number of instances as the levels in SL1;
#' if False: Sum over all levels of SL2 is equal to the number of instances in SL1 levels. The former gives the same number of instances in SL1 and SL2, whereas the latter gives nr_of_levels times more instances in SL1 than in SL2. Default is TRUE.
#' @param seed (int) Random seed. Default = 10
#' @export
Sample_IDs_Two_SourceLang <- function(x.d, tlang, slang1, slang2 = "EN",
                                      var.meta = "type_adv",
                                      aggregate.slang2 = TRUE,
                                      seed = 10) {
  x.d <- x.d[tl == tlang & (sl == slang1 | sl == slang2), ] # subset input data table according to languages
  min.slang1 <- min(x.d[sl == slang1, .N, by = var.meta]$N) # number of items in least frequent level of variable for slang1
  min.slang2 <- min(x.d[sl == slang2, .N, by = var.meta]$N) # number of items in least frequent level of variable for slang2
  min.both <- min(min.slang1, min.slang2) # number of items in least frequent level of variable for both slang's
  set.seed(seed)
  ids.slang1 <- x.d[sl == slang1, ][,.SD[sample(.N, min(.N, min.both))], , by = var.meta]$id # draw sample of n = min.both for slang1
  if (aggregate.slang2 == FALSE) {
    set.seed(seed)
    ids.slang2 <- x.d[sl == slang2, ][,.SD[sample(.N, min(.N, min.both))], by = var.meta]$id # draw sample of n = min.both for slang2
  } else {
    nr.per.level <- length(unique(x.d[, get(var.meta)]))
    set.seed(seed)
    # If aggregate.slang2 == True: draw sample of n=min.both / nr.per.level for slang2 
    # such that sum across all levels for slang2 is equal to min.both (i.e. instances per level for slang1)
    ids.slang2 <- x.d[sl == slang2, ][,.SD[sample(.N, min(.N, ceiling(min.both/nr.per.level)))], by = var.meta]$id
    if (length(ids.slang2) > min.both) {
      excess <- length(ids.slang2) - min.both
      set.seed(seed)
      ids.slang2 <- ids.slang2[-sample(1:length(ids.slang2), excess)]
    }
  }
  ids <- c(ids.slang1, ids.slang2) # concatenate ids of each source language
  return(ids)
}

#' DEPRECATED FUNCTION, USE Sample_IDs_Two_SourceLang(...) INSTEAD
SampleSL_TraDir_TraIndir_OtherTrans <- function(l1, l2, seed = 10) {
  # For selected SL-TL pair, draw balanced sample of T_DIR vs T_ID vs. DIR from l2
  min.l1 <- min(corp$meta[sl == l1, .N, by = .(sl, type_adv)]$N)
  n.l2.td <- corp$meta[sl == l2 & type_adv == "T_DIR", .N]
  npt <- min(min.l1, n.l2.td)
  set.seed(seed)
  op <- corp$meta[sl == l1, ][,.SD[sample(.N, min(.N, npt))], , by = type_adv]$id
  set.seed(seed)
  en <- corp$meta[sl == l2 & type_adv == "T_DIR", ][,.SD[sample(.N, min(.N, npt))], ]$id
  op <- c(op, en)
  return(op)
}

#' DEPRECATED FUNCTION, USE Sample_IDs_Two_SourceLang(...) WITH PARAMETER aggregate.slang2
#' st to TRUE INSTEAD
SampleSL_TraDir_TraIndir_BalancedOtherTrans <- function(l1, l2, seed = 10) {
  # For selected SL-TL pair, draw balanced sample of T_DIR vs T_ID vs. DIR from l2
  min.l1 <- min(corp$meta[sl == l1, .N, by = .(sl, type_adv)]$N)
  min.l2 <- min(corp$meta[sl == l2, .N, by = .(sl, type_adv)]$N)
  npt <- min(min.l1, min.l2)
  set.seed(seed)
  op <- corp$meta[sl == l1, ][,.SD[sample(.N, min(.N, npt))], , by = type_adv]$id
  set.seed(seed)
  en <- corp$meta[sl == l2, ][,.SD[sample(.N, min(.N, ceiling(npt/2)))], by = type_adv]$id
  if (length(en) > npt) {
    excess <- length(en) - npt
    set.seed(seed)
    en <- en[-sample(1:length(en), excess)]
  }
  op <- c(op, en)
  return(op)
}


SampleSL_TraDir_TraIndir_TwoOtherTrans <- function(l1, l2, l3, seed = 10) {
  # For selected SL-TL pair, draw balanced sample of T_DIR vs T_ID
  min.l1 <- min(corp$meta[sl == eval(l1), .N, by = .(type_adv)]$N)
  min.l1.l2.tdir <- min(corp$meta[(sl == l2 | sl == l3) & type_adv == "T_DIR", .N, by = .(sl, type_adv)]$N)
  npt <- min(min.l1, min.l1.l2.tdir)
  set.seed(seed)
  l1.ids <- corp$meta[sl == l1, ][,.SD[sample(.N, min(.N, npt))], , by = type_adv]$id
  set.seed(seed)
  l2.ids <- corp$meta[sl == l2 & type_adv == "T_DIR", ][,.SD[sample(.N, min(.N, npt))], ]$id
  set.seed(seed)
  l3.ids <- corp$meta[sl == l3 & type_adv == "T_DIR", ][,.SD[sample(.N, min(.N, npt))], ]$id
  l1.ids <- c(l1.ids, l2.ids, l3.ids)
  return(l1.ids)
}


################################################ UPDATE PARSE SAMPLED ORIGINALS
#################################################
ParseSampledOriginals <- function(sl, tl, ids = "none",
                                  direction = c("sl>tl", "tl>sl"), dir,
                                  shuffle = FALSE) {
  ptm_reparse <- proc.time()
  direction <- match.arg(direction)

  # Data in both lang.base and lang.target will be reparsed and partitioned,
  # because only docs that exist in boith languages will be used for partitions
  lang.base <- ifelse(direction == "tl>sl", tl, sl) # language whose selected doc IDs serve as basis for reparsing docs
  lang.target <- ifelse(direction == "tl>sl", sl, tl) # language that is targeted by resampling
  dir.out.pair <- file.path(dir, "language_pairs",
                            paste0(tolower(sl), "-", tolower(tl)),
                            ifelse(direction == "sl>tl", "source_to_target", "target_to_source"))
  CreateOutputDir(dir.out.pair)
  
  if (direction == "tl>sl") {
    cat("Re-Parsing ", lang.target, " Originals of ", lang.base,
        " Translations ...\n", sep = "")
  } else {
    cat("Re-Parsing ", lang.target, " Translations of ", lang.base,
        " Originals ...\n", sep = "")
  }

  # Define file names of input annotation files  
  fn.in.anno.target <- file.path(dir, "languages_base" , paste0(tolower(lang.target), "_anno.rds"))
  fn.in.anno.base <- file.path(dir, "languages_base" , paste0(tolower(lang.base), "_anno.rds"))
  
  # Define file names of output files: parsed annotations, doclevel features and
  # ngram features
  fn.out.anno.base <- file.path(dir.out.pair, paste0(tolower(lang.base),
                                                     "_anno.rds"))
  fn.out.anno.target <- file.path(dir.out.pair, paste0(tolower(lang.target),
                                                       "_anno.rds"))
  
  fn.out.corp.base <- file.path(dir.out.pair, paste0(tolower(lang.base),
                                                     "_corp.rds"))
  fn.out.corp.target <- file.path(dir.out.pair, paste0(tolower(lang.target),
                                                       "_corp.rds"))
  fn.out.tok.base <- file.path(dir.out.pair, paste0(tolower(lang.base),
                                                    "_ngrams.rds"))
  fn.out.tok.target <- file.path(dir.out.pair, paste0(tolower(lang.target),
                                                      "_ngrams.rds"))
  
  # Load annotation file for both languages. For tl, annotation file has already
  # been loaded in kMeansClustering.R in order to avoid loading it for each sl
  if (direction == "tl>sl") {
    cat("  Loading Base Annotation (", lang.base, " TTs) ...\n", sep = "")
    if ("none" %in% ids) {
      anno.base <- anno.tl[sl == lang.target, ]
    } else {
      anno.base <- anno.tl[partition %in% ids, ] # anno.tl is "global" variable defined in kMeansClustering.R
    }
    cat("  Loading Target Annotation (", lang.target, " STs) ...\n", sep = "")
    anno.target <- readRDS(fn.in.anno.target)
    anno.target <- anno.target[sl == lang.target]
  } else {
    cat("  Loading Base Annotation (", lang.base, " STs) ...\n", sep = "")
    anno.base <- readRDS(fn.in.anno.base)
    if ("none" %in% ids) {
      anno.base <- anno.base[sl == lang.base]
    } else {
      anno.base <- anno.base[partition %in% ids]
    }
    cat("  Loading Target Annotation (", lang.target, " TTs) ...\n", sep = "")
    anno.target <- anno.tl[sl == lang.base] # anno.tl is "global" variable defined in kMeansClustering.R
  }
  
  # Keep only those docs from base/target annotation that have a translated/original
  # counterpart in docnames of corresponding base/target annotation
  docs.in.both <- intersect(ConvertDocIds(unique(anno.base$doc_id), l.to = tl),
                            unique(anno.target$doc_id)) # has doc-name-format of anno.target
  anno.base <- anno.base[ConvertDocIds(doc_id, l.to = tl) %in% docs.in.both]
  anno.target <- anno.target[doc_id %in% docs.in.both]
  invisible(gc())
  
  # Debugging shows that there are some base partitions that are very short
  # (e.g. consisting of only one document), because the other documents that were
  # originally part of this partition are no longer available after filtering
  # those document IDs that have counterpart in SL and TL.
  
  ## NOW PARTITION (AND SHUFFLE) THE BASE ANNOTATION
  # Shuffle order of documents within each year (optional step)
  if (shuffle == TRUE) {
    anno.base[, TokenOrder := seq(.N), by = doc_id]
    doclengths <- anno.base[, .N, by = doc_id]$N
    set.seed(10)
    anno.base[, RandomDocID := rep(sample(c(1:length(unique(anno.base$doc_id))), replace = F), doclengths)]
    setorder(anno.base, sl, year, RandomDocID, TokenOrder)
    anno.base[, c("RandomDocID", "TokenOrder") := NULL]
  }
  # End shuffeling
  
  #anno.base.BACKUP <- copy(anno.base) # DEBUG
  # Partition base annotation
  anno.base[, partition :=
              PartitionCorpus(doc_sent_id, part.size = 2000), by = .(sl, year)]
  anno.base[, partition := as.numeric(partition)]
  anno.base[, partition := paste(sub("/.+", "", doc_id), "_", year, "_",
                                 formatC(partition,
                                         width = nchar(max(partition)) + 1,
                                         format = "d", flag = 0),
                                 sep = "")]
  # End partitioning

  # Delete partitions that are shorter than 1700 tokens
  # (such partitions are usually the 'remainder' of last partition of a given year
  anno.base[, partitionlength := .N, by = partition]
  anno.base <- anno.base[partitionlength >= 1700]
  anno.base[, partitionlength := NULL]
  
  # Create data table indicating for each partition the starting and ending
  # sentence of all documents contained in given partition
  docs.base <- anno.base[, .(first_sentence = min(sentence_id),
                             last_sentence = max(sentence_id)),
                         by = .(partition, doc_id)]
  docs.base[, doc_id := ConvertDocIds(doc_id, l.to = tl)]
  colnames(docs.base)[1] <- "partition_base"

  anno.base <- anno.base[!is.na(token_id), ]
  anno.target <- anno.target[!is.na(token_id), ]

  anno.target <- anno.target[docs.base, on = "doc_id", allow.cartesian = T]
  anno.target <- anno.target[!is.na(token_id), ]
  anno.target[, doc_nr_in_partition := rleid(doc_id), by = .(partition_base)]
  anno.target <- anno.target[,
                             .SD[(doc_nr_in_partition == 1 & sentence_id >= first_sentence) | (doc_nr_in_partition != 1)],
                             by = .(partition_base)]
  anno.target[, token_id_running := seq(.N), by = .(partition_base)]
  anno.target[, next_sent_is_same := (doc_sent_id == shift(doc_sent_id, type = "lead"))]
  anno.target <- anno.target[,
                             .SD[c(1 : ifelse(.N <= 2100, # set other value than 2100 ???
                                              .N, first(which(token_id_running > 2000 & next_sent_is_same ==  FALSE)))), ],
                             by = .(partition_base)]
  
  anno.target[, c("first_sentence", "last_sentence", "doc_nr_in_partition",
              "token_id_running", "next_sent_is_same", "partition") := NULL]
  colnames(anno.target)[1] <- "partition"
  
  # Delete partitions that are shorter than 1700 tokens
  # (such partitions are usually the 'remainder' of last partition of a given year
  anno.target[, partitionlength := .N, by = partition]
  anno.target <- anno.target[partitionlength >= 1700, ]
  anno.target[, partitionlength := NULL]
  
  partitions.in.both <- intersect(anno.base$partition, anno.target$partition)
  anno.base <- anno.base[partition %in% partitions.in.both, ]
  anno.target <- anno.target[partition %in% partitions.in.both, ]
  
  anno.target[, partition := ConvertPartitionIds(partition, l.to = tl)]

  debug.base <- anno.base[, .(doclength = .N, first_sentence = min(sentence_id),
                              last_sentence = max(sentence_id)),
                          by = .(partition, doc_id)]; debug.base[, partlength := sum(doclength), by = partition]
  #View(debug.base)
  debug.target <- anno.target[, .(doclength = .N, first_sentence = min(sentence_id),
                                  last_sentence = max(sentence_id)),
                              by = .(partition, doc_id)]; debug.target[, partlength := sum(doclength), by = partition]
  #View(debug.target)
  
  debug.base.docs.not.in.target <- sum(!ConvertDocIds(unique(anno.base$doc_id), l.to = tl) %in% anno.target$doc_id) # DEBUG ONLY
  debug.target.docs.not.in.base <- sum(!ConvertDocIds(unique(anno.target$doc_id), l.to = tl) %in% anno.base$doc_id) # DEBUG ONLY
  debug.base.partitions.not.in.target <- sum(!ConvertPartitionIds(unique(anno.base$partition), l.to = tl) %in% anno.target$partition)
  debug.target.partitions.not.in.base <- sum(!ConvertPartitionIds(unique(anno.target$partition), l.to = tl) %in% anno.base$partition)
  
  cat("  Finished re-parsing based on language-pairs. Debug stats:\n")
  cat("    base docs not in target: ", debug.base.docs.not.in.target, "\n", sep = "")
  cat("    target docs not in base: ", debug.target.docs.not.in.base, "\n", sep = "")
  cat("    base partitions not in target: ", debug.base.partitions.not.in.target, "\n", sep = "")
  cat("    target partitions not in base: ", debug.target.partitions.not.in.base, "\n", sep = "")
  
  saveRDS(anno.base, file = fn.out.anno.base)
  saveRDS(anno.target, file = fn.out.anno.target)

  cat ("  Re-partitioning completed, starting feature extraction for ", lang.target,
       " ...\n\n", sep = "")
  
  ExtractFeatures(x = anno.target, lang = lang.target,
                  save.output = TRUE, return.output = FALSE,
                  path.doclevel = fn.out.corp.target,
                  path.ngrams = fn.out.tok.target)
  #fts.corp.target <- feats$doclevel
  #fts.ngr.target <- feats$ngrams
  #rm(feats)
  #View(fts.corp.target$meta); View(fts.corp.target$data)
  #View(fts.ngr.target$meta); View(as.matrix(fts.ngr.target$upos1))
  
  cat ("  Starting feature extraction for ", lang.base, " ...\n")
  ExtractFeatures(x = anno.base, lang = lang.base,
                  save.output = TRUE, return.output = FALSE,
                  path.doclevel = fn.out.corp.base,
                  path.ngrams = fn.out.tok.base)
  #fts.corp.base <- feats.base$doclevel
  #fts.ngr.base <- feats.base$ngrams
  #rm(feats.base)
  #View(fts.corp.base$meta); View(fts.corp.base$data)
  #View(fts.ngr.base$meta); View(as.matrix(fts.ngr.base$upos1))
  
  t_reparse = (proc.time() - ptm_reparse)[3]/60
  cat("\n  --> DONE in ", t_reparse, "minutes!\n\n")
}


SampleSL_Org_TraDir <- function(lang, seed = 10) {
  # For selected SL-TL pair, draw balanced sample of ORIG vs T_DIR
  org <- corp$meta[type_adv == "ORIG", ]
  tra_d <- corp$meta[type_adv == "T_DIR" & sl == eval(lang)]
  npt = min(nrow(org), nrow(tra_d))
  set.seed(seed)
  op <- c(
    org[sample(.N, npt)]$id,
    tra_d[sample(.N, npt)]$id
  )
  return(op)
}

SampleSL_Org_TraIndir <- function(lang) {
  # For selected SL-TL pair, draw balanced sample of ORIG vs T_ID
  org <- corp$meta[type_adv == "ORIG", ]
  tra.id <- corp$meta[type_adv == "T_ID" & sl == eval(lang)]
  npt = min(nrow(org), nrow(tra.id))
  set.seed(10)
  op <- c(
    org[sample(.N, npt)]$id,
    tra.id[sample(.N, npt)]$id
  )
  return(op)
}



SampleSL_Org_TraDir_TraInd <- function(lang) {
  # For selected SL-TL pair, draw balanced sample of ORIG vs T_DIR vs T_ID
  npt <- min(corp$meta[sl == eval(lang) | type == "ORIG", .N, by = .(type_adv)]$N)
  set.seed(10)
  op <- corp$meta[sl == lang, ][,.SD[sample(.N, min(.N, npt))], , by = type_adv]$id
  set.seed(10)
  op <- c(op,
          corp$meta[type == "ORIG", ][sample(.N, npt)]$id)
  return(op)
}

SampleAllSL_Org_TD <- function(lang) {
  # From all SL for given TL, draw balanced sample of 2000 ORIG + 200 T_DIR per SL
  set.seed(10)
  total_origs <- nrow(corp$meta[type_adv == "ORIG"])
  nr_sls <- length(unique(corp$meta[type_adv != "ORIG", sl]))
  if (total_origs < 2000) {
    nr_origs <- total_origs
    nr_per_sl <- round(total_origs / nr_sls, 0)
  } else {
    nr_origs <- 2000
    nr_per_sl <- 200
  }
  op <- c(corp$meta[type_adv == "ORIG", ][sample(.N, nr_origs)]$id,
          corp$meta[type_adv == "T_DIR", ][,.SD[sample(.N,min(.N, nr_per_sl))], , by = sl]$id)
  return(op)
}

SampleAllSL_Before_After <- function(lang) {
  # From all SL for given TL, draw balanced sample of 2000 ORIG + 200 T_DIR per SL
  set.seed(10)
  total_ <- nrow(corp$meta[type_adv == "ORIG"])
  nr_sls <- length(unique(corp$meta[type_adv != "ORIG", sl]))
  if (total_origs < 2000) {
    nr_origs <- total_origs
    nr_per_sl <- round(total_origs / nr_sls, 0)
  } else {
    nr_origs <- 2000
    nr_per_sl <- 200
  }
  op <- c(corp$meta[type_adv == "ORIG", ][sample(.N, nr_origs)]$id,
          corp$meta[type_adv == "T_DIR", ][,.SD[sample(.N,min(.N, nr_per_sl))], , by = sl]$id)
  return(op)
}

SampleAllSL_Org_TD_TI <- function(n) {
  # From all SL for given TL, draw balanced sample of 2000 ORIG + 200 T_DIR  + 200 T_ID per SL
  set.seed(10)
  total_origs <- nrow(corp$meta[type_adv == "ORIG"])
  nr_sls <- length(unique(corp$meta[type_adv != "ORIG", sl]))
  if (total_origs < 2000) {
    nr_origs <- total_origs
    nr_per_sl <- round(total_origs / nr_sls, 0)
  } else {
    nr_origs <- 2000
    nr_per_sl <- 100
  }
  op <- c(corp$meta[type_adv == "ORIG", ][sample(.N, nr_origs)]$id,
          corp$meta[type_adv == "T_DIR", ][,.SD[sample(.N,min(.N, nr_per_sl))], , by = sl]$id)
  # All SLs, balanced: 2000 ORIG + 200 T_DIR per SL + 200 T_ID per SL
  set.seed(10)
  op <- c(op,
          corp$meta[type_adv == "T_ID", ][,.SD[sample(.N,min(.N, nr_per_sl))], , by = sl]$id)
  return(op)
}

ConvertDocIds <- function(x, l.to) {
  # This function converts filenames of translations to filenames of originals (FR-DE/08-de.txt > FR/08-fr.txt) 
  # and vice versa (FR/08-fr.txt > FR-DE/08-de.txt)
  # l.to needs only to be specified if filenames of originals are converted to translation filenames
  if (grepl("^[[:upper:]]{2}-", x[[1]])) { # if filnemae starts with "FR-", i.e. indicates translation
    sl <- substr(x, 1, 2)[1]
    tl <- substr(x,4, 5)[1]
    #Equivalent to regex tl <- sub("([[:upper:]]{2})-([[:upper:]]{2})/.*", "\\2", x)[[2]]
    x <- sub(paste0("-", toupper(tl)),
             "",
             x)
    x <- sub(paste0("-", tolower(tl)),
             paste0("-", tolower(sl)),
             x)
  } else { # if filename indicates non-translated original
    sl <- substr(x, 1, 2)[1]
    tl <- l.to
    x <- sub(toupper(sl),
             paste0(toupper(sl), "-", toupper(tl)),
             x)
    x <- sub(paste0("-", tolower(sl)),
             paste0("-", tolower(tl)),
             x)
  }
  return(x)
}



ConvertPartitionIds <- function(x, l.to) {
  # This function converts partition names of translations to partition names of originals (FR_1996_0001 > FR-DE_1996_0001) 
  # and vice versa (FR-DE_1996_0001 > FR_1996_0001)
  # l.to needs only to be specified if filenames of originals are converted to translation filenames
  if (grepl("^[[:upper:]]{2}-", x[[1]])) { # if filnemae starts with "FR-", i.e. indicates translation
    sl <- substr(x, 1, 2)[1]
    tl <- substr(x,4, 5)[1]
    #Equivalent to regex tl <- sub("([[:upper:]]{2})-([[:upper:]]{2})/.*", "\\2", x)[[2]]
    x <- sub(paste0("-", toupper(tl)),
             "",
             x)
  } else { # if filename indicates non-translated original
    sl <- substr(x, 1, 2)[1]
    tl <- l.to
    x <- sub(toupper(sl),
             paste0(toupper(sl), "-", toupper(tl)),
             x)
  }
  return(x)
}
