setwd("~/indirect-translation/")
rm(list=ls(all=TRUE))
invisible(gc())

library(caret)
library(FactoMineR)
library(factoextra)
library(cluster)
library(ggpubr)
source("R/analysisfunctions.R")
source("R/functions.R")
library(udpipe)
library(data.table)
library(parallel)
library(doSNOW)


### DEFINE FEATURES
feat.meta <- c("id", "sl", "tl", "subcorpus", "year", "type", "type_adv", "phase", "tokens","sents") # "tokens", "tokens_noPct", "sents"
feat.doc <- c("mean_tok_nr_noPunct", "mean_tok_len_noPunct", "lex_dens_noPunct",
              "mean_word_rank_noPunct", "simp_tot", "mtld", "single_names_ratio",
              "mean_propn_seq", "reps_norm", "pmi_mean", "lix")
feat.doc.punct <- c("mean_tok_nr", "mean_tok_len", "lex_dens",
                    "mean_word_rank", "simp_tot", "mtld", "single_names_ratio",
                    "mean_propn_seq", "reps_norm", "pmi_mean", "lix")
# Preference was given to non-punctuation features over punctuation features, because they give better classification accuracies in preliminary tests for all SLs into DE (both in isolation, i.e. using only doc features, and in combination of doc features with UPOS1 features). Furthermore, doc features that do not ignore punctuation implicitly contain information about text length (because shorter average sentences leads to more punctuation marks in texts).
# In the dataset, the suffix “_noPunct” has been deleted from the variale names in columns in order to keep things simple.

feat.upos1 <- c("ADJ", "ADP", "ADV", "AUX", "CCONJ", "DET", "NOUN", "NUM",
                "PART", "PRON", "PROPN", "PUNCT", "SCONJ", "VERB", "X")
# Discarded POS: SYM (because very scarce) and INTJ (because very scarce, to be # more precise 0 for entire data into DE)
feat.selection <- c(feat.meta, feat.doc, feat.upos1)
feat.num <- c(feat.doc, feat.upos1) # Only numeric features (no metadata) - required for PCA and SVM

### DEFINE PARAMETERS FOR RE-PARTITIONING AND ANALYSES
SourceLanguages <- c("DA", "DE", "EL", "EN", "ES", "FI", "FR", "IT", "NL", "PT", "SV")
TargetLanguages <- c("DA", "DE")
Unit <- "partition" # choices: 'partitions' or 'doc_id'
Retain_MultiWordTokens <- TRUE
Dependency_Parsing <- FALSE
Keep_Text <- TRUE
Direction_Repartition <- "tl>sl" # choices: c("sl>tl", "tl>sl")
Dir_Models <- file.path("~", "research", "translationese_2018-01", "models")
Dir_ReferenceCorpora <- file.path("~", "research", "translationese_2018-01", "models", "referencecorpora")
Dir_Data <- file.path("~", "research/translationese_2018-01", "data",
                      ifelse(Unit == "partition", "partitions", "documents"))
Dir_Plots <- file.path("~", "research/translationese_2018-01", "analyses", "TS_Paper")
Res_Path <- file.path(Dir_Plots,
                      ifelse(Unit == "partition", "partitions", "documents"),
                      "00_results_all.csv") # Path to results file
Proj_Data_Path <- file.path(Dir_Plots,
                            ifelse(Unit == "partition", "partitions", "documents"),
                            "01_embeddingprojector_data.csv")
Proj_Meta_Path <- file.path(Dir_Plots,
                            ifelse(Unit == "partition", "partitions", "documents"),
                            "01_embeddingprojector_meta.csv")
Seed_Rand <- 10
Reparse_Corpora <- FALSE # Whether to perform Step 1 (reparsing)
Merge_Corpora <- FALSE # Whether to perform Step 2 (merging data for all language pairs into one large data frame). NOTE: The merged data frame is REQUIRED for step 3; if it does not exist, create it first.
Compute_Analyses <- TRUE # Whether to perform Step 3 (computation for analyses for each language pair). NOTE: The results data frame (res.all) is REQUIRED for step 4; if it does not exist, create it first.
Make_Plots <- TRUE # Whether to perform Step 4 (draw plots for each language pair). NOTE: The results data frame (res.all) is REQUIRED for step 4; if it does not exist, create it first.
Summarize_Corpus <- TRUE # Whether to perform Step 5 (summary statistics for sampled corpora). NOTE: The exported metadata file for the Embedding projector (proj.meta) is REQUIRED for step 5; if it does not exist, create it first.



# Define palette of 11 well-distinguishable colors
#cols11 <- c("dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00",
#            "deeppink1", "gold1", "blue1", "#FB9A99", "palegreen2", "#CAB2D6")

################################################################################
########## STEP 1: RE-PARTITION CORPORA ACCORDING TO LANGUAGE PAIRS
# For each language pair of interest (only translations into DE),
# re-partition both source and target texts. This will give "pseudo-parallel"
# comparable corpora for each language pair. In each partition on the source side, only
# documents that exist in TL are included, and vice versa. It was attempted to match
# SL and TL as much as possible, by accounting for the start and end sentences of
# documents in the partitions.
# The direction of re-partitioning is from source to target, which means that SL
# partitions are recreated in TL.

## 1.1.
# Load TL annotations (in this case: DA and DE).

if (Reparse_Corpora == TRUE) {
  for (TargetLanguage in TargetLanguages) {
    cat ("\n#############\tRE-PARTITIONING ALL SUBCORPORA FOR TARGET LANGUAGE ", TargetLanguage, "\t##############\n\n",
         sep = "")
    
    anno.tl <- readRDS(file.path(Dir_Data, "languages_base", paste0(tolower(TargetLanguage), "_anno.rds")))
    anno.tl <- anno.tl[sl %in% union(SourceLanguages, TargetLanguage), ]
    gc()
    
    ## 1.2.
    # For every language pair into TL, re-partition source and target texts.
    # There is no return variable, but the re-partitioned SL and TL files are stored
    # in data/partitions/language_pairs/. In function call, argument 'ids' is set to
    # 'none' because all SL partitions are to be matched with corresponding TL partitions.
    # However, it is possible to re-partition corpora based on sampled IDs of the base
    # language (i.e. the SL if direction is 'sl>tl> or the TL if direction is 'tl>sl').
    # Once the re-partitioned corpora are available, they can be loaded and analysed
    # (i.e. clustered, PCAed, SVMed, ...) individually, which will be done in Step 3.
    for (SourceLanguage in SourceLanguages) {
      if (SourceLanguage == TargetLanguage) { 
        next # No need to parse DE originals, because they have no TL counterpart
      }
      cat ("## Re-Partitioning Corpus ", SourceLanguage, " to ", TargetLanguage, " ...",
           sep = "")
      ParseSampledOriginals(sl = SourceLanguage, tl = TargetLanguage, ids = "none",
                            direction = Direction_Repartition, dir = Dir_Data,
                            shuffle = TRUE)
    }
  }
  
}


################################################################################
########## STEP 2:  MERGE ALL INDIVIDUAL REPARSED SL and TL CORPORA INTO ONE LARGE
##########          CORPUS FILE (saved in language_pairs/all)
if (Merge_Corpora == TRUE) {
  gc()
  # Initialize empty data frame to store individual language-pair data frames
  if (Keep_Text == TRUE) {
    corp.all <- list(data = setNames(data.table(matrix(nrow = 0, ncol = length(feat.selection))),
                                     feat.selection),
                     texts = setNames(data.table(matrix(nrow = 0, ncol = 2)),
                                      c("id", "text")))
  } else {
    corp.all <- list(data = setNames(data.table(matrix(nrow = 0,
                                                       ncol = length(feat.selection))),
                                     feat.selection),
                     texts = NA)
  }
  
  # Iterate over all language pairs to add them to corp.all
  for (TargetLanguage in TargetLanguages) {
    cat("\n########\tMERGING LANGUAGE-PAIR DATA FOR TARGET LANGUAGE ", TargetLanguage, " ...\n",
        sep = "")
    
    for (SourceLanguage in SourceLanguages) {
      cat("## PAIR: ", SourceLanguage, " > ", TargetLanguage, "\n", sep = "")
      
      if (SourceLanguage != TargetLanguage) {
        subcorpus <- tolower(paste0(SourceLanguage, "-", TargetLanguage))
        
        fn.in.corp.sl <- file.path(Dir_Data, "language_pairs", subcorpus,
                                   ifelse(Direction_Repartition == "sl>tl", "source_to_target", "target_to_source"),
                                   paste0(tolower(SourceLanguage), "_corp.rds"))
        fn.in.ngram.sl <- file.path(Dir_Data, "language_pairs", subcorpus,
                                    ifelse(Direction_Repartition == "sl>tl", "source_to_target", "target_to_source"),
                                    paste0(tolower(SourceLanguage), "_ngrams.rds"))
        fn.in.corp.tl <- file.path(Dir_Data,"language_pairs", subcorpus,
                                   ifelse(Direction_Repartition == "sl>tl", "source_to_target", "target_to_source"),
                                   paste0(tolower(TargetLanguage), "_corp.rds"))
        fn.in.ngram.tl <- file.path(Dir_Data, "language_pairs", subcorpus,
                                    ifelse(Direction_Repartition == "sl>tl", "source_to_target", "target_to_source"),
                                    paste0(tolower(TargetLanguage), "_ngrams.rds"))
        corp.sl <- readRDS(fn.in.corp.sl)
        ngrams.sl <- readRDS(fn.in.ngram.sl)
        corp.tl <- readRDS(fn.in.corp.tl)
        ngrams.tl <- readRDS(fn.in.ngram.tl)
        
        ngrams.sl <- ngrams.sl$upos.1gram.norm.len # Keep only UPOS 1-grams
        ngrams.tl <- ngrams.tl$upos.1gram.norm.len # Keep only UPOS 1-grams
        
        corp.tl$meta[, subcorpus := toupper(subcorpus)] # Add column to identify language pair
        corp.tl$meta <- corp.tl$meta[, c("id", "sl", "tl", "subcorpus", "year", "type",
                                         "type_adv", "phase", "tokens",
                                         "tokens_noPct", "sents")] # Reorder columns
        corp.tl$data <- merge(corp.tl$meta, corp.tl$data)
        corp.tl$data <- corp.tl$data[, c(feat.meta, feat.doc), with = F]
        ngrams.tl <- data.table(as.matrix(ngrams.tl), keep.rownames = T)
        x.tl <- merge(corp.tl$data, ngrams.tl, by.x = "id", by.y = "rn")
      } else {
        subcorpus <- "base"
        fn.in.corp.sl <- file.path(Dir_Data, "languages_base", paste0(tolower(SourceLanguage), "_corp.rds"))
        fn.in.ngram.sl <- file.path(Dir_Data, "languages_base", paste0(tolower(SourceLanguage), "_ngrams-tok.rds"))
        corp.sl <- readRDS(fn.in.corp.sl)
        ngrams.sl <- readRDS(fn.in.ngram.sl)
        
        corp.sl$meta <- corp.sl$meta[sl == SourceLanguage, ]
        corp.sl$data <- corp.sl$data[id %in% corp.sl$meta$id,]
        corp.sl$texts <- corp.sl$texts[id %in% corp.sl$meta$id,]
        
        ngrams.sl <- ngrams.sl$upos.1gram.norm.len # Keep only UPOS 1-grams
        ngrams.sl <- ngrams.sl[rownames(ngrams.sl) %in% corp.sl$meta$id, ]
        
        x.tl <- setNames(data.table(matrix(nrow = 0, ncol = length(feat.selection))),
                         feat.selection)
        corp.tl = list(texts = setNames(data.table(matrix(nrow = 0, ncol = 2)),
                                        c("id", "text")))
      }
      
      corp.sl$meta[, subcorpus := toupper(subcorpus)] # Add column to identify language pair
      corp.sl$meta <- corp.sl$meta[, c("id", "sl", "tl", "subcorpus", "year", "type",
                                       "type_adv", "phase", "tokens",
                                       "tokens_noPct", "sents")] # Reorder columns
      
      corp.sl$data <- merge(corp.sl$meta, corp.sl$data)
      corp.sl$data <- corp.sl$data[, c(feat.meta, feat.doc), with = F] # x[, bla, with = F] == x[, ..bla] == x[, .SD, .SDcols == bla]
      ngrams.sl <- data.table(as.matrix(ngrams.sl), keep.rownames = T)
      x.sl <- merge(corp.sl$data, ngrams.sl, by.x = "id", by.y = "rn")
      
      corp.all$data <- rbindlist(list(corp.all$data, x.sl, x.tl), fill = T)
      if (Keep_Text == TRUE) {
        corp.all$texts <- rbindlist(list(corp.all$texts, corp.sl$texts, corp.tl$texts), fill = T)
      }
    }
    cat(">> DONE!\n")
  }
  
  cat("\nSaving corp.all to folder ", Dir_Data, sep = "")
  dim(corp.all$data)
  corp.all$data[, .N, by = .(sl,tl,subcorpus)]
  
  colnames(corp.all$data) <- gsub("_noPunct", "", colnames(corp.all$data))
  feat.doc <- gsub("_noPunct", "", feat.doc)
  feat.selection <- gsub("_noPunct", "", feat.selection)#
  feat.num <- gsub("_noPunct", "", feat.num)
  fn.out.data.all <- file.path(Dir_Data, "language_pairs", "all",
                               ifelse(Direction_Repartition == "sl>tl", "source_to_target", "target_to_source"),
                               "all-pairs_data.rds")
  CreateOutputDir(dirname(fn.out.data.all))
  saveRDS(corp.all, file = fn.out.data.all)
  View(corp.all$data[, .N, by = .(sl, tl, subcorpus)])
}
###################### END STEP 2





################################################################################
########## STEP 3:  COMPUTE ANALYSES FOR EACH LANGUAGE PAIR
##########

if (Compute_Analyses == TRUE) {
  ## 3.1. Load Data Created in Step 2
  if (! exists("corp.all")) {
    fn.data.all <- file.path(Dir_Data, "language_pairs", "all",
                             ifelse(Direction_Repartition == "sl>tl", "source_to_target", "target_to_source"),
                             "all-pairs_data.rds")
    if (file.exists(fn.data.all)) {
      corp.all <- readRDS(fn.data.all)
      colnames(corp.all$data) <- gsub("_noPunct", "", colnames(corp.all$data))
      feat.doc <- gsub("_noPunct", "", feat.doc)
      feat.selection <- gsub("_noPunct", "", feat.selection)
    } else {
      cat("corp.all does not exist, please create it at path ", fn.data.all)
    }
  }
  
  ## 3.2. Define variables
  feat.doc <- gsub("_noPunct", "", feat.doc)
  feat.selection <- gsub("_noPunct", "", feat.selection)#
  feat.num <- gsub("_noPunct", "", feat.num)
  pivot <- "EN"
  
  # Initialize data table that will store results of SVM classification for each language pair
  res.all <- data.table(SL = character(), TL = character(), SIZE = numeric(),
                        TRANS_ACC = numeric(), TRANS_KAPPA = numeric(),
                        TRANS_EN_ACC = numeric(), TRANS_EN_KAPPA = numeric(),
                        ORIG_ACC = numeric(), ORIG_KAPPA = numeric())
  
  # Initialize data table to export data to Tensorflow Embedding Projector format (https://projector.tensorflow.org/) 
  proj.data <- setNames(data.table(matrix(nrow = 0, ncol = length(feat.num))), feat.num)
  # Initialize data table to export metadata to Tensorflow Embedding Projector format (https://projector.tensorflow.org/) 
  proj.meta <- setNames(data.table(matrix(nrow = 0, ncol = length(feat.meta))), feat.meta)
  for (TargetLanguage in TargetLanguages) {
    cat("\n########\tCOMPUTING STATS FOR TARGET LANGUAGE ", TargetLanguage, " ...\n",
        sep = "")
    
    for (SourceLanguage in setdiff(SourceLanguages, "EN")) {
      subcorpus.current <- paste0(SourceLanguage, "-", TargetLanguage)
      
      if (SourceLanguage != TargetLanguage) {
        cat("## PAIR: ", subcorpus.current, " ...\n", sep = "")
        
        # Subset subcorpora from corp.all
        corp.sl <- corp.all$data[sl == SourceLanguage & tl == SourceLanguage &
                                   subcorpus == subcorpus.current, -c("INTJ", "SYM")]
        corp.tl <- corp.all$data[sl == SourceLanguage & tl == TargetLanguage &
                                   subcorpus == subcorpus.current, -c("INTJ", "SYM")]
        corp.pvt <- corp.all$data[sl == pivot & tl == TargetLanguage,
                                  -c("INTJ", "SYM")] # pivot language subcorpus (EN)
        corp.pvt[, subcorpus := subcorpus.current]
        corp.trans <- rbindlist(list(corp.tl, corp.pvt))
        
        # Draw balanced sample of originals written before vs. after 2004 for given SL
        ids.org <- Sample_IDs_One_SourceLang(x.d = corp.sl,
                                             slang = SourceLanguage,
                                             tlang = SourceLanguage,
                                             var.meta = "phase",
                                             seed = Seed_Rand) 
        corp.sl[, .N, by = .(sl, tl, subcorpus, phase)]
        corp.sl[id %in% ids.org, .N, by = .(sl, tl, subcorpus, phase)]
        
        # Draw balanced sample of direct vs. indirect translations in given language pair.
        ids.trans <- Sample_IDs_One_SourceLang(x.d = corp.trans,
                                               slang = SourceLanguage,
                                               tlang = TargetLanguage,
                                               var.meta = "type_adv",
                                               seed = Seed_Rand)
        
        # For given TL, draw balanced sample of direct vs. indirect translations from
        # given SL as well as from pivot language (EN). Hence, for both SL-TL and EN-TL
        # there is an equal number of direct vs. indirect translations
        ids.trans.vs.en <- Sample_IDs_Two_SourceLang(x.d = corp.all$data,
                                                     tlang = TargetLanguage,
                                                     slang1 = SourceLanguage,
                                                     slang2 = pivot,
                                                     var.meta = "type_adv",
                                                     aggregate.slang2 = TRUE,
                                                     seed = Seed_Rand)
        
        # Do some checks to see if sampling was successful
        #corp.trans[, .N, by = .(sl, tl, subcorpus, type_adv)]
        #corp.trans[id %in% ids.trans, .N, by = .(sl, tl, subcorpus, type_adv)]
        #corp.trans[id %in% ids.trans.vs.en, .N, by = .(sl, tl, subcorpus, type_adv)]
        
        
        # 1. Direct translations vs. indirect translations
        # Prepare data
        df.td.ti <- corp.trans[id %in% ids.trans]
        # Remove columns that have only NAs (e.g. DET or PREP in FI-FI:
        df.td.ti <- df.td.ti[,which(unlist(lapply(df.td.ti, function(x)!all(is.na(x))))),with=F]
        # Convert type_adv to factor
        df.td.ti$type_adv <- factor(df.td.ti$type_adv,
                                    levels = c("T_DIR", "T_ID"))
        
        # 1.1 SVM
        cl <- makeCluster(8, type = "SOCK")  # creates doSNOW cluster for paralellisation
        registerDoSNOW(cl) # registers the cluster for use with caret paralellised by doSNOW
        set.seed(Seed_Rand)
        cv.folds <- createFolds(df.td.ti$type_adv, k = 10)
        cv.cntrl <- trainControl(method = "cv",
                                 number = 10,
                                 index = cv.folds,
                                 savePredictions = "final")
        svm.td.ti <- train(x = df.td.ti[, intersect(colnames(df.td.ti), feat.num), with = F],
                           y = df.td.ti$type_adv,
                           method = "svmLinear",
                           preProcess = c("center", "scale"),
                           trControl = cv.cntrl)
        stopCluster(cl)
        print(svm.td.ti)
        cm <- confusionMatrix(svm.td.ti)
        print(cm)
        cm.path <- file.path(Dir_Plots,
                             ifelse(Unit == "partition", "partitions", "documents"),
                             paste0(toupper(TargetLanguage), "_TL"),
                             paste0("01_td-ti_svm_", subcorpus.current, ".csv"))
        CreateOutputDir(dirname(cm.path))
        write.csv(cm$table, cm.path)
        svm.td.ti.acc <- svm.td.ti$results$Accuracy
        svm.td.ti.kapp <- svm.td.ti$results$Kappa
        
        # 2. Direct vs indirect translations vs. translations from pivot language
        # Prepare data
        df.trans.vs.en <- corp.trans[id %in% ids.trans.vs.en, ] # subset subcorpus
        
        # Remove columns that have only NAs (e.g. DET or PREP in FI-FI:
        df.trans.vs.en <- df.trans.vs.en[,which(unlist(lapply(df.trans.vs.en, function(x)!all(is.na(x))))),with=F]
        
        df.trans.vs.en[sl == "EN", type_adv := "EN-DIR"] # set translations from EN to type_adv 'EN-DIR'
        df.trans.vs.en$type_adv <- factor(df.trans.vs.en$type_adv,
                                          levels = c('T_DIR', 'T_ID', 'EN-DIR'))
        
        # Append data and metadata for export files in Tensorflow Embedding Projector format
        proj.data <- rbindlist(list(proj.data,
                                    df.trans.vs.en[, intersect(colnames(df.trans.vs.en), feat.num), with = F]),
                               fill = TRUE)
        proj.meta <- rbindlist(list(proj.meta,
                                    df.trans.vs.en[, feat.meta, with = F]),
                               fill = TRUE)
        
        # 2.1 PCA: TD vs TI vs EN-SL, coloured by true metadata (type_adv)
        n <- df.trans.vs.en[, .N, by = .(sl,tl,type_adv)]$N[1]
        plot.path.png <- file.path(Dir_Plots,
                                   ifelse(Unit == "partition", "partitions", "documents"),
                                   paste0(toupper(TargetLanguage), "_TL"),
                                   paste0("02_td-ti-en_svm_", subcorpus.current, ".png"))
        plot.path.pdf <- file.path(Dir_Plots,
                                   ifelse(Unit == "partition", "partitions", "documents"),
                                   paste0(toupper(TargetLanguage), "_TL"),
                                   paste0("02_td-ti-en_svm_", subcorpus.current, ".pdf"))
        plot.title <- paste0(subcorpus.current, ": Direct vs. Indirect Translations, Compared to ",
                             pivot, "-", TargetLanguage, " (n=", n, " each)")
        pca.trans.vs.en <- PCA(df.trans.vs.en[, intersect(colnames(df.trans.vs.en), feat.num), with = F],
                               scale.unit = T, ncp = 5, graph = F)
        #pdf(file = plot.path, width = 1280, height = 960)#
        pca.plot <- fviz_pca_ind(pca.trans.vs.en,
                                 col.ind = df.trans.vs.en$type_adv,
                                 legend.title = "Type",
                                 geom.ind = c("point"),
                                 pointsize = 2.5,
                                 alpha.ind = 0.60,
                                 addEllipses = T,
                                 ellipse.alpha = 0.03,
                                 ellipse.type = "norm",
                                 ellipse.level = 0.95,
                                 title = plot.title,
                                 repel = T,
                                 palette = "jco", # "Dark2"
                                 mean.point = T) +
          theme(axis.text.x = element_text(size=12),
                axis.text.y = element_text(size=12),
                legend.text=element_text(size = 12),
                legend.title = element_text(size = 12, face = "bold"),
                axis.title = element_text(size = 12),
                plot.title = element_text(face = "bold", size = 18))
        png(file = plot.path.png, width = 2000, height = 1500, res = 200)#
        print(pca.plot)
        dev.off()
        pdf(file = plot.path.pdf, width = 10, height = 7)#
        print(pca.plot)
        dev.off()
        
        # 2.2 SVM
        cl <- makeCluster(8, type = "SOCK")  # creates doSNOW cluster for paralellisation
        registerDoSNOW(cl) # registers the cluster for use with caret paralellised by doSNOW
        set.seed(Seed_Rand)
        cv.folds <- createFolds(df.trans.vs.en$type_adv, k = 10)
        cv.cntrl <- trainControl(method = "cv",
                                 number = 10,
                                 index = cv.folds,
                                 savePredictions = "final")
        svm.trans.vs.en <- train(x = df.trans.vs.en[, intersect(colnames(df.trans.vs.en), feat.num), with = F],
                                 y = df.trans.vs.en$type_adv,
                                 method = "svmLinear",
                                 preProcess = c("center", "scale"),
                                 trControl = cv.cntrl)
        stopCluster(cl)
        print(svm.trans.vs.en)
        cm <- confusionMatrix(svm.trans.vs.en)
        print(cm)
        cm.path <- file.path(Dir_Plots,
                             ifelse(Unit == "partition", "partitions", "documents"),
                             paste0(toupper(TargetLanguage), "_TL"),
                             paste0("02_td-ti-en_svm_", subcorpus.current, ".csv"))
        write.csv(cm$table, cm.path)
        svm.trans.vs.en.acc <- svm.trans.vs.en$results$Accuracy
        svm.trans.vs.en.kapp <- svm.trans.vs.en$results$Kappa
        
        # 3. Originals: before vs. after 2004
        # Prepare data
        df.org <- corp.sl[id %in% ids.org]
        # Remove columns that have only NAs (e.g. DET or PREP in FI-FI:
        df.org <- (df.org[,which(unlist(lapply(df.org, function(x)!all(is.na(x))))),with=F])
        df.org$phase <- factor(df.org$phase,
                               levels = c('BEFORE', 'AFTER')) # Convert variable into factors
        
        # 3.1 SVM
        cl <- makeCluster(8, type = "SOCK")  # creates doSNOW cluster for paralellisation
        registerDoSNOW(cl) # registers the cluster for use with caret paralellised by doSNOW
        set.seed(Seed_Rand)
        cv.folds <- createFolds(df.org$phase, k = 10)
        cv.cntrl <- trainControl(method = "cv",
                                 number = 10,
                                 index = cv.folds,
                                 savePredictions = "final")
        svm.org <- train(x = df.org[, intersect(colnames(df.org), feat.num), with = F],
                         y = df.org$phase,
                         method = "svmLinear",
                         preProcess = c("center", "scale"),
                         trControl = cv.cntrl)
        stopCluster(cl)
        print(svm.org)
        cm <- confusionMatrix(svm.org)
        print(cm)
        cm.path <- file.path(Dir_Plots,
                             ifelse(Unit == "partition", "partitions", "documents"),
                             paste0(toupper(TargetLanguage), "_TL"),
                             paste0("03_orig_svm_", subcorpus.current, ".csv"))
        write.csv(cm$table, cm.path)
        svm.org.acc <- svm.org$results$Accuracy
        svm.org.kapp <- svm.org$results$Kappa
        
        res.all <- rbind(res.all,
                         data.table(SL=SourceLanguage, TL=TargetLanguage, SIZE = n,
                                    TRANS_ACC = round(svm.td.ti.acc*100, 2),
                                    TRANS_KAPPA = round(svm.td.ti.kapp, 2),
                                    TRANS_EN_ACC = round(svm.trans.vs.en.acc*100, 2),
                                    TRANS_EN_KAPPA = round(svm.trans.vs.en.kapp, 2),
                                    ORIG_ACC = round(svm.org.acc*100, 2),
                                    ORIG_KAPPA = round(svm.org.kapp, 2)))
        
      }
      else {
        cat("-- I N V A L I D  pair ", subcorpus.current, "  !!!\n", sep = "")
      }
    }
  }
  
  # Write res.all to CSV
  write.csv(res.all, Res_Path)
  
  # Write files in embedding projector format to CSV
  write.table(proj.data, file = Proj_Data_Path, col.names = F, row.names = F, sep="\t")
  write.table(proj.meta, file = Proj_Meta_Path, col.names = T, row.names = F, sep="\t")
  
}

### Make proj.meta and proj.data publicly available via Github Gist in order to 
### visualize data with the Tensorflow Embedding Projector:
# 1)  Create new gist: https://gist.github.com/ > Enter Description (Europarl Indirect Translation)
#     > Add two files (EP_IndirTra_Data and EP_IndirTran_Meta) > Copy-Paste Data into these two files
#     > Add configuration file (EP_IndirTra_Projector_Config), copy-pasting the template from Embedding Projector
#       and using the URL of the data/metadata files of the gist
#     > Add the URL of the projector congig gist file in the field of Step3 of Embedding Projector publication window
#     > Share URL shown in Embedding p  rojector publication window
#     > Create Public Gist
# The gist is now available from 
# https://projector.tensorflow.org/?config=https://gist.githubusercontent.com/mustaszewski/cd139efa0652c9ca67453b526a18f1d0/raw/d4c967b9a7ad33f406148c61965541cf3384f314/EP_IndirTra_Meta_ProjectorConfig.json
# Bit.ly short URL: http://bit.ly/europarl_indirtra



##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################


################################################################################
########## STEP 4:  MAKE PLOTS
##########

## 3.1. Load Data Created in Step 2
if (Make_Plots == TRUE) {
  if (! exists("res.all")) {
    if (file.exists(Res_Path)) {
      res.all <- data.table(read.table(Res_Path, sep = ",", header = TRUE))
      res.all <- res.all[, -c(1)]
    } else {
      cat("corp.all does not exist, please create it at path ", fn.data.all)
    }
  }
  
  # Add new column with label of SL plus the corresponding subcorpus size
  res.all[,SL2 := paste0(SL, "\n(n=", SIZE, ")")]
  # Subset and reshape data to long format
  x.de <- melt.data.table(res.all[TL == "DE", c("SL2", "TL", "TRANS_ACC", "ORIG_ACC")],
                          id.vars = c("SL2", "TL"))
  x.da <- melt.data.table(res.all[TL == "DA", c("SL2", "TL", "TRANS_ACC", "ORIG_ACC")],
                          id.vars = c("SL2", "TL"))
  
  x.de.en <- melt.data.table(res.all[TL == "DE", c("SL2", "TL", "TRANS_EN_ACC")],
                             id.vars = c("SL2", "TL"))
  
  x.da.en <- melt.data.table(res.all[TL == "DA", c("SL2", "TL", "TRANS_EN_ACC")],
                             id.vars = c("SL2", "TL"))
  
  setnames(x.de, c("SL", "TL", "Subcorpus", "Accuracy"))
  setnames(x.da, c("SL", "TL", "Subcorpus", "Accuracy"))
  
  setnames(x.de.en, c("SL", "TL", "Subcorpus", "Accuracy"))
  setnames(x.da.en, c("SL", "TL", "Subcorpus", "Accuracy"))
  
  x.de <- x.de[, Subcorpus := ifelse(Subcorpus == "TRANS_ACC", "Direct vs. Indirect", "Source Texts")]
  x.da <- x.da[, Subcorpus := ifelse(Subcorpus == "TRANS_ACC", "Direct vs. Indirect", "Source Texts")]
  
  x.de.en <- x.de.en[, Subcorpus := "Dir vs Indir vs EN_Dir"]
  x.da.en <- x.da.en[, Subcorpus := "Dir vs Indir vs EN_Dir"]
  
  plot.path.de.png <- file.path(Dir_Plots,
                                ifelse(Unit == "partition", "partitions", "documents"),
                                "00_results_DE_all-SLs_barchart.png") # Path to plot
  plot.path.da.png <- file.path(Dir_Plots,
                                ifelse(Unit == "partition", "partitions", "documents"),
                                "00_results_DA_all-SLs_barchart.png") # Path to plot
  plot.path.de.pdf <- file.path(Dir_Plots,
                                ifelse(Unit == "partition", "partitions", "documents"),
                                "00_results_DE_all-SLs_barchart.pdf") # Path to plot
  plot.path.da.pdf <- file.path(Dir_Plots,
                                ifelse(Unit == "partition", "partitions", "documents"),
                                "00_results_DA_all-SLs_barchart.pdf") # Path to plot
  
  plot.path.trans.vs.en.png <- file.path(Dir_Plots,
                                ifelse(Unit == "partition", "partitions", "documents"),
                                "01_results_all-vsEN_barchart.png") # Path to plot
  
  plot.path.trans.vs.en.pdf <- gsub("png", "pdf", plot.path.trans.vs.en.png)
  
  plot.title.de <- "SVM Classification: Direct vs. Indirect Translation (DE), Compared to ST Classification"
  plot.title.da <- "SVM Classification: Direct vs. Indirect Translation (DA), Compared to ST Classification"
  
  plot.title.trans.vs.en <- "SVM Classification Accuracy: Direct vs Indirect vs EN_Direct Translations"
  
  ggplot(x.de, aes(fill= Subcorpus, y = Accuracy, x = SL)) +
    geom_bar(position="dodge", stat="identity") +
    #scale_fill_brewer(palette = "BuPu") + # Good palettes: Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
    ggtitle(plot.title.de) +
    scale_y_continuous(name="Accuracy (%)", limits=c(0, 100),
                       seq(0,100, 10), minor_breaks = NULL) + 
    scale_x_discrete(name = "Source Language") +
    theme_minimal() + 
    theme(axis.text.x = element_text(size=16, face = "bold"),
          axis.text.y = element_text(size=16, face = "bold"),
          legend.text=element_text(size = 16),
          legend.title = element_text(size = 18, face = "bold"),
          axis.title = element_text(size = 16, face = "bold"),
          plot.title = element_text(face = "bold", size = 22)
    )
  ggsave(plot.path.de.png, device = "png",
         width = 350, height = 240, units = "mm", limitsize = FALSE)
  ggsave(plot.path.de.pdf, device = "pdf",
         width = 350, height = 240, units = "mm", limitsize = FALSE)
  
  ggplot(x.da, aes(fill= Subcorpus, y = Accuracy, x = SL)) +
    geom_bar(position="dodge", stat="identity") +
    #scale_fill_brewer(palette = "BuPu") + # Good palettes: Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
    ggtitle(plot.title.da) +
    scale_y_continuous(name="Accuracy (%)", limits=c(0, 100),
                       seq(0,100, 10), minor_breaks = NULL) + 
    scale_x_discrete(name = "Source Language") +
    theme_minimal() + 
    theme(axis.text.x = element_text(size=16, face = "bold"),
          axis.text.y = element_text(size=16, face = "bold"),
          legend.text=element_text(size = 16),
          legend.title = element_text(size = 18, face = "bold"),
          axis.title = element_text(size = 16, face = "bold"),
          plot.title = element_text(face = "bold", size = 22)
    )
  
  ggsave(plot.path.da.png, device = "png",
         width = 350, height = 240, units = "mm", limitsize = FALSE)
  ggsave(plot.path.da.pdf, device = "pdf",
         width = 350, height = 240, units = "mm", limitsize = FALSE)
  
  de.trans.vs.en <- ggplot(x.de.en, aes(fill= Subcorpus, y = Accuracy, x = SL)) +
    geom_bar(position="dodge", stat="identity", width = 0.5) +
    scale_fill_manual(values = c("#5ab4ac")) + 
    scale_y_continuous(name="Accuracy (%)", limits=c(0, 100),
                       seq(0,100, 10), minor_breaks = NULL) + 
    scale_x_discrete(name = "Source Language") +
    theme_minimal() + 
    theme(axis.text.x = element_text(size=14, face = "bold"),
          axis.text.y = element_text(size=14, face = "bold"),
          legend.position = "none",
          axis.title = element_text(size = 14, face = "bold"),
          axis.title.x = element_blank()
    )
  
  
  da.trans.vs.en <- ggplot(x.da.en, aes(fill= Subcorpus, y = Accuracy, x = SL)) +
    geom_bar(position="dodge", stat="identity", width = 0.5) +
    scale_fill_manual(values = c("#5ab4ac")) + 
    scale_y_continuous(name="Accuracy (%)", limits=c(0, 100),
                       seq(0,100, 10), minor_breaks = NULL) + 
    scale_x_discrete(name = "Source Language") +
    theme_minimal() + 
    theme(axis.text.x = element_text(size=14, face = "bold"),
          axis.text.y = element_text(size=14, face = "bold"),
          legend.position = "none",
          axis.title = element_text(size = 14, face = "bold"),
          axis.title.x = element_blank()
    )
  
  trans.vs.en <- ggarrange(de.trans.vs.en, da.trans.vs.en, ncol = 2, nrow = 1,
                           labels = c("DE", "DA"),
                 font.label = list(face = "bold", size = 18)
                 )
  annotate_figure(trans.vs.en,
                  top = text_grob(plot.title.trans.vs.en,
                                  size = 24, face = "bold")
                  )
  
  ggsave(plot.path.trans.vs.en.png, device = "png",
         width = 409, height = 280, units = "mm", limitsize = FALSE)
  ggsave(plot.path.trans.vs.en.pdf, device = "pdf",
         width = 409, height = 280, units = "mm", limitsize = FALSE)
  
}


### STEP 5: SUMMARIZE CORPUS DATA

if (Summarize_Corpus == TRUE) {
  if (! exists("proj.meta")) {
    if (file.exists(Proj_Meta_Path)) {
      proj.meta <- data.table(read.table(Proj_Meta_Path, sep = "\t", header = TRUE))
      proj.meta <- res.all[, -c(1)]
    } else {
      cat("corp.all does not exist, please create it at path ", fn.data.all)
    }
  }
  corp.sum.type <- proj.meta[, .(Texts = .N,
                                 Sentences = sum(sents),
                                 Tokens = sum(tokens)), by = .(subcorpus, sl, tl, type_adv)]
  corp.sum.pair <- proj.meta[, .(Texts = .N,
                                 Sentences = sum(sents),
                                 Tokens = sum(tokens)), by = .(subcorpus, sl, tl)]
  corp.sum.tl <- proj.meta[, .(Texts = .N,
                               Sentences = sum(sents),
                               Tokens = sum(tokens)), by = .(tl)]
  
  corp.sum.type.path <- file.path(Dir_Plots,
                                  ifelse(Unit == "partition", "partitions", "documents"),
                                  "03a_corpusstats_type.csv")
  corp.sum.pair.path <- file.path(Dir_Plots,
                                  ifelse(Unit == "partition", "partitions", "documents"),
                                  "03b_corpusstats_pair.csv")
  corp.sum.tl.path <- file.path(Dir_Plots,
                                  ifelse(Unit == "partition", "partitions", "documents"),
                                  "03c_corpusstats_tl.csv")
  
  write.table(corp.sum.type, file = corp.sum.type.path, col.names = T, row.names = F, sep="\t")
  write.table(corp.sum.pair, file = corp.sum.pair.path, col.names = T, row.names = F, sep="\t")
  write.table(corp.sum.tl, file = corp.sum.tl.path, col.names = T, row.names = F, sep="\t")
}



