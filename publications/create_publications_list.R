
# Publication list
# Script purpose: 
#   * Create list of publications according to the classification:
#     https://www.aka.fi/en/funding/apply-for-funding/az-index-of-application-guidelines/list-of-publications/
#   * You may enter dois and/or plain text citations.
#   * You may  get Dois from ORCID

# Load packages ----------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, dplyr, glue, stringr, zoo, rorcid, tools, RefManageR, rcrossref, usethis, data.table, magrittr)

library(rcrossref)

# Functions: -------------------------------------------------------------------

higlight_name <- function (df, highlight = c("Suokas, K.", "Suokas, K.I.", "Suokas K")){
  #
  # highlight names with ** bold in Rmarkdown
  for (i in highlight){
    df$reference <- str_replace(df$reference, i, paste0('**', i, '**'))
  }
  df
}

# if needed, this adds space before link
fix_links <- function(df){
  #df$item <- str_replace(df$item, '.http', '. http', fixed)
  df$item <- gsub(".http", ". http", df$item, fixed=TRUE)
  df
}

# get publication data ---------------------------------------------------------

#Here dois are from orcid, using rorcid package.
# Authentication, see: https://ciakovx.github.io/rorcid.html
dois_from_orcid <- rorcid::identifiers(rorcid::works("0000-0001-6296-6343"))

# remoe duplicates
dois_from_orcid <- dois_from_orcid %>% unique()


# using rcrossref, get data in data.frame form
pubs_from_dois_table <- cr_works(dois = dois_from_orcid) %>% purrr::pluck("data") #, format = "text", style="apa")

# get reference as text, select style, see rcrossref::get_styles()
pubs_from_dois <- cr_cn(dois = dois_from_orcid, format = "text", style="american-medical-association-no-et-al") %>% 
  unlist() %>% 
  as.data.frame() 

colnames(pubs_from_dois) <- c("reference")

# if publiseh in a journal, assume its peer reviewd, if not in a journal, assume not peer reviewed.
pubs_from_dois$aka_category <- ifelse(is.na(pubs_from_dois_table$container.title), 'b', 'a')
pubs_from_dois$peer_reviewed <- TRUE

# year_month of the publication, from data frame, added to the references
pubs_from_dois$year_month <- format(as.Date(pubs_from_dois_table$created), "%Y-%m")

# url to publication, from data frame, added to the references
pubs_from_dois$url <- pubs_from_dois_table$url

# doi to publication, from data frame, added to the references
pubs_from_dois$doi <- pubs_from_dois_table$doi


# read manually added reference ------------------------------------------------
pubs_from_file <- read.csv(here('publications', 'input_manually_ama_no_et_al.txt'), stringsAsFactors = FALSE, encoding = 'UTF-8')

# combine data from dois and from file, high_light name, arrange ---------------
pubs <- bind_rows(pubs_from_dois, pubs_from_file) %>% 
  higlight_name() %>%
  arrange(desc(year_month)) 

setDT(pubs)
pubs[, reference := ifelse(str_starts(reference, "1. "), str_replace(reference, "1. ", ""), reference)]


# MANUAL Change of type --------------------------------------------------------

pubs[reference %like% 'doi:10.1002/wps.21027',`:=`(aka_category = 'b', peer_reviewed = FALSE)]

# ref + url --------------------------------------------------------------------
pubs[, ref_link := glue::glue("{pubs$reference} [link]({pubs$url})")]

# Exclude preprint if published ------------------------------------------------

excl <- pubs_from_dois_table %>% as.data.table() %>%  .[, .SD[.N>1 & type != "journal-article"], title] %>% .[,doi]

# manual
excl_manual <- c("10.1101/2023.12.07.23299655")
 
pubs <- pubs[!doi %in% c(excl, excl_manual)]

#

# save -------------------------------------------------------------------------
fwrite(pubs, here('publications', 'publications.csv'))









# ### manual not uptodate 25.2.22
# 
# # read manually added reference ------------------------------------------------
# pubs_all_from_file <- read.csv(here('publications', 'input_all_ama_no_et_al.txt'), stringsAsFactors = FALSE, encoding = 'UTF-8')
# 
# # combine data from dois and from file, high_light name, arrange ---------------
# pubs <- pubs_all_from_file %>% #bind_rows(pubs_from_dois, pubs_from_file_apa) %>% 
#   higlight_name() %>%
#   arrange(desc(year_month)) 
# 
# setDT(pubs)
# pubs[, reference := ifelse(str_starts(reference, "1. "), str_replace(reference, "1. ", ""), reference)]
# 
# 
# # save -------------------------------------------------------------------------
# #fwrite(pubs, here('publications', 'publications_list_manual.csv'))
# 
# 
# # / done
