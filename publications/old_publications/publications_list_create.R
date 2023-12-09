
# Script: modifyRefs.R
# Project: Academy of Finland style CV
# Script purpose: 
#   * Create list of publications according to the classification:
#     https://www.aka.fi/en/funding/apply-for-funding/az-index-of-application-guidelines/list-of-publications/
#   * You may enter dois and/or bibtex entries of your publications.
#   * You may also get Dois from your ORCID
# Author: Kimmo Suokas
# Date: 4.8.2020
#

# Load packages ----------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, dplyr, glue, stringr, zoo, rorcid, tools, RefManageR, rcrossref, usethis)


# Get latex

## This needs to be run manually if LaTex is not installed locally.
## tinytex::install_tinytex()


# Functions: -------------------------------------------------------------------

citation <- function(bib, 
                     csl="apa.csl", 
                     toformat="plain", 
                     cslrepo="https://raw.githubusercontent.com/citation-style-language/styles/master") {
  # pass in your reference, and convert it using a standard "CSL" file to desired citation style
  #
  # Args:
  # bib: a bib file or "raw" citation in a text element
  # csl: csl file to control the citation style. The file must be located in the root of the project
  # (could not make it work from publications folder, don't know why)
  # cslrepo: if the desired csl is not found, it will be downloaded from here
  # 
  # Returns:
  # formatted citation in text
  #
  # The source of the function: 
  # https://stackoverflow.com/questions/63141295/creating-formatted-references-with-different-citation-styles-to-academic-papers
  #
  # example:
  #test <- "@article {PMID:14907713,    Title = {Protein measurement with the Folin phenol reagent},    Author = {LOWRY, OH and ROSEBROUGH, NJ and FARR, AL and RANDALL, RJ},   Number = {1},   Volume = {193},     Month = {November},     Year = {1951},  Journal = {The Journal of biological chemistry},    ISSN = {0021-9258},     Pages = {265-275},  URL = {http://www.jbc.org/content/193/1/265.long} } "
  #citation(test, csl="apa-cv.csl")
  #
    if (!file.exists(bib)) {
    message("Assuming input is literal bibtex entry")
    tmpbib <- tempfile(fileext = ".bib")
    on.exit(unlink(tmpbib), add=TRUE)
    if(!validUTF8(bib)) {
      bib <- iconv(bib, to="UTF-8")
    }
    writeLines(bib, tmpbib)
    bib <- tmpbib
  }
  if (tools::file_ext(csl)!="csl") {
    warning("CSL file name should end in '.csl'")
  }
  if (!file.exists(csl)) {
    cslurl <- file.path(cslrepo, csl)
    message(paste("Downling CSL from", cslurl))
    cslresp <- httr::GET(cslurl, httr::write_disk(csl))
    if(httr::http_error(cslresp)) {
      stop(paste("Could not download CSL.", "Code:", httr::status_code(cslresp)))
    }
  }
  tmpcit <- tempfile(fileext = ".md")
  on.exit(unlink(tmpcit), add=TRUE)
  
  writeLines(c("---","nocite: '@*'","---"), tmpcit)
  rmarkdown::find_pandoc()
  command <- paste(shQuote(rmarkdown:::pandoc()), 
                   "--filter", "pandoc-citeproc",
                   "--to", shQuote(toformat),
                   "--csl", shQuote(csl),
                   "--bibliography", shQuote(bib), 
                   shQuote(tmpcit))
  rmarkdown:::with_pandoc_safe_environment({
    result <- system(command, intern = TRUE)
    Encoding(result) <- "UTF-8"
  })
  result
}


create_publications_from_file <- function (pubsFile){
  #
  # Create formatted publications list from bibtex file
  #
  # Details: Uses Function "citation" (above) to formate the input bibtex file based on the style in specific .csl file.
  # finds year and month from the entry and create column of them.
  #  
  # What it does: Lapply function citation to every element in the file of bib entries.
  # Collapse to get each result on one line.
  #
  # Args:
  # pubsFile: bibtex file
  #
  # Output: data.frame with colums: 
  # * item: the formatted reference
  # * yearmon: year and month of the entry
  #
  pubsFromFile <- as.data.frame(unlist(
    lapply(readLines(pubsFile), 
           function(x) paste0(citation(x), collapse = '')
              #define citation style here
    ))) %>% 
    `colnames<-`('item') %>% 
    mutate(yearmon = paste(
      RefManageR::ReadBib(pubsFile, check = FALSE)$year, 
      RefManageR::ReadBib(pubsFile, check = FALSE)$month, 
      sep = '-')
    )
  pubsFromFile
  
}

create_publications_from_dois <- function (pubsDoi){
  #
  # Create formatted publications list from list of Dois
  #
  # Details: Uses Function "citation" (above) to formate the input Dois based on the style in specific .csl file.
  # finds year and month from the entry and creates a column of that.
  #  
  # What it does:
  # * Get bibtex data based on the specified Dois from crossref using rcrossref package
  # * Lapply function "citation" to every latex bibliography element.
  # * Collapse to get each result on one line.
  #
  # Args:
  # pubsDoi: vector of Dois
  #
  # Output: data.frame with columns: 
  # * item: the formatted reference
  # * yearmon: year and month of the entry
  # * writes also a bibtex file of the data
  #
  bibOfDois <- rcrossref::cr_cn(dois = pubsDoi, format = "bibtex", .progress="text")
  # 
  #lapply(bibOfDois, write, here('publications', "PeerReviewedFromDois.bib"), append=TRUE)
  
  pubsFromDois <- as.data.frame(unlist(
  lapply(bibOfDois, 
         function(x) paste0(citation(x), collapse = '')
  )))%>% 
    `colnames<-`('item') %>% 
    mutate(yearmon = paste(
      substr(sub('.*\\year = ', '', bibOfDois),  1, 4),
      substr(sub('.*\\month = ', '', bibOfDois),  2, 4), 
      sep = '-')
    )
  pubsFromDois
}


higlight_name <- function (df, highlight = c("Suokas, K.", "Suokas, K.I.")){
  #
  # highlight names with ** bold in Rmarkdown
  for (i in highlight){
    df$item <- str_replace(df$item, i, paste0('**', i, '**'))
  }
  df
}

fix_links <- function(df){
  #df$item <- str_replace(df$item, '.http', '. http', fixed)
  df$item <- gsub(".http", ". http", df$item, fixed=TRUE)
  df
}


# A Peer-reviewed scientific articles -----------------------------------------
###
# Get pubs from file, where you can manually add references to Peer-reviewed articles
###
peer_reviewd_from_file <- create_publications_from_file(here('publications', 'input_manually_peer_reviewed_articles.bib'))

###
#
# Get pubs based on Dois. 
#
# You can pass a vector of dois yourself or get them from ORCID, Google etc
#
###

#Here dois are from orcid, using rorcid package.
# Authentication, see: https://ciakovx.github.io/rorcid.html
dois_peer_Reviewed <- rorcid::identifiers(rorcid::works("0000-0001-6296-6343"))

# create pubs based on Dois
peer_reviewd_from_dois <- create_publications_from_dois(dois_peer_Reviewed)

###
#
# Combine entries from File and based on Dois, arrange by publication year and month:
###
pubs_peer_reviewed <- bind_rows(peer_reviewd_from_file, peer_reviewd_from_dois) %>% 
  arrange(desc(yearmon)) %>% 
  higlight_name() %>% 
  fix_links()
#pubs_peer_reviewed

# B Non-refereed scientific articles --------

# C Books -----------

# D Publications intended for professional communities: Get pubs from file-------------

pubs_for_profs_from_file <- create_publications_from_file(
  here('publications', 'input_manually_for_professional_communities.bib')) %>% 
  arrange(desc(yearmon))

pubs_for_profs <- pubs_for_profs_from_file %>% 
  higlight_name() %>% 
  fix_links()
#pubs_for_profs



#pubs_for_profs

# E Publications intended for the general public, linked to the applicant’s research ------------

# F Public artistic and design activities -------

# G Theses -------

# H Patents and invention disclosures -----------

# I Audiovisual material, ICT software ------------


# 
pubs_peer_reviewed$class <- 'a'
pubs_for_profs$class <- 'd'
pubs <- bind_rows(pubs_peer_reviewed, pubs_for_profs) 

write.csv(pubs, here('publications', 'publications_list.csv'))

