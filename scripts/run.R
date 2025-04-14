preview_chapter('0100-intro.Rmd')


#################################################################################################
##go to the index.Rmd and change gitbook_on <- TRUE
#################################################################################################

# move over the photos from shared drive to repo and update metadata so that the photos are included in the report interactive map
rmarkdown::render('scripts/02_reporting/photos_import.Rmd')
usethis::use_git_ignore('scripts/02_reporting/photos_import.html')
source('scripts/02_reporting/0180-photos-extract-metadata.R')



# add/update the NEWS.md to the book as an appendix and build the gitbook
{
  # update util file functions from staticimports
  staticimports::import()
  source('scripts/staticimports.R')
  my_news_to_appendix()

  files_to_move <- list.files(pattern = ".Rmd$") |>
    stringr::str_subset('2300', negate = F) #move the attachments out
  files_destination <- paste0('hold/', files_to_move)

  ##move the files
  mapply(file.rename, from = files_to_move, to = files_destination)

  rmarkdown::render_site(output_format = 'bookdown::gitbook',
                         encoding = 'UTF-8')

  ##move the files from the hold file back to the main file
  mapply(file.rename, from = files_destination, to = files_to_move)

}


# not run but available to remove files we don't need in the gitbook build (sometimes appendices are not built in gitbook)
# {
#
#   source('scripts/functions.R')
#   news_to_appendix()
#
#   # These files are included in the gitbook version already so we move them out of the build
#   files_to_move <- list.files(pattern = ".Rmd$") %>%
#     stringr::str_subset(., '2200|2300|2400', negate = F) #move the attachments out
#   files_destination <- paste0('hold/', files_to_move)
#
#   ##move the files
#   mapply(file.rename, from = files_to_move, to = files_destination)
#
#   rmarkdown::render_site(output_format = 'bookdown::gitbook',
#                          encoding = 'UTF-8')
#
#   ##move the files from the hold file back to the main file
#   mapply(file.rename, from = files_destination, to = files_to_move)
# }




#################################################################################################
##go to the index.Rmd and change gitbook_on <- FALSE
#################################################################################################
##move the phase 1 appendix out of the main directory to a backup file or else the file is too big

staticimports::import()
source('scripts/staticimports.R')

# define the _bookfile_name from _bookdown.yml - need to run a chunk of  the index
filename_html <- basename(params$repo_url)

{

  ## move large appendices to hold for pdf build
  file.rename('0600-appendix.Rmd', 'hold/0600-appendix.Rmd')

  ##   then make our printable pdf
  rmarkdown::render_site(output_format = 'pagedown::html_paged',
                         encoding = 'UTF-8')

  ## move large appendices back to main directory
  file.rename('hold/0600-appendix.Rmd', '0600-appendix.Rmd')

  # print to pdf
  pagedown::chrome_print(
    paste0(filename_html, '.html'),
    output = paste0('docs/', filename_html, '.pdf'),
    timeout = 360
  )

  # reduce the size
  tools::compactPDF(paste0("docs/", filename_html, ".pdf"),
                    gs_quality = 'ebook',
                    ##this was on the windows machine
                    # gs_cmd = "C:/Program Files/gs/gs9.56.1/bin/gswin64.exe"
                    gs_cmd = "/opt/homebrew/bin/gs"
  )

  # get rid of the html as its too big and not needed
  file.remove(paste0(filename_html, '.html'))

}



######################################## make Phase 1 appendix seperately only when updated
#################################################################################################
##we need a workflow to print the Phase 1 attachment

{
files_to_move <- list.files(pattern = ".Rmd$") |>
  stringr::str_subset('index|fish_passage_skeena_2024_reporting|0600', negate = T)

files_destination <- paste0('hold/', files_to_move)

##move the files
mapply(file.rename, from = files_to_move, to = files_destination)

##   then make our printable pdf
rmarkdown::render_site(output_format = 'pagedown::html_paged',
                       encoding = 'UTF-8')

##move the files from the hold file back to the main file
mapply(file.rename, from = files_destination, to = files_to_move)

pagedown::chrome_print(
  paste0(filename_html,'.html'),
  output = "docs/Appendix_1_prep.pdf",
  timeout = 120
)


##now get rid of the first pages
length <- pdftools::pdf_length("docs/Appendix_1_prep.pdf")

# this changes so let's define
crop_this_many_pages <- 8

# trim up the file.  We ditch the last page only when there are references.  In the case of the bulkley there are due to the yaml file
pdftools::pdf_subset("docs/Appendix_1_prep.pdf",
                     pages = (crop_this_many_pages + 1):(length - 1), output = "docs/Appendix_1.pdf")

##clean out the old files
file.remove("docs/Appendix_1_prep.pdf")
file.remove(paste0(filename_html,'.html'))

##it is very important to shrink the size of the pdf so we don't blow up our github repo.
# We will set up ghostscript on the mac but for now we do manually.

# reduce the size
tools::compactPDF("docs/Appendix_1.pdf",
                  gs_quality = 'ebook',
                  ##this was on the windows machine
                  # gs_cmd = "C:/Program Files/gs/gs9.56.1/bin/gswin64.exe"
                  gs_cmd = "/opt/homebrew/bin/gs"
)
}
