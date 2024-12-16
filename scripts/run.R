preview_chapter('0100-intro.Rmd')


#################################################################################################
##go to the index.Rmd and change gitbook_on <- TRUE
#################################################################################################

# add/update the NEWS.md to the book as an appendix and build the gitbook
{
  # update util file functions from staticeimports
  staticimports::import()
  source('scripts/staticimports.R')
  my_news_to_appendix()

  rmarkdown::render_site(output_format = 'bookdown::gitbook',
                         encoding = 'UTF-8')

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


# define the _bookfile_name from _bookdown.yml
filename_html <- 'fish_passage_template'

{

  ## move large appendices to hold for pdf build
  ## not required for template
  # file.rename('0600-appendix.Rmd', 'hold/0600-appendix.Rmd')

  ##   then make our printable pdf
  rmarkdown::render_site(output_format = 'pagedown::html_paged',
                         encoding = 'UTF-8')

  ## move large appendices back to main directory
  # file.rename('hold/0600-appendix.Rmd', '0600-appendix.Rmd')

  # print to pdf
  pagedown::chrome_print(
    paste0(filename_html, '.html'),
    output = paste0('docs/', filename_html, '.pdf'),
    timeout = 180
  )

  # reduce the size
  tools::compactPDF(paste0("docs/", filename_html, ".pdf"),
                    gs_quality = 'screen',
                    ##this was on the windows machine
                    # gs_cmd = "C:/Program Files/gs/gs9.56.1/bin/gswin64.exe"
                    gs_cmd = "opt/homebrew/bin/gs"
  )

  # get rid of the html as its too big and not needed
  file.remove(paste0(filename_html, '.html'))

}
