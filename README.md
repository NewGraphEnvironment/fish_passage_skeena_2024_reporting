Custom template for New Graph Environment Ltd. fish passage reporting.

This template was produced with https://github.com/NewGraphEnvironment/mybookdown-template so all issues in that
repo are likely relevant here. See issues at https://github.com/NewGraphEnvironment/mybookdown-template/issues

If we update files common to `mybookdown-template` we should do it there and update specific files here with:   

    git remote add upstream https://github.com/NewGraphEnvironment/mybookdown-template.git
    git config remote.upstream.pushurl "maybe dont push to the template from here bud"
    git fetch upstream
    git checkout upstream/master -- path/to/file

See https://stackoverflow.com/questions/24815952/git-pull-from-another-repository   
    
see `scripts/run.R`

Track version changes in [`NEWS.md`]('NEWS.md`)


In order to avoid commit huge files run this in the `terminal` every once and a while https://stackoverflow.com/questions/4035779/gitignore-by-file-size
https://stackoverflow.com/questions/37768376/remove-duplicate-lines-and-overwrite-file-in-same-command

    find . -size +50M | sed 's|^\./||g' >> .gitignore; awk '!seen[$0]++' .gitignore | sponge .gitignore
    
    
This is a common move to deal with repeated headers in pagedown knitr table outputs when the page breaks.  If we don't have an extra `<br>`

`r if(gitbook_on){knitr::asis_output("<br>")} else knitr::asis_output("\\pagebreak<br>")`
    

   
