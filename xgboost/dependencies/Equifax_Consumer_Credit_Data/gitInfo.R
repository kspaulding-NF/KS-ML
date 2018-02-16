

#' Git Commit Information
#'
#' @return String of git info. Executes the command git show --format="%h %ci"
#'
#' @examples
#' print(gitInfo()) # "[master] [2a759a1] [2016-05-23 08:02:49 -0700]"
#' 
gitInfo = function() {
  # Get current branch
  branches = system('git branch', intern=TRUE)
  is_active = sapply(branches, function(branch) length(grep('^\\s*\\*', branch)) > 0)
  if (any(is_active)) {
    active_branch = branches[which(is_active)[1]]
    active_branch = gsub('^\\s*\\*\\s*', '', active_branch) # strip leading " * "
  } else {
    active_branch = ""
  }
  
  commit = system('git show --format="%h %ci"', intern = TRUE)
  commit = unlist(strsplit(commit[1], ' '))
  names(commit) = c("hash", "date", "time", "tz")
  
  git_info = paste0('[', active_branch, '] ', 
                    '[', commit['hash'], '] ',
                    '[', commit['date'], ' ', commit['time'], ' ', commit['tz'], ']')
  
}