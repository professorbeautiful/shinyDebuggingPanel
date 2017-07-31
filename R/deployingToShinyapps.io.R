.installFromGithub =
function(user='professorbeautiful', project=basename(getwd()), ...)
  devtools::install_github(paste0(user, "/", project), ...)


.deploy =
function(app=dir('inst')[1], user='professorbeautiful', project=basename(getwd()),
         reInstall=TRUE, ...){
  ## TODO: first check that the html files are created committed and pushed.
  packageWD = getwd()
  if(reInstall)
    .installFromGithub(user = user, project = project)
  apps = app
  for (app in apps) {
    if(substr(app, 1, 5) == "inst/")
      warning(".deploy: do not include 'inst' in app name.")
    cat("wd is " , getwd(), "\n")
    setwd(paste0("inst/", app))
    cat("wd changing to ", getwd(), "\n")
    tryCatch({
      require("shinyapps")
      deployApp(...)
    },
    finally={
      cat(paste0("shinyapps::showLogs(appPath = 'inst/", app,"')"), '\n')
      setwd(packageWD)
    }
    )
  }
}


.runDeployed =
function(app="shinyElicit"){
  system("open https://trials.shinyapps.io/" %&% app)
  cat(paste0("shinyapps::showLogs(appPath = 'inst/", app,"')"), '\n')
}
