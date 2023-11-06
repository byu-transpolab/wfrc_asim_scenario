#Install R packages that have weird issues
#install.packages('rgdal', type = "source", configure.args=c('--with-proj-include=/panfs/roc/msisoft/proj/4.9.3/include','--with-proj-lib=/panfs/roc/msisoft/proj/4.9.3/lib'))
#install.packages('rgdal', type = "source", configure.args="--host=host")
#install.packages('sf')

package_list <- list(
#	"targets",
#	"tarchetypes",
	"stringr",
	"broom",
	"tidyr",
#	"tidyverse",
	"sf",
	"tigris",
	"tidycensus"
	)

for(package in package_list) {
	require(package, character.only = TRUE) || install.packages(pkgs = package, quiet = TRUE)
}
