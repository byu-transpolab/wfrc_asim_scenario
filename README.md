# WFRC ActivitySim Scenario

This repository serves as a scenario builder for the ActivitySim implementation
in the Wasatch Front / MAG modeling region. This is a research-only 
implementation to support work in travel modeling at BYU, and should not be used
for any policy analysis.

> This research is supported by the Utah Department of Transportation, and 
by US DOT via the T-SCORE University Transportation Center.

## Environment Setup

The scenario builder is an implementation of the `targets` library for R. This is
a make-style system that traces dependencies in a project and only re-builds if 
required.

Users should install the following R packages:

```r
# only install if necessary
install.packages(c("targets", "tidyverse", "sf", "tigris", "tidycensus"))
```

Additionally, users should install Java 11, and python with the `anaconda`
library. Additionally, users must create an `.Renviron` file in the top-level
folder with the path to java 11 as well as the API key to access the US Census
bureau API.

```sh
JAVA_HOME="<path to java>/jdk-11.0.13.jdk/Contents/Home"
CENSUS_API_KEY="<your key here>"
```


## Input files

Some input files are included in this repository, if they come in under 
GitHub's 100 MB limit. Otherwise, `targets` will attempt to download the 
files from public folders on Box. The overall files are at 
[this link](https://byu.box.com/s/jeqa5akd6h3m2q6c4308wnlam9wizjhm). 


### Skims

The skims are derived from the 2019 forecast scenario (calibrated to 2015 data)
run by WFRC / MAG with model version 8.3. The skims from this model run are stored 
in [this box folder](https://byu.box.com/s/o4l2e7zdgvjfkoaux739laf90pox2m02). 

Because cube cannot run on Mac/Linux, we used the Cube Voyager 
script stored in `sh/convert_cube_omx.s` to convert these matrices to OMX and
begin the targets pipeline with those OMX files, stored
in a [sister Box folder](https://byu.box.com/s/nszd42o14ubqohnjubrztex5h1od4tal).
The skims that are small enough to push to GitHub are included, the two skim sets
that are too large to push are downloaded through targets.

  - `skm_auto_Pk.mtx.omx`
  - `skm_auto_Ok.mtx.omx`

