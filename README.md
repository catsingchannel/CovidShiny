# CovidShiny
Managing COVID-19 mutation information.

## **Install**

### Requirements

* R: https://www.r-project.org/ v4.1.0+
* RStudio: https://rstudio.com/products/rstudio/download
* Shiny Server: https://rstudio.com/products/shiny/download-server (only required for deploying **CovidShiny** on web linux server)

### Initialize app

### To run the app locally:

1.Clone this repository

```
git clone https://github.com/MSQ-123/CovidShiny.git
```

2. Download Mutation database: https://drive.google.com/uc?id=1L4377W2qw5g6TTt8_yKgjt8Fhgr5K5SI&export=download

> This database is a subset of online website which contained GISAID samples submitted from 2022-07-25 to 2023-07-25
> The database is large so it may need some time to finish it.

Please place the .sqlite file to (your app directory path)/data directory after downloaded

3.Open `CovidShiny.Rproj`

4.Install packages. In the RStudio console, run:

```
# First install Bioconductor
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install(version = "3.17")

BiocManager::install("Biostrings")
BiocManager::install("rprimer")

#other packages are available through CRAN:
install.packages("seqinr")
install.packages("shiny")
install.packages("shinydisconnect")
install.packages("ggplot2")
install.packages("ggsci")
install.packages("stringr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("forcats")
install.packages("patchwork")
install.packages("ggpubr")
install.packages("writexl")
install.packages("gridExtra")
install.packages("grid")
install.packages("shinycssloaders")
install.packages("shinydashboard")
install.packages("shinyWidgets")
install.packages("DT")
install.packages("shinythemes")
install.packages("plotly")
install.packages("dashboardthemes")
install.packages("VennDiagram")
install.packages("shinyWidgets")
```

> This may take some time to complete.

5.Start tha app by running

```
shiny::runApp(launch.browser = TRUE)
```

### Deploy CovidShiny on web Linux server

1.Clone/Upload this repository into /srv/shiny-server


```
$ cd /srv/shiny-server
git clone https://github.com/MSQ-123/CovidShiny.git
# Or clone it locally and upload the directory to /srv/shiny-server using scp or other tools
```


2.Configure Shiny Server (/etc/shiny-server/shiny-server.conf)


```
# Instruct Shiny Server to run applications as the user "shiny"
preserve_logs true;
sanitize_errors false;
run_as shiny;

# Define a server that listens on port 3838
server {
  listen 3838;

  # Define a location at the base URL
  location / {

    # Host the directory of Shiny Apps stored in this directory
    site_dir /srv/shiny-server;

    # Log all Shiny output to files in this directory
    log_dir /var/log/shiny-server;

    # When a user visits the base URL rather than a particular application,
    # an index of the applications available in this directory will be shown.
    directory_index on;
  }
}
```

3.Change the owner of the **CovidShiny** directory

```
$ chown -R shiny /srv/shiny-server/CovidShiny  
```

4.Start Shiny-Server

```
$ start shiny-server
```

Now you can access the **CovidShiny** app at http://IPAddressOfYourServer:3838/CovidShiny.

### Process and upload GISAID samples to your local database

#### Requirements:

* This pipeline only available in UNIX environment
* seqkit: https://bioinf.shenwei.me/seqkit/download/
* NUCmer(NUCleotide MUMmer): https://mummer4.github.io/

If you need to compiie the snp-process program, make sure you have the cmake which version equal or higher than 3.26.3

1.Complie the snp-process program

The source code is located on (app directory)/process/snp-process_source

Change your directory into this location and run:

```
$ mkdir build

$ cd ./build

$ cmake ..

$ make
```
Then copy the generated snp-process file to process directory

2.Process and upload GISAID data.

For mutation profile generation and upload, Download the sequence data in input for Augur pipeline format and put downloaded tar files into (app directory)/process directory, Then run this shell command:

```
$ ./process.sh
```

It may take a long times to upload data if your have a large database or upload large amount of samples.
