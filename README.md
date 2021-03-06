# 10k Immunomes Documentation

This is an overview of the 10k Immunomes project. This documentation is for researchers who intend to develop the project further. 10k Immunomes is a web portal where immunologists can explore immunologic reference datasets from healthy control subjects. This documentation provides an introduction to the project and then gives instructions on how to code your own version of 10k Immunomes.


## 10k Immunomes Resources

* [Website](https://10kimmunomes.ucsf.edu/): This is the 10k Immunomes website. This documentation explains how this website is built so that researchers and programmers can build their own version of 10k Immunomes.
* [Video Tutorial](https://youtu.be/pwBs4J4xDOw): This video tutorial is a beginners overview of what 10k Immunomes is about and how to use it. Watching the video is the best way to begin learning about 10k Immunomes. It gives a step-by-step example of how 10k Immunomes is used for research.
* [Published Paper](https://www.cell.com/cell-reports/pdf/S2211-1247(18)31451-7.pdf): This is a published paper describing 10k Immunomes. Reading the paper is a good way to learn more about the purpose of 10k Immunomes and the [ImmPort database](https://www.immport.org/about), where 10k's data comes from.
* [Source Code](https://github.com/buttelab/10kimmunomes_v2): This github repo contains all of the source code. It's useful for scanning through the files, however, we **highly** reccomend using the dockerhub image when working on code. The README file provides a thorough description of all source files.
* [Docker](https://hub.docker.com/r/pupster90/10kimmunomes/tags): This docker image is by far the easiest way to start working on the 10k Immunomes project. It contains a detailed step by step tutorial on how to get the website up and running, how to edit code, and how to publish final results to a shiny proxy server.
* [Data Files](https://drive.google.com/file/d/1SeOhqCkdQTzzZjOR0MVSo5VRbzjGK-jI/view?usp=sharing): This zip file contains the 10k code, as well as code and data used in preprocessing. Use this to learn how the raw data from Immport was converted into 10k Immunomes.

## Getting Started

The best way to get started is by learning to use 10k Immunomes as if you were a user. This way you undertand the purpose of the project. Briefly visit the website, then watch the tutorial video. If you intend to add additional datasets to 10k Immunomes, you must become familiar with [ImmPort](https://www.immport.org/about). Create an ImmPort account then try searching for a study and downloading all the raw datasets from it. Once these steps are done you can then move on to gain a deeper understanding of the code.

## Editing Code

10k Immunomes source code is available on Github and Dockerhub. The README files on Github are a great place to learn about what the different files do in 10k Immunomes and where the best place is to start coding. However, please do not attempt to replicate 10k Immunomes by first cloning the Github repo. 10k Immunomes is an [R Shiny](https://shiny.rstudio.com/tutorial/) website with very specific settings and specific packages installed. If you don't know what R shiny is, go through a few of the practice tutorials on their official site. After that, go to the 10k Immunomes [dockerhub](https://www.docker.com/products/docker-hub#:~:text=Docker%20Hub%20is%20a%20hosted,push%20them%20to%20Docker%20Hub) and follow the steps there to launch an [R Shiny Server](https://shiny.rstudio.com/articles/shiny-server.html) where you can run 10k Immunomes locally. [Docker](https://docs.docker.com/get-started/) is a tool that allows you to launch little pre-set-up "mini computers" from your computer. With docker you can launch 10k Immunomes in minutes. This will save you weeks are work. 

If you are interested in how the raw datasets from ImmPort were processed for 10k Immunomes you must view the code in the Data Files section. The steps for obtaining the raw data and processing it are different, depending on the dataset. Make sure to view the README's to see if there is a docker or github repo for the dataset you are interested in.

## Source Code Summary

We provide a summary of all the source code. This repository consists of an `10k Immunomes Source Code .pynb` file, an `app.R` file, and five folders consitsting of javascript, csv, RData, .zip data, png/jpeg images, and ipynb notebook files. 

* [10k Immunomes Source Code .ipynb](https://github.com/buttelab/10kimmunomes_v2/blob/master/10k%20Immunomes%20Source%20Code%20.ipynb) This one file contains all of the code used to build the 10k Immunomes website. It is by far the most important file in the repository and where you should start reading. `10k Immunomes Source Code` launches an R shiny server. If you are unfamiliar with R shiny, you should do [tutorials](https://shiny.rstudio.com/tutorial/) on the R shiny website before continuing. The `.ipynb` file is very descriptive and provides summary of the code. To run this file you must use [Jupyter](https://jupyter.org/about). Please follow the instructions on the [docker image](https://hub.docker.com/r/pupster90/10kimmunomes/tags) to set up the coding environment.

* [app.R](https://github.com/buttelab/10kimmunomes_v2/blob/master/app.R) This file is used by the Butte lab to launch the official release of 10k Immunomes on an [R Shiny Proxy Sever](https://www.shinyproxy.io/). If you are intersted in the setup of the official release contact [Boris Oskotsky](https://directory.ucsf.edu/people/search/id/109536).

* [analytics](https://github.com/buttelab/10kimmunomes_v2/tree/master/analytics) This folder consists of two javascript files. These files bring [google analytics](https://analytics.google.com/analytics/web/provision/#/provision) functionality to 10k Immunomes. This is used to track the number of users visiting the site as well as to see what buttons they are clicking. The `analytics.ucsf.js` provides the feedback to google analytics that is tracked by the Butte lab team. The other file is the same code, but, at least previously, it sent data to northrop grommon for tracing.

* [data](https://github.com/buttelab/10kimmunomes_v2/tree/master/data) This folder contains the data that is used to create the interactive graphs on the website. The `newtenkdata` files were the original datasets created by Kelly and Zicheng. All the other datasets are used to graph the RNA-Seq data and newborn metabolomics data added by Elliott. In the future these additional files could also be consolidated into simply `.RData` files.

* [data_raw](https://github.com/buttelab/10kimmunomes_v2/tree/master/data_raw)This folder contains the data that is downloaded by the user when they click the "all data" button on the website. To understand how these datasets were processed look through the [Data Files](https://drive.google.com/file/d/1SeOhqCkdQTzzZjOR0MVSo5VRbzjGK-jI/view?usp=sharing). 

* [other_code](https://github.com/buttelab/10kimmunomes_v2/tree/master/other_code) This folder contains other code that was used at the beginning of Elliott's work on the 10k Immunomes project. The `.ipynb` files are very descriptive and provide summaries of their code. This code is deprecated and no longer needed. It is better to read the code in the [Data Files](https://drive.google.com/file/d/1SeOhqCkdQTzzZjOR0MVSo5VRbzjGK-jI/view?usp=sharing). `Metabolomics Analysis.ipynb` is probably the most informative file in the folder.

* [www](https://github.com/buttelab/10kimmunomes_v2/tree/master/www) This folder contains all of the images used by 10k Immunomes. These descriptive images are displayed on the homes screen of [10k Immunomes](https://10kimmunomes.ucsf.edu/) when a user visits the site.

## Help and Contact

If you are another lab interested in 10k Immunomes, we are happy to have you reach out to [Atul Butte's lab](https://buttelab.ucsf.edu/). Sanchita Bhattacharya is the senior scientist and manager of this project (email: Sanchita.Bhattacharya@ucsf.edu). If you are a programmer with technical questions please reach out to the senior programmer of 10k Immunomes, Matthew Elliott (email: melliot1@ucsc.edu). We look forward to hearing from you!



