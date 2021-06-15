FROM rocker/r-ver

# this is necessary for R-package enrichplot (zlib.h and others)
RUN apt-get update  \
    && apt-get -y install libxml2 libglpk-dev zlib1g-dev \
    && rm -rf /tmp/* \
    && apt-get autoremove -y \
    && apt-get autoclean -y \
    && rm -rf /var/lib/apt/lists/*

RUN R -e 'install.packages(c("shiny","shinyalert","shinydashboard","ggplot2", "VennDiagram", "shinycssloaders", "data.table", "dplyr", "msigdbr", "BiocManager"), repos="https://cran.rstudio.com/",dependencies=TRUE, clean=TRUE)' && \
 R -e 'BiocManager::install(c("enrichplot","clusterProfiler"))'

RUN R -e 'BiocManager::install("org.Hs.eg.db")'
RUN R -e 'BiocManager::install("org.Mm.eg.db")'
RUN R -e 'BiocManager::install("org.Dm.eg.db")'  

RUN mkdir /root/clusterProfiler_GOenrich
    
# Fix the time zone
RUN unlink /etc/localtime && \
    ln -s /usr/share/zoneinfo/Europe/Berlin /etc/localtime

EXPOSE 2525


COPY Rprofile.site /usr/lib/R/etc/
COPY ./startup.sh /usr/local/bin/startup.sh

COPY app.R /root/clusterProfiler_GOenrich

COPY shared_files /root/clusterProfiler_GOenrich/shared_files
COPY ShinyApp_documentation /root/clusterProfiler_GOenrich/ShinyApp_documentation

CMD ["/usr/local/bin/startup.sh"]



