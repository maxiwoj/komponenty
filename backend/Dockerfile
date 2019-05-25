FROM r-base

RUN R -e "install.packages('distr',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('data.table',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('dplyr',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('deSolve',dependencies=TRUE, repos='http://cran.rstudio.com/')"

COPY ./R_dialog_testowy.R /R_dialog_testowy.R

WORKDIR /

ARG oral_dose=880
ARG inf_dose=0
ARG inf_time=2
ARG t_end=15
ARG seed=1111

CMD echo $oral_dose

CMD Rscript /R_dialog_testowy.R $oral_dose, $inf_dose, $inf_time, $t_end, $seed
