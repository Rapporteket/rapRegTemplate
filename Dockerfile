# syntax=docker/dockerfile:1
FROM rapporteket/base-r-alpine:main

LABEL maintainer="Arnfinn Hykkerud Steindal <arnfinn.hykkerud.steindal@helse-nord.no>"

ARG GH_PAT
ENV GITHUB_PAT=${GH_PAT}

WORKDIR /app/R

COPY *.tar.gz .

RUN installr -d \
    -t curl-dev \
    remotes \
    curl \
    kableExtra \
    rpivotTable 
RUN R -e "remotes::install_github(\"Rapporteket/rapbase\")" \
    && R -e "remotes::install_local(list.files(pattern = \"*.tar.gz\"))" \
    && rm ./*.tar.gz

EXPOSE 3838

CMD ["R", "-e", "options(shiny.port = 3838,shiny.host = \"0.0.0.0\"); rapRegTemplate::run_app()"]
