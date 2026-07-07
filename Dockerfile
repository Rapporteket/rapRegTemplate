FROM rapporteket/base-r-alpine-latex:main

LABEL maintainer="Arnfinn Hykkerud Steindal <arnfinn.hykkerud.steindal@helse-nord.no>"

WORKDIR /app/R

COPY . pkg

RUN --mount=type=secret,id=github_pat,env=GITHUB_PAT \
    R -e "remotes::install_local(path = './pkg')" \
    && rm -rf ./pkg

EXPOSE 3838

# Needed to run shiny app on NHN infrastructure
RUN adduser --uid "1000" --disabled-password rapporteket && \
    chown -R 1000:1000 /app/R && \
    chmod -R 755 /app/R

USER rapporteket

CMD ["R", "-e", "options(shiny.port = 3838,shiny.host = \"0.0.0.0\"); rapRegTemplate::run_app()"]
