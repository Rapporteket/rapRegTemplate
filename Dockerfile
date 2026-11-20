FROM rapporteket/base-r-alpine-latex:main

WORKDIR /app/R

# Keep the compilers (-c) and font headers (-a) until remotes has finished,
# since it may build updated CRAN dependencies from source. The build tools
# are removed before the final load check, so that check also covers the
# runtime libraries.
RUN --mount=type=secret,id=github_pat,env=GITHUB_PAT \
    --mount=type=bind,source=.,target=/app/R/pkg \
    installr -c -a "fontconfig-dev freetype-dev" RSQLite && \
    R -e "remotes::install_local(path = './pkg')" && \
    apk del gcc g++ musl-dev fontconfig-dev freetype-dev && \
    R -e "library(rapRegTemplate)"

EXPOSE 3838

# Needed to run shiny app on NHN infrastructure
RUN adduser --uid 1000 --disabled-password rapporteket && \
    chown -R 1000:1000 /app/R && \
    chmod -R 755 /app/R

USER 1000

CMD ["R", "-e", "options(shiny.port = 3838,shiny.host = \"0.0.0.0\"); rapRegTemplate::run_app()"]
