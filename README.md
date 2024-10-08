<!-- badges: start -->
[![Version](https://img.shields.io/github/v/release/rapporteket/rapRegTemplate?sort=semver)](https://github.com/rapporteket/rapRegTemplate/releases)
[![R build status](https://github.com/rapporteket/rapRegTemplate/workflows/R-CMD-check/badge.svg)](https://github.com/rapporteket/rapRegTemplate/actions)
[![Codecov test coverage](https://codecov.io/gh/Rapporteket/rapRegTemplate/branch/main/graph/badge.svg)](https://codecov.io/gh/Rapporteket/rapRegTemplate?branch=main)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![GitHub open issues](https://img.shields.io/github/issues/rapporteket/rapRegTemplate.svg)](https://github.com/rapporteket/rapRegTemplate/issues)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Doc](https://img.shields.io/badge/Doc--grey.svg)](https://rapporteket.github.io/rapRegTemplate/)
<!-- badges: end -->
  
# Lag et register i Rapporteket <img src="man/figures/logo.svg" align="right" height="150" />
Beskrivelsen under er ikke nødvendigvis utfyllende og forutsetter kjennskap til RStudio og bruk av git og GitHub. Som en ekstra støtte anbefales [R pacakges](http://r-pkgs.had.co.nz/) av Hadley Wickham og spesielt [beskrivelsen av git og GitHub](http://r-pkgs.had.co.nz/git.html#git-rstudio).

## Prøv templatet
1. Installér pakken [rapRegTemplate](https://github.com/Rapporteket/rapRegTemplate) i RStudio (`remotes::install_github("Rapporteket/rapRegTemplate")`)
1. Hent ned prosjektet [rapRegTemplate](https://github.com/Rapporteket/rapRegTemplate) til RStudio (for mer info, se [her](https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects))
1. Åpne fila R/ui.R og start Shiny-applikasjonen ("Run App")
1. Navigér i applikasjonen for å se på struktur og farger (innhold mangler)

## Valgfritt: lag ditt eget prosjekt basert på templatet
Denne delen er satt som valgfri men kan likevel være relevant, særlig om det er ønskelig å benytte templatetet som utgangspunkt for etablering av nye registre på Rapporteket.

1. Lag et nytt prosjekt i RStudio som en R-pakke
1. Gi pakken et navn som gjerne gjenspeiler overordnet funksjon i pakken, eksempelvis "testRegister"
1. Valgfritt: hak gjerne av for "Create a git repository" også da det vil gi nyttig kunnskap når egne registre skal utvikles seinere
1. Trykk "Create Project".
1. Kopier inn alle filer fra katalogene "inst/" og "R/" i "rapRegTemplate" og legg disse i tilsvarende kataloger i den nye pakken
1. I toppen av "server.ui" endre avhengigheten til R-pakken "rapRegTemplate" til din egen R-pakke
1. Endre DESCRIPTION-fila slik at den blir nogenlunde tilssvarende den som finnes i "rapRegTemplate", særlig det som er gitt under "Depends:", "Imports:" og "Remotes:"
1. Bygg, installér og last pakken i R
1. Test gjerne at innebygget Shiny-applikasjon fungerer på samme vis som i prosjektet "rapRegTemplate"

## Last registerdata
### Alternativ 1: med Docker for Rapporteket
1. Åpne fila R/GetRegData.R
1. Se på de tre delene av funksjonen som definerer registernavn og sql-spørringen samt den som bruker de to forrige til å hente data (og som returnerer ei dataramme fra funksjonen)
1. Sjekk at egen konfigurasjon (i mappen /home/rstudio/rap_config) er i henhold til det datagrunnlaget som er gjort tilgjengelig
1. Endre i SQL-spørringen slik at den er i henhold til datagrunnlaget som ønskes benyttet
1. Prøv funksjonen fra kommandolinja (Console i RStudio), _e.g._ `df <- getRegData("navn_på_register")`
1. Sjekk at du får returnert ei dataramme med X observasjoner for Y variabler, _e.g._ `attributes(df)`

### Alternativ 2: uten Docker for Rapporteket
1. Åpne fila R/GetFakeRegData.R
1. Se at funksjonen returnerer et kjedelig og irrelevant innebygget datasett :-(
1. Prøv funksjonen fra kommandolinja (Console i RStudio), _e.g._ `df <- getFakeRegData()`
1. Sjekk at du får returnert ei dataramme med X observasjoner for Y variabler, _e.g._ `attributes(df)`

## Lag innhold i Shiny-applikasjonen, steg 1
Utgangspunket for de neste stegene er bruk av det innebygde datasettet "mtcars", jf. "Alternativ 2" over.

1. I shiny-applikasjonen, naviger til arkfanen "Figur og tabell"
1. Åpne fila R/ui.R
1. Bla ned til linja `tabPanel("Figur og tabell"`
1. Kommenter inn linjene under, lagre fila og last applikasjonen på nytt ("Reload App")
1. Sjekk at det er kommet inn GUI-elementer i arkfanen "Figur og tabell" som før var tom
1. Prøv gjerne de brukervalg som er i venstre kolonne
1. Oppgave: gjør endringer i R/ui.R (på de linjene som nettopp er kommentert inn) slik at maks antall grupper endres fra 10 til 12 i applikasjonen 

## Lag innhold i Shiny-applikasjonen, steg 2
1. Åpne fila R/server.R
1. Bla ned til kommentaren `# Last inn data` og kommenter inn linja under 
1. Bla videre ned til kommentaren `# Figur og tabell` og kommenter inn de linjene som ligger under `## Figur` og `## Tabell`, hhv
1. Sjekk at det er samsvar mellom id-ene definert i R/ui.R og de datastrukturene (`output$distPlot` og `output$distTable`) du nå har definert i R/server.R
1. Se at `regData` gis inn til de funksjoner som lager figur og tabell, hhv
1. Se også at de samme funksjonene tar i mot de brukervalg som er definert i R/ui.R (`input$var` og `input$bins`)
1. Valgfritt: ta en titt på funksjonen som lager innholdet i figur og tabell: `?makeHist`
1. Lagre fila, start applikasjonen på nytt og sjekk at figur og tabell er på plass og at disse reagerer på ulike brukervalg
1. Oppgave A: lag en ny arkfane "Sammendrag" (etter "Figur" og "Tabell") ved å legge til kode i R/ui.R
1. Oppgave B: fyll "Sammendrag" med en tabell som viser `summary` av valgt variabel ved å legge til kode i R/server.R

Tips til oppgave B:

```r
## Sammendrag
output$distSummary <- shiny::renderTable({
  as.data.frame(sapply(regData, summary))[input$var]
}, rownames = TRUE)
```

## Lag innhold i Shiny-applikasjonen, steg 3
Bruk samme tilnærming som over, men for "Samlerapport". Her er det en del nye elementer, bl.a.

- bruk av en Rmd-fil som rapportmal
- funksjonalitet for nedlasting av rapporten

## Lag innhold i Shiny-applikasjonen, steg 4
Denne delen forutsetter bruk av [Docker for Rapporteket](https://github.com/Rapporteket/docker) eller tilsvarende utviklingsmiljø som "simulerer" Rapporteket. Her skal hver enkelt bruker kunne bestille rutinemessig tilsending per epost av gitte rapporter, eksempelvis slik som samlerapporten over med predefinerte verdier for "Variabel" og "Antall grupper". Tilnærmingen introduserer noen nye elementer, slik som:

- reaktive verdier
- lagring av innstillinger som er "varige" også etter at appliksjonen er avsluttet

## Valgfritt: sjekk inn endringer i git
Git er et verktøy for versjonskontroll som gir mulighet for å spore endringer og samarbeide om kode. Basale funksjoner i git er svært nyttinge, men kan virke forvirrende i starten. Sørg for at egen kode (bestandig) versjonshåndteres (i git) og at koden finnes sentralisert og tilgjengelig for deg selv og andre (på GitHub).

1. Sett opp git lokalt og etabler et sentralt repository for din R-pakke gjennom å følge [Hadley Wickhams veiledning](http://r-pkgs.had.co.nz/git.html#git-rstudio)
1. Om du ikke har det fra før, etabler et ssh-nøkkelpar for sikker kommunikasjon med GitHub

NB Ved etablering av et nøkkelpar for bruk av Secure Shell (ssh) i kommunikasjonen med GitHub (generelt lurt men også nødvendig for avansert bruk av Rapporteket) er det viktig å påse at disse blir etablert på din egen fysiske datamaskin (og eksempelvi ikke inne i en docker-container om det er i bruk)


## Valgfritt: dytt (push) R-pakken til GitHub
1. Om du ikke allerede har gjort det, lag din egen bruker på GitHub (se over)
1. Om du ikke allerede har gjort det, [legg ut den offentlige delen av ditt ssh-nøkkelpar på din github-konto](https://help.github.com/en/articles/adding-a-new-ssh-key-to-your-github-account) 
1. Om du ikke allerede har gjort det, bli medlem av organisasjonen Rapporteket på GitHub
1. Under din egen side på GitHub, opprett et Repository med navn tilsvarende din egen pakke (_e.g._ "testRegister")
1. I RStudio, push pakken til ditt nye Repository på GitHub

## Valgfritt: bygg docker image lokalt

For å bygge og kjøre docker image lokalt kan man gjøre følgende:

1. Bygg pakken til en `tar.gz`-fil
```sh
R CMD build .
```
2. Lag Github Personal Access Token. Dette kan enten gjøres direkte på github (https://github.com/settings/tokens) eller gjennom R (`usethis::create_github_token()`). Det tryggeste er å *ikke* gi den noe særlig med rettigheter (kun lese). Vi lager og bruker en token for å ikke få feil fordi man har for mange api-kall til github.
3. Putt den i miljøvariablen `GITHUB_PAT`.
```sh
export GITHUB_PAT=ghp_ETT_ELLER_ANNET # token du nettop lagde
```
4. Bygg image med navn `some_image_name`. Bruker `--progress plain` for å få ut alt av `stdout`, og mater inn token som en hemmelighet
```sh
docker build -t some_image_name --progress plain --secret id=GITHUB_PAT .
```
5. Kjør image
```sh
# enten
docker run -p 3838:3838 some_image_name
# eller
docker compose up
```
6. Åpne siden http://localhost:3838/ og se resultatet
