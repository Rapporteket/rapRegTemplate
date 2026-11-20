#!/usr/bin/env bash
#
# Turn a copy of rapRegTemplate into a new registry repository: replace the
# template's name and author with the new repository's, and remove files that
# only the template needs. Run from the repository root:
#
#   bash .github/scripts/init-repo.sh <repo-name> <author-name> <author-email>
#
# Used by .github/workflows/init-repo.yml in new repositories.

set -euo pipefail

if [ "$#" -ne 3 ]; then
  echo "Usage: $0 <repo-name> <author-name> <author-email>" >&2
  exit 1
fi

REPO_NAME="$1"
name="$2"
email="$3"
last="${name##* }"
first="${name% *}"

# Search and replace the template name everywhere. This script is skipped,
# so it is not rewritten while it runs; it is deleted after initialization.
find . -type f -not -path '*/.git/*' -not -path './.github/scripts/init-repo.sh' \
  -exec sed -i "s/rapRegTemplate/$REPO_NAME/g" {} +
# Rename the R project file to match the new repository name
mv rapRegTemplate.Rproj "${REPO_NAME}.Rproj"

# Update DESCRIPTION file with repository-specific information
sed -i "s/Version.*/Version: 0.1.0/g" DESCRIPTION
sed -i "s/Title:.*/Title: Rapporteket application for $REPO_NAME/g" DESCRIPTION
sed -i "/Author/,/Maintainer/c\
Authors@R: c(\n\
    person(given = \"${first}\",\n\
           family = \"${last}\",\n\
           role = c(\"aut\", \"cre\"),\n\
           email = \"${email}\"))\n\
Maintainer: ${name} <${email}>" DESCRIPTION

# Delete stuff from README.md
sed -i '/<!-- badges: end -->/q' README.md
echo "" >> README.md
echo "# Rapportek for ${REPO_NAME}" >> README.md
echo "" >> README.md

# Remove unnecessary files and directories
rm -rf docker-compose.yml
rm -rf dev/db
rm -rf dev/mssql
rm -rf .github/workflows/docker.yml
rm -rf R/azure_prep.R
sed -i '/^  if (Sys\.getenv("R_RAP_INSTANCE") == "azure") {/,/^  }/d' R/run_app.R
rm -rf tests/testthat/test-azure_prep.R

# Drop Imports that were only used by R/azure_prep.R
sed -i '/^    DBI,$/d; /^    RSQLite,$/d' DESCRIPTION
