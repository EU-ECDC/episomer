SHELL = bash

define check_epi_home
	@if [ $(origin EPI_HOME) == undefined ]; then echo "please set EPI_HOME is unset"; exit 1; fi
endef

define set_kr_pass
	@export pa=`pass epitools/ecdc_kr_pwd` && if [ -z `printenv pa` ]; then pass insert epitools/ecdc_kr_pwd; fi
endef

# Add social media credentials below
define set_bluesky_user
	@export pa=`pass epitools/ecdc_bluesky_user` && if [ -z `printenv pa` ]; then pass insert epitools/ecdc_bluesky_user; fi
endef

define set_bluesky_password
	@export pa=`pass epitools/ecdc_bluesky_password` && if [ -z `printenv pa` ]; then pass insert epitools/ecdc_bluesky_password; fi
endef
define set_mastodon_tokens
	@export pa=`pass epitools/ecdc_mastodon_mastodon_social_token` && if [ -z `printenv pa` ]; then pass insert epitools/ecdc_mastodon_mastodon_social_token; fi
	@export pa=`pass epitools/ecdc_mastodon_mstdn_party_token` && if [ -z `printenv pa` ]; then pass insert epitools/ecdc_mastodon_mstdn_party_token; fi
endef
# Add social media credentials above

init:
	rm -rf renv/
	rm -f .Rprofile
	rm -f renv.lock
	Rscript devscripts/renv.R


install-dependencies:
	devscripts/install-dependencies.sh

package-full:
	DEVT=1 DEPS=1 JAR=1 PACK=1 DOC=1 RENV=1 devscripts/package.sh

package:
	JAR=1 PACK=1 devscripts/package.sh

install-package:
	JAR=1 PACK=1 RENV=1 INSTALL=1 devscripts/package.sh

package-step:
	devscripts/package.sh

install-test:
	Rscript devscripts/renv.test.R

search-sandboxed:
	$(call check_epi_home)
	$(call set_kr_pass)
	@export EPISOMER_KR_PASSWORD=`pass epitools/ecdc_kr_pwd` && \
	R -e "devtools::load_all('epitweetr');epitweetr::setup_config('$$EPI_HOME');epitweetr::search_loop(sandboxed=TRUE)"

R:
	$(call check_epi_home)
	$(call set_kr_pass)
	$(call set_bluesky_user)
	$(call set_bluesky_password)
	$(call set_mastodon_tokens)
	@export EPISOMER_KR_PASSWORD=`pass epitools/ecdc_kr_pwd` && \
	export EPISOMER_SECRET_BLUESKY_USER=`pass epitools/ecdc_bluesky_user` && \
	export EPISOMER_SECRET_BLUESKY_PASSWORD=`pass epitools/ecdc_bluesky_password` && \
	export EPISOMER_SECRET_MASTODON_MASTODON_SOCIAL_TOKEN=`pass epitools/ecdc_mastodon_mastodon_social_token` && \
	export EPISOMER_SECRET_MASTODON_MSTDN_PARTY_TOKEN=`pass epitools/ecdc_mastodon_mstdn_party_token` && \
	devscripts/r-interpreter.sh

check:
	Rscript devscripts/check-cran.R

check-remote:
	Rscript devscripts/check-cran-remote.R

R-test:
	$(call check_epi_home)
	$(call set_kr_pass)
	devscripts/package.sh
	Rscript devscripts/renv.test.R
	@export EPISOMER_KR_PASSWORD=`pass epitools/ecdc_kr_pwd` && \
	cd test && R

spark:
	$(call check_epi_home)
	$(call set_kr_pass)
	$(call set_bluesky_user)
	$(call set_bluesky_password)
	$(call set_mastodon_tokens)
	@export EPISOMER_KR_PASSWORD=`pass epitools/ecdc_kr_pwd` && \
	export EPISOMER_SECRET_BLUESKY_USER=`pass epitools/ecdc_bluesky_user` && \
	export EPISOMER_SECRET_BLUESKY_PASSWORD=`pass epitools/ecdc_bluesky_password` && \
	export EPISOMER_SECRET_MASTODON_MASTODON_SOCIAL_TOKEN=`pass epitools/ecdc_mastodon_mastodon_social_token` && \
	export EPISOMER_SECRET_MASTODON_MSTDN_PARTY_TOKEN=`pass epitools/ecdc_mastodon_mstdn_party_token` && \
	devscripts/spark-interpreter.sh

scala:
	devscripts/scala-interpreter.sh

run-app:
	EPISOMER_RUN=APP Rscript devscripts/run.R  

run-app-admin:
	EPISOMER_RUN=ADMIN Rscript devscripts/run.R  

run-app-dashboard:
	EPISOMER_RUN=DASH Rscript devscripts/run.R  

run-fs:
	$(call check_epi_home)
	$(call set_kr_pass)
	$(call set_bluesky_user)
	$(call set_bluesky_password)
	$(call set_mastodon_tokens)
	@export EPISOMER_KR_PASSWORD=`pass epitools/ecdc_kr_pwd` && \
	export EPISOMER_SECRET_BLUESKY_USER=`pass epitools/ecdc_bluesky_user` && \
	export EPISOMER_SECRET_BLUESKY_PASSWORD=`pass epitools/ecdc_bluesky_password` && \
	export EPISOMER_SECRET_MASTODON_MASTODON_SOCIAL_TOKEN=`pass epitools/ecdc_mastodon_mastodon_social_token` && \
	export EPISOMER_SECRET_MASTODON_MSTDN_PARTY_TOKEN=`pass epitools/ecdc_mastodon_mstdn_party_token` && \
	EPISOMER_RUN=FS Rscript devscripts/run.R

run-detect:
	EPISOMER_RUN=DETECT Rscript devscripts/run.R  

run-search:
	$(call set_kr_pass)
	@export EPISOMER_KR_PASSWORD=`pass epitools/ecdc_kr_pwd` && \
	EPISOMER_RUN=SEARCH Rscript devscripts/run.R

compose-up:
	$(call check_epi_home)
	$(call set_kr_pass)
	$(call set_bluesky_user)
	$(call set_bluesky_password)
	$(call set_mastodon_tokens)
	@export EPISOMER_KR_PASSWORD=`pass epitools/ecdc_kr_pwd` && \
	export EPISOMER_SECRET_BLUESKY_USER=`pass epitools/ecdc_bluesky_user` && \
	export EPISOMER_SECRET_BLUESKY_PASSWORD=`pass epitools/ecdc_bluesky_password` && \
	export EPISOMER_SECRET_MASTODON_MASTODON_SOCIAL_TOKEN=`pass epitools/ecdc_mastodon_mastodon_social_token` && \
	export EPISOMER_SECRET_MASTODON_MSTDN_PARTY_TOKEN=`pass epitools/ecdc_mastodon_mstdn_party_token` && \
	docker compose -f compose.yml up --build -d	

compose-down:
	docker compose -f compose.yml down
