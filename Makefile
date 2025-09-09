SHELL = bash

define check_epi_home
	@if [ $(origin EPI_HOME) == undefined ]; then echo "please set EPI_HOME is unset"; exit 1; fi
endef

define set_kr_pass
	@export pa=`pass epitools/ecdc_kr_pwd` && if [ -z `printenv pa` ]; then pass insert epitools/ecdc_kr_pwd; fi
endef

init:
	rm -rf renv/
	rm -f .Rprofile
	rm -f renv.lock
	Rscript devscripts/renv.R


install-dependencies:
	devscripts/install-dependencies.sh
package:
	devscripts/package.sh
app:
	devscripts/app.sh

search:
	devscripts/search.sh

search-sandboxed:
	$(call check_epi_home)
	$(call set_kr_pass)
	@export ecdc_wtitter_tool_kr_password=`pass epitools/ecdc_kr_pwd` && \
	R -e "devtools::load_all('epitweetr');epitweetr::setup_config('$$EPI_HOME');epitweetr::search_loop(sandboxed=TRUE)"

r-interpreter:
	devscripts/r-interpreter.sh
