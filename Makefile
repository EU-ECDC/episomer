SHELL = bash

install-dependencies:
	devscripts/install-dependencies.sh
package:
	devscripts/package.sh
launch:
	cd epitweetr && ../devscripts/app.sh
