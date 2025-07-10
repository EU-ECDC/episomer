SHELL = bash

install-dependencies:
	devscripts/install-dependencies.sh
package:
	devscripts/package.sh
