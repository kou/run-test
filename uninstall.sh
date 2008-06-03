#!/bin/sh

base_dir=`dirname $0`
${EMACS:-emacs} --batch --quick --load $base_dir/install.el --eval '(uninstall)'
