#!/bin/bash
${EMACS_EXEC:-emacs} -l spec/env.el -f ert-run-tests-batch-and-exit

# we run this second command in non-batch mode because for some reason
# the readme indentation happens differently in batch
${EMACS_EXEC:-emacs} -l README-spec.el -f readme-write --eval '(kill-emacs)'
