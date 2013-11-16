#!/bin/bash
${EMACS_EXEC:-emacs} -batch -l spec/env.el -f ert-run-tests-batch-and-exit
