#!/bin/bash
emacs -batch -l spec/env.el -f ert-run-tests-batch-and-exit
