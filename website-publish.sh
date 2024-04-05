#!/bin/bash
emacs --batch -l ./website-publish.el \
      --eval="(require 'website-publish)"\
      --eval="(org-publish-all)"
