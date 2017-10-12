
#!/bin/bash
emacs --batch -l ./org/website-publish.el \
      --eval="(require 'website-publish)"\
      --eval="(org-publish-all)"
