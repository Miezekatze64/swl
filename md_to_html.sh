#!/bin/bash
cd `dirname $0`
pandoc -s --to=gfm _README.md --syntax-definition=syntax/swl.xml --metadata title=" " > README.md
