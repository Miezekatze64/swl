#!/bin/bash
cd `dirname $0`
pandoc -s --to=org _README.md --syntax-definition=syntax/swl.xml --metadata title=" " > README.org
