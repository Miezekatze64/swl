#!/bin/bash
cd `dirname $0`
pandoc -s --to=html5 _README.md --syntax-definition=syntax/swl.xml --metadata title=" " > README.html
