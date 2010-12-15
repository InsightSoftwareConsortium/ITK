#!/bin/bash
make
lcov --directory . --zerocounters
ctest
lcov --directory . --capture --output-file app.info
lcov --remove app.info '*test*'  '*vnl*' '/usr/*' '*/v3p/*' '*itksys*' --output-file  app.info2
genhtml app.info2
firefox ./index.html
