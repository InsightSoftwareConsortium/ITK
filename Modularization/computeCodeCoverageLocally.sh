#!/bin/bash
make
lcov --directory . --zerocounters
ctest
lcov --directory . --capture --output-file app.info
lcov --remove '*test*' --output-file  app.info2
genhtml app.info2
firefox ./index.html
