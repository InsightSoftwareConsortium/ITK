/^RCS file/ {lastFile = $3}
/revision 1\.1$/ {
        gsub(",v$","",lastFile);
        gsub(".cxx$","",lastFile);
        gsub(".txx$","",lastFile);
        gsub(".c$","",lastFile);
        gsub(".h$","",lastFile);
        printf "new: %s\n", lastFile}
