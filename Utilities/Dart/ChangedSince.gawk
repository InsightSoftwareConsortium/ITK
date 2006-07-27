BEGIN {lastFile = ""}
/^revision [0-9]*\.[0-9]*$/ {
        if (match(lastFile,"itkVersion") != 0) next;
        if (printOne == "yes") printf "\nChanged: %s\n", lastFile; printOne = "no";
        getline
        getline
        getline line
        while ( match(line,"-------") != 0 || match(line,"=====") != 0 && NF != 0) {printf "    %s\n", $0; if (getline line == 0) next;} 
        }
/^RCS file/ {
        lastFile = $3
        gsub(",v$","",lastFile);
        gsub(".cxx$","",lastFile);
        gsub(".txx$","",lastFile);
        gsub(".c$","",lastFile);
        gsub(".h$","",lastFile);
        printOne = "yes"
        }
