cat > ./typdef_vim_script.vim << EOF
:%s/typedef  *\(.*<\_s*.*[>,]\_s*.*[><,]\_s*.*[><,]\_s*.*[><,]\_s*.*[><,]\_s*.*[^>]>\)\_s *\([A-Za-z0-9][A-Za-z0-9]*\);\{-};/using \2 = \1;/ge
:%s/typedef  *\(.*<\_s*.*[><,]\_s*.*[><,]\_s*.*[><,]\_s*.*[><,]\_s*.*[^>]>\)\_s *\([A-Za-z0-9][A-Za-z0-9]*\);\{-};/using \2 = \1;/ge
:%s/typedef  *\(.*<\_s*.*[><,]\_s*.*[><,]\_s*.*[><,]\_s*.*[^>]>\)\_s *\([A-Za-z0-9][A-Za-z0-9]*\);\{-};/using \2 = \1;/ge
:%s/typedef  *\(.*<\_s*.*[><,]\_s*.*[><,]\_s*.*[^>]>\)\_s *\([A-Za-z0-9][A-Za-z0-9]*\);\{-};/using \2 = \1;/ge
:%s/typedef  *\(.*<\_s*.*[><,]\_s*.*[^>]>\)\_s *\([A-Za-z0-9][A-Za-z0-9]*\);\{-};/using \2 = \1;/ge
:%s/typedef  *\(.*<\_s*.*[^>]>\)\_s *\([A-Za-z0-9][A-Za-z0-9]*\);\{-};/using \2 = \1;/ge

:%s/\*\(.*\) typedef\(s*\)\(.*\)\*/*\1 type alias\3*/ge
:%s/\*\*\(.*\) typedef\(s*\)\(.*\)/**\1 type alias\3/ge
:%s/\/\/\(.*\)typedefs/\/\/\1type alias/ge

:%s/typedef  *\(.*<\_s*.*,\_s*.*,\_s*[^>]*>\)  *\([A-Za-z0-9][A-Za-z0-9]*\);\{-};/using \2 = \1;/ge
:%s/\/\/\(.*\)typedefs/\/\/\1type alias/ge
:%s/typedef  *\(.*<\_s*.*[>,]\_s.*[^>]>\)\_s *\([A-Za-z0-9][A-Za-z0-9]*\);\{-};/using \2 = \1;/ge
:%s/\*\(.*\) typedef\(s*\)\(.*\)\*/*\1 type alias\3*/ge
:%s/\*\*\(.*\) typedef\(s*\)\(.*\)/**\1 type alias\3/ge
:%s/typedef  *\(.*[><,]*\_s*.*[><,]\_s.*[^>]>\)\_s *\([A-Za-z0-9][A-Za-z0-9]*\);\{-};/using \2 = \1;/ge
:%s/typedef  *\(.*<\_s*.*[>,]\_s*.*[><,]\_s*.*[><,]\_s*.*[^>]>\)\_s *\([A-Za-z0-9][A-Za-z0-9]*\);\{-};/using \2 = \1;/ge
:%s/typedef  *\(.*<\_s*.*[>,]\_s*.*[><,]\_s*.*[><,]\_s*.*[><,]\_s*.*[><,]\_s*.*[^>]>\)\_s *\([A-Za-z0-9][A-Za-z0-9]*\);\{-};/using \2 = \1;/ge

:%s/typedef  *\(.*\) * \([a-zA-Z0-9]*\);/using \2 = \1;/g
:%s/typedef  *\(.*<\_s*.*,\n[^>]*>\)  *\([A-Za-z0-9][A-Za-z0-9]*\);\{-};/using \2 = \1;/ge
:%s/typedef  *\(.*<\_s*.*,.*>\{-}>\)  *\([A-Za-z0-9][A-Za-z0-9]*\);\{-};/using \2 = \1;/ge
:%s/\(  *\)typedef  *\(.*<.*>\)\_s\(  *\)\([A-Za-z0-9][A-Za-z0-9]*\);\{-};/\1using \4 =\r\1    \2;/ge
:%s/typedef support\(\.*\)/type alias support/ge

:%s/  *;/;/ge
:w
:q
EOF

for ff in $(git grep -l "typedef" |fgrep -v ThirdParty |head -n 1500); do
   vim -S ./typdef_vim_script.vim $ff;
done

rm ./typdef_vim_script.vim
