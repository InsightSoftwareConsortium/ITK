#! /bin/sh

for blah in `cat ../generic/blah_tr1`; do
  echo $blah
(
echo "#ifndef vcl_tr1_${blah}_h_"
echo "#define vcl_tr1_${blah}_h_"
echo ""
echo "// This is a generated file. DO NOT EDIT! Not even a little bit."
echo ""
echo "#include <tr1/$blah>"
echo ""
echo "#ifdef vcl_generic_${blah}_tr1_STD"
echo "  ** error **"
echo "#else"
echo "# define vcl_generic_${blah}_tr1_STD std::tr1"
echo "#endif"
echo ""
echo "#include \"../generic/vcl_${blah}_tr1.h\""
echo ""
echo "#endif // vcl_tr1_${blah}_h_"
) > vcl_${blah}.h
done
