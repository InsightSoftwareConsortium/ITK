#! /bin/sh

for blah in `cat ../generic/blah`; do
  echo $blah
(
echo "#ifndef vcl_iso_${blah}_h_"
echo "#define vcl_iso_${blah}_h_"
echo ""
echo "// This is a generated file. DO NOT EDIT! Not even a little bit."
echo ""
echo "#include <$blah>"
echo ""
echo "#ifdef vcl_generic_${blah}_STD"
echo "  ** error **"
echo "#else"
echo "# define vcl_generic_${blah}_STD std"
echo "#endif"
echo ""
echo "#include \"../generic/vcl_${blah}.h\""
echo ""
echo "#endif // vcl_iso_${blah}_h_"
) > vcl_${blah}.h
done
