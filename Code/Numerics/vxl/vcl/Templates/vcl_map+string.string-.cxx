#include <vcl_string.h>
#include <vcl_map.txx>

#ifndef VCL_BROKEN_AS
VCL_MULTIMAP_INSTANTIATE(vcl_string, vcl_string, vcl_less<vcl_string>);
VCL_MAP_INSTANTIATE(vcl_string, vcl_string, vcl_less<vcl_string>);
#endif

// if you get missing symbol '_' when linking libvcl.so under
// solaris with gcc 2.95, it may be because of this template
// instantiation (the system assembler sometimes truncates very 
// long symbol names). one solution is to recompile gcc with
// squashed name mangling on by default. fsm@robots.ox.ac.uk
