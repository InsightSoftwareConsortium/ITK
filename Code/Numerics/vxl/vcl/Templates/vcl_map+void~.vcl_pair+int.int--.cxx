#include <vcl_map.txx>
#include <vcl_utility.h>
typedef vcl_pair<int, int> pair_ii;
VCL_MAP_INSTANTIATE(void*, pair_ii, vcl_less<void*>);
