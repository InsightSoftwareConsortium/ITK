#include <vcl_utility.h>
#include <vcl_map.txx>
typedef vcl_pair<int, int> pair_ii;
VCL_MAP_INSTANTIATE(pair_ii, void*, vcl_less<pair_ii>);
