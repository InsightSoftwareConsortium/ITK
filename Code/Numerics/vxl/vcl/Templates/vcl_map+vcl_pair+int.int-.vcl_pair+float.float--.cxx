#include <vcl_utility.h>
#include <vcl_map.txx>

typedef vcl_pair<int,   int>   pair_ii;
typedef vcl_pair<float, float> pair_ff;

VCL_MAP_INSTANTIATE(pair_ii, pair_ff, vcl_less<pair_ii>);
