#include <vcl_map.txx>
#include <vcl_utility.h>
typedef vcl_pair<unsigned int, unsigned int> pair_uu;
VCL_MAP_INSTANTIATE(pair_uu, int, vcl_less<pair_uu>);
