#include <vcl_functional.h>
#include <vcl_utility.h>
#include <vcl_set.txx>

typedef vcl_pair<int, int> pair_t;
VCL_SET_INSTANTIATE(pair_t, vcl_less<pair_t>);
