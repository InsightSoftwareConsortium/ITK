#include <vcl_functional.h>
#include <vcl_utility.h>
#include <vcl_string.h> // C++ specific includes first

#if TEST == 1
// Normal
#include <vcl_stlfwd.h>

#else
#if TEST == 2
// stl included first

#include <vcl_map.h>
#include <vcl_set.h>
#include <vcl_list.h>
#include <vcl_stlfwd.h>

#else
// STL included later
#include <vcl_stlfwd.h>
#include <vcl_map.h>
#include <vcl_set.h>
#include <vcl_list.h>

#endif
#endif

void f(vcl_map<int, vcl_string,vcl_less<int> >*,
       vcl_set<int,vcl_less<int> >*,
       vcl_list<int>*
       )
{
}


main()
{
}
