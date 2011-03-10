#ifndef vcl_sunpro_map_h_
#define vcl_sunpro_map_h_
/*
  fsm
*/

#include <map>

// this avoids the VCL_SUNPRO_ALLOCATOR_HACK

template <typename Key, typename Value, typename Comp>
struct vcl_map_sunpro_50 : std::map<Key, Value, Comp, std::allocator<std::pair<Key const, Value> > >
{
  typedef std::map<Key, Value, Comp, std::allocator<std::pair<Key const, Value> > > base;

  vcl_map_sunpro_50() : base() { }

  vcl_map_sunpro_50(base const &that) : base(that) { }
};
#undef  vcl_map
#define vcl_map      vcl_map_sunpro_50

template <typename Key, typename Value, typename Comp>
struct vcl_multimap_sunpro_50 : std::multimap<Key, Value, Comp, std::allocator<std::pair<Key const, Value> > >
{
  typedef std::multimap<Key, Value, Comp, std::allocator<std::pair<Key const, Value> > > base;

  vcl_multimap_sunpro_50() : base() { }

  vcl_multimap_sunpro_50(base const &that) : base(that) { }
};
#undef  vcl_multimap
#define vcl_multimap vcl_multimap_sunpro_50

#endif // vcl_sunpro_map_h_
