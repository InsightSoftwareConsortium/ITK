#ifndef vcl_sunpro_map_h_
#define vcl_sunpro_map_h_
/*
  fsm@robots.ox.ac.uk
*/

#include <map>

// this avoids the VCL_SUNPRO_ALLOCATOR_HACK

template <typename Key, typename Value, typename Comp>
struct vcl_map_sunpro_50 : public std::map<Key, Value, Comp, std::allocator<std::pair<Key const, Value> > >
{
  typedef std::map<Key, Value, Comp, std::allocator<std::pair<Key const, Value> > > base;

  vcl_map_sunpro_50() : base() { }

  vcl_map_sunpro_50(base const &that) : base(that) { }
};

template <typename Key, typename Value, typename Comp>
struct vcl_multimap_sunpro_50 : public std::multimap<Key, Value, Comp, std::allocator<std::pair<Key const, Value> > >
{
  typedef std::multimap<Key, Value, Comp, std::allocator<std::pair<Key const, Value> > > base;

  vcl_multimap_sunpro_50() : base() { }

  vcl_multimap_sunpro_50(base const &that) : base(that) { }
};

#endif // vcl_sunpro_map_h_
