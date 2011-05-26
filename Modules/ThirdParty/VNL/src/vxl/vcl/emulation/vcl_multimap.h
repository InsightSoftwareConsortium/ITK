/*
 *
 * Copyright (c) 1994
 * Hewlett-Packard Company
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Hewlett-Packard Company makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 *
 * Copyright (c) 1996
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 * Copyright (c) 1997
 * Moscow Center for SPARC Technology
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Moscow Center for SPARC Technology makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 */

#ifndef vcl_emulation_multimap_h
#define vcl_emulation_multimap_h

#include "vcl_tree.h"

__BEGIN_STL_FULL_NAMESPACE
#define vcl_multimap __WORKAROUND_RENAME(vcl_multimap)

template <class Key, class T, VCL_DFL_TMPL_PARAM_STLDECL(Compare,vcl_less<Key>),
                              VCL_DFL_TYPE_PARAM_STLDECL(Alloc,vcl_alloc) >
class vcl_multimap
{
  typedef vcl_multimap<Key, T, Compare, Alloc> self;
 public:
  typedef Key key_type;
  typedef T data_type;
  typedef vcl_pair<const Key, T> value_type;
  typedef Compare key_compare;

  class value_compare : public vcl_binary_function<value_type, value_type, bool>
  {
    friend class vcl_multimap<Key, T, Compare, Alloc>;
   protected:
    Compare comp;
    value_compare(Compare c) : comp(c) {}
   public:
    bool operator()(const value_type& x, const value_type& y) const { return comp(x.first, y.first); }
  };

 private:
  typedef rb_tree<key_type, value_type,
                  vcl_select1st<value_type>, key_compare, Alloc> rep_type;
 public:
  typedef typename rep_type::pointer pointer;
  typedef typename rep_type::reference reference;
  typedef typename rep_type::const_reference const_reference;
  typedef typename rep_type::iterator iterator;
  typedef typename rep_type::const_iterator const_iterator;
  typedef typename rep_type::reverse_iterator reverse_iterator;
  typedef typename rep_type::const_reverse_iterator const_reverse_iterator;
  typedef typename rep_type::size_type size_type;
  typedef typename rep_type::difference_type difference_type;

 private:
  rep_type t;  // red-black vcl_tree representing vcl_multimap

 public:
  // allocation/deallocation
  vcl_multimap() : t(Compare()) {}
  explicit vcl_multimap(const Compare& comp) : t(comp) {}
  vcl_multimap(const value_type* first, const value_type* last) :
      t(Compare()) { t.insert_equal(first, last); }
  vcl_multimap(const value_type* first, const value_type* last,
               const Compare& comp) : t(comp) { t.insert_equal(first, last); }
  vcl_multimap(const_iterator first, const_iterator last) :
      t(Compare()) { t.insert_equal(first, last); }
  vcl_multimap(const_iterator first, const_iterator last,
               const Compare& comp) : t(comp) { t.insert_equal(first, last); }
  vcl_multimap(const self& x) : t(x.t) { }
  self& operator=(const self& x) { t = x.t; return *this; }

  // accessors:

  key_compare key_comp() const { return t.key_comp(); }
  value_compare value_comp() const { return value_compare(t.key_comp()); }
  iterator begin() { return t.begin(); }
  const_iterator begin() const { return t.begin(); }
  iterator end() { return t.end(); }
  const_iterator end() const { return t.end(); }
  reverse_iterator rbegin() { return t.rbegin(); }
  const_reverse_iterator rbegin() const { return t.rbegin(); }
  reverse_iterator rend() { return t.rend(); }
  const_reverse_iterator rend() const { return t.rend(); }
  bool empty() const { return t.empty(); }
  size_type size() const { return t.size(); }
  size_type max_size() const { return t.max_size(); }
  void swap(vcl_multimap<Key, T, Compare, Alloc>& x) { t.swap(x.t); }

  // insert/erase

  iterator insert(const value_type& x) { return t.insert_equal(x); }
  iterator insert(iterator position, const value_type& x) { return t.insert_equal(position, x); }
  void insert(const value_type* first, const value_type* last) { t.insert_equal(first, last); }
  void insert(const_iterator first, const_iterator last) { t.insert_equal(first, last); }
  void erase(iterator position) { t.erase(position); }
  size_type erase(const key_type& x) { return t.erase(x); }
  void erase(iterator first, iterator last) { t.erase(first, last); }
  void clear() { t.clear(); }

  // vcl_multimap operations:

  iterator find(const key_type& x) { return t.find(x); }
  const_iterator find(const key_type& x) const { return t.find(x); }
  size_type count(const key_type& x) const { return t.count(x); }
  iterator lower_bound(const key_type& x) {return t.lower_bound(x); }
  const_iterator lower_bound(const key_type& x) const { return t.lower_bound(x); }
  iterator upper_bound(const key_type& x) {return t.upper_bound(x); }
  const_iterator upper_bound(const key_type& x) const { return t.upper_bound(x); }
   vcl_pair<iterator,iterator> equal_range(const key_type& x) { return t.equal_range(x); }
  vcl_pair<const_iterator,const_iterator> equal_range(const key_type& x) const { return t.equal_range(x); }
  bool operator==(const self& y) const { return t == y.t; }
  bool operator< (const self& y) const { return t <  y.t; }
};
__END_STL_FULL_NAMESPACE

// do a cleanup
#  undef vcl_multimap
// provide a way to access full functionality
#  define __multimap__  __FULL_NAME(vcl_multimap)

# if defined (__STL_CLASS_PARTIAL_SPECIALIZATION )
template <class Key, class T, class Compare, class Alloc>
inline void swap(__multimap__<Key, T, Compare, Alloc>& a,
                 __multimap__<Key, T, Compare, Alloc>& b) { a.swap(b); }
# endif

# if !defined(MULTIMAP_H) && !defined(__STL_DEFAULT_TYPE_PARAM) &&  ( !defined(__STL_NAMESPACES) || defined(__STL_NO_NAMESPACES) )
// provide a "default" vcl_multimap adaptor
template <class Key, class T, class Compare>
class vcl_multimap : public __multimap__<Key, T, Compare, vcl_alloc>
{
  typedef vcl_multimap<Key, T, Compare> self;
 public:
  typedef __multimap__<Key, T, Compare, vcl_alloc> super;
  __CONTAINER_SUPER_TYPEDEFS
  // copy & assignment from super
  __IMPORT_SUPER_COPY_ASSIGNMENT(vcl_multimap)
  vcl_multimap() : super(Compare()) {}
  explicit vcl_multimap(const Compare& comp) : super(comp) {}
  vcl_multimap(const typename super::value_type* first, const typename super::value_type* last) :
      super(first, last, Compare()) { }
  vcl_multimap(const typename super::value_type* first, const typename super::value_type* last,
               const Compare& comp) : super(first, last, comp) { }
  vcl_multimap(typename super::const_iterator first, typename super::const_iterator last) :
      super(first, last, Compare()) { }
  vcl_multimap(typename super::const_iterator first, typename super::const_iterator last,
               const Compare& comp) : super(first, last, comp) { }
};

#  if defined (__STL_BASE_MATCH_BUG)
template <class Key, class T, class Compare>
inline bool operator==(const vcl_multimap<Key, T, Compare>& x,
                       const vcl_multimap<Key, T, Compare>& y)
{
  typedef __multimap__<Key,T,Compare,vcl_alloc> super;
  return (const super&)x == (const super&)y;
}

template <class Key, class T, class Compare>
inline bool operator<(const vcl_multimap<Key, T, Compare>& x,
                      const vcl_multimap<Key, T, Compare>& y)
{
  typedef __multimap__<Key,T,Compare,vcl_alloc> super;
  return (const super&)x < (const super&)y;
}
#  endif
# endif /*  __STL_DEFAULT_TYPE_PARAM */

#endif // vcl_emulation_multimap_h
