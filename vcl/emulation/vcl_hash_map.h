/*
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

#ifndef vcl_emulation_hash_map_h
#define vcl_emulation_hash_map_h

#include <vcl_functional.h>
//#include <vcl_alloc.h>
#include "vcl_hashtable.h"
#include "vcl_alloc.h"

#ifdef __FULL_NAME
# define VCL_FULL_NAME(x) __FULL_NAME(x)
# define VCL_IMPORT_CONTAINER_TYPEDEFS(super) __IMPORT_CONTAINER_TYPEDEFS(super)
# define VCL_IMPORT_ITERATORS(super) __IMPORT_ITERATORS(super)
# define VCL_IMPORT_REVERSE_ITERATORS(super) __IMPORT_REVERSE_ITERATORS(super)
#else
// Using emulated hashtable, but not stlconf -- mini stlconf is built here

#define VCL_FULL_NAME(x) x

#  define VCL_IMPORT_CONTAINER_TYPEDEFS(super)                            \
    typedef typename super::value_type value_type;                               \
    typedef typename super::reference reference;                                 \
    typedef typename super::size_type size_type;                                 \
    typedef typename super::const_reference const_reference;                     \
    typedef typename super::difference_type difference_type;

#  define VCL_IMPORT_ITERATORS(super)                                              \
    typedef typename super::iterator iterator;                                   \
    typedef typename super::const_iterator const_iterator;

#  define VCL_IMPORT_REVERSE_ITERATORS(super)                                      \
    typedef typename super::const_reverse_iterator  const_reverse_iterator;      \
    typedef typename super::reverse_iterator reverse_iterator;

# ifndef __STL_DEFAULT_TYPE_PARAM
#  define vcl_hash_map  VCL_hash_map__
#  define vcl_hash_multimap  VCL_hash_multimap__
# endif
#endif

//# define  vcl_hash_map      vcl_hM
//# define  vcl_hash_multimap vcl_hmM

template <class Key, class T,
          VCL_DFL_TMPL_PARAM_STLDECL(HashFcn,vcl_hash<Key>),
          VCL_DFL_TMPL_PARAM_STLDECL(EqualKey,vcl_equal_to<Key>),
          VCL_DFL_TYPE_PARAM_STLDECL(Alloc,vcl_alloc) >
class vcl_hash_map
{
private:
  typedef vcl_select1st<vcl_pair<const Key, T> > sel1st;
  typedef vcl_hashtable<vcl_pair<const Key, T>, Key, HashFcn, sel1st, EqualKey, Alloc> ht;
  typedef vcl_hash_map<Key, T, HashFcn, EqualKey, Alloc> self;
public:
  VCL_IMPORT_CONTAINER_TYPEDEFS(ht)
  VCL_IMPORT_ITERATORS(ht)
  typedef typename ht::key_type key_type;
  typedef typename ht::hasher hasher;
  typedef typename ht::key_equal key_equal;
  typedef T data_type;
  typedef typename ht::pointer pointer;
  typedef typename ht::const_pointer const_pointer;
private:
  ht rep;

public:
  hasher hash_funct() const { return rep.hash_funct(); }
  key_equal key_eq() const { return rep.key_eq(); }

public:
  vcl_hash_map() : rep(100, hasher(), key_equal()) {}
  vcl_hash_map(size_type n) : rep(n, hasher(), key_equal()) {}
  vcl_hash_map(size_type n, const hasher& hf) : rep(n, hf, key_equal()) {}
  vcl_hash_map(size_type n, const hasher& hf, const key_equal& eql)
    : rep(n, hf, eql) {}

  vcl_hash_map(const value_type* f, const value_type* l)
    : rep(100, hasher(), key_equal()) { rep.insert_unique(f, l); }
  vcl_hash_map(const value_type* f, const value_type* l, size_type n)
    : rep(n, hasher(), key_equal()) { rep.insert_unique(f, l); }
  vcl_hash_map(const value_type* f, const value_type* l, size_type n,
               const hasher& hf)
    : rep(n, hf, key_equal()) { rep.insert_unique(f, l); }
  vcl_hash_map(const value_type* f, const value_type* l, size_type n,
               const hasher& hf, const key_equal& eql)
    : rep(n, hf, eql) { rep.insert_unique(f, l); }

  vcl_hash_map(const_iterator f, const_iterator l)
    : rep(100, hasher(), key_equal()) { rep.insert_unique(f, l); }
  vcl_hash_map(const_iterator f, const_iterator l, size_type n)
    : rep(n, hasher(), key_equal()) { rep.insert_unique(f, l); }
  vcl_hash_map(const_iterator f, const_iterator l, size_type n,
               const hasher& hf)
    : rep(n, hf, key_equal()) { rep.insert_unique(f, l); }
  vcl_hash_map(const_iterator f, const_iterator l, size_type n,
               const hasher& hf, const key_equal& eql)
    : rep(n, hf, eql) { rep.insert_unique(f, l); }

public:
  size_type size() const { return rep.size(); }
  size_type max_size() const { return rep.max_size(); }
  bool empty() const { return rep.empty(); }
  void swap(self& hs) { rep.swap(hs.rep); }
  friend bool operator==(const vcl_hash_map<Key,T,HashFcn,EqualKey,Alloc>&,
                         const vcl_hash_map<Key,T,HashFcn,EqualKey,Alloc>&);

  iterator begin() { return rep.begin(); }
  iterator end() { return rep.end(); }
  const_iterator begin() const { return rep.begin(); }
  const_iterator end() const { return rep.end(); }

public:
  vcl_pair<iterator, bool> insert(const value_type& obj)
    { return rep.insert_unique(obj); }
  void insert(const value_type* f, const value_type* l) { rep.insert_unique(f,l); }
  void insert(const_iterator f, const_iterator l) { rep.insert_unique(f, l); }
  vcl_pair<iterator, bool> insert_noresize(const value_type& obj)
    { return rep.insert_unique_noresize(obj); }

  iterator find(const key_type& key) { return rep.find(key); }
  const_iterator find(const key_type& key) const { return rep.find(key); }

  T& operator[](const key_type& key)
  {
      value_type val(key, T());
      return rep.find_or_insert(val).second;
  }

  size_type count(const key_type& key) const { return rep.count(key); }

  vcl_pair<iterator, iterator> equal_range(const key_type& key)
    { return rep.equal_range(key); }
  vcl_pair<const_iterator, const_iterator> equal_range(const key_type& key) const
    { return rep.equal_range(key); }

  size_type erase(const key_type& key) {return rep.erase(key); }
  void erase(iterator it) { rep.erase(it); }
  void erase(iterator f, iterator l) { rep.erase(f, l); }
  void clear() { rep.clear(); }

public:
  void resize(size_type hint) { rep.resize(hint); }
  size_type bucket_count() const { return rep.bucket_count(); }
  size_type max_bucket_count() const { return rep.max_bucket_count(); }
  size_type elems_in_bucket(size_type n) const
    { return rep.elems_in_bucket(n); }
};


template <class Key, class T, VCL_DFL_TMPL_PARAM_STLDECL(HashFcn,vcl_hash<Key>),
          VCL_DFL_TMPL_PARAM_STLDECL(EqualKey,vcl_equal_to<Key>),
          VCL_DFL_TYPE_PARAM_STLDECL(Alloc,vcl_alloc) >
class vcl_hash_multimap
{
private:
  typedef vcl_hashtable<vcl_pair<const Key, T>, Key, HashFcn,
      vcl_select1st<vcl_pair<const Key, T> >, EqualKey, Alloc> ht;
  typedef vcl_hash_multimap<Key, T, HashFcn, EqualKey, Alloc> self;
public:
  VCL_IMPORT_CONTAINER_TYPEDEFS(ht)
  VCL_IMPORT_ITERATORS(ht)
  typedef typename ht::key_type key_type;
  typedef typename ht::hasher hasher;
  typedef typename ht::key_equal key_equal;
  typedef T data_type;
  typedef typename ht::pointer pointer;
  typedef typename ht::const_pointer const_pointer;

  hasher hash_funct() const { return rep.hash_funct(); }
  key_equal key_eq() const { return rep.key_eq(); }
private:
  ht rep;

public:
  vcl_hash_multimap() : rep(100, hasher(), key_equal()) {}
  vcl_hash_multimap(size_type n) : rep(n, hasher(), key_equal()) {}
  vcl_hash_multimap(size_type n, const hasher& hf) : rep(n, hf, key_equal()) {}
  vcl_hash_multimap(size_type n, const hasher& hf, const key_equal& eql)
    : rep(n, hf, eql) {}

  vcl_hash_multimap(const value_type* f, const value_type* l)
    : rep(100, hasher(), key_equal()) { rep.insert_equal(f, l); }
  vcl_hash_multimap(const value_type* f, const value_type* l, size_type n)
    : rep(n, hasher(), key_equal()) { rep.insert_equal(f, l); }
  vcl_hash_multimap(const value_type* f, const value_type* l, size_type n,
                    const hasher& hf)
    : rep(n, hf, key_equal()) { rep.insert_equal(f, l); }
  vcl_hash_multimap(const value_type* f, const value_type* l, size_type n,
                    const hasher& hf, const key_equal& eql)
    : rep(n, hf, eql) { rep.insert_equal(f, l); }

  vcl_hash_multimap(const_iterator f, const_iterator l)
    : rep(100, hasher(), key_equal()) { rep.insert_equal(f, l); }
  vcl_hash_multimap(const_iterator f, const_iterator l, size_type n)
    : rep(n, hasher(), key_equal()) { rep.insert_equal(f, l); }
  vcl_hash_multimap(const_iterator f, const_iterator l, size_type n,
                    const hasher& hf)
    : rep(n, hf, key_equal()) { rep.insert_equal(f, l); }
  vcl_hash_multimap(const_iterator f, const_iterator l, size_type n,
                    const hasher& hf, const key_equal& eql)
    : rep(n, hf, eql) { rep.insert_equal(f, l); }

public:
  size_type size() const { return rep.size(); }
  size_type max_size() const { return rep.max_size(); }
  bool empty() const { return rep.empty(); }
  void swap(self& hs) { rep.swap(hs.rep); }
  friend bool operator==(const vcl_hash_multimap<Key,T,HashFcn,EqualKey,Alloc>&,
                         const vcl_hash_multimap<Key,T,HashFcn,EqualKey,Alloc>&);

  iterator begin() { return rep.begin(); }
  iterator end() { return rep.end(); }
  const_iterator begin() const { return rep.begin(); }
  const_iterator end() const { return rep.end(); }

public:
  iterator insert(const value_type& obj) { return rep.insert_equal(obj); }
  void insert(const value_type* f, const value_type* l) { rep.insert_equal(f,l); }
  void insert(const_iterator f, const_iterator l) { rep.insert_equal(f, l); }
  iterator insert_noresize(const value_type& obj)
    { return rep.insert_equal_noresize(obj); }

  iterator find(const key_type& key) { return rep.find(key); }
  const_iterator find(const key_type& key) const { return rep.find(key); }

  size_type count(const key_type& key) const { return rep.count(key); }

  vcl_pair<iterator, iterator> equal_range(const key_type& key)
    { return rep.equal_range(key); }
  vcl_pair<const_iterator, const_iterator> equal_range(const key_type& key) const
    { return rep.equal_range(key); }

  size_type erase(const key_type& key) {return rep.erase(key); }
  void erase(iterator it) { rep.erase(it); }
  void erase(iterator f, iterator l) { rep.erase(f, l); }
  void clear() { rep.clear(); }

public:
  void resize(size_type hint) { rep.resize(hint); }
  size_type bucket_count() const { return rep.bucket_count(); }
  size_type max_bucket_count() const { return rep.max_bucket_count(); }
  size_type elems_in_bucket(size_type n) const
    { return rep.elems_in_bucket(n); }
};

template <class Key, class T, class HashFcn, class EqualKey, class Alloc>
inline bool operator==(const vcl_hash_map<Key, T, HashFcn, EqualKey, Alloc>& hm1,
                       const vcl_hash_map<Key, T, HashFcn, EqualKey, Alloc>& hm2)
{
  return hm1.rep == hm2.rep;
}

template <class Key, class T, class HashFcn, class EqualKey, class Alloc>
inline bool operator==(const vcl_hash_multimap<Key, T, HashFcn, EqualKey, Alloc>& hm1,
                       const vcl_hash_multimap<Key, T, HashFcn, EqualKey, Alloc>& hm2)
{
  return hm1.rep == hm2.rep;
}

// do a cleanup
# undef vcl_hash_map
# undef vcl_hash_multimap

# if defined (__STL_CLASS_PARTIAL_SPECIALIZATION )
template <class Key, class T, class HashFcn, class EqualKey, class Alloc>
inline void swap(VCL_hash_map__<Key, T, HashFcn, EqualKey, Alloc>& a,
                 VCL_hash_map__<Key, T, HashFcn, EqualKey, Alloc>& b) { a.swap(b); }
template <class Key, class T, class HashFcn, class EqualKey, class Alloc>
inline void swap(VCL_hash_multimap__<Key, T, HashFcn, EqualKey, Alloc>& a,
                 VCL_hash_multimap__<Key, T, HashFcn, EqualKey, Alloc>& b) { a.swap(b); }
# endif

# ifndef __STL_DEFAULT_TYPE_PARAM

// provide a "default" vcl_hash_map adaptor
template <class Key, class T, class HashFcn, class EqualKey >
class vcl_hash_map : public VCL_hash_map__<Key, T, HashFcn, EqualKey, vcl_alloc >
{
  typedef vcl_hash_map<Key, T, HashFcn, EqualKey> self;
public:
//rick  typedef typename VCL_hash_map__<Key, T, HashFcn, EqualKey, vcl_alloc> super;
  typedef VCL_hash_map__<Key, T, HashFcn, EqualKey, vcl_alloc> super;
  VCL_IMPORT_CONTAINER_TYPEDEFS(super)
  typedef typename super::key_type key_type;
  typedef typename super::hasher hasher;
  typedef typename super::key_equal key_equal;
  //rick  typedef typename T data_type;
  typedef T data_type;
  typedef typename super::pointer pointer;
  typedef typename super::const_pointer const_pointer;
  vcl_hash_map() {}
  vcl_hash_map(size_type n) : super(n) {}
  vcl_hash_map(size_type n, const hasher& hf) : super(n, hf) {}
  vcl_hash_map(size_type n, const hasher& hf, const key_equal& eql): super(n, hf, eql) {}
  vcl_hash_map(const value_type* f, const value_type* l) : super(f,l) {}
  vcl_hash_map(const value_type* f, const value_type* l, size_type n): super(f,l,n) {}
  vcl_hash_map(const value_type* f, const value_type* l, size_type n,
               const hasher& hf) : super(f,l,n,hf) {}
  vcl_hash_map(const value_type* f, const value_type* l, size_type n,
               const hasher& hf, const key_equal& eql) : super(f,l,n,hf, eql) {}
  vcl_hash_map(const_iterator f, const_iterator l) : super(f,l) { }
  vcl_hash_map(const_iterator f, const_iterator l, size_type n) : super(f,l,n) { }
  vcl_hash_map(const_iterator f, const_iterator l, size_type n,
               const hasher& hf) : super(f, l, n, hf) { }
  vcl_hash_map(const_iterator f, const_iterator l, size_type n,
               const hasher& hf, const key_equal& eql) : super(f, l, n, hf, eql) { }
  friend inline bool operator==(const self& hm1, const self& hm2);
};


template <class Key, class T, class HashFcn, class EqualKey >
inline bool operator==(const vcl_hash_map<Key, T, HashFcn,EqualKey>& hm1,
                       const vcl_hash_map<Key, T, HashFcn,EqualKey>& hm2)
{
    typedef vcl_hash_map<Key, T, HashFcn,EqualKey>::super super;
    return (const super&)hm1 == (const super&)hm2;
}

// provide a "default" vcl_hash_multimap adaptor
template <class Key, class T, class HashFcn, class EqualKey >
class vcl_hash_multimap : public VCL_hash_multimap__<Key, T, HashFcn, EqualKey, vcl_alloc>
{
  typedef vcl_hash_multimap<Key, T, HashFcn, EqualKey> self;
public:
  typedef VCL_hash_multimap__<Key, T, HashFcn, EqualKey, vcl_alloc> super;
  VCL_IMPORT_CONTAINER_TYPEDEFS(super)
  typedef typename super::key_type key_type;
  typedef typename super::hasher hasher;
  typedef typename super::key_equal key_equal;
  typedef T data_type;
  typedef typename super::pointer pointer;
  typedef typename super::const_pointer const_pointer;
  vcl_hash_multimap() {}
  vcl_hash_multimap(size_type n) : super(n) {}
  vcl_hash_multimap(size_type n, const hasher& hf) : super(n, hf) {}
  vcl_hash_multimap(size_type n, const hasher& hf, const key_equal& eql): super(n, hf, eql) {}
  vcl_hash_multimap(const value_type* f, const value_type* l) : super(f,l) {}
  vcl_hash_multimap(const value_type* f, const value_type* l, size_type n): super(f,l,n) {}
  vcl_hash_multimap(const value_type* f, const value_type* l, size_type n,
                    const hasher& hf) : super(f,l,n,hf) {}
  vcl_hash_multimap(const value_type* f, const value_type* l, size_type n,
                    const hasher& hf, const key_equal& eql) : super(f,l,n,hf, eql) {}

  vcl_hash_multimap(const_iterator f, const_iterator l) : super(f,l) { }
  vcl_hash_multimap(const_iterator f, const_iterator l, size_type n) : super(f,l,n) { }
  vcl_hash_multimap(const_iterator f, const_iterator l, size_type n,
                    const hasher& hf) : super(f, l, n, hf) { }
  vcl_hash_multimap(const_iterator f, const_iterator l, size_type n,
                    const hasher& hf, const key_equal& eql) : super(f, l, n, hf, eql) { }
  friend inline bool operator==(const self& hm1, const self& hm2);
};

template <class Key, class T, class HashFcn, class EqualKey >
inline bool operator==(const vcl_hash_multimap<Key, T, HashFcn,EqualKey>& hm1,
                       const vcl_hash_multimap<Key, T, HashFcn,EqualKey>& hm2)
{
    typedef vcl_hash_multimap<Key, T, HashFcn,EqualKey>::super super;
    return (const super&)hm1 == (const super&)hm2;
}

# endif /* VCL_STL_DEFAULT_TYPE_PARAM */

#define VCL_HASH_MAP_INSTANTIATE \
extern "please include emulation/vcl_hash_map.txx instead"

#endif // vcl_emulation_hash_map_h
