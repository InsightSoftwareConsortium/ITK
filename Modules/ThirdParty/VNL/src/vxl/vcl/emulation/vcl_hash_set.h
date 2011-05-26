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

#ifndef vcl_emulation_hash_set_h
#define vcl_emulation_hash_set_h

#include "vcl_hashtable.h"

__BEGIN_STL_FULL_NAMESPACE
# define  vcl_hash_set      __WORKAROUND_RENAME(vcl_hash_set)
# define  vcl_hash_multiset __WORKAROUND_RENAME(vcl_hash_multiset)

template <class Value, VCL_DFL_TMPL_PARAM_STLDECL(HashFcn,vcl_hash<Value>),
          VCL_DFL_TMPL_PARAM_STLDECL(EqualKey,vcl_equal_to<Value>),
          VCL_DFL_TYPE_PARAM_STLDECL(Alloc,vcl_alloc) >
class vcl_hash_set
{
private:
  typedef vcl_hashtable<Value, Value, HashFcn, vcl_identity<Value>,
      EqualKey, Alloc> ht;
  typedef vcl_hash_set<Value, HashFcn, EqualKey, Alloc> self;
public:
  typedef typename ht::key_type key_type;
  typedef typename ht::value_type value_type;
  typedef typename ht::hasher hasher;
  typedef typename ht::key_equal key_equal;

  typedef typename ht::size_type size_type;
  typedef typename ht::difference_type difference_type;
  typedef typename ht::const_pointer pointer;
  typedef typename ht::const_pointer const_pointer;
  typedef typename ht::const_reference reference;
  typedef typename ht::const_reference const_reference;
  // SunPro bug
  typedef typename ht::const_iterator const_iterator;
  typedef const_iterator iterator;

  // vc6 addition
  typedef typename ht::iterator ht_iterator;

  hasher hash_funct() const { return rep.hash_funct(); }
  key_equal key_eq() const { return rep.key_eq(); }

private:
  ht rep;

public:
  vcl_hash_set() : rep(100, hasher(), key_equal()) {}
  vcl_hash_set(size_type n) : rep(n, hasher(), key_equal()) {}
  vcl_hash_set(size_type n, const hasher& hf) : rep(n, hf, key_equal()) {}
  vcl_hash_set(size_type n, const hasher& hf, const key_equal& eql)
    : rep(n, hf, eql) {}

  vcl_hash_set(const value_type* f, const value_type* l)
    : rep(100, hasher(), key_equal()) { rep.insert_unique(f, l); }
  vcl_hash_set(const value_type* f, const value_type* l, size_type n)
    : rep(n, hasher(), key_equal()) { rep.insert_unique(f, l); }
  vcl_hash_set(const value_type* f, const value_type* l, size_type n,
               const hasher& hf)
    : rep(n, hf, key_equal()) { rep.insert_unique(f, l); }
  vcl_hash_set(const value_type* f, const value_type* l, size_type n,
               const hasher& hf, const key_equal& eql)
    : rep(n, hf, eql) { rep.insert_unique(f, l); }

  vcl_hash_set(const_iterator f, const_iterator l)
    : rep(100, hasher(), key_equal()) { rep.insert_unique(f, l); }
  vcl_hash_set(const_iterator f, const_iterator l, size_type n)
    : rep(n, hasher(), key_equal()) { rep.insert_unique(f, l); }
  vcl_hash_set(const_iterator f, const_iterator l, size_type n,
               const hasher& hf)
    : rep(n, hf, key_equal()) { rep.insert_unique(f, l); }
  vcl_hash_set(const_iterator f, const_iterator l, size_type n,
               const hasher& hf, const key_equal& eql)
    : rep(n, hf, eql) { rep.insert_unique(f, l); }

public:
  size_type size() const { return rep.size(); }
  size_type max_size() const { return rep.max_size(); }
  bool empty() const { return rep.empty(); }
  void swap(self& hs) { rep.swap(hs.rep); }
  friend inline bool operator==(const vcl_hash_set<Value,HashFcn,EqualKey,Alloc>&,
                                const vcl_hash_set<Value,HashFcn,EqualKey,Alloc>&);

  iterator begin() const { return rep.begin(); }
  iterator end() const { return rep.end(); }

public:
  vcl_pair<iterator, bool> insert(const value_type& obj)
    {
#ifdef VC50
      vcl_pair< ht::iterator, bool> p = rep.insert_unique(obj);
#else
      vcl_pair<typename ht::iterator, bool> p = rep.insert_unique(obj);
#endif
      return vcl_pair<iterator, bool>(p.first, p.second);
    }
  void insert(const value_type* f, const value_type* l) { rep.insert_unique(f,l); }
  void insert(const_iterator f, const_iterator l) { rep.insert_unique(f, l); }
  vcl_pair<iterator, bool> insert_noresize(const value_type& obj)
    {
#ifdef VC50
          vcl_pair<ht::iterator, bool> p = rep.insert_unique_noresize(obj);
#else
      vcl_pair<typename ht::iterator, bool> p = rep.insert_unique_noresize(obj);
#endif
      return vcl_pair<iterator, bool>(p.first, p.second);
    }

  iterator find(const key_type& key) const { return rep.find(key); }

  size_type count(const key_type& key) const { return rep.count(key); }

  vcl_pair<iterator, iterator> equal_range(const key_type& key) const
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


template <class Value, VCL_DFL_TMPL_PARAM_STLDECL(HashFcn,vcl_hash<Value>),
          VCL_DFL_TMPL_PARAM_STLDECL(EqualKey,vcl_equal_to<Value>),
          VCL_DFL_TYPE_PARAM_STLDECL(Alloc,vcl_alloc) >
class vcl_hash_multiset
{
private:
  typedef vcl_hashtable<Value, Value, HashFcn, vcl_identity<Value>,
      EqualKey, Alloc> ht;
  typedef vcl_hash_multiset<Value, HashFcn, EqualKey, Alloc> self;
public:
  typedef typename ht::key_type key_type;
  typedef typename ht::value_type value_type;
  typedef typename ht::hasher hasher;
  typedef typename ht::key_equal key_equal;

  typedef typename ht::size_type size_type;
  typedef typename ht::difference_type difference_type;
  typedef typename ht::const_pointer pointer;
  typedef typename ht::const_pointer const_pointer;
  typedef typename ht::const_reference reference;
  typedef typename ht::const_reference const_reference;

  typedef typename ht::const_iterator const_iterator;
  // SunPro bug
  typedef const_iterator iterator;

  hasher hash_funct() const { return rep.hash_funct(); }
  key_equal key_eq() const { return rep.key_eq(); }
private:
  ht rep;

public:
  vcl_hash_multiset() : rep(100, hasher(), key_equal()) {}
  vcl_hash_multiset(size_type n) : rep(n, hasher(), key_equal()) {}
  vcl_hash_multiset(size_type n, const hasher& hf) : rep(n, hf, key_equal()) {}
  vcl_hash_multiset(size_type n, const hasher& hf, const key_equal& eql)
    : rep(n, hf, eql) {}

  vcl_hash_multiset(const value_type* f, const value_type* l)
    : rep(100, hasher(), key_equal()) { rep.insert_equal(f, l); }
  vcl_hash_multiset(const value_type* f, const value_type* l, size_type n)
    : rep(n, hasher(), key_equal()) { rep.insert_equal(f, l); }
  vcl_hash_multiset(const value_type* f, const value_type* l, size_type n,
                    const hasher& hf)
    : rep(n, hf, key_equal()) { rep.insert_equal(f, l); }
  vcl_hash_multiset(const value_type* f, const value_type* l, size_type n,
                    const hasher& hf, const key_equal& eql)
    : rep(n, hf, eql) { rep.insert_equal(f, l); }

  vcl_hash_multiset(const_iterator f, const_iterator l)
    : rep(100, hasher(), key_equal()) { rep.insert_equal(f, l); }
  vcl_hash_multiset(const_iterator f, const_iterator l, size_type n)
    : rep(n, hasher(), key_equal()) { rep.insert_equal(f, l); }
  vcl_hash_multiset(const_iterator f, const_iterator l, size_type n,
                    const hasher& hf)
    : rep(n, hf, key_equal()) { rep.insert_equal(f, l); }
  vcl_hash_multiset(const_iterator f, const_iterator l, size_type n,
                    const hasher& hf, const key_equal& eql)
    : rep(n, hf, eql) { rep.insert_equal(f, l); }

public:
  size_type size() const { return rep.size(); }
  size_type max_size() const { return rep.max_size(); }
  bool empty() const { return rep.empty(); }
  void swap(self& hs) { rep.swap(hs.rep); }
  friend inline bool operator==(const vcl_hash_multiset<Value,HashFcn,EqualKey,Alloc>&,
                                const vcl_hash_multiset<Value,HashFcn,EqualKey,Alloc>&);

  iterator begin() const { return rep.begin(); }
  iterator end() const { return rep.end(); }

public:
  iterator insert(const value_type& obj) { return rep.insert_equal(obj); }
  void insert(const value_type* f, const value_type* l) { rep.insert_equal(f,l); }
  void insert(const_iterator f, const_iterator l) { rep.insert_equal(f, l); }
  iterator insert_noresize(const value_type& obj)
    { return rep.insert_equal_noresize(obj); }

  iterator find(const key_type& key) const { return rep.find(key); }

  size_type count(const key_type& key) const { return rep.count(key); }

  vcl_pair<iterator, iterator> equal_range(const key_type& key) const
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
__END_STL_FULL_NAMESPACE

// do a cleanup
#  undef vcl_hash_set
#  undef vcl_hash_multiset
// provide a uniform way to access full functionality
#  define __hash_set__       __FULL_NAME(vcl_hash_set)
#  define __hash_multiset__  __FULL_NAME(vcl_hash_multiset)

template <class Value, class HashFcn, class EqualKey, class Alloc>
inline bool operator==(const __hash_set__<Value, HashFcn, EqualKey, Alloc>& hs1,
                       const __hash_set__<Value, HashFcn, EqualKey, Alloc>& hs2)
{
  return hs1.rep == hs2.rep;
}

template <class Value, class HashFcn, class EqualKey, class Alloc>
inline bool operator==(const __hash_multiset__<Value, HashFcn, EqualKey, Alloc>& hs1,
                       const __hash_multiset__<Value, HashFcn, EqualKey, Alloc>& hs2)
{
  return hs1.rep == hs2.rep;
}

# if defined (__STL_CLASS_PARTIAL_SPECIALIZATION )
template <class Value, class HashFcn, class EqualKey, class Alloc>
inline void swap(__hash_multiset__<Value, HashFcn, EqualKey, Alloc>& a,
                 __hash_multiset__<Value, HashFcn, EqualKey, Alloc>& b) { a.swap(b); }
template <class Value, class HashFcn, class EqualKey, class Alloc>
inline void swap(__hash_set__<Value, HashFcn, EqualKey, Alloc>& a,
                 __hash_set__<Value, HashFcn, EqualKey, Alloc>& b) { a.swap(b); }
# endif

# ifndef __STL_DEFAULT_TYPE_PARAM
// provide a "default" vcl_hash_set adaptor
template <class Value, class HashFcn, class EqualKey >
class vcl_hash_set : public __hash_set__<Value, HashFcn, EqualKey, vcl_alloc>
{
  typedef vcl_hash_set<Value, HashFcn, EqualKey> self;
public:
  typedef __hash_set__<Value, HashFcn, EqualKey, vcl_alloc> super;
  __IMPORT_CONTAINER_TYPEDEFS(super)
  typedef typename super::key_type key_type;
  typedef typename super::hasher hasher;
  typedef typename super::key_equal key_equal;
  typedef typename super::pointer pointer;
  typedef typename super::const_pointer const_pointer;
  vcl_hash_set() {}
  vcl_hash_set(size_type n) : super(n) {}
  vcl_hash_set(size_type n, const hasher& hf) : super(n, hf) {}
  vcl_hash_set(size_type n, const hasher& hf, const key_equal& eql): super(n, hf, eql) {}

  vcl_hash_set(const value_type* f, const value_type* l) : super(f,l) {}
  vcl_hash_set(const value_type* f, const value_type* l, size_type n): super(f,l,n) {}
  vcl_hash_set(const value_type* f, const value_type* l, size_type n,
               const hasher& hf) : super(f,l,n,hf) {}
  vcl_hash_set(const value_type* f, const value_type* l, size_type n,
               const hasher& hf, const key_equal& eql) : super(f,l,n,hf, eql) {}

  vcl_hash_set(const_iterator f, const_iterator l) : super(f,l) { }
  vcl_hash_set(const_iterator f, const_iterator l, size_type n) : super(f,l,n) { }
  vcl_hash_set(const_iterator f, const_iterator l, size_type n,
               const hasher& hf) : super(f, l, n, hf) { }
  vcl_hash_set(const_iterator f, const_iterator l, size_type n,
               const hasher& hf, const key_equal& eql) : super(f, l, n, hf, eql) { }
  friend inline bool operator==(const self& hs1, const self& hs2);
};

template <class Value, class HashFcn,class EqualKey >
inline bool operator==(const vcl_hash_set<Value, HashFcn,EqualKey>& hs1,
                       const vcl_hash_set<Value, HashFcn,EqualKey>& hs2)
{
    typedef vcl_hash_set<Value, HashFcn,EqualKey>::super super;
    return (const super&)hs1 == (const super&)hs2;
}

// provide a "default" vcl_hash_multiset adaptor
template <class Value, class HashFcn, class EqualKey >
class vcl_hash_multiset : public __hash_multiset__<Value, HashFcn, EqualKey, vcl_alloc>
{
  typedef vcl_hash_multiset<Value, HashFcn, EqualKey> self;
public:
  typedef __hash_multiset__<Value, HashFcn, EqualKey, vcl_alloc> super;
  __IMPORT_CONTAINER_TYPEDEFS(super)
  typedef typename super::key_type key_type;
  typedef typename super::hasher hasher;
  typedef typename super::key_equal key_equal;
  typedef typename super::pointer pointer;
  typedef typename super::const_pointer const_pointer;

  vcl_hash_multiset() {}
  vcl_hash_multiset(size_type n) : super(n) {}
  vcl_hash_multiset(size_type n, const hasher& hf) : super(n, hf) {}
  vcl_hash_multiset(size_type n, const hasher& hf, const key_equal& eql): super(n, hf, eql) {}

  vcl_hash_multiset(const value_type* f, const value_type* l) : super(f,l) {}
  vcl_hash_multiset(const value_type* f, const value_type* l, size_type n): super(f,l,n) {}
  vcl_hash_multiset(const value_type* f, const value_type* l, size_type n,
                    const hasher& hf) : super(f,l,n,hf) {}
  vcl_hash_multiset(const value_type* f, const value_type* l, size_type n,
                    const hasher& hf, const key_equal& eql) : super(f,l,n,hf, eql) {}

  vcl_hash_multiset(const_iterator f, const_iterator l) : super(f,l) { }
  vcl_hash_multiset(const_iterator f, const_iterator l, size_type n) : super(f,l,n) { }
  vcl_hash_multiset(const_iterator f, const_iterator l, size_type n,
                    const hasher& hf) : super(f, l, n, hf) { }
  vcl_hash_multiset(const_iterator f, const_iterator l, size_type n,
                    const hasher& hf, const key_equal& eql) : super(f, l, n, hf, eql) { }
  friend inline bool operator==(const self& hs1, const self& hs2);
};

template <class Value, class HashFcn,class EqualKey >
inline bool operator==(const vcl_hash_multiset<Value, HashFcn,EqualKey>& hs1,
                       const vcl_hash_multiset<Value, HashFcn,EqualKey>& hs2)
{
    typedef vcl_hash_multiset<Value, HashFcn,EqualKey>::super super;
    return (const super&)hs1 == (const super&)hs2;
}

# endif /*  __STL_DEFAULT_TYPE_PARAM */

#endif // vcl_emulation_hash_set_h
