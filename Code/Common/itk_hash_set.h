/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itk_hash_set.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
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

#ifndef itk_emulation_hash_set_h
#define itk_emulation_hash_set_h

#include "itk_hashtable.h"
#include <functional>

// ---

namespace itk
{
  
/** \brief Replacement for STL hash set because some systems do not support it,
 * or support it incorrectly.
 */
template <class Value, VCL_DFL_TMPL_PARAM_STLDECL(HashFcn,hash<Value>),
          VCL_DFL_TMPL_PARAM_STLDECL(EqualKey,std::equal_to<Value>),
          VCL_DFL_TYPE_PARAM_STLDECL(Alloc,std::allocator<char> ) >
class hash_set
{
private:
  typedef hashtable<Value, Value, HashFcn, std::identity<Value>, 
      EqualKey, Alloc> ht;
  typedef hash_set<Value, HashFcn, EqualKey, Alloc> self;
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
  hash_set() : rep(100, hasher(), key_equal()) {}
  hash_set(size_type n) : rep(n, hasher(), key_equal()) {}
  hash_set(size_type n, const hasher& hf) : rep(n, hf, key_equal()) {}
  hash_set(size_type n, const hasher& hf, const key_equal& eql)
    : rep(n, hf, eql) {}

  hash_set(const value_type* f, const value_type* l)
    : rep(100, hasher(), key_equal()) { rep.insert_unique(f, l); }
  hash_set(const value_type* f, const value_type* l, size_type n)
    : rep(n, hasher(), key_equal()) { rep.insert_unique(f, l); }
  hash_set(const value_type* f, const value_type* l, size_type n,
           const hasher& hf)
    : rep(n, hf, key_equal()) { rep.insert_unique(f, l); }
  hash_set(const value_type* f, const value_type* l, size_type n,
           const hasher& hf, const key_equal& eql)
    : rep(n, hf, eql) { rep.insert_unique(f, l); }

  hash_set(const_iterator f, const_iterator l)
    : rep(100, hasher(), key_equal()) { rep.insert_unique(f, l); }
  hash_set(const_iterator f, const_iterator l, size_type n)
    : rep(n, hasher(), key_equal()) { rep.insert_unique(f, l); }
  hash_set(const_iterator f, const_iterator l, size_type n,
           const hasher& hf)
    : rep(n, hf, key_equal()) { rep.insert_unique(f, l); }
  hash_set(const_iterator f, const_iterator l, size_type n,
           const hasher& hf, const key_equal& eql)
    : rep(n, hf, eql) { rep.insert_unique(f, l); }

public:
  size_type size() const { return rep.size(); }
  size_type max_size() const { return rep.max_size(); }
  bool empty() const { return rep.empty(); }
  void swap(self& hs) { rep.swap(hs.rep); }
  friend bool operator==VCL_NULL_TMPL_ARGS(const hash_set<Value,HashFcn,EqualKey,Alloc>&,
                           const hash_set<Value,HashFcn,EqualKey,Alloc>&);

  iterator begin() const { return rep.begin(); }
  iterator end() const { return rep.end(); }

public:
  std::pair<iterator, bool> insert(const value_type& obj)
    {
#ifdef _MSC_VER
      std::pair< ht::iterator, bool> p = rep.insert_unique(obj);
#else
      std::pair<typename ht::iterator, bool> p = rep.insert_unique(obj);
#endif
      return std::pair<iterator, bool>(p.first, p.second);
    }
  void insert(const value_type* f, const value_type* l) { rep.insert_unique(f,l); }
  void insert(const_iterator f, const_iterator l) { rep.insert_unique(f, l); }
  std::pair<iterator, bool> insert_noresize(const value_type& obj)
    {
#ifdef _MSC_VER
    std::pair<ht::iterator, bool> p = rep.insert_unique_noresize(obj);
#else
      std::pair<typename ht::iterator, bool> p = rep.insert_unique_noresize(obj);
#endif
      return std::pair<iterator, bool>(p.first, p.second);
    }

  iterator find(const key_type& key) const { return rep.find(key); }

  size_type count(const key_type& key) const { return rep.count(key); }
  
  std::pair<iterator, iterator> equal_range(const key_type& key) const
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


template <class Value, VCL_DFL_TMPL_PARAM_STLDECL(HashFcn,hash<Value>),
          VCL_DFL_TMPL_PARAM_STLDECL(EqualKey,std::equal_to<Value>),
          VCL_DFL_TYPE_PARAM_STLDECL(Alloc,std::allocator<char> ) >
class hash_multiset
{
private:
  typedef hashtable<Value, Value, HashFcn, std::identity<Value>, 
      EqualKey, Alloc> ht;
  typedef hash_multiset<Value, HashFcn, EqualKey, Alloc> self;
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
  hash_multiset() : rep(100, hasher(), key_equal()) {}
  hash_multiset(size_type n) : rep(n, hasher(), key_equal()) {}
  hash_multiset(size_type n, const hasher& hf) : rep(n, hf, key_equal()) {}
  hash_multiset(size_type n, const hasher& hf, const key_equal& eql)
    : rep(n, hf, eql) {}

  hash_multiset(const value_type* f, const value_type* l)
    : rep(100, hasher(), key_equal()) { rep.insert_equal(f, l); }
  hash_multiset(const value_type* f, const value_type* l, size_type n)
    : rep(n, hasher(), key_equal()) { rep.insert_equal(f, l); }
  hash_multiset(const value_type* f, const value_type* l, size_type n,
           const hasher& hf)
    : rep(n, hf, key_equal()) { rep.insert_equal(f, l); }
  hash_multiset(const value_type* f, const value_type* l, size_type n,
           const hasher& hf, const key_equal& eql)
    : rep(n, hf, eql) { rep.insert_equal(f, l); }

  hash_multiset(const_iterator f, const_iterator l)
    : rep(100, hasher(), key_equal()) { rep.insert_equal(f, l); }
  hash_multiset(const_iterator f, const_iterator l, size_type n)
    : rep(n, hasher(), key_equal()) { rep.insert_equal(f, l); }
  hash_multiset(const_iterator f, const_iterator l, size_type n,
           const hasher& hf)
    : rep(n, hf, key_equal()) { rep.insert_equal(f, l); }
  hash_multiset(const_iterator f, const_iterator l, size_type n,
           const hasher& hf, const key_equal& eql)
    : rep(n, hf, eql) { rep.insert_equal(f, l); }

public:
  size_type size() const { return rep.size(); }
  size_type max_size() const { return rep.max_size(); }
  bool empty() const { return rep.empty(); }
  void swap(self& hs) { rep.swap(hs.rep); }
  friend bool operator==VCL_NULL_TMPL_ARGS(const hash_multiset<Value,HashFcn,EqualKey,Alloc>&,
                         const hash_multiset<Value,HashFcn,EqualKey,Alloc>&);

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
  
  std::pair<iterator, iterator> equal_range(const key_type& key) const
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


template <class Value, class HashFcn, class EqualKey, class Alloc>
inline bool operator==(const hash_set<Value, HashFcn, EqualKey, Alloc>& hs1,
                       const hash_set<Value, HashFcn, EqualKey, Alloc>& hs2)
{
  return hs1.rep == hs2.rep;
}

template <class Value, class HashFcn, class EqualKey, class Alloc>
inline bool operator==(const hash_multiset<Value, HashFcn, EqualKey, Alloc>& hs1,
                       const hash_multiset<Value, HashFcn, EqualKey, Alloc>& hs2)
{
  return hs1.rep == hs2.rep;
}

# if defined (__STL_CLASS_PARTIAL_SPECIALIZATION )
template <class Value, class HashFcn, class EqualKey, class Alloc>
inline void swap(hash_multiset<Value, HashFcn, EqualKey, Alloc>& a,
                 hash_multiset<Value, HashFcn, EqualKey, Alloc>& b) { a.swap(b); }
template <class Value, class HashFcn, class EqualKey, class Alloc>
inline void swap(hash_set<Value, HashFcn, EqualKey, Alloc>& a,
                 hash_set<Value, HashFcn, EqualKey, Alloc>& b) { a.swap(b); }
# endif

} // end namespace itk

#endif // itk_emulation_hash_set_h
