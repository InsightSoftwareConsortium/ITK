/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itk_hashtable.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
 * Exception Handling:
 * Copyright (c) 1997
 * Mark of the Unicorn, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Mark of the Unicorn makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 * Adaptation:
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


#ifndef itk_emulation_hashtable_h
#define itk_emulation_hashtable_h

#if !defined(__GNUC__) || !((__GNUC__==3) && (__GNUC_MINOR__>=1))

/** \brief Hashtable class, used to implement the hashed associative containers
 * itk_hash_set, itk_hash_map, itk_hash_multiset, and itk_hash_multimap.
 */
#include "itkMacro.h"
#include <iostream>
#include "itk_alloc.h"
#include <vector>
#include <utility>
#include <memory>
#include "vcl_compiler.h"
#include <functional>
#include <algorithm>
#include <iterator>


namespace itk
{
template <class Key> struct hash { };

inline size_t hash_string(const char* s)
{
  unsigned long h = 0; 
  for ( ; *s; ++s)
    h = 5*h + *s;
  
  return size_t(h);
}

template<>
struct hash<char*>
{
  size_t operator()(const char* s) const { return hash_string(s); }
};

template<>
struct hash<const char*>
{
  size_t operator()(const char* s) const { return hash_string(s); }
};

template<>
struct hash<char> {
  size_t operator()(char x) const { return x; }
};

template<>
struct hash<unsigned char> {
  size_t operator()(unsigned char x) const { return x; }
};

template<>
struct hash<signed char> {
  size_t operator()(unsigned char x) const { return x; }
};

template<>
struct hash<short> {
  size_t operator()(short x) const { return x; }
};

template<>
struct hash<unsigned short> {
  size_t operator()(unsigned short x) const { return x; }
};

template<>
struct hash<int> {
  size_t operator()(int x) const { return x; }
};

template<>
struct hash<unsigned int> {
  size_t operator()(unsigned int x) const { return x; }
};

template<>
struct hash<long> {
  size_t operator()(long x) const { return x; }
};

template<>
struct hash<unsigned long> {
  size_t operator()(unsigned long x) const { return x; }
};

template <class Value>
struct hashtable_node
{
  typedef hashtable_node<Value> self;
  self* next;
  Value val;
};  

template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey ,  VCL_DFL_TYPE_PARAM_STLDECL(Alloc,std::allocator<char>)>
class hashtable;

template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
struct hashtable_iterator;

template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
struct hashtable_const_iterator;

template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
struct hashtable_iterator 
{
  typedef hashtable<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>
          hash_table;
  typedef hashtable_iterator<Value, Key, HashFcn, 
                               ExtractKey, EqualKey, Alloc>
          iterator;
  typedef hashtable_const_iterator<Value, Key, HashFcn, 
                                     ExtractKey, EqualKey, Alloc>
          const_iterator;
  typedef hashtable_node<Value> node;
  typedef size_t size_type;
  typedef Value& reference;
  typedef const Value& const_reference;

  node* cur;
  hash_table* ht;

  hashtable_iterator(node* n, hash_table* tab) : cur(n), ht(tab) {}
  hashtable_iterator() {}
  reference operator*() const { 
        return cur->val; 
  }
  IUEi_STL_INLINE iterator& operator++();
  IUEi_STL_INLINE iterator operator++(int);
  bool operator==(const iterator& it) const { 
      return cur == it.cur; 
  }
  bool operator!=(const iterator& it) const { 
      return cur != it.cur; 
  }
};


template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
struct hashtable_const_iterator 
{
  typedef hashtable<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>
          hash_table;
  typedef hashtable_iterator<Value, Key, HashFcn, 
     ExtractKey, EqualKey, Alloc> iterator;
  typedef hashtable_const_iterator<Value, Key, HashFcn, 
     ExtractKey, EqualKey, Alloc> const_iterator;
  typedef hashtable_node<Value> node;
  typedef size_t size_type;
  typedef Value& reference;
  typedef const Value& const_reference;

  const node* cur;
  const hash_table* ht;

  hashtable_const_iterator(const node* n, const hash_table* tab) : cur(n), ht(tab) {}
  hashtable_const_iterator() {}
  hashtable_const_iterator(const iterator& it) : cur(it.cur), ht(it.ht) {}

  const_reference operator*() const { 
      return cur->val; 
  }
  IUEi_STL_INLINE const_iterator& operator++();
  IUEi_STL_INLINE const_iterator operator++(int);
  bool operator==(const const_iterator& it) const { 
      return cur == it.cur; 
  }
  bool operator!=(const const_iterator& it) const { 
      return cur != it.cur; 
  }
};

// Note: assumes long is at least 32 bits.
// fbp: try to avoid intances in every module
enum { num_primes = 28 };

#if ( __STL_STATIC_TEMPLATE_DATA > 0 ) && ! defined (WIN32)
#  define prime_list prime<false>::list_
   template <bool dummy>
   struct prime {
   public:
       static const unsigned long list_[];
   };
      static const unsigned long prime_list_dummy[num_primes] =
#  else
#  if ( __STL_WEAK_ATTRIBUTE > 0 )
      extern const unsigned long prime_list[num_primes] __attribute__((weak)) =
#  else
      // give up
      static const unsigned long prime_list[num_primes] =
#  endif /* __STL_WEAK_ATTRIBUTE */
#endif /* __STL_STATIC_TEMPLATE_DATA */
{
  53,         97,         193,       389,       769,
  1543,       3079,       6151,      12289,     24593,
  49157,      98317,      196613,    393241,    786433,
  1572869,    3145739,    6291469,   12582917,  25165843,
  50331653,   100663319,  201326611, 402653189, 805306457, 
  1610612741, 3221225473U, 4294967291U
};

inline unsigned long next_prime(unsigned long n)
{
  const unsigned long* first = prime_list;
  const unsigned long* last = prime_list;
  last += num_primes;
  const unsigned long* pos = std::lower_bound(first, last, n);
  return pos == last ? *(last - 1) : *pos;
}

template <class Value, class Alloc>
class hashtable_base 
{
private:
    typedef Value value_type;
    typedef size_t size_type;
    typedef hashtable_node<Value> node;
    typedef itk_simple_alloc<node, Alloc> node_allocator;
public: // These are public to get around restriction on protected access
    typedef std::vector<VCL_SUNPRO_ALLOCATOR_HACK(node*) > buckets_type ;
    buckets_type buckets; // awf killed optional allocator
    size_type num_elements;
protected:
    IUEi_STL_INLINE void clear();

    node* new_node(const value_type& obj)
  {
            node* n = node_allocator::allocate();
            try {
        new (&(n->val)) value_type(obj);
            }
            catch (...) {
        node_allocator::deallocate(n);
        throw "";
            }
            n->next = 0;
            return n;
  }
  
    void delete_node(node* n)
  {
#define vcli_destroy(T, p)    ((T*)p)->~T()
            vcli_destroy(Value, &(n->val));
#undef vcli_destroy
            node_allocator::deallocate(n);
  }

    IUEi_STL_INLINE void copy_from(const hashtable_base<Value,Alloc>& ht);
  
public: // These are public to get around restriction on protected access
    hashtable_base() : num_elements(0) { }
//    hashtable_base(size_type n) : num_elements(0) {}
    ~hashtable_base() { clear(); }
};


// forward declarations
template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc> class hashtable;
template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
  bool operator== (hashtable<Value,Key,HashFcn,ExtractKey,EqualKey,Alloc>const&,hashtable<Value,Key,HashFcn,ExtractKey,EqualKey,Alloc>const&);

template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
class hashtable : protected hashtable_base<Value, Alloc> 
{
  typedef hashtable_base<Value, Alloc> super;
  typedef hashtable<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc> self;
public:
  typedef Key key_type;
  typedef Value value_type;
  typedef HashFcn hasher;
  typedef EqualKey key_equal;

  typedef size_t            size_type;
  typedef ptrdiff_t         difference_type;
  typedef value_type*       pointer;
  typedef const value_type* const_pointer;
  typedef value_type&       reference;
  typedef const value_type& const_reference;

  hasher hash_funct() const { return hashfun; }
  key_equal key_eq() const { return equals; }

private:
  hasher hashfun;
  key_equal equals;
  ExtractKey get_key;

  typedef hashtable_node<Value> node;
  typedef itk_simple_alloc<node, Alloc> node_allocator;

public:
  typedef hashtable_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc> iterator;
  typedef hashtable_const_iterator<Value, Key, HashFcn, ExtractKey, EqualKey,Alloc> const_iterator;
  friend struct
  hashtable_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>;
  friend struct
  hashtable_const_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>;

public:
  hashtable(size_type n,
            const HashFcn&    hf,
            const EqualKey&   eql,
            const ExtractKey& ext)
      : hashfun(hf), equals(eql), get_key(ext) {
        initialize_buckets(n);
    }

  hashtable(size_type n,
            const HashFcn&    hf,
            const EqualKey&   eql)
      : hashfun(hf), equals(eql), get_key(ExtractKey()) {
        initialize_buckets(n);
    }

  hashtable(const self& ht)
    : hashfun(ht.hashfun), equals(ht.equals), get_key(ht.get_key) {
        copy_from(ht);
  }

  self& operator= (const self& ht)
  {
    if (&ht != this) {
      hashfun = ht.hashfun;
      equals = ht.equals;
      get_key = ht.get_key;
      clear();
      buckets.clear();
      copy_from(ht);
    }
    return *this;
  }

  ~hashtable() {}

  size_type size() const { return num_elements; }
  size_type max_size() const { return size_type(-1); }
  bool empty() const { return size() == 0; }

  void swap(self& ht)
  {
    std::swap(hashfun, ht.hashfun);
    std::swap(equals, ht.equals);
    std::swap(get_key, ht.get_key);
    buckets.swap(ht.buckets);
    std::swap(num_elements, ht.num_elements);
  }

  iterator begin()
  { 
    for (size_type n = 0; n < buckets.size(); ++n)
      if (buckets[n])
        return iterator(buckets[n], this);
    return end();
  }

  iterator end() { return iterator((node*)0, this); }

  const_iterator begin() const
  {
    for (size_type n = 0; n < buckets.size(); ++n)
      if (buckets[n])
        return const_iterator(buckets[n], this);
    return end();
  }

  const_iterator end() const { return const_iterator((node*)0, this); }

  //  friend IUEi_STL_INLINE bool operator== VCL_NULL_TMPL_ARGS (const
  //  self&,const self&);
  friend bool operator== VCL_NULL_TMPL_ARGS (const self&,const self&);
public:

  size_type bucket_count() const { return buckets.size(); }

  size_type max_bucket_count() const
    { return prime_list[num_primes - 1]; } 

  size_type elems_in_bucket(size_type bucket) const
  {
    size_type result = 0;
    for (node* cur = buckets[bucket]; cur; cur = cur->next)
      result += 1;
    return result;
  }

  std::pair<iterator, bool> insert_unique(const value_type& obj)
  {
    resize(num_elements + 1);
    return insert_unique_noresize(obj);
  }

  iterator insert_equal(const value_type& obj)
  {
    resize(num_elements + 1);
    return insert_equal_noresize(obj);
  }

  IUEi_STL_INLINE std::pair<iterator, bool> insert_unique_noresize(const value_type& obj);
  IUEi_STL_INLINE iterator insert_equal_noresize(const value_type& obj);
 
  void insert_unique(const value_type* f, const value_type* l)
  {
    size_type n = l - f;
    resize(num_elements + n);
    for ( ; n > 0; --n)
      insert_unique_noresize(*f++);
  }

  void insert_equal(const value_type* f, const value_type* l)
  {
    size_type n = l - f;
    resize(num_elements + n);
    for ( ; n > 0; --n)
      insert_equal_noresize(*f++);
  }

 void insert_unique(const_iterator f, const_iterator l)
  {
    size_type n = 0;
    std::distance(f, l, n);
    resize(num_elements + n);
    for ( ; n > 0; --n)
      insert_unique_noresize(*f++);
  }

  void insert_equal(const_iterator f, const_iterator l)
  {
    size_type n = 0;
    std::distance(f, l, n);
    resize(num_elements + n);
    for ( ; n > 0; --n)
      insert_equal_noresize(*f++);
  }

  IUEi_STL_INLINE reference find_or_insert(const value_type& obj);

  iterator find(const key_type& key) 
  {
    size_type n = bkt_num_key(key);
    node* first;
    for ( first = buckets[n];
          first && !equals(get_key(first->val), key);
          first = first->next)
      {}
    return iterator(first, this);
  } 

  const_iterator find(const key_type& key) const
  {
    size_type n = bkt_num_key(key);
    const node* first;
    for ( first = buckets[n];
          first && !equals(get_key(first->val), key);
          first = first->next)
      {}
    return const_iterator(first, this);
  } 

  size_type count(const key_type& key) const
  {
    const size_type n = bkt_num_key(key);
    size_type result = 0;

    for (const node* cur = buckets[n]; cur; cur = cur->next)
      if (equals(get_key(cur->val), key))
        ++result;
    return result;
  }

  IUEi_STL_INLINE std::pair<iterator, iterator> equal_range(const key_type& key);
  IUEi_STL_INLINE std::pair<const_iterator, const_iterator> equal_range(const key_type& key) const;

  IUEi_STL_INLINE size_type erase(const key_type& key);
  IUEi_STL_INLINE void erase(const iterator& it);
  IUEi_STL_INLINE void erase(iterator first, iterator last);

  IUEi_STL_INLINE void erase(const const_iterator& it);
  IUEi_STL_INLINE void erase(const_iterator first, const_iterator last);

  IUEi_STL_INLINE void resize(size_type num_elements_hint);
  void clear() { super::clear(); }
private:
    size_type next_size(size_type n) const { 
       return static_cast<size_type>( 
          next_prime( static_cast<unsigned long>(n) ) ); }

    void initialize_buckets(size_type n)
    {
        const size_type n_buckets = next_size(n);
        buckets.reserve(n_buckets);
        buckets.insert(buckets.end(), n_buckets, (node*) 0);
        num_elements = 0;
    }
    size_type bkt_num_key(const key_type& key) const{
        return bkt_num_key(key, buckets.size());
    }

    size_type bkt_num(const value_type& obj) const {
        return bkt_num_key(get_key(obj));
    }

    size_type bkt_num_key(const key_type& key, size_t n) const {
        return hashfun(key) % n;
    }

    size_type bkt_num(const value_type& obj, size_t n) const {
        return bkt_num_key(get_key(obj), n);
    }
    IUEi_STL_INLINE void erase_bucket(const size_type n, node* first, node* last);
    IUEi_STL_INLINE void erase_bucket(const size_type n, node* last);
};

// fbp: these defines are for outline methods definitions.
// needed to definitions to be portable. Should not be used in method bodies.

# if defined ( __STL_NESTED_TYPE_PARAM_BUG )
#  define __difference_type__ ptrdiff_t
#  define __size_type__       size_t
#  define __value_type__      Value
#  define __key_type__        Key
#  define __node__            hashtable_node<Value>
#  define __reference__       Value&
# else
#  define __difference_type__  hashtable<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>::difference_type
#  define __size_type__        hashtable<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>::size_type
#  define __value_type__       hashtable<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>::value_type
#  define __key_type__         hashtable<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>::key_type
#  define __node__             hashtable<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>::node
#  define __reference__        hashtable<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>::reference
# endif

template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
inline hashtable_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>&
hashtable_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>::operator++()
{
  const node* old = cur;
  cur = cur->next;
  if (!cur) {
    size_type bucket = ht->bkt_num(old->val);
    while (!cur && ++bucket < ht->buckets.size())
      cur = ht->buckets[bucket];
  }
  return *this;
}

template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
inline hashtable_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>
hashtable_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>::operator++(int)
{
  iterator tmp = *this;
  ++*this;
  return tmp;
}

template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
inline hashtable_const_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>&
hashtable_const_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>::operator++()
{
  const node* old = cur;
  cur = cur->next;
  if (!cur) {
    size_type bucket = ht->bkt_num(old->val);
    while (!cur && ++bucket < ht->buckets.size())
      cur = ht->buckets[bucket];
  }
  return *this;
}

template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
inline hashtable_const_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>
hashtable_const_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>::operator++(int)
{
  const_iterator tmp = *this;
  ++*this;
  return tmp;
}

template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
inline std::forward_iterator_tag
iterator_category (const hashtable_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>&)
{
  return std::forward_iterator_tag();
}

template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
inline Value* 
value_type(const hashtable_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>&)
{
  return (Value*) 0;
}

template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
inline ptrdiff_t*
distance_type(const hashtable_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>&)
{
  return (ptrdiff_t*) 0;
}

template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
inline std::forward_iterator_tag
iterator_category (const hashtable_const_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>&)
{
  return std::forward_iterator_tag();
}

template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
inline Value* 
value_type(const hashtable_const_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>&)
{
  return (Value*) 0;
}

template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
inline ptrdiff_t*
distance_type(const hashtable_const_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>&)
{
  return (ptrdiff_t*) 0;
}



template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
IUEi_STL_INLINE 
bool operator==(const hashtable<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>& ht1,
                const hashtable<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>& ht2)
{
  typedef typename hashtable<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>::node node;
  if (ht1.buckets.size() != ht2.buckets.size())
    return false;
  for (int n = 0; n < ht1.buckets.size(); ++n) {
    node* cur1 = ht1.buckets[n];
    node* cur2 = ht2.buckets[n];
    for ( ; cur1 && cur2 && cur1->val == cur2->val;
          cur1 = cur1->next, cur2 = cur2->next)
      {}
    if (cur1 || cur2)
      return false;
  }
  return true;
}  

template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
std::pair<hashtable_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>, bool> 
hashtable<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>::insert_unique_noresize(const __value_type__& obj)
{
  const size_type n = bkt_num(obj);
  node* first = buckets[n];

  for (node* cur = first; cur; cur = cur->next) 
    if (equals(get_key(cur->val), get_key(obj)))
      return std::pair<iterator, bool>(iterator(cur, this), false);

  node* tmp = new_node(obj);
  tmp->next = first;
  buckets[n] = tmp;
  ++num_elements;
  return std::pair<iterator, bool>(iterator(tmp, this), true);
}

template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
hashtable_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc> 
hashtable<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>::insert_equal_noresize(const __value_type__& obj)
{
  const size_type n = bkt_num(obj);
  node* first = buckets[n];

  for (node* cur = first; cur; cur = cur->next) 
    if (equals(get_key(cur->val), get_key(obj))) {
      node* tmp = new_node(obj);
      tmp->next = cur->next;
      cur->next = tmp;
      ++num_elements;
      return iterator(tmp, this);
    }

  node* tmp = new_node(obj);
  tmp->next = first;
  buckets[n] = tmp;
  ++num_elements;
  return iterator(tmp, this);
}

template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
__reference__ 
hashtable<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>::find_or_insert(const __value_type__& obj)
{
  resize(num_elements + 1);

  size_type n = bkt_num(obj);
  node* first = buckets[n];

  for (node* cur = first; cur; cur = cur->next)
    if (equals(get_key(cur->val), get_key(obj)))
      return cur->val;

  node* tmp = new_node(obj);
  tmp->next = first;
  buckets[n] = tmp;
  ++num_elements;
  return tmp->val;
}

template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
std::pair<hashtable_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>,
     hashtable_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc> > 
hashtable<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>::equal_range(const __key_type__& key)
{
  typedef std::pair<iterator, iterator> pii;
  const size_type n = bkt_num_key(key);

  for (node* first = buckets[n]; first; first = first->next) {
    if (equals(get_key(first->val), key)) {
      for (node* cur = first->next; cur; cur = cur->next)
        if (!equals(get_key(cur->val), key))
          return pii(iterator(first, this), iterator(cur, this));
      for (size_type m = n + 1; m < buckets.size(); ++m)
        if (buckets[m])
          return pii(iterator(first, this),
                     iterator(buckets[m], this));
      return pii(iterator(first, this), end());
    }
  }
  return pii(end(), end());
}

template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
std::pair<hashtable_const_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>, 
     hashtable_const_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc> > 
hashtable<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>::equal_range(const __key_type__& key) const
{
  typedef std::pair<const_iterator, const_iterator> pii;
  const size_type n = bkt_num_key(key);

  for (const node* first = buckets[n] ; first; first = first->next) {
    if (equals(get_key(first->val), key)) {
      for (const node* cur = first->next; cur; cur = cur->next)
        if (!equals(get_key(cur->val), key))
          return pii(const_iterator(first, this),
                     const_iterator(cur, this));
      for (size_type m = n + 1; m < buckets.size(); ++m)
        if (buckets[m])
          return pii(const_iterator(first, this),
                     const_iterator(buckets[m], this));
      return pii(const_iterator(first, this), end());
    }
  }
  return pii(end(), end());
}

template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
__size_type__ 
hashtable<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>::erase(const __key_type__& key)
{
  const size_type n = bkt_num_key(key);
  node* first = buckets[n];
  size_type erased = 0;

  if (first) {
    node* cur = first;
    node* next = cur->next;
    while (next) {
      if (equals(get_key(next->val), key)) {
        cur->next = next->next;
        delete_node(next);
        next = cur->next;
        ++erased;
      }
      else {
        cur = next;
        next = cur->next;
      }
    }
    if (equals(get_key(first->val), key)) {
      buckets[n] = first->next;
      delete_node(first);
      ++erased;
    }
  }
  num_elements -= erased;
  return erased;
}

template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
void 
hashtable<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>::erase(const hashtable_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>& it)
{
  node* const p = it.cur;
  if (p) {
    const size_type n = bkt_num(p->val);
    node* cur = buckets[n];

    if (cur == p) {
      buckets[n] = cur->next;
      delete_node(cur);
      --num_elements;
    }
    else {
      node* next = cur->next;
      while (next) {
        if (next == p) {
          cur->next = next->next;
          delete_node(next);
          --num_elements;
          break;
        }
        else {
          cur = next;
          next = cur->next;
        }
      }
    }
  }
}

template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
void 
hashtable<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>::erase(hashtable_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc> first, 
                                        hashtable_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc> last)
{
  size_type f_bucket = first.cur ? bkt_num(first.cur->val) : buckets.size();
  size_type l_bucket = last.cur ? bkt_num(last.cur->val) : buckets.size();
  if (first.cur == last.cur)
    return;
  else if (f_bucket == l_bucket)
    erase_bucket(f_bucket, first.cur, last.cur);
  else {
    erase_bucket(f_bucket, first.cur, 0);
    for (size_type n = f_bucket + 1; n < l_bucket; ++n)
      erase_bucket(n, 0);
    if (l_bucket != buckets.size())
      erase_bucket(l_bucket, last.cur);
  }
}

template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
inline void
hashtable<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>::erase(hashtable_const_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc> first, 
                                        hashtable_const_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc> last)
{
  erase(iterator(const_cast<node*>(first.cur),
                 const_cast<self*>(first.ht)),
        iterator(const_cast<node*>(last.cur),
                 const_cast<self*>(last.ht)));
}

template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
inline void
hashtable<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>::erase(const hashtable_const_iterator<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>& it)
{
  erase(iterator(const_cast<node*>(it.cur),
                 const_cast<self*>(it.ht)));
}

template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
void 
hashtable<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>::resize(__size_type__ num_elements_hint)
{
    const size_type old_n = buckets.size();
    if (num_elements_hint > old_n) {
        const size_type n = next_size(num_elements_hint);
        if (n > old_n) {
      hashtable<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>::buckets_type tmp(n, (node*)0);
            for (size_type bucket = 0; bucket < old_n; ++bucket) {
                node* first = buckets[bucket];
                while (first) {
                    size_type new_bucket = bkt_num(first->val, n);
                    buckets[bucket] = first->next;
                    first->next = tmp[new_bucket];
                    tmp[new_bucket] = first;
                    first = buckets[bucket];          
                }
            }
            buckets.clear();
            buckets.swap(tmp);
        }
    }
}


template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
void 
hashtable<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>::erase_bucket(const size_t n, 
                                             hashtable_node<Value>* first, 
                                             hashtable_node<Value>* last)
{
  node* cur = buckets[n];
  if (cur == first)
    erase_bucket(n, last);
  else {
    node* next;
    for (next = cur->next; next != first; cur = next, next = cur->next)
      ;
    while (next) {
      cur->next = next->next;
      delete_node(next);
      next = cur->next;
      --num_elements;
    }
  }
}

template <class Value, class Key, class HashFcn, class ExtractKey, class EqualKey, class Alloc>
void 
hashtable<Value, Key, HashFcn, ExtractKey, EqualKey, Alloc>::erase_bucket(const size_t n,
                                                            hashtable_node<Value>* last)
{
  node* cur = buckets[n];
  while (cur != last) {
    node* next = cur->next;
    delete_node(cur);
    cur = next;
    buckets[n] = cur;
    --num_elements;
  }
}

template <class Value, class Alloc>
void hashtable_base<Value, Alloc>::clear()
{
  for (size_type i = 0; i < buckets.size(); ++i) {
    node* cur = buckets[i];
    while (cur != 0) {
      node* next = cur->next;
      delete_node(cur);
      cur = next;
    }
    buckets[i] = 0;
  }
  num_elements = 0;
}
  
  
template <class Value, class Alloc>
void hashtable_base<Value, Alloc>::copy_from(const hashtable_base<Value, Alloc>& ht)
{
  buckets.reserve(ht.buckets.size());
  buckets.insert(buckets.end(), ht.buckets.size(), (node*) 0);
  for (size_type i = 0; i < ht.buckets.size(); ++i) {
    const node* cur = ht.buckets[i];
    if (cur) {
      node* copy = new_node(cur->val);
      buckets[i] = copy;
      ++num_elements;
      
      for (node* next = cur->next; next; cur = next, next = cur->next) {
        copy->next = new_node(next->val);
        ++num_elements;
        copy = copy->next;
      }
    }
  }
}

}// end namespace itk


# undef __difference_type__ 
# undef __size_type__       
# undef __value_type__      
# undef __key_type__        
# undef __node__            

// the following is added for itk compatability:

// --

// A few compatability fixes.  Placed here for automatic include in
// both the hash_set and the hash_map sources.
# if defined(VCL_SUNPRO_CC) || defined (_MSC_VER) || defined(__BORLANDC__) || (defined(__ICC) && defined(linux))
namespace std 
{
template <class T>
struct identity : public std::unary_function<T, T> {
public:
  const T& operator()(const T& x) const { return x; }
};
}

template <class _Pair>
struct itk_Select1st : public std::unary_function<_Pair, typename _Pair::first_type> {
  typename _Pair::first_type const & operator()(_Pair const & __x) const {
    return __x.first;
  }
};
 
template <class _Pair>
struct itk_Select2nd : public std::unary_function<_Pair, typename _Pair::second_type> {
  typename _Pair::second_type const & operator()(_Pair const & __x) const {
    return __x.second;
  }
};

// Add select* to std.
namespace std {
  template <class _Pair>
  struct select1st : public itk_Select1st<_Pair> { };
  template <class _Pair> struct select2nd : public itk_Select2nd<_Pair> { };
};

#endif

#endif

#endif // itk_emulation_hashtable_h
