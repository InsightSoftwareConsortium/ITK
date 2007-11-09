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


#ifndef vcl_emulation_stack_h
#define vcl_emulation_stack_h

#include <vcl_compiler.h> // for configuration macros

#include "vcl_functional.h"
#include "vcl_heap.h"
#include "vcl_vector.h"
#include "vcl_deque.h"

template <class T, VCL_DFL_TMPL_PARAM_STLDECL(Sequence,vcl_vector<T>) >
class vcl_stack
{
  bool operator==(const vcl_stack<T, Sequence>& y) const {return c == y.c;}
  bool operator<(const vcl_stack<T, Sequence>& y) const {return c < y.c;}
 public:
  typedef typename Sequence::value_type value_type;
  typedef typename Sequence::size_type size_type;
 protected:
  Sequence c;
 public:
  bool empty() const { return c.empty(); }
  size_type size() const { return c.size(); }
  value_type& top() { return c.back(); }
  const value_type& top() const { return c.back(); }
  void push(const value_type& x) { c.push_back(x); }
  void pop() { c.pop_back(); }
};

template <class T, VCL_DFL_TMPL_PARAM_STLDECL(Sequence,vcl_deque<T>) >
class vcl_queue
{
  bool operator==(const vcl_queue<T, Sequence>& y) const {return c == y.c;}
  bool operator<(const vcl_queue<T, Sequence>& y) const {return c < y.c;}
 public:
  typedef typename Sequence::value_type value_type;
  typedef typename Sequence::size_type size_type;
 protected:
  Sequence c;
 public:
  bool empty() const { return c.empty(); }
  size_type size() const { return c.size(); }
  value_type& front() { return c.front(); }
  const value_type& front() const { return c.front(); }
  value_type& back() { return c.back(); }
  const value_type& back() const { return c.back(); }
  void push(const value_type& x) { c.push_back(x); }
  void pop() { c.pop_front(); }
};

template <class T, VCL_DFL_TMPL_PARAM_STLDECL(Sequence,vcl_vector<T>),
          VCL_DFL_TMPL_PARAM_STLDECL(Compare,vcl_less<typename Sequence::value_type>) >
class  vcl_priority_queue
{
 public:
  typedef typename Sequence::value_type value_type;
  typedef typename Sequence::size_type size_type;
 protected:
  Sequence c;
  Compare comp;
 public:
  vcl_priority_queue() :  c(), comp(Compare()) {}
  explicit vcl_priority_queue(const Compare& x) :  c(), comp(x) {}
  vcl_priority_queue(const value_type* first, const value_type* last,
                     const Compare& x = Compare()) : c(first, last), comp(x)
  {
      vcl_make_heap(c.begin(), c.end(), comp);
  }
  bool empty() const { return c.empty(); }
  size_type size() const { return c.size(); }
  const value_type& top() const { return c.front(); }
  void push(const value_type& x) { c.push_back(x); vcl_push_heap(c.begin(), c.end(), comp); }
  void pop() { vcl_pop_heap(c.begin(), c.end(), comp); c.pop_back(); }
};

// no equality is provided

#if defined  (__STL_DEFAULT_TEMPLATE_PARAM)
#  define vcl_simple_stack vcl_stack
#  define vcl_simple_queue vcl_queue
#  define vcl_simple_priority_queue vcl_priority_queue
# else
// provide ways to access short functionality

// provide a "default" stack adaptor

template <class T>
class vcl_simple_stack : public vcl_stack<T, vcl_vector<T> > { };

// provide a "default" queue adaptor
template <class T>
class vcl_simple_queue : public vcl_queue<T, vcl_deque<T> > { };

// provide a "simple" priority queue adaptor
template <class T>
class vcl_simple_priority_queue : public vcl_priority_queue<T, vcl_deque<T> , vcl_less<T> >
{
 public:
  typedef vcl_priority_queue<T, vcl_deque<T> , vcl_less<T> > super;
  __CONTAINER_SUPER_TYPEDEFS
  vcl_simple_priority_queue() : super() {}
  vcl_simple_priority_queue(const typename super::value_type* first, typename const super::value_type* last) :
      super(first,last){}
};

#endif /* __STL_DEFAULT_TEMPLATE_PARAM */

#endif // vcl_emulation_stack_h
