// This is vcl/emulation/vcl_tree.h
#ifndef vcl_emulation_tree_h
#define vcl_emulation_tree_h
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif

/*
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

/*

Red-black vcl_tree class, designed for use in implementing STL
associative containers (vcl_set, vcl_multiset, vcl_map, and vcl_multimap). The
insertion and deletion algorithms are based on those in Cormen,
Leiserson, and Rivest, Introduction to Algorithms (MIT Press, 1990),
except that

(1) the header cell is maintained with links not only to the root
but also to the leftmost node of the vcl_tree, to enable constant time
begin(), and to the rightmost node of the vcl_tree, to enable linear time
performance when used with the generic vcl_set algorithms (set_union,
etc.);

(2) when a node being deleted has two children its successor node is
relinked into its place, rather than copied, so that the only
iterators invalidated are those referring to the deleted node.

*/

#include <vcl_cstddef.h>
#include "vcl_algobase.h"
#include "vcl_iterator.h"
#include "vcl_alloc.h"

# if defined ( __STL_USE_ABBREVS )
// ugliness is intentional - to reduce conflicts possibility
#  define __rb_tree_node_base       rbTNB
#  define __rb_tree_node            rbTN
#  define __rb_tree_base_iterator   rbTBIt
#  define __rb_tree_iterator        rbTIt
#  define __rb_tree_const_iterator  rbTcIt
#  define __rb_tree_base            rbTB
# endif

typedef bool __rb_tree_color_type;
const __rb_tree_color_type __rb_tree_red = false;
const __rb_tree_color_type __rb_tree_black = true;

struct __rb_tree_node_base
{
  typedef __rb_tree_color_type color_type;
  typedef __rb_tree_node_base* base_ptr;

  color_type color;
  base_ptr parent;
  base_ptr left;
  base_ptr right;

  static base_ptr minimum(base_ptr x)
  {
    while (x->left != 0) x = x->left;
    return x;
  }

  static base_ptr maximum(base_ptr x)
  {
    while (x->right != 0) x = x->right;
    return x;
  }
};

template <class Value>
struct __rb_tree_node : public __rb_tree_node_base
{
  Value value_field;
};


struct __rb_tree_base_iterator
{
  typedef __rb_tree_node_base::base_ptr base_ptr;
  typedef vcl_ptrdiff_t distance_type;
  base_ptr node;
  void increment()
  {
    __stl_verbose_assert(valid(), __STL_MSG_INVALID_ITERATOR);
    __stl_verbose_assert(node!=owner(), __STL_MSG_INVALID_ADVANCE);
    if (node->right != 0) {
      node = node->right;
      while (node->left != 0)
        node = node->left;
    }
    else {
      base_ptr y = node->parent;
      while (node == y->right) {
        node = y;
        y = y->parent;
      }
      if (node->right != y)
        node = y;
    }
  }

  void decrement()
  {
    __stl_verbose_assert(valid(), __STL_MSG_INVALID_ITERATOR);
    __stl_verbose_assert(node!=owner()->left, __STL_MSG_INVALID_ADVANCE);
    if (node->color == __rb_tree_red &&
        node->parent->parent == node)
      node = node->right;
    else if (node->left != 0) {
      base_ptr y = node->left;
      while (y->right != 0)
        y = y->right;
      node = y;
    }
    else {
      base_ptr y = node->parent;
      while (node == y->left) {
        node = y;
        y = y->parent;
      }
      node = y;
    }
  }
};

template <class Value>
struct __rb_tree_iterator : public __rb_tree_base_iterator
{
  typedef Value& reference;
  typedef Value* pointer;
  typedef const Value& const_reference;
  typedef __rb_tree_node<Value>* link_type;
 private:
  typedef __rb_tree_iterator<Value> self;
 public:
  __rb_tree_iterator() {}
  __rb_tree_iterator(link_type x) { node = x; }
  reference operator*() const {
      __stl_verbose_assert(node!=owner(), __STL_MSG_NOT_DEREFERENCEABLE);
      return link_type(node)->value_field;
  }

//  pointer operator->() const { return &(operator*()); }
// This is not const correct and gives warnings and
// compile errors on the PC  WAH

  self& operator++() { increment(); return *this; }
  self operator++(int) {
    self tmp = *this;
    increment();
    return tmp;
  }

  self& operator--() { decrement(); return *this; }
  self operator--(int) {
    self tmp = *this;
    decrement();
    return tmp;
  }
};

template <class Value>
struct __rb_tree_const_iterator : public __rb_tree_base_iterator
{
  typedef Value& reference;
  typedef const Value& const_reference;
  typedef Value* pointer;
  typedef Value const* const_pointer;
  typedef __rb_tree_node<Value>* link_type;
  typedef __rb_tree_const_iterator<Value> self;
 public:
  __rb_tree_const_iterator() {}
  __rb_tree_const_iterator(link_type x) { node = x; }
  __rb_tree_const_iterator(const __rb_tree_iterator<Value>& it) { node = it.node; }

  const_reference operator*() const {
      __stl_verbose_assert(node!=owner(), __STL_MSG_NOT_DEREFERENCEABLE);
      return link_type(node)->value_field;
  }

//  pointer operator->() const { return &(operator*()); }
// This is not const correct and gives warnings and
// compile errors on the PC  WAH

  self& operator++() { increment(); return *this; }
  self operator++(int) {
    self tmp = *this;
    increment();
    return tmp;
  }

  self& operator--() { decrement(); return *this; }
  self operator--(int) {
    self tmp = *this;
    decrement();
    return tmp;
  }
};


inline bool operator==(const __rb_tree_base_iterator& x,
                       const __rb_tree_base_iterator& y) {
  __stl_debug_check(__check_same_owner(x,y));
  return x.node == y.node;
}

inline bool operator!=(const __rb_tree_base_iterator& x,
                       const __rb_tree_base_iterator& y) {
  return x.node != y.node;
}

inline vcl_bidirectional_iterator_tag
iterator_category(const __rb_tree_base_iterator&) {
  return vcl_bidirectional_iterator_tag();
}

inline __rb_tree_base_iterator::distance_type*
distance_type(const __rb_tree_base_iterator&) {
  return (__rb_tree_base_iterator::distance_type*) 0;
}

template <class Value>
inline Value* value_type(const __rb_tree_iterator<Value>&) {
  return (Value*) 0;
}

template <class Value>
inline Value* value_type(const __rb_tree_const_iterator<Value>&) {
  return (Value*) 0;
}

inline void
__rb_tree_rotate_left(__rb_tree_node_base* x, __rb_tree_node_base*& root)
{
  __rb_tree_node_base* y = x->right;
  x->right = y->left;
  if (y->left != 0)
    y->left->parent = x;
  y->parent = x->parent;

  if (x == root)
    root = y;
  else if (x == x->parent->left)
    x->parent->left = y;
  else
    x->parent->right = y;
  y->left = x;
  x->parent = y;
}

inline void
__rb_tree_rotate_right(__rb_tree_node_base* x, __rb_tree_node_base*& root)
{
  __rb_tree_node_base* y = x->left;
  x->left = y->right;
  if (y->right != 0)
    y->right->parent = x;
  y->parent = x->parent;

  if (x == root)
    root = y;
  else if (x == x->parent->right)
    x->parent->right = y;
  else
    x->parent->left = y;
  y->right = x;
  x->parent = y;
}

inline void
__rb_tree_rebalance(__rb_tree_node_base* x, __rb_tree_node_base*& root)
{
  x->color = __rb_tree_red;
  while (x != root && x->parent->color == __rb_tree_red) {
    if (x->parent == x->parent->parent->left) {
      __rb_tree_node_base* y = x->parent->parent->right;
      if (y && y->color == __rb_tree_red) {
        x->parent->color = __rb_tree_black;
        y->color = __rb_tree_black;
        x->parent->parent->color = __rb_tree_red;
        x = x->parent->parent;
      }
      else {
        if (x == x->parent->right) {
          x = x->parent;
          __rb_tree_rotate_left(x, root);
        }
        x->parent->color = __rb_tree_black;
        x->parent->parent->color = __rb_tree_red;
        __rb_tree_rotate_right(x->parent->parent, root);
      }
    }
    else {
      __rb_tree_node_base* y = x->parent->parent->left;
      if (y && y->color == __rb_tree_red) {
        x->parent->color = __rb_tree_black;
        y->color = __rb_tree_black;
        x->parent->parent->color = __rb_tree_red;
        x = x->parent->parent;
      }
      else {
        if (x == x->parent->left) {
          x = x->parent;
          __rb_tree_rotate_right(x, root);
        }
        x->parent->color = __rb_tree_black;
        x->parent->parent->color = __rb_tree_red;
        __rb_tree_rotate_left(x->parent->parent, root);
      }
    }
  }
  root->color = __rb_tree_black;
}

inline __rb_tree_node_base*
__rb_tree_rebalance_for_erase(__rb_tree_node_base* z,
                              __rb_tree_node_base*& root,
                              __rb_tree_node_base*& leftmost,
                              __rb_tree_node_base*& rightmost)
{
  __rb_tree_node_base* y = z;
  __rb_tree_node_base* x = 0;
  __rb_tree_node_base* x_parent = 0;
  if (y->left == 0)             // z has at most one non-null child. y == z.
    x = y->right;               // x might be null.
  else
    if (y->right == 0)          // z has exactly one non-null child.  y == z.
      x = y->left;              // x is not null.
    else {                      // z has two non-null children.  Set y to
      y = y->right;             //   z's successor.  x might be null.
      while (y->left != 0)
        y = y->left;
      x = y->right;
    }
  if (y != z) {                 // relink y in place of z.  y is z's successor
    z->left->parent = y;
    y->left = z->left;
    if (y != z->right) {
      x_parent = y->parent;
      if (x) x->parent = y->parent;
      y->parent->left = x;      // y must be a left child
      y->right = z->right;
      z->right->parent = y;
    }
    else
      x_parent = y;
    if (root == z)
      root = y;
    else if (z->parent->left == z)
      z->parent->left = y;
    else
      z->parent->right = y;
    y->parent = z->parent;
    vcl_swap(y->color, z->color);
    y = z;
    // y now points to node to be actually deleted
  }
  else {                        // y == z
    x_parent = y->parent;
    if (x) x->parent = y->parent;
    if (root == z)
      root = x;
    else
      if (z->parent->left == z)
        z->parent->left = x;
      else
        z->parent->right = x;
    if (leftmost == z)
      if (z->right == 0)        // z->left must be null also
        leftmost = z->parent;
    // makes leftmost == header if z == root
      else
        leftmost = __rb_tree_node_base::minimum(x);
    if (rightmost == z)
      if (z->left == 0)         // z->right must be null also
        rightmost = z->parent;
    // makes rightmost == header if z == root
      else                      // x == z->left
        rightmost = __rb_tree_node_base::maximum(x);
  }
  if (y->color != __rb_tree_red) {
    while (x != root && (x == 0 || x->color == __rb_tree_black))
      if (x == x_parent->left) {
        __rb_tree_node_base* w = x_parent->right;
        if (w->color == __rb_tree_red) {
          w->color = __rb_tree_black;
          x_parent->color = __rb_tree_red;
          __rb_tree_rotate_left(x_parent, root);
          w = x_parent->right;
        }
        if ((w->left == 0 || w->left->color == __rb_tree_black) &&
            (w->right == 0 || w->right->color == __rb_tree_black)) {
          w->color = __rb_tree_red;
          x = x_parent;
          x_parent = x_parent->parent;
        } else {
          if (w->right == 0 || w->right->color == __rb_tree_black) {
            if (w->left) w->left->color = __rb_tree_black;
            w->color = __rb_tree_red;
            __rb_tree_rotate_right(w, root);
            w = x_parent->right;
          }
          w->color = x_parent->color;
          x_parent->color = __rb_tree_black;
          if (w->right) w->right->color = __rb_tree_black;
          __rb_tree_rotate_left(x_parent, root);
          break;
        }
      } else {                  // same as above, with right <-> left.
        __rb_tree_node_base* w = x_parent->left;
        if (w->color == __rb_tree_red) {
          w->color = __rb_tree_black;
          x_parent->color = __rb_tree_red;
          __rb_tree_rotate_right(x_parent, root);
          w = x_parent->left;
        }
        if ((w->right == 0 || w->right->color == __rb_tree_black) &&
            (w->left == 0 || w->left->color == __rb_tree_black)) {
          w->color = __rb_tree_red;
          x = x_parent;
          x_parent = x_parent->parent;
        } else {
          if (w->left == 0 || w->left->color == __rb_tree_black) {
            if (w->right) w->right->color = __rb_tree_black;
            w->color = __rb_tree_red;
            __rb_tree_rotate_left(w, root);
            w = x_parent->left;
          }
          w->color = x_parent->color;
          x_parent->color = __rb_tree_black;
          if (w->left) w->left->color = __rb_tree_black;
          __rb_tree_rotate_right(x_parent, root);
          break;
        }
      }
    if (x) x->color = __rb_tree_black;
  }
  return y;
}

template <class Value, class Alloc>
class __rb_tree_base
{
    typedef __rb_tree_base<Value,Alloc> self;
 public:
    typedef Value value_type;
    typedef value_type* pointer;
    typedef const value_type* const_pointer;
    typedef value_type& reference;
    typedef const value_type& const_reference;
    typedef vcl_size_t size_type;
    typedef vcl_ptrdiff_t difference_type;
 protected:
    typedef __rb_tree_node_base* base_ptr;
    typedef __rb_tree_node<Value> rb_tree_node;
    typedef __rb_tree_color_type color_type;
    typedef rb_tree_node* link_type;
    typedef vcl_simple_alloc<rb_tree_node, Alloc> rb_tree_node_allocator;
    static link_type get_node() { return rb_tree_node_allocator::allocate(); }
    static void put_node(link_type p) { rb_tree_node_allocator::deallocate(p); }
 protected:
    link_type header;

    link_type& root() const { return (link_type&) header->parent; }
    link_type& leftmost() const { return (link_type&) header->left; }
    link_type& rightmost() const { return (link_type&) header->right; }
    size_type node_count; // keeps track of size of vcl_tree

    static link_type& left(link_type x) { return (link_type&)(x->left); }
    static link_type& right(link_type x) { return (link_type&)(x->right); }
    static link_type& parent(link_type x) { return (link_type&)(x->parent); }
    static reference value(link_type x) { return x->value_field; }
    static color_type& color(link_type x) { return (color_type&)(x->color); }

    static link_type& left(base_ptr x) { return (link_type&)(x->left); }
    static link_type& right(base_ptr x) { return (link_type&)(x->right); }
    static link_type& parent(base_ptr x) { return (link_type&)(x->parent); }
    static reference value(base_ptr x) { return ((link_type)x)->value_field; }
    static color_type& color(base_ptr x) { return (color_type&)(link_type(x)->color); }

    static link_type minimum(link_type x) {
        return (link_type)  __rb_tree_node_base::minimum(x);
    }
    static link_type maximum(link_type x) {
        return (link_type) __rb_tree_node_base::maximum(x);
    }

 public:
    __rb_tree_base() : header( get_node() ), node_count(0) {
        color(header) = __rb_tree_red; // used to distinguish header from
        // root, in iterator.operator++
        __stl_debug_do(iter_list.safe_init(header));
    }
    ~__rb_tree_base() {
        put_node(header);
        __stl_debug_do(iter_list.invalidate());
    }

 public:
    bool empty() const { return node_count == 0; }
    size_type size() const { return node_count; }
    size_type max_size() const { return size_type(-1); }

 protected:
    static link_type __new_node(const value_type& v) {
        link_type z = get_node();
        IUEg__TRY {
        vcl_construct(&(value(z)), v);
        left(z) = 0;
        right(z) = 0;
        }
#  if defined (__STL_USE_EXCEPTIONS)
        catch(...) {
            put_node(z);
            throw;
        }
#  endif
        return z;
    }
    static inline link_type __copy_aux(link_type x, link_type p);
    static inline void __erase(link_type x);
    static inline link_type __copy(link_type x, link_type p);

 public:
    void clear() {
      if (node_count != 0) {
        __erase(root());
        leftmost() = header;
        root() = 0;
        rightmost() = header;
        node_count = 0;
        __stl_debug_do(invalidate_all());
      }
    }
};

template <class Key, class Value, class KeyOfValue, class Compare, VCL_DFL_TYPE_PARAM_STLDECL(Alloc,vcl_alloc) >
class rb_tree : public __rb_tree_base<Value,Alloc>
{
  typedef __rb_tree_base<Value,Alloc> super;
  typedef rb_tree<Key,Value,KeyOfValue,Compare,Alloc> self;
 public:
  __IMPORT_CONTAINER_TYPEDEFS(super)
  typedef __rb_tree_node_base* base_ptr;
  typedef __rb_tree_node<Value> rb_tree_node;
  typedef __rb_tree_color_type color_type;
  typedef rb_tree_node* link_type;
  typedef __rb_tree_iterator<value_type> iterator;
  typedef __rb_tree_const_iterator<value_type> const_iterator;
  typedef vcl_reverse_bidirectional_iterator<iterator, value_type, reference,
                                             difference_type> reverse_iterator;
  typedef vcl_reverse_bidirectional_iterator<const_iterator, value_type,
                                             const_reference, difference_type>
      const_reverse_iterator;
  typedef Key key_type;
 protected:
  Compare key_compare;
  static const Key& key(link_type x) { return KeyOfValue()(value(x)); }
  static const Key& key(base_ptr x) { return KeyOfValue()(value(link_type(x)));}
 private:
  inline iterator __insert(base_ptr x, base_ptr y, const value_type& v);
  void init() {
      root() = 0;
      leftmost() = header;
      rightmost() = header;
  }
 public:
  // allocation/deallocation
  rb_tree(): key_compare(Compare())  { init(); }
  rb_tree(const Compare& comp): key_compare(comp)  { init(); }
  rb_tree(const self& x)
    : key_compare(x.key_compare)  {
      root() = __copy(x.root(), header);
      if (root() == 0) {
          leftmost() = header;
          rightmost() = header;
      } else {
          leftmost() = minimum(root());
          rightmost() = maximum(root());
      }
      node_count = x.node_count;
  }

  ~rb_tree() { clear();}
  inline self& operator=(const self& x);

 public:
  // accessors:
  iterator make_iterator(link_type l) { return iterator(l); }
  const_iterator make_const_iterator(link_type l) const { return const_iterator(l); }
  iterator begin() { return leftmost(); }
  const_iterator begin() const { return leftmost(); }
  iterator end() { return header; }
  const_iterator end() const { return header; }
  reverse_iterator rbegin() { return reverse_iterator(end()); }
  const_reverse_iterator rbegin() const { return const_reverse_iterator(end()); }
  reverse_iterator rend() { return reverse_iterator(begin()); }
  const_reverse_iterator rend() const { return const_reverse_iterator(begin()); }
  Compare key_comp() const { return key_compare; }

  void swap(self& t) {
      __stl_debug_do(iter_list.swap_owners(t.iter_list));
      vcl_swap(header, t.header);
      vcl_swap(node_count, t.node_count);
      vcl_swap(key_compare, t.key_compare);
  }

 public:
  // insert/erase
  vcl_pair<iterator,bool> insert_unique(const value_type& v){
    link_type y = header;
    link_type x = root();
    bool comp = true;
    while (x != 0) {
        y = x;
        comp = key_compare(KeyOfValue()(v), key(x));
        x = comp ? left(x) : right(x);
    }
    iterator j = make_iterator(y);
    if (comp)
        if (j == begin())
            return vcl_pair<iterator,bool>(__insert(x, y, v), true);
        else
            --j;
    if (key_compare(key(j.node), KeyOfValue()(v)))
        return vcl_pair<iterator,bool>(__insert(x, y, v), true);
    return vcl_pair<iterator,bool>(j, false);
  }

  iterator insert_equal(const value_type& v){
    link_type y = header;
    link_type x = root();
    while (x != 0) {
        y = x;
        x = key_compare(KeyOfValue()(v), key(x)) ? left(x) : right(x);
    }
    return __insert(x, y, v);
  }

  iterator insert_unique(iterator position, const value_type& v){
    __stl_debug_check(__check_if_owner(header,position));
    if (position.node == header->left) // begin()
        if (size() > 0 && key_compare(KeyOfValue()(v), key(position.node)))
            return __insert(position.node, position.node, v);
            // first argument just needs to be non-null
        else
            return insert_unique(v).first;
    else if (position.node == header) // end()
        if (key_compare(key(rightmost()), KeyOfValue()(v)))
            return __insert(0, rightmost(), v);
        else
            return insert_unique(v).first;
    else {
        iterator before = position;
        --before;
        if (key_compare(key(before.node), KeyOfValue()(v))
            && key_compare(KeyOfValue()(v), key(position.node)))
            if (right(before.node) == 0)
                return __insert(0, before.node, v);
            else
                return __insert(position.node, position.node, v);
                // first argument just needs to be non-null
        else
            return insert_unique(v).first;
    }
  }

  iterator insert_equal(iterator position, const value_type& v){
    __stl_debug_check(__check_if_owner(header,position));
    if (position.node == header->left) // begin()
        if (size() > 0 && key_compare(KeyOfValue()(v), key(position.node)))
            return __insert(position.node, position.node, v);
            // first argument just needs to be non-null
        else
            return insert_equal(v);
    else if (position.node == header) // end()
        if (!key_compare(KeyOfValue()(v), key(rightmost())))
            return __insert(0, rightmost(), v);
        else
            return insert_equal(v);
    else {
        iterator before = position;
        --before;
        if (!key_compare(KeyOfValue()(v), key(before.node))
            && !key_compare(key(position.node), KeyOfValue()(v)))
            if (right(before.node) == 0)
                return __insert(0, before.node, v);
            else
                return __insert(position.node, position.node, v);
                // first argument just needs to be non-null
        else
            return insert_equal(v);
    }
  }

  void insert_unique(const_iterator    first, const_iterator    last){while (first != last) insert_unique(*first++);}
  void insert_unique(const value_type* first, const value_type* last){while (first != last) insert_unique(*first++);}
  void insert_equal(const_iterator first, const_iterator last) {
    while (first != last) insert_equal(*first++);
  }

  void insert_equal(const value_type* first, const value_type* last) {
    while (first != last) insert_equal(*first++);
  }

  inline void erase(iterator position);
  inline void erase(iterator first, iterator last);
  inline size_type erase(const key_type& x);
  inline void erase(const key_type* first, const key_type* last);

 public:
                                // vcl_set operations:
  inline iterator find(const key_type& x);
  inline const_iterator find(const key_type& x) const;
  inline size_type count(const key_type& x) const;
  inline iterator lower_bound(const key_type& x);
  inline const_iterator lower_bound(const key_type& x) const;
  inline iterator upper_bound(const key_type& x);
  inline const_iterator upper_bound(const key_type& x) const;
  inline vcl_pair<iterator,iterator> equal_range(const key_type& x);
  inline vcl_pair<const_iterator, const_iterator> equal_range(const key_type& x) const;
 public:
                                // Debugging.
  inline bool __rb_verify() const;
};

// fbp: these defines are for outline methods definitions.
// needed for definitions to be portable. Should not be used in method bodies.
# if defined  ( __STL_NESTED_TYPE_PARAM_BUG )
#  define __iterator__        __rb_tree_iterator<Value>
#  define __const_iterator__  __rb_tree_const_iterator<Value>
#  define __size_type__       vcl_size_t
#  define __link_type__       __rb_tree_node<Value>*
#  define __base_ptr__        __rb_tree_node_base*
#  define __value_type__      Value
#  define __key_type__        Key
# else
#  define __iterator__        rb_tree<Key, Value, KeyOfValue, Compare, Alloc>::iterator
#  define __const_iterator__  rb_tree<Key, Value, KeyOfValue, Compare, Alloc>::const_iterator
#  define __link_type__       rb_tree<Key, Value, KeyOfValue, Compare, Alloc>::link_type
#  define __size_type__       rb_tree<Key, Value, KeyOfValue, Compare, Alloc>::size_type
#  define __base_ptr__        rb_tree<Key, Value, KeyOfValue, Compare, Alloc>::base_ptr
#  define __value_type__      rb_tree<Key, Value, KeyOfValue, Compare, Alloc>::value_type
#  define __key_type__        rb_tree<Key, Value, KeyOfValue, Compare, Alloc>::key_type
# endif

template <class Key, class Value, class KeyOfValue, class Compare, class Alloc>
inline bool operator==(const rb_tree<Key, Value, KeyOfValue, Compare, Alloc>& x,
                       const rb_tree<Key, Value, KeyOfValue, Compare, Alloc>& y) {
    return x.size() == y.size() && vcl_equal(x.begin(), x.end(), y.begin());
}

template <class Key, class Value, class KeyOfValue, class Compare, class Alloc>
inline bool operator<(const rb_tree<Key, Value, KeyOfValue, Compare, Alloc>& x,
                      const rb_tree<Key, Value, KeyOfValue, Compare, Alloc>& y) {
    return lexicographical_compare(x.begin(), x.end(), y.begin(), y.end());
}

template <class Key, class Value, class KeyOfValue, class Compare, class Alloc>
rb_tree<Key, Value, KeyOfValue, Compare, Alloc>&
rb_tree<Key, Value, KeyOfValue, Compare, Alloc>::operator=
(const rb_tree<Key, Value, KeyOfValue, Compare, Alloc>& x) {
    if (this != &x) {
        // can't be done as in vcl_list because Key may be a constant type
        clear();
        root() = __copy(x.root(), header);
        if (root() == 0) {
            leftmost() = header;
            rightmost() = header;
        } else {
            leftmost() = minimum(root());
            rightmost() = maximum(root());
        }
        node_count = x.node_count;
        key_compare = x.key_compare;
    }
    return *this;
}


template <class Key, class Value, class KeyOfValue, class Compare, class Alloc>
typename __iterator__
rb_tree<Key, Value, KeyOfValue, Compare, Alloc>::__insert(__base_ptr__ x_,
                                                          __base_ptr__ y_,
                                                          const __value_type__& v) {
    link_type x = (link_type) x_;
    link_type y = (link_type) y_;
    // determine link before allocating new node
    bool link_to_left = y == header || x != 0 || key_compare(KeyOfValue()(v), key(y));
    link_type z = __new_node(v);
    if (link_to_left) {
        left(y) = z;  // also makes leftmost() = z when y == header
        if (y == header) {
            root() = z;
            rightmost() = z;
        } else if (y == leftmost())
            leftmost() = z;   // maintain leftmost() pointing to minimum node
    } else {
        right(y) = z;
        if (y == rightmost())
            rightmost() = z;   // maintain rightmost() pointing to maximum node
    }
    parent(z) = y;
    left(z) = 0;
    right(z) = 0;
    __rb_tree_rebalance(z, header->parent);
    ++node_count;
    return make_iterator(z);
}


template <class Key, class Value, class KeyOfValue, class Compare, class Alloc>
inline void
rb_tree<Key, Value, KeyOfValue, Compare, Alloc>::erase(__iterator__
                                                       position) {
    __stl_debug_check(__check_if_owner(header,position));
    __stl_verbose_assert(position.node!=header, __STL_MSG_ERASE_PAST_THE_END);
    __stl_debug_do(invalidate_iterator(position));
  link_type y = (link_type) __rb_tree_rebalance_for_erase(position.node,
                                                          header->parent,
                                                          header->left,
                                                          header->right);
  vcl_destroy(&(value(y)));
  put_node(y);
  --node_count;
}

template <class Key, class Value, class KeyOfValue, class Compare, class Alloc>
typename __size_type__
rb_tree<Key, Value, KeyOfValue, Compare, Alloc>::erase(const __key_type__& x) {
    vcl_pair<iterator,iterator> p = equal_range(x);
    size_type n = 0;
    vcl_distance(p.first, p.second, n);
    erase(p.first, p.second);
    return n;
}

template <class Value, class Alloc>
inline __rb_tree_node<Value>*
__rb_tree_base<Value, Alloc>::__copy(__rb_tree_node<Value>* x, __rb_tree_node<Value>* p) {
    link_type l;
#  if defined (__STL_USE_EXCEPTIONS)
    l = left(p);
#  endif
    IUEg__TRY {
    l = __copy_aux(x, p);
    }
#  if defined (__STL_USE_EXCEPTIONS)
    catch(...) {
        if (left(p) != l) {
            __erase(left(p));
            left(p) = l;
        }
        throw;
    }
#  endif
    return l;
}

template <class Value, class Alloc>
inline
__rb_tree_node<Value>*
__rb_tree_base<Value, Alloc>::__copy_aux(__rb_tree_node<Value>* x,
                                         __rb_tree_node<Value>* p) {
   // structural copy
   link_type r = x;
   while (x != 0) {
      link_type y = __new_node(value(x));
      if (r == x) r = y;  // save for return value
      left(p) = y;
      parent(y) = p;
      color(y) = color(x);
      right(y) = __copy_aux(right(x), y);
      left(y) = 0;
      p = y;
      x = left(x);
   }
   left(p) = 0;
   return r;
}


template <class Value, class Alloc>
inline
void __rb_tree_base<Value, Alloc>::__erase(__rb_tree_node<Value>* x) {
    // erase without rebalancing
    while (x != 0) {
       __erase(right(x));
       link_type y = left(x);
       vcl_destroy(&(value(x)));
       put_node(x);
       x = y;
    }
}

template <class Key, class Value, class KeyOfValue, class Compare, class Alloc>
void
rb_tree<Key, Value, KeyOfValue, Compare, Alloc>::erase(__iterator__ first,
                                                       __iterator__ last) {
    if (first == begin() && last == end())
        clear();
    else
         while (first != last) erase(first++);
}

template <class Key, class Value, class KeyOfValue, class Compare, class Alloc>
void rb_tree<Key, Value, KeyOfValue, Compare, Alloc>::erase(const Key* first,
                                                            const Key* last) {
    while (first != last) erase(*first++);
}

template <class Key, class Value, class KeyOfValue, class Compare, class Alloc>
typename __iterator__
rb_tree<Key, Value, KeyOfValue, Compare, Alloc>::find(const __key_type__& k) {
   link_type y = header; /* Last node which is not vcl_less than k. */
   link_type x = root(); /* Current node. */

   while (x != 0)
     if (!key_compare(key(x), k))
       y = x, x = left(x);
   else
       x = right(x);

   return make_iterator((y == header || key_compare(k, key(y))) ? header : y);
}

template <class Key, class Value, class KeyOfValue, class Compare, class Alloc>
typename __const_iterator__
rb_tree<Key, Value, KeyOfValue, Compare, Alloc>::find(const __key_type__& k) const {
   link_type y = header; /* Last node which is not vcl_less than k. */
   link_type x = root(); /* Current node. */

   while (x != 0) {
     if (!key_compare(key(x), k))
       y = x, x = left(x);
   else
       x = right(x);
   }
   return make_const_iterator((y == header || key_compare(k, key(y))) ? header : y);
}

template <class Key, class Value, class KeyOfValue, class Compare, class Alloc>
typename __size_type__
rb_tree<Key, Value, KeyOfValue, Compare, Alloc>::count(const __key_type__& k) const {
    vcl_pair<const_iterator, const_iterator> p = equal_range(k);
    size_type n = 0;
    vcl_distance(p.first, p.second, n);
    return n;
}

template <class Key, class Value, class KeyOfValue, class Compare, class Alloc>
typename __iterator__
rb_tree<Key, Value, KeyOfValue, Compare, Alloc>::lower_bound(const __key_type__& k) {
   link_type y = header; /* Last node which is not vcl_less than k. */
   link_type x = root(); /* Current node. */

   while (x != 0)
     if (!key_compare(key(x), k))
       y = x, x = left(x);
     else
       x = right(x);

   return make_iterator(y);
}

template <class Key, class Value, class KeyOfValue, class Compare, class Alloc>
typename __const_iterator__
rb_tree<Key, Value, KeyOfValue, Compare, Alloc>::lower_bound(const __key_type__& k) const {
   link_type y = header; /* Last node which is not vcl_less than k. */
   link_type x = root(); /* Current node. */

   while (x != 0)
     if (!key_compare(key(x), k))
       y = x, x = left(x);
     else
       x = right(x);

   return make_const_iterator(y);
}

template <class Key, class Value, class KeyOfValue, class Compare, class Alloc>
typename __iterator__
rb_tree<Key, Value, KeyOfValue, Compare, Alloc>::upper_bound(const __key_type__& k) {
  link_type y = header; /* Last node which is vcl_greater than k. */
  link_type x = root(); /* Current node. */

   while (x != 0)
     if (key_compare(k, key(x)))
       y = x, x = left(x);
     else
       x = right(x);

   return make_iterator(y);
}

template <class Key, class Value, class KeyOfValue, class Compare, class Alloc>
typename __const_iterator__
rb_tree<Key, Value, KeyOfValue, Compare, Alloc>::upper_bound(const __key_type__& k) const {
  link_type y = header; /* Last node which is vcl_greater than k. */
  link_type x = root(); /* Current node. */

   while (x != 0)
     if (key_compare(k, key(x)))
       y = x, x = left(x);
     else
       x = right(x);

   return make_const_iterator(y);
}

template <class Key, class Value, class KeyOfValue, class Compare, class Alloc>
inline vcl_pair<typename __iterator__,typename __iterator__>
rb_tree<Key, Value, KeyOfValue, Compare, Alloc>::equal_range(const __key_type__& k) {
    return vcl_pair<iterator, iterator>(lower_bound(k), upper_bound(k));
}

template <class Key, class Value, class KeyOfValue, class Compare, class Alloc>
inline vcl_pair<typename __const_iterator__,typename __const_iterator__>
rb_tree<Key, Value, KeyOfValue, Compare, Alloc>::equal_range(const __key_type__& k) const {
    return vcl_pair<const_iterator,const_iterator>(lower_bound(k), upper_bound(k));
}

// awf patched
#ifdef __PUT_STATIC_DATA_MEMBERS_HERE
int __black_count(__rb_tree_node_base* node, __rb_tree_node_base* root)
{
  if (node == 0)
    return 0;
  else {
    int bc = node->color == __rb_tree_black ? 1 : 0;
    if (node == root)
      return bc;
    else
      return bc + __black_count(node->parent, root);
  }
}
#else
extern int __black_count(__rb_tree_node_base* node, __rb_tree_node_base* root);
#endif

template <class Key, class Value, class KeyOfValue, class Compare, class Alloc>
bool
rb_tree<Key, Value, KeyOfValue, Compare, Alloc>::__rb_verify() const
{
  int len = __black_count(leftmost(), root());
  for (const_iterator it = begin(); it != end(); ++it) {
    link_type x = (link_type) it.node;
    link_type L = left(x);
    link_type R = right(x);

    if (x->color == __rb_tree_red)
      if ((L && L->color == __rb_tree_red) ||
          (R && R->color == __rb_tree_red))
        return false;

    if (L && key_compare(key(x), key(L)))
      return false;
    if (R && key_compare(key(R), key(x)))
      return false;

    if (!L && !R && __black_count(x, root()) != len)
      return false;
  }

  if ( !empty() )
  {
          if (leftmost() != __rb_tree_node_base::minimum(root()))
                return false;
          if (rightmost() != __rb_tree_node_base::maximum(root()))
                return false;
  }

  return true;
}

# undef __iterator__
# undef __const_iterator__
# undef __size_type__
# undef __link_type__
# undef __base_ptr__
# undef __value_type__
# undef __key_type__

#endif // vcl_emulation_tree_h
