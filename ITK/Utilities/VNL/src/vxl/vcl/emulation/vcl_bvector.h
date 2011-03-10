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

// vcl_vector<bool> is replaced by vcl_bit_vector at present because partial
// specialization is not yet implemented.

#ifndef vcl_emulation_bvector_h
#define vcl_emulation_bvector_h

#include <vcl_cstddef.h>
#include "vcl_algobase.h"
#include "vcl_alloc.h"

#define __WORD_BIT (int(CHAR_BIT*sizeof(unsigned int)))

// fbp : give a chance to overload vcl_allocator
# if ! defined ( Alloc )
#  define __AUTO_BVEC_ALLOC
#  define Alloc vcl_alloc
# endif

class __bvec_iterator;
class __bvec_const_iterator;

class __bvec_reference {
    friend class __bvec_iterator;
    friend class __bvec_const_iterator;
    typedef __bvec_reference reference;
protected:
    unsigned int* p;
    unsigned int mask;
    __bvec_reference(unsigned int* x, unsigned int y) : p(x), mask(y) {}
public:
    __bvec_reference() : p(0), mask(0) {}
    operator bool() const { return !(!(*p & mask)); }
    reference& operator=(bool x) {
        if (x)
            *p |= mask;
        else
            *p &= ~mask;
        return *this;
    }
    reference& operator=(const reference& x) { return *this = bool(x); }
    bool operator==(const reference& x) const {
        return bool(*this) == bool(x);
    }
    bool operator<(const reference& x) const {
        return ! bool(*this) && bool(x); // was: bool(*this) < bool(x);
    }
    void flip() { *p ^= mask; }
};


class __bvec_iterator {
    friend class vcl_bit_vector;
    friend class __bvec_const_iterator;
    typedef __bvec_iterator iterator;
    typedef __bvec_const_iterator const_iterator;
    typedef __bvec_reference reference;
    typedef bool const_reference;
    typedef bool value_type;
    typedef vcl_ptrdiff_t difference_type;
    typedef vcl_size_t size_type;
protected:
    unsigned int* p;
    unsigned int offset;
    void bump_up() {
        if (offset++ == __WORD_BIT - 1) {
            offset = 0;
            ++p;
        }
    }
    void bump_down() {
        if (offset-- == 0) {
            offset = __WORD_BIT - 1;
            --p;
        }
    }
public:
    __bvec_iterator() : p(0), offset(0) {}
    __bvec_iterator(unsigned int* x, unsigned int y) : p(x), offset(y) {}
    reference operator*() const { return reference(p, 1U << offset); }
    iterator& operator++() {
        bump_up();
        return *this;
    }
    iterator operator++(int) {
        iterator tmp = *this;
        bump_up();
        return tmp;
    }
    iterator& operator--() {
        bump_down();
        return *this;
    }
    iterator operator--(int) {
        iterator tmp = *this;
        bump_down();
        return tmp;
    }
    iterator& operator+=(difference_type i) {
        difference_type n = i + offset;
        p += n / __WORD_BIT;
        n = n % __WORD_BIT;
        if (n < 0) {
            offset = n + __WORD_BIT;
            --p;
        } else
            offset = n;
        return *this;
    }
    iterator& operator-=(difference_type i) {
        *this += -i;
        return *this;
    }
    iterator operator+(difference_type i) const {
        iterator tmp = *this;
        return tmp += i;
    }
    iterator operator-(difference_type i) const {
        iterator tmp = *this;
        return tmp -= i;
    }
    difference_type operator-(iterator x) const {
        return __WORD_BIT * (p - x.p) + offset - x.offset;
    }
    reference operator[](difference_type i) { return *(*this + i); }
    bool operator==(const iterator& x) const {
        return p == x.p && offset == x.offset;
    }
    bool operator!=(const iterator& x) const {
        return p != x.p || offset != x.offset;
    }
    bool operator<(iterator x) const {
        return p < x.p || (p == x.p && offset < x.offset);
    }
};

class __bvec_const_iterator
{
    friend class vcl_bit_vector;
    typedef __bvec_iterator iterator;
    typedef __bvec_const_iterator const_iterator;
    typedef __bvec_reference reference;
    typedef bool value_type;
    typedef bool const_reference;
    typedef vcl_ptrdiff_t difference_type;
    typedef vcl_size_t size_type;
protected:
    unsigned int* p;
    unsigned int offset;
    void bump_up() {
        if (offset++ == __WORD_BIT - 1) {
            offset = 0;
            ++p;
        }
    }
    void bump_down() {
        if (offset-- == 0) {
            offset = __WORD_BIT - 1;
            --p;
        }
    }
public:
    __bvec_const_iterator() : p(0), offset(0) {}
    __bvec_const_iterator(unsigned int* x, unsigned int y) : p(x), offset(y) {}
    __bvec_const_iterator(const iterator& x) : p(x.p), offset(x.offset) {}
    const_reference operator*() const {
        return reference(p, 1U << offset);
    }
    const_iterator& operator++() {
        bump_up();
        return *this;
    }
    const_iterator operator++(int) {
        const_iterator tmp = *this;
        bump_up();
        return tmp;
    }
    const_iterator& operator--() {
        bump_down();
        return *this;
    }
    const_iterator operator--(int) {
        const_iterator tmp = *this;
        bump_down();
        return tmp;
    }
    const_iterator& operator+=(difference_type i) {
        difference_type n = i + offset;
        p += n / __WORD_BIT;
        n = n % __WORD_BIT;
        if (n < 0) {
            offset = n + __WORD_BIT;
            --p;
        } else
            offset = n;
        return *this;
    }
    const_iterator& operator-=(difference_type i) {
        *this += -i;
        return *this;
    }
    const_iterator operator+(difference_type i) const {
        const_iterator tmp = *this;
        return tmp += i;
    }
    const_iterator operator-(difference_type i) const {
        const_iterator tmp = *this;
        return tmp -= i;
    }
    difference_type operator-(const_iterator x) const {
        return __WORD_BIT * (p - x.p) + offset - x.offset;
    }
    const_reference operator[](difference_type i) {
        return *(*this + i);
    }
    bool operator==(const const_iterator& x) const {
        return p == x.p && offset == x.offset;
    }
    bool operator!=(const const_iterator& x) const {
        return p != x.p || offset != x.offset;
    }
    bool operator<(const_iterator x) const {
        return p < x.p || (p == x.p && offset < x.offset);
    }
};

inline vcl_random_access_iterator_tag
iterator_category(const __bvec_iterator&) {return vcl_random_access_iterator_tag();}
inline vcl_random_access_iterator_tag
iterator_category(const __bvec_const_iterator&) {return vcl_random_access_iterator_tag();}
inline vcl_ptrdiff_t*
distance_type(const __bvec_iterator&) {return (vcl_ptrdiff_t*)0;}
inline vcl_ptrdiff_t*
distance_type(const __bvec_const_iterator&) {return (vcl_ptrdiff_t*)0;}
inline bool* value_type(const __bvec_iterator&) {return (bool*)0;}
inline bool* value_type(const __bvec_const_iterator&) {return (bool*)0;}

class vcl_bit_vector {
public:
    typedef bool value_type;
    typedef vcl_size_t size_type;
    typedef vcl_ptrdiff_t difference_type;
    typedef __bvec_iterator iterator;
    typedef __bvec_const_iterator const_iterator;
    typedef __bvec_reference reference;
    typedef bool const_reference;

    typedef vcl_reverse_iterator<const_iterator, value_type, const_reference,
                                 difference_type> const_reverse_iterator;
    typedef vcl_reverse_iterator<iterator, value_type, reference, difference_type>
        reverse_iterator;
protected:
    typedef vcl_simple_alloc<unsigned int, Alloc> data_allocator;
    iterator start;
    iterator finish;
    unsigned int* end_of_storage;
    unsigned int* bit_alloc(size_type n) {
        return data_allocator::allocate((n + __WORD_BIT - 1)/__WORD_BIT);
    }
    void deallocate() {
      if (start.p)
        data_allocator::deallocate(start.p, end_of_storage - start.p);
    }
    void initialize(size_type n) {
        unsigned int* q = bit_alloc(n);
        end_of_storage = q + (n + __WORD_BIT - 1)/__WORD_BIT;
        start = iterator(q, 0);
        finish = start + n;
    }
    void insert_aux(iterator position, bool x) {
      if (finish.p != end_of_storage) {
        copy_backward(position, finish, finish + 1);
        *position = x;
        ++finish;
      } else {
        size_type len = size() ? 2 * size() : __WORD_BIT;
        unsigned int* q = bit_alloc(len);
        iterator i = copy(begin(), position, iterator(q, 0));
        *i++ = x;
        finish = copy(position, end(), i);
        deallocate();
        end_of_storage = q + (len + __WORD_BIT - 1)/__WORD_BIT;
        start = iterator(q, 0);
      }
    }

    typedef vcl_bit_vector self;
public:
    iterator begin() { return start; }
    const_iterator begin() const { return start; }
    iterator end() { return finish; }
    const_iterator end() const { return finish; }

    reverse_iterator rbegin() { return reverse_iterator(end()); }
    const_reverse_iterator rbegin() const {
        return const_reverse_iterator(end());
    }
    reverse_iterator rend() { return reverse_iterator(begin()); }
    const_reverse_iterator rend() const {
        return const_reverse_iterator(begin());
    }

    size_type size() const { return size_type(end() - begin()); }
    size_type max_size() const { return size_type(-1); }
    size_type capacity() const {
        return size_type(const_iterator(end_of_storage, 0) - begin());
    }
    bool empty() const { return begin() == end(); }
    reference operator[](size_type n) { return *(begin() + n); }
    const_reference operator[](size_type n) const { return *(begin() + n); }
    vcl_bit_vector() : start(iterator()), finish(iterator()), end_of_storage(0) {}
    vcl_bit_vector(size_type n, bool value = bool()) {
        initialize(n);
        fill(start.p, end_of_storage, value ? ~0 : 0);
    }
    vcl_bit_vector(const self& x) {
        initialize(x.size());
        copy(x.begin(), x.end(), start);
    }
    vcl_bit_vector(const_iterator first, const_iterator last) {
        size_type n = 0;
        vcl_distance(first, last, n);
        initialize(n);
        copy(first, last, start);
    }
    vcl_bit_vector(const bool* first, const bool* last) {
        size_type n = 0;
        vcl_distance(first, last, n);
        initialize(n);
        copy(first, last, start);
    }
    ~vcl_bit_vector() { deallocate(); }
    self& operator=(const self& x) {
        if (&x == this) return *this;
        if (x.size() > capacity()) {
            deallocate();
            initialize(x.size());
        }
        copy(x.begin(), x.end(), begin());
        finish = begin() + x.size();
        return *this;
    }
    void reserve(size_type n) {
        if (capacity() < n) {
            unsigned int* q = bit_alloc(n);
            finish = copy(begin(), end(), iterator(q, 0));
            deallocate();
            start = iterator(q, 0);
            end_of_storage = q + (n + __WORD_BIT - 1)/__WORD_BIT;
        }
    }
    reference front() { return *begin(); }
    const_reference front() const { return *begin(); }
    reference back() { return *(end() - 1); }
    const_reference back() const { return *(end() - 1); }
    void push_back(bool x) {
        if (finish.p != end_of_storage)
            *finish++ = x;
        else
            insert_aux(end(), x);
    }
    void swap(vcl_bit_vector& x) {
        vcl_swap(start, x.start);
        vcl_swap(finish, x.finish);
        vcl_swap(end_of_storage, x.end_of_storage);
    }
    iterator insert(iterator position, bool x = bool()) {
        size_type n = position - begin();
        if (finish.p != end_of_storage && position == end())
            *finish++ = x;
        else
            insert_aux(position, x);
        return begin() + n;
    }
    void insert(iterator position, const_iterator first,
                const_iterator last) {
      if (first == last) return;
      size_type n = 0;
      vcl_distance(first, last, n);
      if (capacity() - size() >= n) {
        copy_backward(position, end(), finish + n);
        copy(first, last, position);
        finish += n;
      } else {
        size_type len = size() + vcl_max(size(), n);
        unsigned int* q = bit_alloc(len);
        iterator i = copy(begin(), position, iterator(q, 0));
        i = copy(first, last, i);
        finish = copy(position, end(), i);
        deallocate();
        end_of_storage = q + (len + __WORD_BIT - 1)/__WORD_BIT;
        start = iterator(q, 0);
      }
    }

    void insert(iterator position, const bool* first, const bool* last) {
      if (first == last) return;
      size_type n = 0;
      vcl_distance(first, last, n);
      if (capacity() - size() >= n) {
        copy_backward(position, end(), finish + n);
        copy(first, last, position);
        finish += n;
      } else {
        size_type len = size() + vcl_max(size(), n);
        unsigned int* q = bit_alloc(len);
        iterator i = copy(begin(), position, iterator(q, 0));
        i = copy(first, last, i);
        finish = copy(position, end(), i);
        deallocate();
        end_of_storage = q + (len + __WORD_BIT - 1)/__WORD_BIT;
        start = iterator(q, 0);
      }
    }

    void insert(iterator position, size_type n, bool x) {
      if (n == 0) return;
      if (capacity() - size() >= n) {
        copy_backward(position, end(), finish + n);
        fill(position, position + n, x);
        finish += n;
      } else {
        size_type len = size() + vcl_max(size(), n);
        unsigned int* q = bit_alloc(len);
        iterator i = copy(begin(), position, iterator(q, 0));
        fill_n(i, n, x);
        finish = copy(position, end(), i + n);
        deallocate();
        end_of_storage = q + (len + __WORD_BIT - 1)/__WORD_BIT;
        start = iterator(q, 0);
      }
    }

    void pop_back() { --finish; }
    void erase(iterator position) {
        if (position + 1 != end())
            copy(position + 1, end(), position);
        --finish;
    }
    void erase(iterator first, iterator last) {
        finish = copy(last, end(), first);
    }
};

inline bool operator==(const vcl_bit_vector& x, const vcl_bit_vector& y) {
    return x.size() == y.size() && vcl_equal(x.begin(), x.end(), y.begin());
}

inline bool operator<(const vcl_bit_vector& x, const vcl_bit_vector& y) {
    return lexicographical_compare(x.begin(), x.end(), y.begin(), y.end());
}

inline void swap(vcl_bit_vector::reference x, vcl_bit_vector::reference y) {
    bool tmp = x;
    x = y;
    y = tmp;
}

# undef __WORD_BIT

# if defined ( __AUTO_BVEC_ALLOC )
#  undef Alloc
# endif

#endif // vcl_emulation_bvector_h
