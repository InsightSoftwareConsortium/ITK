//
// Copyright (c) 1994
// Hewlett-Packard Company
//
// Permission to use, copy, modify, distribute and sell this software
// and its documentation for any purpose is hereby granted without fee,
// provided that the above copyright notice appear in all copies and
// that both that copyright notice and this permission notice appear
// in supporting documentation.  Hewlett-Packard Company makes no
// representations about the suitability of this software for any
// purpose.  It is provided "as is" without express or implied warranty.
//
//
// Copyright (c) 1996
// Silicon Graphics Computer Systems, Inc.
//
// Permission to use, copy, modify, distribute and sell this software
// and its documentation for any purpose is hereby granted without fee,
// provided that the above copyright notice appear in all copies and
// that both that copyright notice and this permission notice appear
// in supporting documentation.  Silicon Graphics makes no
// representations about the suitability of this software for any
// purpose.  It is provided "as is" without express or implied warranty.
//
// Copyright (c) 1997
// Moscow Center for SPARC Technology
//
// Permission to use, copy, modify, distribute and sell this software
// and its documentation for any purpose is hereby granted without fee,
// provided that the above copyright notice appear in all copies and
// that both that copyright notice and this permission notice appear
// in supporting documentation.  Moscow Center for SPARC Technology makes no
// representations about the suitability of this software for any
// purpose.  It is provided "as is" without express or implied warranty.
//

#ifndef vcl_emulation_iterator_h
#define vcl_emulation_iterator_h

#include <vcl_cstddef.h>
#include <vcl_iostream.h>
#include "vcl_functional.h"

#if defined ( __STL_USE_ABBREVS )
// ugliness is intentional - to reduce conflicts
# define vcl_input_iterator_tag             InItT
# define vcl_output_iterator_tag            OuItT
# define vcl_bidirectional_iterator_tag     BdItT
# define vcl_random_access_iterator_tag     RaItT
# define vcl_forward_iterator               FwIt
# define vcl_input_iterator                 InIt
# define vcl_output_iterator                OuIt
# define vcl_bidirectional_iterator         BdIt
# define vcl_random_access_iterator         RaIt
# define vcl_reverse_bidirectional_iterator rBdIt
# define vcl_reverse_iterator               rIt
# define vcl_back_insert_iterator           bIIt
# define vcl_front_insert_iterator          fIIt
# define vcl_raw_storage_iterator           rSIt
# define vcl_istream_iterator               iSIt
# define vcl_ostream_iterator               oSIt
#endif

struct vcl_input_iterator_tag {};
struct vcl_output_iterator_tag { vcl_output_iterator_tag() {} };
struct vcl_forward_iterator_tag {};
struct vcl_bidirectional_iterator_tag { vcl_bidirectional_iterator_tag() {} };
struct vcl_random_access_iterator_tag {};

template <class T, class Distance> struct vcl_input_iterator {};
struct vcl_output_iterator {};
template <class T, class Distance> struct vcl_forward_iterator {};
template <class T, class Distance> struct vcl_bidirectional_iterator {};
template <class T, class Distance> struct vcl_random_access_iterator {};

template <class T, class Distance>
inline vcl_input_iterator_tag
iterator_category(const vcl_input_iterator<T, Distance>&) {
    return vcl_input_iterator_tag();
}

inline vcl_output_iterator_tag
iterator_category(const vcl_output_iterator&) {
    return vcl_output_iterator_tag();
}

template <class T, class Distance>
inline vcl_forward_iterator_tag
iterator_category(const vcl_forward_iterator<T, Distance>&) {
    return vcl_forward_iterator_tag();
}

template <class T, class Distance>
inline vcl_bidirectional_iterator_tag
iterator_category(const vcl_bidirectional_iterator<T, Distance>&) {
    return vcl_bidirectional_iterator_tag();
}

template <class T, class Distance>
inline vcl_random_access_iterator_tag
iterator_category(const vcl_random_access_iterator<T, Distance>&) {
    return vcl_random_access_iterator_tag();
}

template <class T>
inline vcl_random_access_iterator_tag
iterator_category(const T*) {
    return vcl_random_access_iterator_tag();
}

template <class T, class Distance>
inline T*
value_type(const vcl_input_iterator<T, Distance>&) {
    return (T*)(0);
}

template <class T, class Distance>
inline T*
value_type(const vcl_forward_iterator<T, Distance>&) {
    return (T*)(0);
}

template <class T, class Distance>
inline T*
value_type(const vcl_bidirectional_iterator<T, Distance>&) {
    return (T*)(0);
}

template <class T, class Distance>
inline T*
value_type(const vcl_random_access_iterator<T, Distance>&) {
    return (T*)(0);
}

template <class T>
inline T*
value_type(const T*) { return (T*)(0); }

template <class T, class Distance>
inline Distance*
distance_type(const vcl_input_iterator<T, Distance>&) {
    return (Distance*)(0);
}

template <class T, class Distance>
inline Distance*
distance_type(const vcl_forward_iterator<T, Distance>&) {
    return (Distance*)(0);
}

template <class T, class Distance>
inline Distance*
distance_type(const vcl_bidirectional_iterator<T, Distance>&) {
    return (Distance*)(0);
}

template <class T, class Distance>
inline Distance*
distance_type(const vcl_random_access_iterator<T, Distance>&) {
    return (Distance*)(0);
}

template <class T>
inline vcl_ptrdiff_t*
distance_type(const T*) { return (vcl_ptrdiff_t*)(0); }

template <class Container>
class vcl_back_insert_iterator {
protected:
    typedef typename Container::value_type value_type;
    Container* container;
public:
    vcl_back_insert_iterator(Container& x) : container(&x) {}
    vcl_back_insert_iterator<Container>&
    operator=(const value_type& value) {
        container->push_back(value);
        return *this;
    }
    vcl_back_insert_iterator<Container>& operator*() { return *this; }
    vcl_back_insert_iterator<Container>& operator++() { return *this; }
    vcl_back_insert_iterator<Container>& operator++(int) { return *this; }
};

template <class Container>
inline vcl_output_iterator_tag
iterator_category(const vcl_back_insert_iterator<Container>&)
{
    return vcl_output_iterator_tag();
}

template <class Container>
inline vcl_back_insert_iterator<Container>
vcl_back_inserter(Container& x)  {
    return vcl_back_insert_iterator<Container>(x);
}

template <class Container>
class vcl_front_insert_iterator {
protected:
    Container* container;
public:
    vcl_front_insert_iterator(Container& x) : container(&x) {}
    vcl_front_insert_iterator<Container>&
    operator=(const typename Container::value_type& value) {
        container->push_front(value);
        return *this;
    }
    vcl_front_insert_iterator<Container>& operator*() { return *this; }
    vcl_front_insert_iterator<Container>& operator++() { return *this; }
    vcl_front_insert_iterator<Container>& operator++(int) { return *this; }
};

template <class Container>
inline vcl_output_iterator_tag
iterator_category(const vcl_front_insert_iterator<Container>&)
{
    return vcl_output_iterator_tag();
}

template <class Container>
inline vcl_front_insert_iterator<Container>
front_inserter(Container& x)  {
    return vcl_front_insert_iterator<Container>(x);
}

template <class Container>
class vcl_insert_iterator {
protected:
    Container* container;
    typename Container::iterator iter;
public:
    vcl_insert_iterator(Container& x, typename Container::iterator i)
        : container(&x), iter(i) {}
    vcl_insert_iterator<Container>&
    operator=(const typename Container::value_type& value) {
        iter = container->insert(iter, value);
        ++iter;
        return *this;
    }
    vcl_insert_iterator<Container>& operator*() { return *this; }
    vcl_insert_iterator<Container>& operator++() { return *this; }
    vcl_insert_iterator<Container>& operator++(int) { return *this; }
};

template <class Container>
inline vcl_output_iterator_tag
iterator_category(const vcl_insert_iterator<Container>&)
{
    return vcl_output_iterator_tag();
}

template <class Container, class Iterator>
inline vcl_insert_iterator<Container> vcl_inserter(Container& x, Iterator i) {
  typedef typename Container::iterator iter;
  return vcl_insert_iterator<Container>(x, iter(i));
}

// Forward declarations
template <class BidirectionalIterator, class T, class Reference, class Distance> class vcl_reverse_bidirectional_iterator;
template <class BidirectionalIterator, class T, class Reference, class Distance> inline bool operator==
   (
    const vcl_reverse_bidirectional_iterator<BidirectionalIterator, T, Reference, Distance>& x,
    const vcl_reverse_bidirectional_iterator<BidirectionalIterator, T, Reference, Distance>& y);

template <class BidirectionalIterator, class T,
          VCL_DFL_TMPL_PARAM_STLDECL(Reference, T& ),
          VCL_DFL_TYPE_PARAM_STLDECL(Distance, vcl_ptrdiff_t)>
class vcl_reverse_bidirectional_iterator {
    typedef vcl_reverse_bidirectional_iterator<BidirectionalIterator, T, Reference,
                                               Distance> self;
    friend bool operator== VCL_NULL_TMPL_ARGS (const self& x, const self& y);
protected:
    BidirectionalIterator current;
public:
    vcl_reverse_bidirectional_iterator() {}
    vcl_reverse_bidirectional_iterator(const BidirectionalIterator&x): current(x) {}
    BidirectionalIterator base() { return current; }
    Reference operator*() const {
        BidirectionalIterator tmp = current;
        return *--tmp;
    }
    self& operator++() {
        --current;
        return *this;
    }
    self operator++(int) {
        self tmp = *this;
        --current;
        return tmp;
    }
    self& operator--() {
        ++current;
        return *this;
    }
    self operator--(int) {
        self tmp = *this;
        ++current;
        return tmp;
    }
};


template <class BidirectionalIterator, class T, class Reference, class Distance>
inline vcl_bidirectional_iterator_tag
iterator_category(const vcl_reverse_bidirectional_iterator<BidirectionalIterator, T, Reference, Distance>&) {
    return vcl_bidirectional_iterator_tag();
}

template <class BidirectionalIterator, class T, class Reference, class Distance>
inline T*
value_type(const vcl_reverse_bidirectional_iterator<BidirectionalIterator, T, Reference, Distance>&) {
    return (T*) 0;
}

template <class BidirectionalIterator, class T, class Reference, class Distance>
inline Distance*
distance_type(const vcl_reverse_bidirectional_iterator<BidirectionalIterator, T, Reference, Distance>&) {
  return (Distance*) 0;
}

template <class BidirectionalIterator, class T, class Reference, class Distance>
inline bool operator==
   (
    const vcl_reverse_bidirectional_iterator<BidirectionalIterator, T, Reference, Distance>& x,
    const vcl_reverse_bidirectional_iterator<BidirectionalIterator, T, Reference, Distance>& y)
{
    return x.current == y.current;
}

// Forward declarations
template <class RandomAccessIterator, class T, class Reference, class Distance> class vcl_reverse_iterator;
template <class RandomAccessIterator, class T, class Reference, class Distance> inline bool operator==
   (
    const vcl_reverse_iterator<RandomAccessIterator, T, Reference, Distance>& x,
    const vcl_reverse_iterator<RandomAccessIterator, T, Reference, Distance>& y);
template <class RandomAccessIterator, class T, class Reference, class Distance> inline bool operator<
   (
    const vcl_reverse_iterator<RandomAccessIterator, T, Reference, Distance>& x,
    const vcl_reverse_iterator<RandomAccessIterator, T, Reference, Distance>& y);
template <class RandomAccessIterator, class T, class Reference, class Distance> inline Distance operator-
   (
    const vcl_reverse_iterator<RandomAccessIterator, T, Reference, Distance>& x,
    const vcl_reverse_iterator<RandomAccessIterator, T, Reference, Distance>& y);
template <class RandomAccessIterator, class T, class Reference, class Distance>
   inline vcl_reverse_iterator<RandomAccessIterator, T, Reference, Distance> operator+
   (
    const vcl_reverse_iterator<RandomAccessIterator, T, Reference, Distance>& x,
    const vcl_reverse_iterator<RandomAccessIterator, T, Reference, Distance>& y);

template <class RandomAccessIterator, class T,
          VCL_DFL_TMPL_PARAM_STLDECL(Reference,T&),
          VCL_DFL_TYPE_PARAM_STLDECL(Distance,vcl_ptrdiff_t)>
class vcl_reverse_iterator
{
    typedef Distance distance_type;
    typedef vcl_reverse_iterator<RandomAccessIterator, T, Reference, Distance> self;
    friend bool operator== VCL_NULL_TMPL_ARGS (const self& x, const self& y);
    friend bool operator< VCL_NULL_TMPL_ARGS (const self& x, const self& y);
    friend Distance operator- VCL_NULL_TMPL_ARGS (const self& x, const self& y);
    friend self operator+ VCL_NULL_TMPL_ARGS (Distance n, const self& x);
  protected:
    RandomAccessIterator current;
  public:
    vcl_reverse_iterator() {}
    vcl_reverse_iterator(const RandomAccessIterator& x) : current(x) {}
    RandomAccessIterator base() const { return current; }
    Reference operator*() const { return *(current - 1); }
    self& operator++() {
        --current;
        return *this;
    }
    self operator++(int) {
        self tmp = *this;
        --current;
        return tmp;
    }
    self& operator--() {
        ++current;
        return *this;
    }
    self operator--(int) {
        self tmp = *this;
        ++current;
        return tmp;
    }
    self operator+(distance_type n) const {
        return self(current - n);
    }
    self& operator+=(distance_type n) {
        current -= n;
        return *this;
    }
    self operator-(distance_type n) const {
        return self(current + n);
    }
    self& operator-=(distance_type n) {
        current += n;
        return *this;
    }
    Reference operator[](distance_type n) { return *(*this + n); }
};

template <class RandomAccessIterator, class T, class Reference, class Distance>
inline vcl_random_access_iterator_tag
iterator_category(const vcl_reverse_iterator<RandomAccessIterator, T,
                                             Reference, Distance>&) {
  return vcl_random_access_iterator_tag();
}

template <class RandomAccessIterator, class T, class Reference, class Distance>
inline T* value_type(const vcl_reverse_iterator<RandomAccessIterator, T,
                                                Reference, Distance>&) {
  return (T*) 0;
}

template <class RandomAccessIterator, class T, class Reference, class Distance>
inline Distance* distance_type(const vcl_reverse_iterator<RandomAccessIterator, T,
                                                          Reference, Distance>&) {
  return (Distance*) 0;
}


template <class RandomAccessIterator, class T, class Reference, class Distance>
inline bool operator==(const vcl_reverse_iterator<RandomAccessIterator, T,
                                                  Reference, Distance>& x,
                       const vcl_reverse_iterator<RandomAccessIterator, T,
                                                  Reference, Distance>& y) {
    return x.current == y.current;
}

template <class RandomAccessIterator, class T, class Reference, class Distance>
inline bool operator<(const vcl_reverse_iterator<RandomAccessIterator, T,
                                                 Reference, Distance>& x,
                      const vcl_reverse_iterator<RandomAccessIterator, T,
                                                 Reference, Distance>& y) {
    return y.current < x.current;
}

template <class RandomAccessIterator, class T, class Reference, class Distance>
inline Distance operator-(const vcl_reverse_iterator<RandomAccessIterator, T,
                                                     Reference, Distance>& x,
                          const vcl_reverse_iterator<RandomAccessIterator, T,
                                                     Reference, Distance>& y) {
//    return y.current - x.current;
    return y.base() - x.base();
}

template <class RandomAccessIterator, class T, class Reference, class Distance>
inline vcl_reverse_iterator<RandomAccessIterator, T, Reference, Distance>
operator+(Distance n,
          const vcl_reverse_iterator<RandomAccessIterator, T, Reference,
                                     Distance>& x) {
    return vcl_reverse_iterator<RandomAccessIterator, T, Reference, Distance>
        (x.current - n);
}


template <class ForwardIterator, class T>
class vcl_raw_storage_iterator {
protected:
    ForwardIterator iter;
public:
    explicit vcl_raw_storage_iterator(const ForwardIterator& x) : iter(x) {}
    vcl_raw_storage_iterator<ForwardIterator, T>& operator*() { return *this; }
    vcl_raw_storage_iterator<ForwardIterator, T>& operator=(const T& element) {
        construct(&*iter, element);
        return *this;
    }
    vcl_raw_storage_iterator<ForwardIterator, T>& operator++() {
        ++iter;
        return *this;
    }
    vcl_raw_storage_iterator<ForwardIterator, T> operator++(int) {
        vcl_raw_storage_iterator<ForwardIterator, T> tmp = *this;
        ++iter;
        return tmp;
    }
};

template <class ForwardIterator, class T>
inline vcl_output_iterator_tag
iterator_category(const vcl_raw_storage_iterator<ForwardIterator, T>&)
{
    return vcl_output_iterator_tag();
}

// Forward declarations
template <class T, class Distance> class vcl_istream_iterator;
template <class T, class Distance> inline bool operator==(
    const vcl_istream_iterator<T, Distance>& x,
    const vcl_istream_iterator<T, Distance>& y);

template <class T, VCL_DFL_TYPE_PARAM_STLDECL(Distance, vcl_ptrdiff_t)>
class vcl_istream_iterator {
    friend bool operator== VCL_NULL_TMPL_ARGS
                          (const vcl_istream_iterator<T, Distance>& x,
                           const vcl_istream_iterator<T, Distance>& y);
protected:
    vcl_istream* stream;
    T value;
    bool end_marker;
    void read() {
        end_marker = bool(*stream);
        if (end_marker) *stream >> value;
        end_marker = bool(*stream);
    }
public:
    vcl_istream_iterator() : stream(&vcl_cin), end_marker(false) {}
    vcl_istream_iterator(vcl_istream& s) : stream(&s) { read(); }
    const T& operator*() const { return value; }
    vcl_istream_iterator<T, Distance>& operator++() {
        read();
        return *this;
    }
    vcl_istream_iterator<T, Distance> operator++(int)  {
        vcl_istream_iterator<T, Distance> tmp = *this;
        read();
        return tmp;
    }
};

template <class T, class Distance>
inline vcl_input_iterator_tag
iterator_category(const vcl_istream_iterator<T, Distance>&) {
  return vcl_input_iterator_tag();
}

template <class T, class Distance>
inline T* value_type(const vcl_istream_iterator<T, Distance>&) { return (T*) 0; }

template <class T, class Distance>
inline Distance* distance_type(const vcl_istream_iterator<T, Distance>&) {
  return (Distance*) 0;
}

template <class T, class Distance>
inline
bool operator==(const vcl_istream_iterator<T, Distance>& x,
                const vcl_istream_iterator<T, Distance>& y) {
    return x.stream == y.stream && x.end_marker == y.end_marker ||
           ! x.end_marker && ! y.end_marker;
}

template <class T>
class vcl_ostream_iterator {
protected:
    vcl_ostream* stream;
    char* string;
public:
    vcl_ostream_iterator(vcl_ostream& s) : stream(&s), string(0) {}
    vcl_ostream_iterator(vcl_ostream& s, char* c) : stream(&s), string(c)  {}
    vcl_ostream_iterator<T>& operator=(const T& value) {
        *stream << value;
        if (string) *stream << string;
        return *this;
    }
    vcl_ostream_iterator<T>& operator*() { return *this; }
    vcl_ostream_iterator<T>& operator++() { return *this; }
    vcl_ostream_iterator<T>& operator++(int) { return *this; }
};

template <class T>
inline vcl_output_iterator_tag
iterator_category(const vcl_ostream_iterator<T>&) {
  return vcl_output_iterator_tag();
}

#endif // vcl_emulation_iterator_h
