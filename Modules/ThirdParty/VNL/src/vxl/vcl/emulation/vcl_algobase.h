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

#ifndef vcl_emulation_algobase_h
#define vcl_emulation_algobase_h

#include <vcl_cstring.h>
//#include <vcl_climits.h>
#include "vcl_functional.h"
#include "vcl_pair.h"
#include "vcl_iterator.h"
#include <vcl_new.h>

# if defined ( __BORLANDC__ ) && defined ( __NO_NAMESPACES )
#  include <vcl_cstdlib.h>
# endif

template <class T>
inline void vcl_swap(T& a, T& b) { T tmp = a; a = b; b = tmp; }

template <class ForwardIterator1, class ForwardIterator2, class T>
inline void __iter_swap(const ForwardIterator1& a, const ForwardIterator2& b, T*)
{
  vcl_swap((T&)*a,(T&)*b);
}

template <class ForwardIterator1, class ForwardIterator2>
inline void iter_swap(const ForwardIterator1& a, const ForwardIterator2& b)
{
  __iter_swap(a, b, value_type(a));
}

# if !( defined ( __STL_NO_NAMESPACES ) && defined ( __MINMAX_DEFINED ) )
# define __MINMAX_DEFINED
template <class T>
inline const T& vcl_min(const T& a, const T& b) { return b < a ? b : a; }

template <class T>
inline const T& vcl_max(const T& a, const T& b) { return  a < b ? b : a; }
# endif

template <class T, class Compare>
inline const T& vcl_min(const T& a, const T& b, Compare comp) { return comp(b, a) ? b : a; }

template <class T, class Compare>
inline const T& vcl_max(const T& a, const T& b, Compare comp) { return comp(a, b) ? b : a; }

template <class InputIterator, class Distance>
INLINE_LOOP void __distance(InputIterator first, const InputIterator& last,
                            Distance& n, vcl_input_iterator_tag)
{
  while (first != last) { ++first; ++n; }
}

template <class ForwardIterator, class Distance>
INLINE_LOOP void __distance(ForwardIterator first, const ForwardIterator& last,
                            Distance& n,
                            vcl_forward_iterator_tag)
{
  while (first != last) { ++first; ++n; }
}

template <class BidirectionalIterator, class Distance>
INLINE_LOOP void __distance(BidirectionalIterator first,
                            const BidirectionalIterator& last,
                            Distance& n, vcl_bidirectional_iterator_tag)
{
  while (first != last) { ++first; ++n; }
}

template <class RandomAccessIterator, class Distance>
inline void __distance(const RandomAccessIterator& first,
                       const RandomAccessIterator& last,
                       Distance& n, vcl_random_access_iterator_tag)
{
  n += last - first;
}

template <class InputIterator, class Distance>
inline void vcl_distance(InputIterator first, InputIterator last, Distance& n)
{
  __distance(first, last, n, iterator_category(first));
}

template <class InputIterator, class Distance>
INLINE_LOOP void __advance(InputIterator& i, Distance n, vcl_input_iterator_tag)
{
  while (n--) ++i;
}

template <class ForwardIterator, class Distance>
INLINE_LOOP void __advance(ForwardIterator& i, Distance n, vcl_forward_iterator_tag)
{
  while (n--) ++i;
}

template <class BidirectionalIterator, class Distance>
INLINE_LOOP void __advance(BidirectionalIterator& i, Distance n, vcl_bidirectional_iterator_tag)
{
  if (n > 0) while (n--) ++i;
  else       while (n++) --i;
}

template <class RandomAccessIterator, class Distance>
inline void __advance(RandomAccessIterator& i, Distance n,
                      vcl_random_access_iterator_tag)
{
  i += n;
}

template <class InputIterator, class Distance>
inline void vcl_advance(InputIterator& i, Distance n)
{
  __advance(i, n, iterator_category(i));
}

#ifdef _MSC_VER
#if _MSC_VER == 1100
#define VC50_DESTRUCTOR_BUG
#endif
#endif
template <class T>
inline void vcl_destroy(T* pointer)
{
// horrible VC50 compiler will leak with stl containers
// of objects, upgrade to VC60
// DO NOT CHANGE BACK TO ifndef VC60
// This is defined by the VC50 compiler and only the VC50 compiler

#ifndef VC50_DESTRUCTOR_BUG
  pointer->~T();
#endif
#ifdef __STL_SHRED_BYTE
  vcl_fill_n((char*)pointer, sizeof(T), STL_SHRED_BYTE);
#endif
}

template <class T1, class T2>
inline void vcl_construct(T1* p, const T2& value)
{
#ifdef __STL_SHRED_BYTE
  vcl_fill_n((char*)p, sizeof(T1), STL_SHRED_BYTE);
#endif
  new (p) T1(value);
}

template <class T>
inline void __default_construct(T* p)
{
#ifdef __STL_SHRED_BYTE
  vcl_fill_n((char*)p, sizeof(T), STL_SHRED_BYTE);
#endif
  new (p) T();
}

template <class ForwardIterator>
INLINE_LOOP void vcl_destroy(ForwardIterator first, ForwardIterator last)
{
  while (first != last) { vcl_destroy(&*first); ++first; }
}

template <class InputIterator, class ForwardIterator>
INLINE_LOOP ForwardIterator vcl_uninitialized_copy(InputIterator first,
                                                   InputIterator last,
                                                   ForwardIterator result)
{
    __stl_debug_check(__check_range(first, last));
#  if defined ( __STL_USE_EXCEPTIONS )
        ForwardIterator resultBase = result;
#  endif
        IUEg__TRY
        {
    for (; first != last; ++result, ++first )
        vcl_construct(&*result, *first);
        }
#  if defined ( __STL_USE_EXCEPTIONS )
        catch(...)
        {
                vcl_destroy( resultBase, result );
                throw;
        }
#  endif
    return result;
}

template <class ForwardIterator, class T>
INLINE_LOOP void
vcl_uninitialized_fill(ForwardIterator first, ForwardIterator last,
                       const T& x)
{
    __stl_debug_check(__check_range(first, last));
#  if defined ( __STL_USE_EXCEPTIONS )
    ForwardIterator saveFirst = first;
#  endif
    IUEg__TRY
    {
    for (; first != last; ++first )
        vcl_construct( &*first, x );
    }
#  if defined ( __STL_USE_EXCEPTIONS )
   catch(...)
   {
        vcl_destroy(saveFirst, first);
        throw;
    }
#  endif
}

template <class ForwardIterator, class Size, class T>
INLINE_LOOP ForwardIterator vcl_uninitialized_fill_n(ForwardIterator first, Size n,
                                                     const T& x)
{
#  if defined ( __STL_USE_EXCEPTIONS )
    ForwardIterator saveFirst = first;
#  endif
    IUEg__TRY
    {
    while (n--) { vcl_construct(&*first, x); ++first; }
    }
#  if defined ( __STL_USE_EXCEPTIONS )
    catch(...)
    {
        vcl_destroy(saveFirst, first);
        throw;
    }
#  endif
    return first;
}

// fbp : extension
template <class ForwardIterator>
INLINE_LOOP void
__default_initialize(ForwardIterator first, ForwardIterator last)
{
    __stl_debug_check(__check_range(first, last));
#  if defined ( __STL_USE_EXCEPTIONS )
    ForwardIterator saveFirst = first;
#  endif
    IUEg__TRY
    {
    for (; first != last; ++first )
        __default_construct(&*first);
    }
#  if defined ( __STL_USE_EXCEPTIONS )
   catch(...)
   {
        vcl_destroy(saveFirst, first);
        throw;
    }
#  endif
}

template <class ForwardIterator, class Size>
INLINE_LOOP ForwardIterator __default_initialize_n(ForwardIterator first, Size n)
{
#  if defined ( __STL_USE_EXCEPTIONS )
    ForwardIterator saveFirst = first;
#  endif
    IUEg__TRY
    {
    while (n--) { __default_construct(&*first); ++first; }
    }
#  if defined ( __STL_USE_EXCEPTIONS )
    catch(...)
    {
        vcl_destroy(saveFirst, first);
        throw;
    }
#  endif
    return first;
}

template <class InputIterator, class OutputIterator>
INLINE_LOOP OutputIterator __copy(InputIterator first, InputIterator last,
                                  OutputIterator result, vcl_input_iterator_tag)
{
  for (; first != last; ++result, ++first)
    *result = *first;
  return result;
}

template <class InputIterator, class OutputIterator>
inline OutputIterator __copy(InputIterator first, InputIterator last,
                             OutputIterator result, vcl_forward_iterator_tag)
{
  return __copy(first, last, result, vcl_input_iterator_tag());
}

template <class InputIterator, class OutputIterator>
inline OutputIterator __copy(InputIterator first, InputIterator last,
                             OutputIterator result, vcl_bidirectional_iterator_tag)
{
  return __copy(first, last, result, vcl_input_iterator_tag());
}

template <class RandomAccessIterator, class OutputIterator, class Distance>
INLINE_LOOP OutputIterator
__copy_d(RandomAccessIterator first, RandomAccessIterator last,
         OutputIterator result, Distance*)
{
  for (Distance n = last - first; n > 0; --n, ++result, ++first)
    *result = *first;
  return result;
}

template <class RandomAccessIterator, class OutputIterator>
inline OutputIterator
__copy(RandomAccessIterator first, RandomAccessIterator last,
       OutputIterator result, vcl_random_access_iterator_tag)
{
  return __copy_d(first, last, result, distance_type(first));
}

template <class InputIterator, class OutputIterator>
inline OutputIterator vcl_copy(InputIterator first, InputIterator last,
                               OutputIterator result)
{
  __stl_debug_check(__check_range(first, last));
  return __copy(first, last, result, iterator_category(first));
}

template <class BidirectionalIterator1, class BidirectionalIterator2>
INLINE_LOOP BidirectionalIterator2 vcl_copy_backward(BidirectionalIterator1 first,
                                                     BidirectionalIterator1 last,
                                                     BidirectionalIterator2 result)
{
  __stl_debug_check(__check_range(first, last));
  while (first != last) *--result = *--last;
  return result;
}

template <class ForwardIterator, class T>
INLINE_LOOP void
vcl_fill(ForwardIterator first, ForwardIterator last, const T& value)
{
  __stl_debug_check(__check_range(first, last));
  for (; first != last; ++first)
    *first = value;
}

template <class OutputIterator, class Size, class T>
INLINE_LOOP OutputIterator
vcl_fill_n(OutputIterator first, Size n, const T& value)
{
  for (; n > 0; --n, ++first)
    *first = value;
  return first;
}

template <class InputIterator1, class InputIterator2>
INLINE_LOOP vcl_pair<InputIterator1, InputIterator2> vcl_mismatch(InputIterator1 first1,
                                                                  InputIterator1 last1,
                                                                  InputIterator2 first2)
{
  __stl_debug_check(__check_range(first1, last1));
  while (first1 != last1 && *first1 == *first2) { ++first1; ++first2; }
  return vcl_pair<InputIterator1, InputIterator2>(first1, first2);
}

template <class InputIterator1, class InputIterator2, class BinaryPredicate>
INLINE_LOOP vcl_pair<InputIterator1, InputIterator2> vcl_mismatch(InputIterator1 first1,
                                                                  InputIterator1 last1,
                                                                  InputIterator2 first2,
                                                                  BinaryPredicate binary_pred)
{
  __stl_debug_check(__check_range(first1, last1));
  while (first1 != last1 && binary_pred(*first1, *first2)) { ++first1; ++first2; }
  return vcl_pair<InputIterator1, InputIterator2>(first1, first2);
}

template <class InputIterator1, class InputIterator2>
INLINE_LOOP bool vcl_equal(InputIterator1 first1, InputIterator1 last1,
                           InputIterator2 first2)
{
  __stl_debug_check(__check_range(first1, last1));
  for (; first1 != last1; ++first1, ++first2)
    if (!(*first1 == *first2))
      return false;
  return true;
}

template <class InputIterator1, class InputIterator2, class BinaryPredicate>
INLINE_LOOP bool vcl_equal(InputIterator1 first1, InputIterator1 last1,
                           InputIterator2 first2, BinaryPredicate binary_pred)
{
  __stl_debug_check(__check_range(first1, last1));
  for (; first1 != last1; ++first1, ++first2)
    if (!binary_pred(*first1, *first2))
      return false;
  return true;
}

template <class InputIterator1, class InputIterator2>
INLINE_LOOP bool
vcl_lexicographical_compare(InputIterator1 first1, InputIterator1 last1,
                            InputIterator2 first2, InputIterator2 last2)
{
  __stl_debug_check(__check_range(first1, last1));
  __stl_debug_check(__check_range(first2, last2));
  for (;first1 != last1 && first2 != last2;++first1,++first2)
  {
    if (*first1 < *first2) return true;
    if (*first2 < *first1) return false;
  }
  return first1 == last1 && first2 != last2;
}

template <class InputIterator1, class InputIterator2, class Compare>
INLINE_LOOP bool
vcl_lexicographical_compare(InputIterator1 first1, InputIterator1 last1,
                            InputIterator2 first2, InputIterator2 last2,
                            Compare comp)
{
  __stl_debug_check(__check_range(first1, last1));
  __stl_debug_check(__check_range(first2, last2));
  for (; first1 != last1 && first2 != last2; ++first2, ++first1)
  {
    if (comp(*first1, *first2)) return true;
    if (comp(*first2, *first1)) return false;
  }
  return first1 == last1 && first2 != last2;
}

inline bool
vcl_lexicographical_compare(unsigned char* first1, unsigned char* last1,
                            unsigned char* first2, unsigned char* last2)
{
  __stl_debug_check(__check_range(first1, last1));
  __stl_debug_check(__check_range(first2, last2));
  vcl_size_t len1 = last1 - first1;  // awf removed const
  vcl_size_t len2 = last2 - first2;
  const int result = vcl_memcmp(first1, first2, vcl_min(len1, len2));
  return result != 0 ? result < 0 : len1 < len2;
}

inline bool vcl_lexicographical_compare(char* first1, char* last1,
                                        char* first2, char* last2)
{
  __stl_debug_check(__check_range(first1, last1));
  __stl_debug_check(__check_range(first2, last2));
#if CHAR_MAX == SCHAR_MAX
  return vcl_lexicographical_compare((signed char*) first1, (signed char*) last1,
                                     (signed char*) first2, (signed char*) last2);
#else
  return vcl_lexicographical_compare((unsigned char*) first1,
                                     (unsigned char*) last1,
                                     (unsigned char*) first2,
                                     (unsigned char*) last2);
#endif
}

#endif // vcl_emulation_algobase_h
