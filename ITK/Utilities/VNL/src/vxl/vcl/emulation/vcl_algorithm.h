#ifndef vcl_emulation_algorithm_h_
#define vcl_emulation_algorithm_h_
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

//#include <vcl_cstdlib.h>
//#include <vcl_climits.h>
#include "vcl_algobase.h"
#include "vcl_heap.h"
#include "vcl_tempbuf.h"
#define vcl_remove vcl_remove

template <class T>
inline const T& __median(const T& a, const T& b, const T& c) {
    if (a < b)
        if (b < c)
            return b;
        else if (a < c)
            return c;
        else
            return a;
    else if (a < c)
        return a;
    else if (b < c)
        return c;
    else
        return b;
}

template <class T, class Compare>
inline const T& __median(const T& a, const T& b, const T& c, Compare comp) {
    if (comp(a, b))
        if (comp(b, c))
            return b;
        else if (comp(a, c))
            return c;
        else
            return a;
    else if (comp(a, c))
        return a;
    else if (comp(b, c))
        return c;
    else
        return b;
}

template <class InputIterator, class Function>
inline
Function vcl_for_each(InputIterator first, InputIterator last, Function f) {
    __stl_debug_check(__check_range(first, last));
    for (;first != last;++first) f(*first);
    return f;
}

template <class InputIterator, class T>
inline
InputIterator vcl_find(InputIterator first, InputIterator last, const T& value) {
    __stl_debug_check(__check_range(first, last));
    while (first != last && *first != value) ++first;
    return first;
}

template <class InputIterator, class Predicate>
inline
InputIterator vcl_find_if(InputIterator first, InputIterator last,
                          Predicate pred) {
    __stl_debug_check(__check_range(first, last));
    while (first != last && !pred(*first)) ++first;
    return first;
}

template <class ForwardIterator>
inline
ForwardIterator vcl_adjacent_find(ForwardIterator first, ForwardIterator last) {
    __stl_debug_check(__check_range(first, last));
    if (first == last) return last;
    ForwardIterator next = first;
    while (++next != last) {
        if (*first == *next) return first;
        first = next;
    }
    return last;
}

template <class ForwardIterator, class BinaryPredicate>
inline
ForwardIterator vcl_adjacent_find(ForwardIterator first, ForwardIterator last,
                                  BinaryPredicate binary_pred) {
    __stl_debug_check(__check_range(first, last));
    if (first == last) return last;
    ForwardIterator next = first;
    while (++next != last) {
        if (binary_pred(*first, *next)) return first;
        first = next;
    }
    return last;
}

template <class InputIterator, class T, class Size>
inline
void vcl_count(InputIterator first, InputIterator last, const T& value,
               Size& n) {
    __stl_debug_check(__check_range(first, last));
    for (; first != last; ++first)
        if (*first == value) ++n;
}

template <class InputIterator, class Predicate, class Size>
inline
void vcl_count_if(InputIterator first, InputIterator last, Predicate pred,
                  Size& n) {
    __stl_debug_check(__check_range(first, last));
    for (;first != last;++first)
        if (pred(*first)) ++n;
}

template <class ForwardIterator1, class ForwardIterator2, class Distance1, class Distance2>
inline
ForwardIterator1 __search(ForwardIterator1 first1, ForwardIterator1 last1,
                          ForwardIterator2 first2, ForwardIterator2 last2,
                          Distance1*, Distance2*) {
    Distance1 d1 = 0;
    vcl_distance(first1, last1, d1);
    Distance2 d2 = 0;
    vcl_distance(first2, last2, d2);

    if (d1 < d2) return last1;

    ForwardIterator1 current1 = first1;
    ForwardIterator2 current2 = first2;

    while (current2 != last2)
        if (*current1++ != *current2++)
            if (d1-- == d2)
                return last1;
            else {
                current1 = ++first1;
                current2 = first2;
            }
    return (current2 == last2) ? first1 : last1;
}

template <class ForwardIterator1, class ForwardIterator2>
inline ForwardIterator1 vcl_search(ForwardIterator1 first1, ForwardIterator1 last1,
                                   ForwardIterator2 first2, ForwardIterator2 last2)
{
    __stl_debug_check(__check_range(first1, last1));
    __stl_debug_check(__check_range(first2, last2));
    return __search(first1, last1, first2, last2, distance_type(first1),
                    distance_type(first2));
}

template <class ForwardIterator1, class ForwardIterator2, class BinaryPredicate, class Distance1, class Distance2>
inline
ForwardIterator1 __search(ForwardIterator1 first1, ForwardIterator1 last1,
                          ForwardIterator2 first2, ForwardIterator2 last2,
                          BinaryPredicate binary_pred, Distance1*, Distance2*) {
    Distance1 d1 = 0;
    vcl_distance(first1, last1, d1);
    Distance2 d2 = 0;
    vcl_distance(first2, last2, d2);

    if (d1 < d2) return last1;

    ForwardIterator1 current1 = first1;
    ForwardIterator2 current2 = first2;

    while (current2 != last2)
        if (!binary_pred(*current1++, *current2++))
            if (d1-- == d2)
                return last1;
            else {
                current1 = ++first1;
                current2 = first2;
            }
    return (current2 == last2) ? first1 : last1;
}

template <class ForwardIterator1, class ForwardIterator2, class BinaryPredicate>
inline ForwardIterator1 vcl_search(ForwardIterator1 first1, ForwardIterator1 last1,
                                   ForwardIterator2 first2, ForwardIterator2 last2,
                                   BinaryPredicate binary_pred) {
    __stl_debug_check(__check_range(first1, last1));
    __stl_debug_check(__check_range(first2, last2));
    return __search(first1, last1, first2, last2, binary_pred,
                    distance_type(first1), distance_type(first2));
}

template <class ForwardIterator1, class ForwardIterator2>
inline
ForwardIterator2 vcl_swap_ranges(ForwardIterator1 first1, ForwardIterator1 last1,
                                 ForwardIterator2 first2) {
    __stl_debug_check(__check_range(first1, last1));
    for (;first1 != last1;++first1,++first2) iter_swap(first1, first2);
    return first2;
}

template <class InputIterator, class OutputIterator, class UnaryOperation>
inline
OutputIterator vcl_transform(InputIterator first, InputIterator last,
                             OutputIterator result, UnaryOperation op) {
    __stl_debug_check(__check_range(first, last));
    for (;first != last;++first,++result) *result = op(*first);
    return result;
}

template <class InputIterator1, class InputIterator2, class OutputIterator, class BinaryOperation>
inline
OutputIterator vcl_transform(InputIterator1 first1, InputIterator1 last1,
                             InputIterator2 first2, OutputIterator result,
                             BinaryOperation binary_op) {
    __stl_debug_check(__check_range(first1, last1));
    for (;first1 != last1;++first1,++first2,++result) *result = binary_op(*first1, *first2);
    return result;
}

template <class ForwardIterator, class T>
inline
void vcl_replace(ForwardIterator first, ForwardIterator last, const T& old_value,
                 const T& new_value) {
    __stl_debug_check(__check_range(first, last));
    while (first != last) {
        if (*first == old_value) *first = new_value;
        ++first;
    }
}

template <class ForwardIterator, class Predicate, class T>
inline
void vcl_replace_if(ForwardIterator first, ForwardIterator last, Predicate pred,
                    const T& new_value) {
    __stl_debug_check(__check_range(first, last));
    while (first != last) {
        if (pred(*first)) *first = new_value;
        ++first;
    }
}

template <class InputIterator, class OutputIterator, class T>
inline
OutputIterator vcl_replace_copy(InputIterator first, InputIterator last,
                                OutputIterator result, const T& old_value,
                                const T& new_value) {
    __stl_debug_check(__check_range(first, last));
    for (;first != last;++first,++result) {
        *result = *first == old_value ? new_value : *first;
    }
    return result;
}

template <class Iterator, class OutputIterator, class Predicate, class T>
inline
OutputIterator vcl_replace_copy_if(Iterator first, Iterator last,
                                   OutputIterator result, Predicate pred,
                                   const T& new_value) {
    __stl_debug_check(__check_range(first, last));
    for (; first != last; ++first,++result) {
        *result = pred(*first) ? new_value : *first;
    }
    return result;
}

template <class ForwardIterator, class Generator>
inline
void vcl_generate(ForwardIterator first, ForwardIterator last, Generator gen) {
    __stl_debug_check(__check_range(first, last));
    for (; first != last; ++first) *first = gen();
}

template <class OutputIterator, class Size, class Generator>
inline
OutputIterator vcl_generate_n(OutputIterator first, Size n, Generator gen) {
    for (; n > 0; --n, ++first) *first = gen();
    return first;
}

template <class InputIterator, class OutputIterator, class T>
inline
OutputIterator vcl_remove_copy(InputIterator first, InputIterator last,
                               OutputIterator result, const T& value) {
    __stl_debug_check(__check_range(first, last));
    for (; first != last; ++first) {
        if (*first != value)  {
            *result = *first;
            ++result;
        }
    }
    return result;
}

template <class InputIterator, class OutputIterator, class Predicate>
inline
OutputIterator vcl_remove_copy_if(InputIterator first, InputIterator last,
                                  OutputIterator result, Predicate pred) {
    __stl_debug_check(__check_range(first, last));
    for (; first != last; ++first) {
        if (!pred(*first)) {
            *result = *first;
            ++result;
        }
    }
    return result;
}

template <class ForwardIterator, class T>
inline
ForwardIterator vcl_remove(ForwardIterator first, ForwardIterator last, const T& value) {
    __stl_debug_check(__check_range(first, last));
    first = vcl_find(first, last, value);
    ForwardIterator next = first;
    return first == last ? first : vcl_remove_copy(++next, last, first, value);
}

template <class ForwardIterator, class Predicate>
inline
ForwardIterator vcl_remove_if(ForwardIterator first, ForwardIterator last, Predicate pred) {
    __stl_debug_check(__check_range(first, last));
    first = vcl_find_if(first, last, pred);
    ForwardIterator next = first;
    return first == last ? first : vcl_remove_copy_if(++next, last, first, pred);
}

template <class InputIterator, class ForwardIterator>
inline
ForwardIterator __unique_copy(InputIterator first, InputIterator last,
                              ForwardIterator result, vcl_forward_iterator_tag) {
    *result = *first;
    while (++first != last)
        if (*result != *first) *++result = *first;
    return ++result;
}

template <class InputIterator, class BidirectionalIterator>
inline BidirectionalIterator __unique_copy(InputIterator first,
                                           InputIterator last,
                                           BidirectionalIterator result,
                                           vcl_bidirectional_iterator_tag) {
    return __unique_copy(first, last, result, vcl_forward_iterator_tag());
}

template <class InputIterator, class RandomAccessIterator>
inline RandomAccessIterator __unique_copy(InputIterator first,
                                          InputIterator last,
                                          RandomAccessIterator result,
                                          vcl_random_access_iterator_tag) {
    return __unique_copy(first, last, result, vcl_forward_iterator_tag());
}

template <class InputIterator, class OutputIterator, class T>
inline
OutputIterator __unique_copy(InputIterator first, InputIterator last,
                             OutputIterator result, T*) {
    T value = *first;
    *result = value;
    while (++first != last)
        if (value != *first) {
            value = *first;
            *++result = value;
        }
    return ++result;
}

template <class InputIterator, class OutputIterator>
inline OutputIterator __unique_copy(InputIterator first, InputIterator last,
                                    OutputIterator result,
                                    vcl_output_iterator_tag) {
    return __unique_copy(first, last, result, value_type(first));
}

template <class InputIterator, class OutputIterator>
inline OutputIterator vcl_unique_copy(InputIterator first, InputIterator last,
                                      OutputIterator result) {
    __stl_debug_check(__check_range(first, last));
    if (first == last) return result;
    return __unique_copy(first, last, result, iterator_category(result));
}
template <class InputIterator, class ForwardIterator, class BinaryPredicate>
inline
ForwardIterator __unique_copy(InputIterator first, InputIterator last,
                              ForwardIterator result,
                              BinaryPredicate binary_pred,
                              vcl_forward_iterator_tag) {
    *result = *first;
    while (++first != last)
        if (!binary_pred(*result, *first)) *++result = *first;
    return ++result;
}

template <class InputIterator, class BidirectionalIterator, class BinaryPredicate>
inline BidirectionalIterator __unique_copy(InputIterator first,
                                           InputIterator last,
                                           BidirectionalIterator result,
                                           BinaryPredicate binary_pred,
                                           vcl_bidirectional_iterator_tag) {
    return __unique_copy(first, last, result, binary_pred,
                         vcl_forward_iterator_tag());
}

template <class InputIterator, class RandomAccessIterator, class BinaryPredicate>
inline RandomAccessIterator __unique_copy(InputIterator first,
                                          InputIterator last,
                                          RandomAccessIterator result,
                                          BinaryPredicate binary_pred,
                                          vcl_random_access_iterator_tag) {
    return __unique_copy(first, last, result, binary_pred,
                         vcl_forward_iterator_tag());
}

template <class InputIterator, class OutputIterator, class BinaryPredicate, class T>
inline
OutputIterator __unique_copy(InputIterator first, InputIterator last,
                             OutputIterator result,
                             BinaryPredicate binary_pred, T*) {
    T value = *first;
    *result = value;
    while (++first != last)
        if (!binary_pred(value, *first)) {
            value = *first;
            *++result = value;
        }
    return ++result;
}

template <class InputIterator, class OutputIterator, class BinaryPredicate>
inline OutputIterator __unique_copy(InputIterator first, InputIterator last,
                                    OutputIterator result,
                                    BinaryPredicate binary_pred,
                                    vcl_output_iterator_tag) {
    return __unique_copy(first, last, result, binary_pred, value_type(first));
}

template <class InputIterator, class OutputIterator, class BinaryPredicate>
inline OutputIterator vcl_unique_copy(InputIterator first, InputIterator last,
                                      OutputIterator result,
                                      BinaryPredicate binary_pred) {
    __stl_debug_check(__check_range(first, last));
    if (first == last) return result;
    return __unique_copy(first, last, result, binary_pred,
                         iterator_category(result));
}

template <class ForwardIterator>
inline
ForwardIterator vcl_unique(ForwardIterator first, ForwardIterator last) {
    __stl_debug_check(__check_range(first, last));
    first = vcl_adjacent_find(first, last);
    return vcl_unique_copy(first, last, first);
}

template <class ForwardIterator, class BinaryPredicate>
inline
ForwardIterator vcl_unique(ForwardIterator first, ForwardIterator last,
                           BinaryPredicate binary_pred) {
    __stl_debug_check(__check_range(first, last));
    first = vcl_adjacent_find(first, last, binary_pred);
    return vcl_unique_copy(first, last, first, binary_pred);
}

template <class BidirectionalIterator>
inline
void __reverse(BidirectionalIterator first, BidirectionalIterator last,
               vcl_bidirectional_iterator_tag) {
    while (true)
        if (first == last || first == --last)
            return;
        else {
            iter_swap(first, last);
            ++first;
        }
}

template <class RandomAccessIterator>
inline
void __reverse(RandomAccessIterator first, RandomAccessIterator last,
               vcl_random_access_iterator_tag) {
    for (; first < last; ++first) iter_swap(first, --last);
}

template <class BidirectionalIterator>
inline void vcl_reverse(BidirectionalIterator first, BidirectionalIterator last) {
    __stl_debug_check(__check_range(first, last));
    __reverse(first, last, iterator_category(first));
}

template <class BidirectionalIterator, class OutputIterator>
INLINE_LOOP OutputIterator vcl_reverse_copy(BidirectionalIterator first,
                                            BidirectionalIterator last,
                                            OutputIterator result) {
    __stl_debug_check(__check_range(first, last));
    for (; first != last; ++result) *result = *--last;
    return result;
}

template <class ForwardIterator, class Distance>
inline
void __rotate(ForwardIterator first, ForwardIterator middle,
              ForwardIterator last, Distance*, vcl_forward_iterator_tag) {
    for (ForwardIterator i = middle; true; ) {
        iter_swap(first, i);
        ++first;
        ++i;
        if (first == middle) {
            if (i == last) return;
            middle = i;
        } else if (i == last)
            i = middle;
    }
}

template <class BidirectionalIterator, class Distance>
inline
void __rotate(BidirectionalIterator first, BidirectionalIterator middle,
              BidirectionalIterator last, Distance*,
              vcl_bidirectional_iterator_tag) {
    vcl_reverse(first, middle);
    vcl_reverse(middle, last);
    vcl_reverse(first, last);
}

template <class EuclideanRingElement>
inline
EuclideanRingElement __gcd(EuclideanRingElement m, EuclideanRingElement n)
{
    while (n != 0) {
        EuclideanRingElement t = m % n;
        m = n;
        n = t;
    }
    return m;
}

template <class RandomAccessIterator, class Distance, class T>
inline
void __rotate_cycle(RandomAccessIterator first, RandomAccessIterator last,
                    RandomAccessIterator initial, Distance shift, T*) {
    T value = *initial;
    RandomAccessIterator ptr1 = initial;
    RandomAccessIterator ptr2 = ptr1 + shift;
    while (ptr2 != initial) {
        *ptr1 = *ptr2;
        ptr1 = ptr2;
        if (last - ptr2 > shift)
            ptr2 += shift;
        else
            ptr2 = first + (shift - (last - ptr2));
    }
    *ptr1 = value;
}

template <class RandomAccessIterator, class Distance>
INLINE_LOOP void __rotate(RandomAccessIterator first, RandomAccessIterator middle,
                          RandomAccessIterator last, Distance*,
                          vcl_random_access_iterator_tag) {
    Distance n = __gcd(last - first, middle - first);
    while (n--)
        __rotate_cycle(first, last, first + n, middle - first,
                       value_type(first));
}

template <class ForwardIterator>
inline void vcl_rotate(ForwardIterator first, ForwardIterator middle,
                       ForwardIterator last) {
    __stl_debug_check(__check_range(middle,first, last));
    if (first == middle || middle == last) return;
    __rotate(first, middle, last, distance_type(first),
             iterator_category(first));
}

template <class ForwardIterator, class OutputIterator>
inline
OutputIterator vcl_rotate_copy(ForwardIterator first, ForwardIterator middle,
                               ForwardIterator last, OutputIterator result) {
    __stl_debug_check(__check_range(middle, first, last));
    return vcl_copy(first, middle, vcl_copy(middle, last, result));
}

template <class RandomAccessIterator, class Distance>
inline
void __random_shuffle(RandomAccessIterator first, RandomAccessIterator last,
                      Distance*) {
    if (first == last) return;
    for (RandomAccessIterator i = first + 1; i != last; ++i)
        iter_swap(i, first + Distance(__rand() % ((i - first) + 1)));
}

template <class RandomAccessIterator>
inline void vcl_random_shuffle(RandomAccessIterator first,
                               RandomAccessIterator last) {
    __stl_debug_check(__check_range(first, last));
    __random_shuffle(first, last, distance_type(first));
}

template <class RandomAccessIterator, class RandomNumberGenerator>
inline
void vcl_random_shuffle(RandomAccessIterator first, RandomAccessIterator last,
                        RandomNumberGenerator& rand) {
    __stl_debug_check(__check_range(first, last));
    if (first == last) return;
    for (RandomAccessIterator i = first + 1; i != last; ++i)
        iter_swap(i, first + rand((i - first) + 1));
}

template <class ForwardIterator, class OutputIterator, class Distance>
inline
OutputIterator vcl_random_sample_n(ForwardIterator first, ForwardIterator last,
                                   OutputIterator out, const Distance n)
{
    __stl_debug_check(__check_range(first, last));
    Distance remaining = 0;
    vcl_distance(first, last, remaining);
    Distance m = vcl_min(n, remaining);

    while (m > 0) {
        if (__rand() % remaining < m) {
            *out = *first;
            ++out;
            --m;
        }

        --remaining;
        ++first;
    }
    return out;
}

template <class ForwardIterator, class OutputIterator, class Distance, class RandomNumberGenerator>
inline
OutputIterator vcl_random_sample_n(ForwardIterator first, ForwardIterator last,
                                   OutputIterator out, const Distance n,
                                   RandomNumberGenerator& rand)
{
    __stl_debug_check(__check_range(first, last));
    Distance remaining = 0;
    vcl_distance(first, last, remaining);
    Distance m = vcl_min(n, remaining);

    while (m > 0) {
        if (rand(remaining) < m) {
            *out = *first;
            --m;
            ++out;
        }

        --remaining;
        ++first;
    }
    return out;
}

template <class InputIterator, class RandomAccessIterator, class Distance>
inline
RandomAccessIterator __random_sample(InputIterator first, InputIterator last,
                                     RandomAccessIterator out,
                                     const Distance n)
{
    Distance m = 0;
    Distance t = n;
    for (; first != last && m < n; ++m,++first)
        out[m] = *first;

    while (first != last) {
        ++t;
        Distance M = __rand() % t;
        if (M < n)
            out[M] = *first;
        ++first;
    }

    return out + m;
}

template <class InputIterator, class RandomAccessIterator, class RandomNumberGenerator, class Distance>
inline
RandomAccessIterator __random_sample(InputIterator first, InputIterator last,
                                     RandomAccessIterator out,
                                     RandomNumberGenerator& rand,
                                     const Distance n)
{
    Distance m = 0;
    Distance t = n;
    for (; first != last && m < n; ++m,++first)
        out[m] = *first;

    while (first != last) {
        ++t;
        Distance M = rand(t);
        if (M < n)
            out[M] = *first;
        ++first;
    }

    return out + m;
}

template <class InputIterator, class RandomAccessIterator>
inline RandomAccessIterator
vcl_random_sample(InputIterator first, InputIterator last,
                  RandomAccessIterator out_first, RandomAccessIterator out_last)
{
    __stl_debug_check(__check_range(first, last));
    return __random_sample(first, last, out_first, out_last - out_first);
}

template <class InputIterator, class RandomAccessIterator, class RandomNumberGenerator>
inline RandomAccessIterator
vcl_random_sample(InputIterator first, InputIterator last,
                  RandomAccessIterator out_first, RandomAccessIterator out_last,
                  RandomNumberGenerator& rand)
{
    __stl_debug_check(__check_range(first, last));
    return __random_sample(first, last, out_first, rand, out_last - out_first);
}

template <class BidirectionalIterator, class Predicate>
inline
BidirectionalIterator vcl_partition(BidirectionalIterator first,
                                    BidirectionalIterator last, Predicate pred) {
    __stl_debug_check(__check_range(first, last));
    while (true) {
        while (true)
            if (first == last)
                return first;
            else if (pred(*first))
                ++first;
            else
                break;
        --last;
        while (true)
            if (first == last)
                return first;
            else if (!pred(*last))
                --last;
            else
                break;
        iter_swap(first, last);
        ++first;
    }
}

template <class ForwardIterator, class Predicate, class Distance>
inline
ForwardIterator __inplace_stable_partition(ForwardIterator first,
                                           ForwardIterator last,
                                           Predicate pred, Distance len) {
    if (len == 1) return pred(*first) ? last : first;
    ForwardIterator middle = first;
    vcl_advance(middle, len / 2);
    ForwardIterator
        first_cut = __inplace_stable_partition(first, middle, pred, len / 2);
    ForwardIterator
        second_cut = __inplace_stable_partition(middle, last, pred,
                                                len - len / 2);
    rotate(first_cut, middle, second_cut);
    len = 0;
    vcl_distance(middle, second_cut, len);
    vcl_advance(first_cut, len);
    return first_cut;
}

template <class ForwardIterator, class Predicate, class Distance, class T>
inline
ForwardIterator __stable_partition_adaptive(ForwardIterator first,
                                            ForwardIterator last,
                                            Predicate pred, Distance len,
                                            __stl_tempbuf<T,Distance>& buffer) {
    typedef typename  __stl_tempbuf<T,Distance>::pointer Pointer;
    Distance fill_pointer = buffer.size();
    if (len <= buffer.capacity()) {
        len = 0;
        ForwardIterator result1 = first;
        Pointer result2 = buffer.begin();
        for (; first != last && len < fill_pointer; ++first)
            if (pred(*first)) {
                *result1 = *first;
                ++result1;
            }
            else {
                *result2++ = *first;
                ++len;
            }
        if (first != last) {
            raw_storage_iterator<Pointer, T> result3(result2);
            IUEg__TRY {
                for (; first != last; ++first)
                    if (pred(*first)) {
                        *result1 = *first;
                        ++result1;
                    }
                    else {
                        *result3 = *first;
                        ++len;
                        ++result3;
                    }
            }
#  if defined ( __STL_USE_EXCEPTIONS )
            catch(...) {
                buffer.adjust_size(len);
                throw;
            }
#  endif
            buffer.adjust_size(len);
        }
        vcl_copy(buffer.begin(), buffer.begin() + len, result1);
        return result1;
    }
    ForwardIterator middle = first;
    vcl_advance(middle, len / 2);
    ForwardIterator first_cut = __stable_partition_adaptive
        (first, middle, pred, len / 2, buffer);
    ForwardIterator second_cut = __stable_partition_adaptive
        (middle, last, pred, len - len / 2, buffer);
    vcl_rotate(first_cut, middle, second_cut);
    len = 0;
    vcl_distance(middle, second_cut, len);
    vcl_advance(first_cut, len);
    return first_cut;
}

template <class ForwardIterator, class Predicate, class T, class Distance>
inline
ForwardIterator __stable_partition(ForwardIterator first, ForwardIterator last,
                                   Predicate pred, Distance len,
                                   __stl_tempbuf<T, Distance>& buffer) {
    if ( buffer.capacity() >0 )
       return __stable_partition_adaptive(first, last, pred, len, buffer);
    else
       return __inplace_stable_partition(first, last, pred, len);
}

template <class ForwardIterator, class Predicate, class Distance, class T>
inline ForwardIterator __stable_partition_aux(ForwardIterator first,
                                              ForwardIterator last,
                                              Predicate pred, Distance*, T*) {
    Distance len = 0;
    vcl_distance(first, last, len);
    __stl_tempbuf<T,Distance> buf(len);
    return __stable_partition(first, last, pred, len, buf);
}

template <class ForwardIterator, class Predicate>
inline ForwardIterator stable_partition(ForwardIterator first,
                                        ForwardIterator last,
                                        Predicate pred) {
    __stl_debug_check(__check_range(first, last));
    return __stable_partition_aux(first, last, pred, distance_type(first),value_type(first));
}

template <class RandomAccessIterator, class T>
inline
RandomAccessIterator __unguarded_partition(RandomAccessIterator first,
                                           RandomAccessIterator last,
                                           T pivot) {
    while (1) {
        while (*first < pivot) {
            ++first;
        }
        --last;
        while (pivot < *last) --last;
        if (!(first < last)) {
            return first;
        }
        iter_swap(first, last);
        ++first;
    }
}

template <class RandomAccessIterator, class T, class Compare>
inline
RandomAccessIterator __unguarded_partition(RandomAccessIterator first,
                                           RandomAccessIterator last,
                                           T pivot, Compare comp) {
    while (1) {
        while (comp(*first, pivot)) ++first;
        --last;
        while (comp(pivot, *last)) --last;
        if (!(first < last)) return first;
        iter_swap(first, last);
        ++first;
    }
}

# define  __stl_threshold  16

template <class RandomAccessIterator, class T>
inline
void __unguarded_linear_insert(RandomAccessIterator last, T value) {
    RandomAccessIterator next = last;
    --next;
    while (value < *next) {
        *last = *next;
        last = next;
        --next;
    }
    *last = value;
}

template <class RandomAccessIterator, class T, class Compare>
inline
void __unguarded_linear_insert(RandomAccessIterator last, T value,
                               Compare comp) {
    RandomAccessIterator next = last;
    --next;
    while (comp(value , *next)) {
        *last = *next;
        last = next;
        --next;
    }
    *last = value;
}

template <class RandomAccessIterator, class T>
inline void __linear_insert(RandomAccessIterator first,
                            RandomAccessIterator last, T*) {
    T value = *last;
    if (value < *first) {
        vcl_copy_backward(first, last, last + 1);
        *first = value;
    } else
        __unguarded_linear_insert(last, value);
}

template <class RandomAccessIterator, class T, class Compare>
inline void __linear_insert(RandomAccessIterator first,
                            RandomAccessIterator last, T*, Compare comp) {
    T value = *last;
    if (comp(value, *first)) {
        vcl_copy_backward(first, last, last + 1);
        *first = value;
    } else
        __unguarded_linear_insert(last, value, comp);
}

template <class RandomAccessIterator>
inline
void __insertion_sort(RandomAccessIterator first, RandomAccessIterator last) {
    if (first == last) return;
    for (RandomAccessIterator i = first + 1; i != last; ++i)
        __linear_insert(first, i, value_type(first));
}

template <class RandomAccessIterator, class Compare>
inline
void __insertion_sort(RandomAccessIterator first,
                      RandomAccessIterator last, Compare comp) {
    if (first == last) return;
    for (RandomAccessIterator i = first + 1; i != last; ++i)
        __linear_insert(first, i, value_type(first), comp);
}

template <class RandomAccessIterator, class T>
inline
void __unguarded_insertion_sort_aux(RandomAccessIterator first,
                                    RandomAccessIterator last, T*) {
    for (RandomAccessIterator i = first; i != last; ++i)
        __unguarded_linear_insert(i, T(*i));
}

template <class RandomAccessIterator>
inline void __unguarded_insertion_sort(RandomAccessIterator first,
                                       RandomAccessIterator last) {
    __unguarded_insertion_sort_aux(first, last, value_type(first));
}

template <class RandomAccessIterator, class T, class Compare>
inline
void __unguarded_insertion_sort_aux(RandomAccessIterator first,
                                    RandomAccessIterator last,
                                    T*, Compare comp) {
    for (RandomAccessIterator i = first; i != last; ++i)
        __unguarded_linear_insert(i, T(*i), comp);
}

template <class RandomAccessIterator, class Compare>
inline void __unguarded_insertion_sort(RandomAccessIterator first,
                                       RandomAccessIterator last,
                                       Compare comp) {
    __unguarded_insertion_sort_aux(first, last, value_type(first), comp);
}

template <class RandomAccessIterator>
inline
void __final_insertion_sort(RandomAccessIterator first,
                            RandomAccessIterator last) {
    if (last - first > __stl_threshold) {
        __insertion_sort(first, first + __stl_threshold);
        __unguarded_insertion_sort(first + __stl_threshold, last);
    } else
        __insertion_sort(first, last);
}

template <class RandomAccessIterator, class Compare>
inline
void __final_insertion_sort(RandomAccessIterator first,
                            RandomAccessIterator last, Compare comp) {
    if (last - first > __stl_threshold) {
        __insertion_sort(first, first + __stl_threshold, comp);
        __unguarded_insertion_sort(first + __stl_threshold, last, comp);
    } else
        __insertion_sort(first, last, comp);
}

template <class Size>
inline
Size __lg(Size n) {
    Size k;
    for (k = 0; n != 1; n = n / 2) ++k;
    return k;
}

template <class RandomAccessIterator, class T, class Size>
inline
void __introsort_loop(RandomAccessIterator first,
                      RandomAccessIterator last, T*,
                      Size depth_limit) {
    while (last - first > __stl_threshold) {
        if (depth_limit == 0) {
            vcl_partial_sort(first, last, last);
            return;
        }
        --depth_limit;
        RandomAccessIterator cut = __unguarded_partition
            (first, last, T(__median(*first, *(first + (last - first)/2),
                                     *(last - 1))));
        __introsort_loop(cut, last, value_type(first), depth_limit);
        last = cut;
    }
}

template <class RandomAccessIterator, class T, class Size, class Compare>
inline
void __introsort_loop(RandomAccessIterator first,
                      RandomAccessIterator last, T*,
                      Size depth_limit, Compare comp) {
    while (last - first > __stl_threshold) {
        if (depth_limit == 0) {
            vcl_partial_sort(first, last, last, comp);
            return;
        }
        --depth_limit;
        RandomAccessIterator cut = __unguarded_partition
            (first, last, T(__median(*first, *(first + (last - first)/2),
                                     *(last - 1), comp)), comp);
        __introsort_loop(cut, last, value_type(first), depth_limit, comp);
        last = cut;
    }
}

template <class RandomAccessIterator>
inline void vcl_sort(RandomAccessIterator first, RandomAccessIterator last) {
    __stl_debug_check(__check_range(first, last));
    if (first==last) return;
    __introsort_loop(first, last, value_type(first), __lg(last - first) * 2);
    __final_insertion_sort(first, last);
}

template <class RandomAccessIterator, class Compare>
inline void vcl_sort(RandomAccessIterator first, RandomAccessIterator last,
                     Compare comp) {
    __stl_debug_check(__check_range(first, last));
    if (first == last) return;
    __introsort_loop(first, last, value_type(first), __lg(last - first) * 2, comp);
    __final_insertion_sort(first, last, comp);
}


template <class RandomAccessIterator>
inline
void __inplace_stable_sort(RandomAccessIterator first,
                           RandomAccessIterator last) {
    if (last - first < 15) {
        __insertion_sort(first, last);
        return;
    }
    RandomAccessIterator middle = first + (last - first) / 2;
    __inplace_stable_sort(first, middle);
    __inplace_stable_sort(middle, last);
    __merge_without_buffer(first, middle, last, middle - first, last - middle);
}

template <class RandomAccessIterator, class Compare>
inline
void __inplace_stable_sort(RandomAccessIterator first,
                           RandomAccessIterator last, Compare comp) {
    if (last - first < 15) {
        __insertion_sort(first, last, comp);
        return;
    }
    RandomAccessIterator middle = first + (last - first) / 2;
    __inplace_stable_sort(first, middle, comp);
    __inplace_stable_sort(middle, last, comp);
    __merge_without_buffer(first, middle, last, middle - first,
                           last - middle, comp);
}

template <class RandomAccessIterator1, class RandomAccessIterator2, class Distance>
inline
void __merge_sort_loop(RandomAccessIterator1 first,
                       RandomAccessIterator1 last,
                       RandomAccessIterator2 result, Distance step_size) {
    Distance two_step = 2 * step_size;

    while (last - first >= two_step) {
        result = merge(first, first + step_size,
                       first + step_size, first + two_step, result);
        first += two_step;
    }
    Distance len(last-first); // VC++ temporary ref warning
    step_size = vcl_min(len, step_size);
    merge(first, first + step_size, first + step_size, last, result);
}

template <class RandomAccessIterator1, class RandomAccessIterator2, class Distance, class Compare>
inline
void __merge_sort_loop(RandomAccessIterator1 first,
                       RandomAccessIterator1 last,
                       RandomAccessIterator2 result, Distance step_size,
                       Compare comp) {
    Distance two_step = 2 * step_size;

    while (last - first >= two_step) {
        result = merge(first, first + step_size,
                       first + step_size, first + two_step, result, comp);
        first += two_step;
    }
    Distance len(last-first); // VC++ temporary ref warning
    step_size = vcl_min(len, step_size);

    merge(first, first + step_size, first + step_size, last, result, comp);
}

const int __stl_chunk_size = 7;

template <class RandomAccessIterator, class Distance>
inline
void __chunk_insertion_sort(RandomAccessIterator first,
                            RandomAccessIterator last, Distance chunk_size) {
    while (last - first >= chunk_size) {
        __insertion_sort(first, first + chunk_size);
        first += chunk_size;
    }
    __insertion_sort(first, last);
}

template <class RandomAccessIterator, class Distance, class Compare>
inline
void __chunk_insertion_sort(RandomAccessIterator first,
                            RandomAccessIterator last,
                            Distance chunk_size, Compare comp) {
    while (last - first >= chunk_size) {
        __insertion_sort(first, first + chunk_size, comp);
        first += chunk_size;
    }
    __insertion_sort(first, last, comp);
}

template <class RandomAccessIterator, class Distance, class T>
inline
void __merge_sort_with_buffer(RandomAccessIterator first,
                              RandomAccessIterator last,
                              __stl_tempbuf<T, Distance>& buffer) {
    typedef typename  __stl_tempbuf<T,Distance>::pointer Pointer;
    Distance len = last - first;
    Pointer buffer_last = buffer.begin() + len;

    Distance step_size = __stl_chunk_size;
    __chunk_insertion_sort(first, last, step_size);

    while (step_size < len) {
        __merge_sort_loop(first, last, buffer.begin(), step_size);
        step_size *= 2;
        __merge_sort_loop(buffer.begin(), buffer_last, first, step_size);
        step_size *= 2;
    }
}


template <class RandomAccessIterator, class Distance, class T, class Compare>
inline
void __merge_sort_with_buffer(RandomAccessIterator first,
                              RandomAccessIterator last,
                              __stl_tempbuf<T, Distance>& buffer,
                              Compare comp) {
    typedef typename  __stl_tempbuf<T,Distance>::pointer Pointer;
    Distance len = last - first;
    Pointer buffer_last = buffer.begin() + len;

    Distance step_size = __stl_chunk_size;
    __chunk_insertion_sort(first, last, step_size, comp);

    while (step_size < len) {
        __merge_sort_loop(first, last, buffer.begin(), step_size, comp);
        step_size *= 2;
        __merge_sort_loop(buffer.begin(), buffer_last, first, step_size, comp);
        step_size *= 2;
    }
}

template <class RandomAccessIterator, class Distance, class T>
inline
void __stable_sort_adaptive(RandomAccessIterator first,
                            RandomAccessIterator last,
                            __stl_tempbuf<T,Distance>& buffer) {
    Distance len = (last - first + 1) / 2;
    RandomAccessIterator middle = first + len;
    if (len > buffer.capacity()) {
        __stable_sort_adaptive(first, middle, buffer);
        __stable_sort_adaptive(middle, last, buffer);
    } else {
        __merge_sort_with_buffer(first, middle, buffer);
        __merge_sort_with_buffer(middle, last, buffer);
    }
    __merge_adaptive(first, middle, last, Distance(middle - first),
                     Distance(last - middle), buffer);
}

template <class RandomAccessIterator, class Distance, class T, class Compare>
inline
void __stable_sort_adaptive(RandomAccessIterator first,
                            RandomAccessIterator last,
                            __stl_tempbuf<T,Distance>& buffer,
                            Compare comp) {
    Distance len = (last - first + 1) / 2;
    RandomAccessIterator middle = first + len;
    if (len > buffer.capacity()) {
        __stable_sort_adaptive(first, middle, buffer, comp);
        __stable_sort_adaptive(middle, last, buffer, comp);
    } else {
        __merge_sort_with_buffer(first, middle, buffer, comp);
        __merge_sort_with_buffer(middle, last, buffer, comp);
    }
    __merge_adaptive(first, middle, last, Distance(middle - first),
                     Distance(last - middle), buffer, comp);
}

template <class RandomAccessIterator, class Distance, class T>
inline void __stable_sort(RandomAccessIterator first,
                          RandomAccessIterator last,
                          __stl_tempbuf<T,Distance>& buffer) {
    if (buffer.capacity() == 0) {
        __inplace_stable_sort(first, last);
    }
    else {
        Distance len(last-first); // VC++ temporary ref warning
        len = vcl_min(buffer.capacity(), len);
        vcl_uninitialized_copy(first, first + len, buffer.begin());
        buffer.adjust_size(len);
        __stable_sort_adaptive(first, last, buffer);
    }
}

template <class RandomAccessIterator, class Distance, class T, class Compare>
inline void __stable_sort(RandomAccessIterator first,
                          RandomAccessIterator last,
                          __stl_tempbuf<T,Distance>& buffer, Compare comp) {
    if (buffer.capacity() == 0) {
        __inplace_stable_sort(first, last, comp);
    }
    else {
        Distance len(last-first); // VC++ temporary ref warning
        len = vcl_min(buffer.capacity(), len);
        vcl_uninitialized_copy(first, first + len, buffer.begin());
        buffer.adjust_size(len);
        __stable_sort_adaptive(first, last, buffer, comp);
    }
}

template <class RandomAccessIterator, class T, class Distance>
inline void __stable_sort_aux(RandomAccessIterator first,
                              RandomAccessIterator last, T*, Distance*) {
    __stl_tempbuf<T,Distance> buffer(Distance(last - first));
    __stable_sort(first, last, buffer);
}

template <class RandomAccessIterator, class T, class Distance, class Compare>
inline void __stable_sort_aux(RandomAccessIterator first,
                              RandomAccessIterator last, T*, Distance*,
                              Compare comp) {
    __stl_tempbuf<T,Distance> buffer(Distance(last - first));
    __stable_sort(first, last, buffer,comp);
}

template <class RandomAccessIterator>
inline void vcl_stable_sort(RandomAccessIterator first,
                            RandomAccessIterator last) {
    __stl_debug_check(__check_range(first, last));
    __stable_sort_aux(first, last, value_type(first), distance_type(first));
}

template <class RandomAccessIterator, class Compare>
inline void vcl_stable_sort(RandomAccessIterator first,
                            RandomAccessIterator last, Compare comp) {
    __stl_debug_check(__check_range(first, last));
    __stable_sort_aux(first, last, value_type(first), distance_type(first),
                      comp);
}

template <class RandomAccessIterator, class T>
inline
void __partial_sort(RandomAccessIterator first, RandomAccessIterator middle,
                    RandomAccessIterator last, T*) {
    vcl_make_heap(first, middle);
    for (RandomAccessIterator i = middle; i < last; ++i)
        if (*i < *first)
            __pop_heap(first, middle, i, T(*i), distance_type(first));
    sort_heap(first, middle);
}

template <class RandomAccessIterator>
inline void vcl_partial_sort(RandomAccessIterator first,
                             RandomAccessIterator middle,
                             RandomAccessIterator last) {
    __stl_debug_check(__check_range(middle,first, last));
    __partial_sort(first, middle, last, value_type(first));
}

template <class RandomAccessIterator, class T, class Compare>
inline
void __partial_sort(RandomAccessIterator first, RandomAccessIterator middle,
                    RandomAccessIterator last, T*, Compare comp) {
    vcl_make_heap(first, middle, comp);
    for (RandomAccessIterator i = middle; i < last; ++i)
        if (comp(*i, *first))
            __pop_heap(first, middle, i, T(*i), comp, distance_type(first));
    sort_heap(first, middle, comp);
}

template <class RandomAccessIterator, class Compare>
inline void vcl_partial_sort(RandomAccessIterator first,
                             RandomAccessIterator middle,
                             RandomAccessIterator last, Compare comp) {
    __stl_debug_check(__check_range(middle,first, last));
    __partial_sort(first, middle, last, value_type(first), comp);
}

template <class InputIterator, class RandomAccessIterator, class Distance, class T>
inline
RandomAccessIterator __partial_sort_copy(InputIterator first,
                                         InputIterator last,
                                         RandomAccessIterator result_first,
                                         RandomAccessIterator result_last,
                                         Distance*, T*) {
    if (result_first == result_last) return result_last;
    RandomAccessIterator result_real_last = result_first;
    for (; first != last && result_real_last != result_last; ++result_real_last,++first)
        *result_real_last = *first;
    vcl_make_heap(result_first, result_real_last);
    while (first != last) {
        if (*first < *result_first)
            __adjust_heap(result_first, Distance(0),
                          Distance(result_real_last - result_first), T(*first));
        ++first;
    }
    vcl_sort_heap(result_first, result_real_last);
    return result_real_last;
}

template <class InputIterator, class RandomAccessIterator>
inline RandomAccessIterator
vcl_partial_sort_copy(InputIterator first, InputIterator last,
                      RandomAccessIterator result_first,
                      RandomAccessIterator result_last) {
    __stl_debug_check(__check_range(first, last));
    __stl_debug_check(__check_range(result_first, result_last));
    return __partial_sort_copy(first, last, result_first, result_last,
                               distance_type(result_first), value_type(first));
}

template <class InputIterator, class RandomAccessIterator, class Compare, class Distance, class T>
inline
RandomAccessIterator __partial_sort_copy(InputIterator first,
                                         InputIterator last,
                                         RandomAccessIterator result_first,
                                         RandomAccessIterator result_last,
                                         Compare comp, Distance*, T*) {
    if (result_first == result_last) return result_last;
    RandomAccessIterator result_real_last = result_first;
    for (; first != last && result_real_last != result_last; ++result_real_last,++first)
        *result_real_last = *first;
    vcl_make_heap(result_first, result_real_last, comp);
    while (first != last) {
        if (comp(*first, *result_first))
            __adjust_heap(result_first, Distance(0),
                          Distance(result_real_last - result_first), T(*first),
                          comp);
        ++first;
    }
    vcl_sort_heap(result_first, result_real_last, comp);
    return result_real_last;
}

template <class InputIterator, class RandomAccessIterator, class Compare>
inline RandomAccessIterator
vcl_partial_sort_copy(InputIterator first, InputIterator last,
                      RandomAccessIterator result_first,
                      RandomAccessIterator result_last, Compare comp) {
    __stl_debug_check(__check_range(first, last));
    __stl_debug_check(__check_range(result_first, result_last));
    return __partial_sort_copy(first, last, result_first, result_last, comp,
                               distance_type(result_first), value_type(first));
}

template <class RandomAccessIterator, class T>
inline
void __nth_element(RandomAccessIterator first, RandomAccessIterator nth,
                   RandomAccessIterator last, T*) {
    while (last - first > 3) {
        RandomAccessIterator cut = __unguarded_partition
            (first, last, T(__median(*first, *(first + (last - first)/2),
                                     *(last - 1))));
        if (cut <= nth)
            first = cut;
        else
            last = cut;
    }
    __insertion_sort(first, last);
}

template <class RandomAccessIterator>
inline void vcl_nth_element(RandomAccessIterator first, RandomAccessIterator nth,
                            RandomAccessIterator last) {
    __stl_debug_check(__check_range(nth,first, last));
    __nth_element(first, nth, last, value_type(first));
}

template <class RandomAccessIterator, class T, class Compare>
inline
void __nth_element(RandomAccessIterator first, RandomAccessIterator nth,
                   RandomAccessIterator last, T*, Compare comp) {
    while (last - first > 3) {
        RandomAccessIterator cut = __unguarded_partition
            (first, last, T(__median(*first, *(first + (last - first)/2),
                                     *(last - 1), comp)), comp);
        if (cut <= nth)
            first = cut;
        else
            last = cut;
    }
    __insertion_sort(first, last, comp);
}

template <class RandomAccessIterator, class Compare>
inline void vcl_nth_element(RandomAccessIterator first, RandomAccessIterator nth,
                            RandomAccessIterator last, Compare comp) {
    __stl_debug_check(__check_range(nth, first, last));
    __nth_element(first, nth, last, value_type(first), comp);
}

template <class ForwardIterator, class T, class Distance>
inline
ForwardIterator __lower_bound(ForwardIterator first, ForwardIterator last,
                              const T& value, Distance*,
                              vcl_forward_iterator_tag) {
    Distance len = 0;
    vcl_distance(first, last, len);
    Distance half;
    ForwardIterator middle;

    while (len > 0) {
        half = len / 2;
        middle = first;
        vcl_advance(middle, half);
        if (*middle < value) {
            first = middle;
            ++first;
            len = len - half - 1;
        } else
            len = half;
    }
    return first;
}

template <class ForwardIterator, class T, class Distance>
inline ForwardIterator __lower_bound(ForwardIterator first,
                                     ForwardIterator last,
                                     const T& value, Distance*,
                                     vcl_bidirectional_iterator_tag) {
  return __lower_bound(first, last, value, (Distance*)0,
                       vcl_forward_iterator_tag());
}

template <class RandomAccessIterator, class T, class Distance>
inline
RandomAccessIterator __lower_bound(RandomAccessIterator first,
                                   RandomAccessIterator last, const T& value,
                                   Distance*, vcl_random_access_iterator_tag) {
    Distance len = last - first;
    Distance half;
    RandomAccessIterator middle;

    while (len > 0) {
        half = len / 2;
        middle = first + half;
        if (*middle < value) {
            first = middle + 1;
            len = len - half - 1;
        } else
            len = half;
    }
    return first;
}

template <class ForwardIterator, class T>
inline ForwardIterator vcl_lower_bound(ForwardIterator first, ForwardIterator last,
                                       const T& value) {
    __stl_debug_check(__check_range(first, last));
    return __lower_bound(first, last, value, distance_type(first),
                         iterator_category(first));
}

template <class ForwardIterator, class T, class Compare, class Distance>
inline
ForwardIterator __lower_bound(ForwardIterator first, ForwardIterator last,
                              const T& value, Compare comp, Distance*,
                              vcl_forward_iterator_tag) {
    Distance len = 0;
    vcl_distance(first, last, len);
    Distance half;
    ForwardIterator middle;

    while (len > 0) {
        half = len / 2;
        middle = first;
        vcl_advance(middle, half);
        if (comp(*middle, value)) {
            first = middle;
            ++first;
            len = len - half - 1;
        } else
            len = half;
    }
    return first;
}

template <class ForwardIterator, class T, class Compare, class Distance>
inline ForwardIterator __lower_bound(ForwardIterator first,
                                     ForwardIterator last,
                                     const T& value, Compare comp, Distance*,
                                     vcl_bidirectional_iterator_tag) {
    return __lower_bound(first, last, value, comp, (Distance*)0,
                         vcl_forward_iterator_tag());
}

template <class RandomAccessIterator, class T, class Compare, class Distance>
inline
RandomAccessIterator __lower_bound(RandomAccessIterator first,
                                   RandomAccessIterator last,
                                   const T& value, Compare comp, Distance*,
                                   vcl_random_access_iterator_tag) {
    Distance len = last - first;
    Distance half;
    RandomAccessIterator middle;

    while (len > 0) {
        half = len / 2;
        middle = first + half;
        if (comp(*middle, value)) {
            first = middle + 1;
            len = len - half - 1;
        } else
            len = half;
    }
    return first;
}

template <class ForwardIterator, class T, class Compare>
inline ForwardIterator vcl_lower_bound(ForwardIterator first, ForwardIterator last,
                                       const T& value, Compare comp) {
  __stl_debug_check(__check_range(first, last));
  return __lower_bound(first, last, value, comp, distance_type(first),
                       iterator_category(first));
}

template <class ForwardIterator, class T, class Distance>
inline
ForwardIterator __upper_bound(ForwardIterator first, ForwardIterator last,
                              const T& value, Distance*,
                              vcl_forward_iterator_tag) {
    Distance len = 0;
    vcl_distance(first, last, len);
    Distance half;
    ForwardIterator middle;

    while (len > 0) {
        half = len / 2;
        middle = first;
        vcl_advance(middle, half);
        if (value < *middle)
            len = half;
        else {
            first = middle;
            ++first;
            len = len - half - 1;
        }
    }
    return first;
}

template <class ForwardIterator, class T, class Distance>
inline ForwardIterator __upper_bound(ForwardIterator first,
                                     ForwardIterator last,
                                     const T& value, Distance*,
                                     vcl_bidirectional_iterator_tag) {
    return __upper_bound(first, last, value, (Distance*)0,
                         vcl_forward_iterator_tag());
}

template <class RandomAccessIterator, class T, class Distance>
inline
RandomAccessIterator __upper_bound(RandomAccessIterator first,
                                   RandomAccessIterator last, const T& value,
                                   Distance*, vcl_random_access_iterator_tag) {
    Distance len = last - first;
    Distance half;
    RandomAccessIterator middle;

    while (len > 0) {
        half = len / 2;
        middle = first + half;
        if (value < *middle)
            len = half;
        else {
            first = middle + 1;
            len = len - half - 1;
        }
    }
    return first;
}

template <class ForwardIterator, class T>
inline ForwardIterator vcl_upper_bound(ForwardIterator first, ForwardIterator last,
                                       const T& value) {
    __stl_debug_check(__check_range(first, last));
    return __upper_bound(first, last, value, distance_type(first),
                         iterator_category(first));
}

template <class ForwardIterator, class T, class Compare, class Distance>
inline
ForwardIterator __upper_bound(ForwardIterator first, ForwardIterator last,
                              const T& value, Compare comp, Distance*,
                              vcl_forward_iterator_tag) {
    Distance len = 0;
    vcl_distance(first, last, len);
    Distance half;
    ForwardIterator middle;

    while (len > 0) {
        half = len / 2;
        middle = first;
        vcl_advance(middle, half);
        if (comp(value, *middle))
            len = half;
        else {
            first = middle;
            ++first;
            len = len - half - 1;
        }
    }
    return first;
}

template <class ForwardIterator, class T, class Compare, class Distance>
inline ForwardIterator __upper_bound(ForwardIterator first,
                                     ForwardIterator last,
                                     const T& value, Compare comp, Distance*,
                                     vcl_bidirectional_iterator_tag) {
    return __upper_bound(first, last, value, comp, (Distance*)0,
                         vcl_forward_iterator_tag());
}

template <class RandomAccessIterator, class T, class Compare, class Distance>
inline
RandomAccessIterator __upper_bound(RandomAccessIterator first,
                                   RandomAccessIterator last,
                                   const T& value, Compare comp, Distance*,
                                   vcl_random_access_iterator_tag) {
    Distance len = last - first;
    Distance half;
    RandomAccessIterator middle;

    while (len > 0) {
        half = len / 2;
        middle = first + half;
        if (comp(value, *middle))
            len = half;
        else {
            first = middle + 1;
            len = len - half - 1;
        }
    }
    return first;
}

template <class ForwardIterator, class T, class Compare>
inline ForwardIterator vcl_upper_bound(ForwardIterator first, ForwardIterator last,
                                       const T& value, Compare comp) {
    __stl_debug_check(__check_range(first, last));
    return __upper_bound(first, last, value, comp, distance_type(first),
                         iterator_category(first));
}

template <class ForwardIterator, class T, class Distance>
inline
vcl_pair<ForwardIterator, ForwardIterator>
__equal_range(ForwardIterator first, ForwardIterator last, const T& value,
              Distance*, vcl_forward_iterator_tag) {
    Distance len = 0;
    vcl_distance(first, last, len);
    Distance half;
    ForwardIterator middle, left, right;

    while (len > 0) {
        half = len / 2;
        middle = first;
        vcl_advance(middle, half);
        if (*middle < value) {
            first = middle;
            ++first;
            len = len - half - 1;
        } else if (value < *middle)
            len = half;
        else {
            left = vcl_lower_bound(first, middle, value);
            vcl_advance(first, len);
            right = vcl_upper_bound(++middle, first, value);
            return vcl_pair<ForwardIterator, ForwardIterator>(left, right);
        }
    }
    return vcl_pair<ForwardIterator, ForwardIterator>(first, first);
}

template <class ForwardIterator, class T, class Distance>
inline vcl_pair<ForwardIterator, ForwardIterator>
__equal_range(ForwardIterator first, ForwardIterator last, const T& value,
              Distance*, vcl_bidirectional_iterator_tag) {
    return __equal_range(first, last, value, (Distance*)0,
                         vcl_forward_iterator_tag());
}

template <class RandomAccessIterator, class T, class Distance>
inline
vcl_pair<RandomAccessIterator, RandomAccessIterator>
__equal_range(RandomAccessIterator first, RandomAccessIterator last,
              const T& value, Distance*, vcl_random_access_iterator_tag) {
    Distance len = last - first;
    Distance half;
    RandomAccessIterator middle, left, right;

    while (len > 0) {
        half = len / 2;
        middle = first + half;
        if (*middle < value) {
            first = middle + 1;
            len = len - half - 1;
        } else if (value < *middle)
            len = half;
        else {
            left = vcl_lower_bound(first, middle, value);
            right = vcl_upper_bound(++middle, first + len, value);
            return vcl_pair<RandomAccessIterator, RandomAccessIterator>(left, right);
        }
    }
    return vcl_pair<RandomAccessIterator, RandomAccessIterator>(first, first);
}

template <class ForwardIterator, class T>
inline vcl_pair<ForwardIterator, ForwardIterator>
vcl_equal_range(ForwardIterator first, ForwardIterator last, const T& value) {
    __stl_debug_check(__check_range(first, last));
    return __equal_range(first, last, value, distance_type(first),
                         iterator_category(first));
}

template <class ForwardIterator, class T, class Compare, class Distance>
inline
vcl_pair<ForwardIterator, ForwardIterator>
__equal_range(ForwardIterator first, ForwardIterator last, const T& value,
              Compare comp, Distance*, vcl_forward_iterator_tag) {
    Distance len = 0;
    vcl_distance(first, last, len);
    Distance half;
    ForwardIterator middle, left, right;

    while (len > 0) {
        half = len / 2;
        middle = first;
        vcl_advance(middle, half);
        if (comp(*middle, value)) {
            first = middle;
            ++first;
            len = len - half - 1;
        } else if (comp(value, *middle))
            len = half;
        else {
            left = vcl_lower_bound(first, middle, value, comp);
            vcl_advance(first, len);
            right = vcl_upper_bound(++middle, first, value, comp);
            return vcl_pair<ForwardIterator, ForwardIterator>(left, right);
        }
    }
    return vcl_pair<ForwardIterator, ForwardIterator>(first, first);
}

template <class ForwardIterator, class T, class Compare, class Distance>
inline vcl_pair<ForwardIterator, ForwardIterator>
__equal_range(ForwardIterator first, ForwardIterator last, const T& value,
              Compare comp, Distance*, vcl_bidirectional_iterator_tag) {
    return __equal_range(first, last, value, comp, (Distance*)0,
                         vcl_forward_iterator_tag());
}

template <class RandomAccessIterator, class T, class Compare, class Distance>
inline
vcl_pair<RandomAccessIterator, RandomAccessIterator>
__equal_range(RandomAccessIterator first, RandomAccessIterator last,
              const T& value, Compare comp, Distance*,
              vcl_random_access_iterator_tag) {
    Distance len = last - first;
    Distance half;
    RandomAccessIterator middle, left, right;

    while (len > 0) {
        half = len / 2;
        middle = first + half;
        if (comp(*middle, value)) {
            first = middle + 1;
            len = len - half - 1;
        } else if (comp(value, *middle))
            len = half;
        else {
            left = vcl_lower_bound(first, middle, value, comp);
            right = vcl_upper_bound(++middle, first + len, value, comp);
            return vcl_pair<RandomAccessIterator, RandomAccessIterator>(left, right);
        }
    }
    return vcl_pair<RandomAccessIterator, RandomAccessIterator>(first, first);
}

template <class ForwardIterator, class T, class Compare>
inline vcl_pair<ForwardIterator, ForwardIterator>
vcl_equal_range(ForwardIterator first, ForwardIterator last, const T& value,
                Compare comp) {
    __stl_debug_check(__check_range(first, last));
    return __equal_range(first, last, value, comp, distance_type(first),
                         iterator_category(first));
}

template <class ForwardIterator, class T>
inline
bool vcl_binary_search(ForwardIterator first, ForwardIterator last,
                       const T& value) {
    ForwardIterator i = vcl_lower_bound(first, last, value);
    return i != last && !(value < *i);
}

template <class ForwardIterator, class T, class Compare>
inline
bool vcl_binary_search(ForwardIterator first, ForwardIterator last, const T& value,
                       Compare comp) {
    ForwardIterator i = vcl_lower_bound(first, last, value, comp);
    return i != last && !comp(value, *i);
}

template <class InputIterator1, class InputIterator2, class OutputIterator>
inline
OutputIterator vcl_merge(InputIterator1 first1, InputIterator1 last1,
                         InputIterator2 first2, InputIterator2 last2,
                         OutputIterator result) {
    __stl_debug_check(__check_range(first1, last1));
    __stl_debug_check(__check_range(first2, last2));
    for (; first1 != last1 && first2 != last2; ++result)
        if (*first2 < *first1)
            *result = *first2++;
        else
            *result = *first1++;
    return vcl_copy(first2, last2, vcl_copy(first1, last1, result));
}

template <class InputIterator1, class InputIterator2, class OutputIterator, class Compare>
inline
OutputIterator vcl_merge(InputIterator1 first1, InputIterator1 last1,
                         InputIterator2 first2, InputIterator2 last2,
                         OutputIterator result, Compare comp) {
    __stl_debug_check(__check_range(first1, last1));
    __stl_debug_check(__check_range(first2, last2));
    for (; first1 != last1 && first2 != last2; ++result)
        if (comp(*first2, *first1))
            *result = *first2++;
        else
            *result = *first1++;
    return vcl_copy(first2, last2, vcl_copy(first1, last1, result));
}

template <class BidirectionalIterator, class Distance>
inline
void __merge_without_buffer(BidirectionalIterator first,
                            BidirectionalIterator middle,
                            BidirectionalIterator last,
                            Distance len1, Distance len2) {
    if (len1 == 0 || len2 == 0) return;
    if (len1 + len2 == 2) {
        if (*middle < *first) iter_swap(first, middle);
        return;
    }
    BidirectionalIterator first_cut = first;
    BidirectionalIterator second_cut = middle;
    Distance len11 = 0;
    Distance len22 = 0;
    if (len1 > len2) {
        len11 = len1 / 2;
        vcl_advance(first_cut, len11);
        second_cut = vcl_lower_bound(middle, last, *first_cut);
        vcl_distance(middle, second_cut, len22);
    } else {
        len22 = len2 / 2;
        vcl_advance(second_cut, len22);
        first_cut = vcl_upper_bound(first, middle, *second_cut);
        vcl_distance(first, first_cut, len11);
    }
    vcl_rotate(first_cut, middle, second_cut);
    BidirectionalIterator new_middle = first_cut;
    vcl_advance(new_middle, len22);
    __merge_without_buffer(first, first_cut, new_middle, len11, len22);
    __merge_without_buffer(new_middle, second_cut, last, len1 - len11,
                           len2 - len22);
}

template <class BidirectionalIterator, class Distance, class Compare>
inline
void __merge_without_buffer(BidirectionalIterator first,
                            BidirectionalIterator middle,
                            BidirectionalIterator last,
                            Distance len1, Distance len2, Compare comp) {
    if (len1 == 0 || len2 == 0) return;
    if (len1 + len2 == 2) {
        if (comp(*middle, *first)) iter_swap(first, middle);
        return;
    }
    BidirectionalIterator first_cut = first;
    BidirectionalIterator second_cut = middle;
    Distance len11 = 0;
    Distance len22 = 0;
    if (len1 > len2) {
        len11 = len1 / 2;
        vcl_advance(first_cut, len11);
        second_cut = vcl_lower_bound(middle, last, *first_cut, comp);
        vcl_distance(middle, second_cut, len22);
    } else {
        len22 = len2 / 2;
        vcl_advance(second_cut, len22);
        first_cut = vcl_upper_bound(first, middle, *second_cut, comp);
        vcl_distance(first, first_cut, len11);
    }
    vcl_rotate(first_cut, middle, second_cut);
    BidirectionalIterator new_middle = first_cut;
    vcl_advance(new_middle, len22);
    __merge_without_buffer(first, first_cut, new_middle, len11, len22, comp);
    __merge_without_buffer(new_middle, second_cut, last, len1 - len11,
                           len2 - len22, comp);
}

template <class BidirectionalIterator1, class BidirectionalIterator2, class Distance>
inline
BidirectionalIterator1 __rotate_adaptive(BidirectionalIterator1 first,
                                         BidirectionalIterator1 middle,
                                         BidirectionalIterator1 last,
                                         Distance len1, Distance len2,
                                         BidirectionalIterator2 buffer,
                                         Distance buffer_size) {
    BidirectionalIterator2 buffer_end;
    if (len1 > len2 && len2 <= buffer_size) {
        buffer_end = vcl_copy(middle, last, buffer);
        vcl_copy_backward(first, middle, last);
        return vcl_copy(buffer, buffer_end, first);
    } else if (len1 <= buffer_size) {
        buffer_end = vcl_copy(first, middle, buffer);
        vcl_copy(middle, last, first);
        return vcl_copy_backward(buffer, buffer_end, last);
    } else  {
        vcl_rotate(first, middle, last);
        vcl_advance(first, len2);
        return first;
    }
}

template <class BidirectionalIterator1, class BidirectionalIterator2, class BidirectionalIterator3>
inline
BidirectionalIterator3 __merge_backward(BidirectionalIterator1 first1,
                                        BidirectionalIterator1 last1,
                                        BidirectionalIterator2 first2,
                                        BidirectionalIterator2 last2,
                                        BidirectionalIterator3 result) {
    if (first1 == last1) return vcl_copy_backward(first2, last2, result);
    if (first2 == last2) return vcl_copy_backward(first1, last1, result);
    --last1;
    --last2;
    while (true) {
        if (*last2 < *last1) {
            *--result = *last1;
            if (first1 == last1) return vcl_copy_backward(first2, ++last2, result);
            --last1;
        } else {
            *--result = *last2;
            if (first2 == last2) return vcl_copy_backward(first1, ++last1, result);
            --last2;
        }
    }
}

template <class BidirectionalIterator1, class BidirectionalIterator2, class BidirectionalIterator3, class Compare>
inline
BidirectionalIterator3 __merge_backward(BidirectionalIterator1 first1,
                                        BidirectionalIterator1 last1,
                                        BidirectionalIterator2 first2,
                                        BidirectionalIterator2 last2,
                                        BidirectionalIterator3 result,
                                        Compare comp) {
    if (first1 == last1) return vcl_copy_backward(first2, last2, result);
    if (first2 == last2) return vcl_copy_backward(first1, last1, result);
    --last1;
    --last2;
    while (true) {
        if (comp(*last2, *last1)) {
            *--result = *last1;
            if (first1 == last1) return vcl_copy_backward(first2, ++last2, result);
            --last1;
        } else {
            *--result = *last2;
            if (first2 == last2) return vcl_copy_backward(first1, ++last1, result);
            --last2;
        }
    }
}

template <class BidirectionalIterator, class Distance, class T>
inline
void __merge_adaptive(BidirectionalIterator first,
                      BidirectionalIterator middle,
                      BidirectionalIterator last, Distance len1, Distance len2,
                      __stl_tempbuf<T,Distance>& buffer) {
    typedef typename  __stl_tempbuf<T,Distance>::pointer Pointer;
    if (len1 <= len2 && len1 <= buffer.capacity()) {
        Pointer end_buffer = vcl_copy(first, middle, buffer.begin());
        merge(buffer.begin(), end_buffer, middle, last, first);
    } else if (len2 <= buffer.capacity()) {
        Pointer end_buffer = vcl_copy(middle, last, buffer.begin());
        __merge_backward(first, middle, buffer.begin(), end_buffer, last);
    } else {
        BidirectionalIterator first_cut = first;
        BidirectionalIterator second_cut = middle;
        Distance len11 = 0;
        Distance len22 = 0;
        if (len1 > len2) {
            len11 = len1 / 2;
            vcl_advance(first_cut, len11);
            second_cut = vcl_lower_bound(middle, last, *first_cut);
            vcl_distance(middle, second_cut, len22);
        } else {
            len22 = len2 / 2;
            vcl_advance(second_cut, len22);
            first_cut = vcl_upper_bound(first, middle, *second_cut);
            vcl_distance(first, first_cut, len11);
        }
        BidirectionalIterator new_middle =
            __rotate_adaptive(first_cut, middle, second_cut, len1 - len11,
                              len22, buffer.begin(), buffer.capacity());
        __merge_adaptive(first, first_cut, new_middle, len11, len22, buffer);
        __merge_adaptive(new_middle, second_cut, last, len1 - len11,
                         len2 - len22, buffer);
    }
}

template <class BidirectionalIterator, class Distance, class T, class Compare>
inline
void __merge_adaptive(BidirectionalIterator first,
                      BidirectionalIterator middle,
                      BidirectionalIterator last, Distance len1, Distance len2,
                      __stl_tempbuf<T,Distance>& buffer, Compare comp) {
    typedef typename  __stl_tempbuf<T,Distance>::pointer Pointer;
    if (len1 <= len2 && len1 <= buffer.capacity()) {
        Pointer end_buffer = vcl_copy(first, middle, buffer.begin());
        merge(buffer.begin(), end_buffer, middle, last, first, comp);
    } else if (len2 <= buffer.capacity()) {
        Pointer end_buffer = vcl_copy(middle, last, buffer.begin());
        __merge_backward(first, middle, buffer.begin(), end_buffer, last, comp);
    } else {
        BidirectionalIterator first_cut = first;
        BidirectionalIterator second_cut = middle;
        Distance len11 = 0;
        Distance len22 = 0;
        if (len1 > len2) {
            len11 = len1 / 2;
            vcl_advance(first_cut, len11);
            second_cut = vcl_lower_bound(middle, last, *first_cut, comp);
            vcl_distance(middle, second_cut, len22);
        } else {
            len22 = len2 / 2;
            vcl_advance(second_cut, len22);
            first_cut = vcl_upper_bound(first, middle, *second_cut, comp);
            vcl_distance(first, first_cut, len11);
        }
        BidirectionalIterator new_middle =
            __rotate_adaptive(first_cut, middle, second_cut, len1 - len11,
                              len22, buffer.begin(), buffer.capacity());
        __merge_adaptive(first, first_cut, new_middle, len11, len22, buffer,comp);
        __merge_adaptive(new_middle, second_cut, last, len1 - len11,
                         len2 - len22, buffer, comp);
    }
}

template <class BidirectionalIterator, class Distance, class T>
inline
void __inplace_merge(BidirectionalIterator first,
                     BidirectionalIterator middle,
                     BidirectionalIterator last, Distance len1, Distance len2,
                     __stl_tempbuf<T,Distance>& buffer) {
    if (buffer.capacity() == 0) {
        __merge_without_buffer(first, middle, last, len1, len2);
    }
    else {
        Distance len(len1+len2); // VC++ temporary ref warning
        len = vcl_min(buffer.capacity(), len);
        __default_initialize_n(buffer.begin(), len);
//        vcl_uninitialized_fill_n(buffer.begin(), len, *first);
        buffer.adjust_size(len);
        __merge_adaptive(first, middle, last, len1, len2, buffer);
    }
}

template <class BidirectionalIterator, class Distance, class T, class Compare>
inline
void __inplace_merge(BidirectionalIterator first,
                     BidirectionalIterator middle,
                     BidirectionalIterator last, Distance len1, Distance len2,
                     __stl_tempbuf<T,Distance>& buffer, Compare comp) {
    if (buffer.capacity() == 0) {
        __merge_without_buffer(first, middle, last, len1, len2, comp);
    }
    else {
        Distance len(len1+len2);
        len = vcl_min(buffer.capacity(), len);
        __default_initialize_n(buffer.begin(), len);
//        vcl_uninitialized_fill_n(buffer.begin(), len, *first);
        buffer.adjust_size(len);
        __merge_adaptive(first, middle, last, len1, len2, buffer,comp);
    }
}

template <class BidirectionalIterator, class T, class Distance>
inline void __inplace_merge_aux(BidirectionalIterator first,
                                BidirectionalIterator middle,
                                BidirectionalIterator last, T*, Distance*) {
    Distance len1 = 0;
    vcl_distance(first, middle, len1);
    Distance len2 = 0;
    vcl_distance(middle, last, len2);
    __stl_tempbuf<T,Distance> buf(len1 + len2);
    __inplace_merge(first, middle, last, len1, len2, buf);
}

template <class BidirectionalIterator, class T, class Distance, class Compare>
inline void __inplace_merge_aux(BidirectionalIterator first,
                                BidirectionalIterator middle,
                                BidirectionalIterator last, T*, Distance*,
                                Compare comp) {
    Distance len1 = 0;
    vcl_distance(first, middle, len1);
    Distance len2 = 0;
    vcl_distance(middle, last, len2);
    __stl_tempbuf<T,Distance> buf(len1 + len2);
    __inplace_merge(first, middle, last, len1, len2, buf, comp);
}

template <class BidirectionalIterator>
inline void inplace_merge(BidirectionalIterator first,
                          BidirectionalIterator middle,
                          BidirectionalIterator last) {
    __stl_debug_check(__check_range(middle, first, last));
    if (first == middle || middle == last) return;
    __inplace_merge_aux(first, middle, last, value_type(first),
                        distance_type(first));
}

template <class BidirectionalIterator, class Compare>
inline void inplace_merge(BidirectionalIterator first,
                          BidirectionalIterator middle,
                          BidirectionalIterator last, Compare comp) {
    __stl_debug_check(__check_range(middle, first, last));
    if (first == middle || middle == last) return;
    __inplace_merge_aux(first, middle, last, value_type(first),
                        distance_type(first), comp);
}

template <class InputIterator1, class InputIterator2>
inline
bool includes(InputIterator1 first1, InputIterator1 last1,
              InputIterator2 first2, InputIterator2 last2) {
    __stl_debug_check(__check_range(first1, last1));
    __stl_debug_check(__check_range(first2, last2));
    while (first1 != last1 && first2 != last2)
        if (*first2 < *first1)
            return false;
        else if (*first1 < *first2)
            ++first1;
        else
            ++first1, ++first2;

    return first2 == last2;
}

template <class InputIterator1, class InputIterator2, class Compare>
inline
bool includes(InputIterator1 first1, InputIterator1 last1,
              InputIterator2 first2, InputIterator2 last2, Compare comp) {
    __stl_debug_check(__check_range(first1, last1));
    __stl_debug_check(__check_range(first2, last2));
    while (first1 != last1 && first2 != last2)
        if (comp(*first2, *first1))
            return false;
        else if (comp(*first1, *first2))
            ++first1;
        else
            ++first1, ++first2;

    return first2 == last2;
}

template <class InputIterator1, class InputIterator2, class OutputIterator>
inline
OutputIterator set_union(InputIterator1 first1, InputIterator1 last1,
                         InputIterator2 first2, InputIterator2 last2,
                         OutputIterator result) {
    __stl_debug_check(__check_range(first1, last1));
    __stl_debug_check(__check_range(first2, last2));
//    __stl_debug_check(__check_not_range(result,first1, last1));
    for (; first1 != last1 && first2 != last2; ++result)
        if (*first1 < *first2) {
            *result = *first1; ++first1;
        }
        else if (*first2 < *first1) {
            *result = *first2; ++first2;
        }
        else {
            *result = *first1++;
            ++first2;
        }
    return vcl_copy(first2, last2, vcl_copy(first1, last1, result));
}

template <class InputIterator1, class InputIterator2, class OutputIterator, class Compare>
inline
OutputIterator set_union(InputIterator1 first1, InputIterator1 last1,
                         InputIterator2 first2, InputIterator2 last2,
                         OutputIterator result, Compare comp) {
    __stl_debug_check(__check_range(first1, last1));
    __stl_debug_check(__check_range(first2, last2));
    for (; first1 != last1 && first2 != last2; ++result)
        if (comp(*first1, *first2)) {
            *result = *first1; ++first1;
        }
        else if (comp(*first2, *first1)) {
            *result = *first2; ++first2;
        }
        else {
            *result = *first1;
            ++first1;
            ++first2;
        }
    return vcl_copy(first2, last2, vcl_copy(first1, last1, result));
}

template <class InputIterator1, class InputIterator2, class OutputIterator>
inline
OutputIterator set_intersection(InputIterator1 first1, InputIterator1 last1,
                                InputIterator2 first2, InputIterator2 last2,
                                OutputIterator result) {
    __stl_debug_check(__check_range(first1, last1));
    __stl_debug_check(__check_range(first2, last2));
    while (first1 != last1 && first2 != last2)
        if (*first1 < *first2)
            ++first1;
        else if (*first2 < *first1)
            ++first2;
        else {
            *result = *first1;
            ++result;
            ++first1;
            ++first2;
        }
    return result;
}

template <class InputIterator1, class InputIterator2, class OutputIterator, class Compare>
inline
OutputIterator set_intersection(InputIterator1 first1, InputIterator1 last1,
                                InputIterator2 first2, InputIterator2 last2,
                                OutputIterator result, Compare comp) {
    __stl_debug_check(__check_range(first1, last1));
    __stl_debug_check(__check_range(first2, last2));
    while (first1 != last1 && first2 != last2)
        if (comp(*first1, *first2))
            ++first1;
        else if (comp(*first2, *first1))
            ++first2;
        else {
            *result = *first1;
            ++first2;
            ++result;
            ++first1;
        }
    return result;
}

template <class InputIterator1, class InputIterator2, class OutputIterator>
inline
OutputIterator set_difference(InputIterator1 first1, InputIterator1 last1,
                              InputIterator2 first2, InputIterator2 last2,
                              OutputIterator result) {
    __stl_debug_check(__check_range(first1, last1));
    __stl_debug_check(__check_range(first2, last2));
    while (first1 != last1 && first2 != last2)
        if (*first1 < *first2) {
            *result = *first1;
            ++result;
            ++first1;
        }
        else if (*first2 < *first1)
            ++first2;
        else {
            ++first1;
            ++first2;
        }
    return vcl_copy(first1, last1, result);
}

template <class InputIterator1, class InputIterator2, class OutputIterator, class Compare>
inline
OutputIterator set_difference(InputIterator1 first1, InputIterator1 last1,
                              InputIterator2 first2, InputIterator2 last2,
                              OutputIterator result, Compare comp) {
    __stl_debug_check(__check_range(first1, last1));
    __stl_debug_check(__check_range(first2, last2));
    while (first1 != last1 && first2 != last2)
        if (comp(*first1, *first2)) {
            *result = *first1;
            ++result;
            ++first1;
        }
        else if (comp(*first2, *first1))
            ++first2;
        else {
            ++first1;
            ++first2;
        }
    return vcl_copy(first1, last1, result);
}

template <class InputIterator1, class InputIterator2, class OutputIterator>
inline
OutputIterator set_symmetric_difference(InputIterator1 first1,
                                        InputIterator1 last1,
                                        InputIterator2 first2,
                                        InputIterator2 last2,
                                        OutputIterator result) {
    __stl_debug_check(__check_range(first1, last1));
    __stl_debug_check(__check_range(first2, last2));
    while (first1 != last1 && first2 != last2)
        if (*first1 < *first2) {
            *result = *first1;
            ++result;
            ++first1;
        }
        else if (*first2 < *first1)  {
            *result = *first2;
            ++result;
            ++first2;
        }
        else {
            ++first1;
            ++first2;
        }
    return vcl_copy(first2, last2, vcl_copy(first1, last1, result));
}

template <class InputIterator1, class InputIterator2, class OutputIterator, class Compare>
inline
OutputIterator set_symmetric_difference(InputIterator1 first1,
                                        InputIterator1 last1,
                                        InputIterator2 first2,
                                        InputIterator2 last2,
                                        OutputIterator result, Compare comp) {
    __stl_debug_check(__check_range(first1, last1));
    __stl_debug_check(__check_range(first2, last2));
    while (first1 != last1 && first2 != last2)
        if (comp(*first1, *first2))  {
            *result = *first1;
            ++result;
            ++first1;
        }
        else if (comp(*first2, *first1)) {
            *result = *first2;
            ++result;
            ++first2;
        }
        else {
            ++first1;
            ++first2;
        }
    return vcl_copy(first2, last2, vcl_copy(first1, last1, result));
}

template <class ForwardIterator>
inline
ForwardIterator vcl_max_element(ForwardIterator first, ForwardIterator last) {
    __stl_debug_check(__check_range(first, last));
    if (first == last) return first;
    ForwardIterator result = first;
    while (++first != last)
        if (*result < *first) result = first;
    return result;
}

template <class ForwardIterator, class Compare>
inline
ForwardIterator vcl_max_element(ForwardIterator first, ForwardIterator last,
                                Compare comp) {
    __stl_debug_check(__check_range(first, last));
    if (first == last) return first;
    ForwardIterator result = first;
    while (++first != last)
        if (comp(*result, *first)) result = first;
    return result;
}

template <class ForwardIterator>
inline
ForwardIterator min_element(ForwardIterator first, ForwardIterator last) {
    __stl_debug_check(__check_range(first, last));
    if (first == last) return first;
    ForwardIterator result = first;
    while (++first != last)
        if (*first < *result) result = first;
    return result;
}

template <class ForwardIterator, class Compare>
inline
ForwardIterator min_element(ForwardIterator first, ForwardIterator last,
                            Compare comp) {
    __stl_debug_check(__check_range(first, last));
    if (first == last) return first;
    ForwardIterator result = first;
    while (++first != last)
        if (comp(*first, *result)) result = first;
    return result;
}

template <class BidirectionalIterator>
inline
bool next_permutation(BidirectionalIterator first,
                      BidirectionalIterator last) {
    __stl_debug_check(__check_range(first, last));
    if (first == last) return false;
    BidirectionalIterator i = first;
    ++i;
    if (i == last) return false;
    i = last;
    --i;

    for (;;) {
        BidirectionalIterator ii = i;
        if (*--i < *ii) {
            BidirectionalIterator j = last;
            while (!(*i < *--j));
            iter_swap(i, j);
            vcl_reverse(ii, last);
            return true;
        }
        if (i == first) {
            vcl_reverse(first, last);
            return false;
        }
    }
}

template <class BidirectionalIterator, class Compare>
inline
bool next_permutation(BidirectionalIterator first, BidirectionalIterator last,
                      Compare comp) {
    __stl_debug_check(__check_range(first, last));
    if (first == last) return false;
    BidirectionalIterator i = first;
    ++i;
    if (i == last) return false;
    i = last;
    --i;

    for (;;) {
        BidirectionalIterator ii = i;
        if (comp(*--i, *ii)) {
            BidirectionalIterator j = last;
            while (!comp(*i, *--j));
            iter_swap(i, j);
            vcl_reverse(ii, last);
            return true;
        }
        if (i == first) {
            vcl_reverse(first, last);
            return false;
        }
    }
}

template <class BidirectionalIterator>
inline
bool prev_permutation(BidirectionalIterator first,
                      BidirectionalIterator last) {
    __stl_debug_check(__check_range(first, last));
    if (first == last) return false;
    BidirectionalIterator i = first;
    ++i;
    if (i == last) return false;
    i = last;
    --i;

    for (;;) {
        BidirectionalIterator ii = i;
        if (*ii < *--i) {
            BidirectionalIterator j = last;
            while (!(*--j < *i));
            iter_swap(i, j);
            vcl_reverse(ii, last);
            return true;
        }
        if (i == first) {
            vcl_reverse(first, last);
            return false;
        }
    }
}

template <class BidirectionalIterator, class Compare>
inline
bool prev_permutation(BidirectionalIterator first, BidirectionalIterator last,
                      Compare comp) {
    __stl_debug_check(__check_range(first, last));
    if (first == last) return false;
    BidirectionalIterator i = first;
    ++i;
    if (i == last) return false;
    i = last;
    --i;

    for (;;) {
        BidirectionalIterator ii = i;
        if (comp(*ii, *--i)) {
            BidirectionalIterator j = last;
            while (!comp(*--j, *i));
            iter_swap(i, j);
            vcl_reverse(ii, last);
            return true;
        }
        if (i == first) {
            vcl_reverse(first, last);
            return false;
        }
    }
}

template <class InputIterator, class T>
inline
T vcl_accumulate(InputIterator first, InputIterator last, T init) {
    __stl_debug_check(__check_range(first, last));
    for (; first != last; ++first)
        init = init + *first;
    return init;
}

template <class InputIterator, class T, class BinaryOperation>
inline
T vcl_accumulate(InputIterator first, InputIterator last, T init,
                 BinaryOperation binary_op) {
    __stl_debug_check(__check_range(first, last));
    for (; first != last; ++first)
        init = binary_op(init, *first);
    return init;
}

template <class InputIterator1, class InputIterator2, class T>
inline
T vcl_inner_product(InputIterator1 first1, InputIterator1 last1,
                    InputIterator2 first2, T init) {
    __stl_debug_check(__check_range(first1, last1));
    for (; first1 != last1; ++first1,++first2)
        init = init + (*first1 * *first2);
    return init;
}

template <class InputIterator1, class InputIterator2, class T, class BinaryOperation1, class BinaryOperation2>
inline
T vcl_inner_product(InputIterator1 first1, InputIterator1 last1,
                    InputIterator2 first2, T init, BinaryOperation1 binary_op1,
                    BinaryOperation2 binary_op2) {
    __stl_debug_check(__check_range(first1, last1));
    for (; first1 != last1; ++first1,++first2)
        init = binary_op1(init, binary_op2(*first1, *first2));
    return init;
}

template <class InputIterator, class OutputIterator, class T>
INLINE_LOOP OutputIterator __partial_sum(InputIterator first, InputIterator last,
                                         OutputIterator result, T*) {
    T value = *first;
    while (++first != last) {
        value = value + *first;
        *++result = value;
    }
    return ++result;
}

template <class InputIterator, class OutputIterator>
inline
OutputIterator vcl_partial_sum(InputIterator first, InputIterator last,
                               OutputIterator result) {
    __stl_debug_check(__check_range(first, last));
    if (first == last) return result;
    *result = *first;
    return __partial_sum(first, last, result, value_type(first));
}

template <class InputIterator, class OutputIterator, class T, class BinaryOperation>
INLINE_LOOP OutputIterator __partial_sum(InputIterator first, InputIterator last,
                                         OutputIterator result, T*,
                                         BinaryOperation binary_op) {
    T value = *first;
    while (++first != last) {
        value = binary_op(value, *first);
        *++result = value;
    }
    return ++result;
}

template <class InputIterator, class OutputIterator, class BinaryOperation>
inline
OutputIterator vcl_partial_sum(InputIterator first, InputIterator last,
                               OutputIterator result, BinaryOperation binary_op) {
    __stl_debug_check(__check_range(first, last));
    if (first == last) return result;
    *result = *first;
    return __partial_sum(first, last, result, value_type(first), binary_op);
}

template <class InputIterator, class OutputIterator, class T>
INLINE_LOOP OutputIterator __adjacent_difference(InputIterator first, InputIterator last,
                                                 OutputIterator result, T*) {
    T value = *first;
    while (++first != last) {
        T tmp = *first;
        *++result = tmp - value;
        value = tmp;
    }
    return ++result;
}

template <class InputIterator, class OutputIterator>
inline
OutputIterator vcl_adjacent_difference(InputIterator first, InputIterator last,
                                       OutputIterator result) {
    __stl_debug_check(__check_range(first, last));
    if (first == last) return result;
    *result = *first;
    return __adjacent_difference(first, last, result, value_type(first));
}

template <class InputIterator, class OutputIterator, class T, class BinaryOperation>
INLINE_LOOP OutputIterator __adjacent_difference(InputIterator first, InputIterator last,
                                                 OutputIterator result, T*,
                                                 BinaryOperation binary_op) {
    T value = *first;
    while (++first != last) {
        T tmp = *first;
        *++result = binary_op(tmp, value);
        value = tmp;
    }
    return ++result;
}

template <class InputIterator, class OutputIterator, class BinaryOperation>
inline
OutputIterator vcl_adjacent_difference(InputIterator first, InputIterator last,
                                       OutputIterator result,
                                       BinaryOperation binary_op) {
    __stl_debug_check(__check_range(first, last));
    if (first == last) return result;
    *result = *first;
    return __adjacent_difference(first, last, result, value_type(first),
                                 binary_op);
}

template <class ForwardIterator, class T>
inline
void vcl_iota(ForwardIterator first, ForwardIterator last, T value) {
    __stl_debug_check(__check_range(first, last));
    for (; first != last; ++first,++value) *first = value;
}

template <class RandomAccessIterator, class Distance>
inline
bool __is_heap(RandomAccessIterator first, RandomAccessIterator last,
               Distance*)
{
    const Distance n = last - first;

    Distance parent = 0;
    for (Distance child = 1; child < n; ++child) {
        if (first[parent] < first[child])
            return false;
        if (child % 2 == 0)
            ++parent;
    }
    return true;
}

template <class RandomAccessIterator>
inline bool vcl_is_heap(RandomAccessIterator first, RandomAccessIterator last)
{
    __stl_debug_check(__check_range(first, last));
    return __is_heap(first, last, distance_type(first));
}


template <class RandomAccessIterator, class Distance, class StrictWeakOrdering>
inline
bool __is_heap(RandomAccessIterator first, RandomAccessIterator last,
               StrictWeakOrdering comp,
               Distance*)
{
    const Distance n = last - first;

    Distance parent = 0;
    for (Distance child = 1; child < n; ++child) {
        if (comp(first[parent], first[child]))
            return false;
        if (child % 2 == 0)
            ++parent;
    }
    return true;
}

template <class RandomAccessIterator, class StrictWeakOrdering>
inline bool vcl_is_heap(RandomAccessIterator first, RandomAccessIterator last,
                        StrictWeakOrdering comp)
{
    __stl_debug_check(__check_range(first, last));
    return __is_heap(first, last, comp, distance_type(first));
}


template <class ForwardIterator>
inline
bool vcl_is_sorted(ForwardIterator first, ForwardIterator last)
{
    __stl_debug_check(__check_range(first, last));
    if (first == last)
        return true;

    ForwardIterator next = first;
    for (++next; next != last; first = next, ++next) {
        if (*next < *first)
            return false;
    }

    return true;
}

template <class ForwardIterator, class StrictWeakOrdering>
inline
bool vcl_is_sorted(ForwardIterator first, ForwardIterator last,
                   StrictWeakOrdering comp)
{
    __stl_debug_check(__check_range(first, last));
    if (first == last)
        return true;

    ForwardIterator next = first;
    for (++next; next != last; first = next, ++next) {
        if (comp(*next, *first))
            return false;
    }

    return true;
}

# undef __stl_threshold

#endif // vcl_emulation_algorithm_h_
