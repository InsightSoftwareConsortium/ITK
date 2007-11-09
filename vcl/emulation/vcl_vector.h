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

#ifndef vcl_emulation_vector_h
#define vcl_emulation_vector_h

#include <vcl_new.h>
#include <vcl_cstddef.h>
#include "vcl_algobase.h"
#include "vcl_alloc.h"

template <class T, class Alloc>
class __vector_base
{
    typedef __vector_base<T,Alloc> self;
public:
    typedef T value_type;
    typedef vcl_size_t size_type;
    typedef T* pointer;
    typedef const T* const_pointer;
protected:
    typedef vcl_simple_alloc<value_type, Alloc> data_allocator;
    pointer start;
    pointer finish;
    pointer end_of_storage;

    void deallocate() {
        if (start) {
            __stl_debug_do(invalidate_all());
            data_allocator::deallocate(start, end_of_storage - start);
        }
    }
public:
    __vector_base() : start(0), finish(0), end_of_storage(0) {
        __stl_debug_do(safe_init(this));
    }

    __vector_base(size_type n) :
        start( data_allocator::allocate(n) ),
        finish(start),
        end_of_storage(start + n) {
        __stl_debug_do(safe_init(this));
    }

    ~__vector_base() {
        vcl_destroy(start, finish);
        deallocate();
        __stl_debug_do(invalidate());
    }
};

# if defined ( __STL_NESTED_TYPE_PARAM_BUG )
#  define __pointer__             T*
#  define __const_pointer__       const T*
#  define __size_type__           vcl_size_t
#  define __difference_type__     vcl_ptrdiff_t
# else
#  define __pointer__         pointer
#  define __const_pointer__   const_pointer
#  define __size_type__       size_type
#  define __difference_type__ difference_type
# endif

# define __iterator__       __pointer__
# define __const_iterator__ __const_pointer__

__BEGIN_STL_FULL_NAMESPACE
#  define vcl_vector __WORKAROUND_RENAME(vcl_vector)

template <class T, VCL_DFL_TYPE_PARAM_STLDECL(Alloc,vcl_alloc)>
class vcl_vector : protected __vector_base<T, Alloc> {
    typedef __vector_base<T,Alloc> super;
    typedef vcl_vector<T,Alloc> self;
public:
    typedef T value_type;
    typedef value_type* pointer;
    typedef const value_type* const_pointer;
    typedef value_type& reference;
    typedef const value_type& const_reference;
    typedef vcl_ptrdiff_t difference_type;
    typedef vcl_size_t size_type;
    typedef __iterator__ iterator;
    typedef __const_iterator__ const_iterator;
    typedef vcl_reverse_iterator<const_iterator, value_type, const_reference,
                                 difference_type>  const_reverse_iterator;
    typedef vcl_reverse_iterator<iterator, value_type, reference,
                                 difference_type> reverse_iterator;
protected:
    inline void insert_aux(pointer position, const T& x);
public:
    pointer begin_() { return start; }
    const_pointer begin_() const { return start; }
    pointer end_() { return finish; }
    const_pointer end_() const { return finish; }
#   define __ptr(x) x
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
    size_type size() const { return size_type(end_() - begin_()); }
    size_type max_size() const { return size_type(-1)/sizeof(T); }
    size_type capacity() const { return size_type(end_of_storage - start); }
    bool empty() const { return begin_() == end_(); }
    reference operator[](size_type n) {
        __stl_verbose_assert(n<size(), __STL_MSG_OUT_OF_BOUNDS);
        return *(start+n);
    }
    const_reference operator[](size_type n) const {
        __stl_verbose_assert(n<size(), __STL_MSG_OUT_OF_BOUNDS);
        return *(start+n);
    }
    vcl_vector() {}
    vcl_vector(size_type n, const T& value) : super(n) {
        finish = vcl_uninitialized_fill_n(start, n, value);
    }
    explicit vcl_vector(size_type n) : super(n) {
        finish = __default_initialize_n(start, n);
    }
    vcl_vector(const self& x) : super(x.end_() - x.begin_()) {
        finish = vcl_uninitialized_copy(x.begin_(), x.end_(), start);
    }
    vcl_vector(const_iterator first, const_iterator last) {
        __stl_debug_check(__check_range(first,last));
        size_type n = 0;
        vcl_distance(__ptr(first), __ptr(last), n);
        start = finish = data_allocator::allocate(n);
        end_of_storage = start+n;
        finish = vcl_uninitialized_copy(__ptr(first), __ptr(last), start);
    }
    ~vcl_vector() {}
    inline self& operator=(const self& x);
    void reserve(size_type n) {
        const size_type old_size(size());
        if (capacity() < n) {
            pointer tmp = data_allocator::allocate(n);
            IUEg__TRY {
                vcl_uninitialized_copy(begin_(), end_(), tmp);
                vcl_destroy(start, finish);
                deallocate();
                finish = tmp + old_size;
                start = tmp;
                end_of_storage = begin_() + n;
            }
# if defined (__STL_USE_EXCEPTIONS)
            catch(...) {
                data_allocator::deallocate(tmp, n);
                throw;
            }
# endif
        }
    }
    reference front() {
        __stl_verbose_assert(!empty(), __STL_MSG_EMPTY_CONTAINER);
        return *begin_();
    }
    const_reference front() const {
        __stl_verbose_assert(!empty(), __STL_MSG_EMPTY_CONTAINER);
        return *begin_();
    }
    reference back() {
        __stl_verbose_assert(!empty(), __STL_MSG_EMPTY_CONTAINER);
        return *(end_() - 1);
    }
    const_reference back() const {
        __stl_verbose_assert(!empty(), __STL_MSG_EMPTY_CONTAINER);
        return *(end_() - 1);
    }
    void push_back(const T& x) {
        if (finish != end_of_storage) {
            vcl_construct(finish, x);
            ++finish;
        } else
            insert_aux(end_(), x);
    }
    void swap(self& x) {
        __stl_debug_do(swap_owners(x));
        vcl_swap(start, x.start);
        vcl_swap(finish, x.finish);
        vcl_swap(end_of_storage, x.end_of_storage);
    }
    iterator insert(iterator position, const T& x) {
        __stl_debug_check(__check_if_owner(this,position));
        size_type n = __ptr(position) - begin_();
        if (finish != end_of_storage && __ptr(position) == end_()) {
            vcl_construct(finish, x);
            ++finish;
        } else
            insert_aux(__ptr(position), x);
        return begin()+n;
    }
    iterator insert(iterator position) { return insert(position, T()); }
    inline void insert (iterator position, size_type n, const T& x);
    inline void insert (iterator position, const_iterator first, const_iterator last);

    void pop_back() {
        __stl_verbose_assert(!empty(), __STL_MSG_EMPTY_CONTAINER);
        --finish;
        vcl_destroy(finish);
    }
    void erase(iterator position) {
        __stl_debug_check(__check_if_owner(this,position));
        __stl_verbose_assert(__ptr(position)!=finish,__STL_MSG_ERASE_PAST_THE_END);
        if (__ptr(position) + 1 != end_())
            vcl_copy(__ptr(position) + 1, end_(), __ptr(position));
        __stl_debug_do(invalidate(__ptr(position),finish));
        --finish;
        vcl_destroy(finish);
    }
    void erase(iterator first, iterator last) {
        __stl_debug_check(__check_if_owner(this,first)
                          &&__check_range(first,last));
        pointer i = vcl_copy(__ptr(last), end_(), __ptr(first));
        vcl_destroy(i, finish);
        __stl_debug_do(invalidate(__ptr(first),finish));
        finish = finish - (__ptr(last) - __ptr(first));
    }
    void resize(size_type new_size, const T& x) {
        if (new_size < size())
            erase(begin() + new_size, end());
        else
            insert(end(), new_size - size(), x);
    }
    void resize(size_type new_size) { resize(new_size, T()); }
    void clear() { erase(begin(), end()); }
};

template <class T, class Alloc>
vcl_vector<T, Alloc>& vcl_vector<T, Alloc>::operator=(const vcl_vector<T, Alloc>& x) {
    if (&x == this) return *this;
    __stl_debug_do(invalidate_all());
    if (x.size() > capacity()) {
        vcl_destroy(start, finish);
        deallocate();
        start = finish = 0;
        start = finish = data_allocator::allocate(x.size());
        end_of_storage = start + (x.size());
        vcl_uninitialized_copy(x.begin_(), x.end_(), start);
    } else if (size() >= x.size()) {
        pointer i = vcl_copy(x.begin_(), x.end_(), begin_());
        vcl_destroy(i, finish);
    } else {
        vcl_copy(x.begin_(), x.begin_() + size(), begin_());
        vcl_uninitialized_copy(x.begin_() + size(), x.end_(), begin_() + size());
    }
    finish = begin_() + x.size();
    return *this;
}

template <class T, class Alloc>
void vcl_vector<T, Alloc>::insert_aux(__pointer__ position, const T& x) {
    if (finish != end_of_storage) {
        vcl_construct(finish, *(finish - 1));
        ++finish;
        T x_copy = x;
        vcl_copy_backward(position, finish - 2, finish-1);
        *position = x_copy;
    } else {
        const size_type old_size = size();
        const size_type len = old_size != 0 ? 2 * old_size : 1;
        pointer tmp = data_allocator::allocate(len);
        pointer tmp_end = tmp;
        IUEg__TRY {
            tmp_end = vcl_uninitialized_copy(begin_(), position, tmp);
            vcl_construct(tmp_end, x);
            ++tmp_end;
            tmp_end = vcl_uninitialized_copy(position, end_(), tmp_end);
            vcl_destroy(begin_(), end_());
            deallocate();
            end_of_storage = tmp + len;
            finish = tmp_end;
            start = tmp;
        }
# if defined (__STL_USE_EXCEPTIONS)
        catch(...) {
            vcl_destroy(tmp, tmp_end);
            data_allocator::deallocate(tmp, len);
            throw;
        }
# endif
    }
}

template <class T, class Alloc>
void vcl_vector<T, Alloc>::insert(__iterator__ position, __size_type__ n, const T& x) {
    if (n == 0) return;
    if (end_of_storage - finish >= (difference_type)n) {
        pointer old_end = end_();
        size_type distance_to_end = end_() - position;
        if (distance_to_end > n) {
            vcl_uninitialized_copy(end_() - n, end_(), end_());
            finish += n;
            vcl_copy_backward(position, old_end - n, old_end);
            vcl_fill(position, position + n, x);
        } else {
            vcl_uninitialized_fill_n(end_(), n - distance_to_end, x);
            finish += n - distance_to_end;
            vcl_uninitialized_copy(position, old_end, end());
            finish += distance_to_end;
            vcl_fill(position, old_end, x);
        }
        __stl_debug_do(invalidate(position,old_end));
    } else {
        const size_type old_size = size();
        const size_type len = old_size + vcl_max(old_size, n);
        pointer tmp = data_allocator::allocate(len);
        pointer tmp_end = tmp;
        IUEg__TRY {
            tmp_end = vcl_uninitialized_copy(begin_(), position, tmp);
            tmp_end = vcl_uninitialized_fill_n(tmp_end, n, x);
            tmp_end = vcl_uninitialized_copy(position, end_(), tmp_end);
            vcl_destroy(begin_(), end_());
            deallocate();
            end_of_storage = tmp + len;
            finish = tmp_end;
            start = tmp;
        }
# if defined (__STL_USE_EXCEPTIONS)
        catch(...) {
            vcl_destroy(tmp, tmp_end);
            data_allocator::deallocate(tmp, len);
            throw;
        }
# endif
    }
}

template <class T, class Alloc>
void vcl_vector<T, Alloc>::insert(__iterator__ position, __const_iterator__ first,
                                  __const_iterator__ last) {
    if (first == last) return;
    size_type n = 0;
    vcl_distance(first, last, n);
    if (end_of_storage - finish >= (difference_type)n) {
        pointer old_end = end_();
        size_type distance_to_end = end_() - position;
        if (end_() - position > (difference_type)n) {
            vcl_uninitialized_copy(end_() - n, end_(), end_());
            finish += n;
            vcl_copy_backward(position, old_end - n, old_end);
            vcl_copy(first, last, position);
        } else {
            vcl_uninitialized_copy(first + distance_to_end, last, end_());
            finish += n - distance_to_end;
            vcl_uninitialized_copy(position, old_end, end_());
            finish += distance_to_end;
            vcl_copy(first, first + distance_to_end, position);
        }
        __stl_debug_do(invalidate(position,old_end));
    } else {
        const size_type old_size = size();
        const size_type len = old_size + vcl_max(old_size, n);
        pointer tmp = data_allocator::allocate(len);
        pointer tmp_end = tmp;
        IUEg__TRY {
            tmp_end = vcl_uninitialized_copy(begin_(), position, tmp);
            tmp_end = vcl_uninitialized_copy(first, last, tmp_end);
            tmp_end = vcl_uninitialized_copy(position, end_(), tmp_end);
            vcl_destroy(begin_(), end_());
            deallocate();
            end_of_storage = tmp + len;
            finish = tmp_end;
            start = tmp;
        }
# if defined (__STL_USE_EXCEPTIONS)
        catch(...) {
            vcl_destroy(tmp, tmp_end);
            data_allocator::deallocate(tmp, len);
            throw;
        }
# endif
    }
}

// do a cleanup
# undef  vcl_vector
# undef  __iterator__
# undef  __const_iterator__
# undef  __pointer__
# undef  __const_pointer__
# undef  __size_type__
# undef  __ptr
# undef  __difference_type__

// provide a uniform way to access full functionality
#  define __vector__ __FULL_NAME(vcl_vector)
__END_STL_FULL_NAMESPACE

# if !defined(VECTOR_H) && !defined(__STL_DEFAULT_TYPE_PARAM) && ( !defined(__STL_NAMESPACES) || defined(__STL_NO_NAMESPACES))
// provide a "default" vcl_vector adaptor
template <class T>
class vcl_vector : public __vector__<T,vcl_alloc>
{
    typedef vcl_vector<T> self;
public:
    typedef __vector__<T,vcl_alloc> super;
    __CONTAINER_SUPER_TYPEDEFS
    __IMPORT_SUPER_COPY_ASSIGNMENT(vcl_vector)
    vcl_vector() {}
    explicit vcl_vector(size_type n, const T& value) : super(n, value) { }
    explicit vcl_vector(size_type n) : super(n) { }
    vcl_vector(const_iterator first, const_iterator last) : super(first,last) { }
    ~vcl_vector() {}
};

#  if defined (__STL_BASE_MATCH_BUG)
template <class T>
    inline bool operator==(const vcl_vector<T>& x, const vcl_vector<T>& y) {
    typedef  __vector__<T,vcl_alloc> super;
    return operator == ((const super&)x,(const super&)y);
}

template <class T>
    inline bool operator<(const vcl_vector<T>& x, const vcl_vector<T>& y) {
    typedef  __vector__<T,vcl_alloc> super;
    return operator < ((const super&)x,(const super&)y);
}
#  endif /* __STL_BASE_MATCH_BUG */
# endif /* __STL_DEFAULT_TYPE_PARAM */

template <class T, class Alloc>
    inline bool operator==(const __vector__<T, Alloc>& x, const __vector__<T, Alloc>& y) {
    return x.size() == y.size() && vcl_equal(x.begin_(), x.end_(), y.begin_());
}

template <class T, class Alloc>
    inline bool operator!=(const __vector__<T, Alloc>& x, const __vector__<T, Alloc>& y) {
    return !(x == y);
}

template <class T, class Alloc>
    inline bool operator<(const __vector__<T, Alloc>& x, const __vector__<T, Alloc>& y) {
    return lexicographical_compare(x.begin_(), x.end_(), y.begin_(), y.end_());
}

# if defined (__STL_CLASS_PARTIAL_SPECIALIZATION )
template <class T, class Alloc>
    inline void vcl_swap(__vector__<T,Alloc>& a, __vector__<T,Alloc>& b) { a.swap(b); }
# endif

#endif // vcl_emulation_vector_h
