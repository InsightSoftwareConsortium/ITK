/*
 * Copyright (c) 1996-1997
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
#ifndef vcl_emulation_alloc_h
#define vcl_emulation_alloc_h
//:
// \file
// \brief This implements some standard node allocators
//
// These are \b NOT the same as the allocators in the C++ draft standard or in
// in the original STL.  They do not encapsulate different pointer
// types; indeed we assume that there is only one pointer type.
// The allocation primitives are intended to allocate individual objects,
// not larger arenas as with the original STL allocators.
//
// \verbatim
// Modifications
//   180598 AWF Indented ifdefs properly. Very important task that.
// \endverbatim

#include "vcl_stlconf.h"

#ifndef __ALLOC
#   define __ALLOC vcl_alloc
#endif

//#include <vcl_cstdlib.h>
#include <vcl_cstddef.h>
#include <vcl_cstring.h>
#include <vcl_cassert.h>

#ifndef __RESTRICT
#  define __RESTRICT
#endif

#if !defined(_PTHREADS) && !defined(_NOTHREADS) \
 && !defined(__STL_SGI_THREADS) && !defined(__STL_WIN32THREADS)
#  define _NOTHREADS
#endif

#include "vcl_bool.h"

#if !defined ( __STL_NO_EXCEPTIONS )
# if defined (__STL_BAD_ALLOC_DEFINED)
#   include <vcl_new.h>
# else
    struct bad_alloc {};
# endif
#   define __THROW_BAD_ALLOC throw bad_alloc()
#elif !defined(__THROW_BAD_ALLOC)
extern void vcl_alloc_throw_bad_alloc(char const *, int);
#   define __THROW_BAD_ALLOC vcl_alloc_throw_bad_alloc(__FILE__, __LINE__)
#endif

# if defined ( __STL_USE_ABBREVS )
// ugliness is intentional - to reduce conflicts probability
#  define __malloc_alloc   vcl_MA
#  define __alloc  vcl_DA
# endif

//: Allocator adaptor to check size arguments for debugging.
// Reports errors using assert.  Checking can be disabled with
// NDEBUG, but it's far better to just use the underlying allocator
// instead when no checking is desired.
// There is some evidence that this can confuse Purify.
template <class Alloc>
class debug_alloc
{
 public:
  typedef Alloc allocator_type;
  typedef typename Alloc::value_type value_type; //awf for SGI
 private:
#if !__STL_EAGER_TYPECHECK
  enum {
    type_size=sizeof(Alloc::value_type), // awf
    safe_size=(type_size>0 ? type_size :1),
    extra_chunk=8/safe_size+(int)(8%safe_size>0),
    extra = 8
  };
#else
#define type_size (sizeof(Alloc::value_type))
#define safe_size (type_size()>0 ? type_size() :1)
#define extra_chunk (8/safe_size+(int)(8%safe_size>0))
#define extra 8
#endif

  // Size of space used to store size.  Note that this must be
  // large enough to preserve alignment.
 public:
  static void * allocate(vcl_size_t n)
  {
    char *result = (char *)allocator_type::allocate(n + extra_chunk);
    *(vcl_size_t *)result = n;
    return result + extra;
  }

  static void deallocate(void *p, vcl_size_t n)
  {
    char * real_p = (char *)p - extra;
    assert(*(vcl_size_t *)real_p == n);
    allocator_type::deallocate(real_p, n + extra);
  }

  static void *
  reallocate(void *p, vcl_size_t old_sz, vcl_size_t new_sz)
  {
    char * real_p = (char *)p - extra;
    assert(*(vcl_size_t *)real_p == old_sz);
    char * result = (char *)
        allocator_type::reallocate(real_p, old_sz + extra_chunk, new_sz + extra_chunk);
    *(vcl_size_t *)result = new_sz;
    return result + extra;
  }
#undef type_size
#undef safe_size
#undef extra_chunk
#undef extra
};

// That is an adaptor for working with any alloc provided below
template<class T, class Alloc>
class vcl_simple_alloc
{
  typedef Alloc alloc_type;
 public:
  typedef typename Alloc::value_type alloc_value_type; // awf
  typedef T value_type;

#if !__STL_EAGER_TYPECHECK
  enum {
    chunk = sizeof(value_type)/sizeof(alloc_value_type)+(sizeof(value_type)%sizeof(alloc_value_type)>0)
  };
#else
  // note: any out-of-line template definitions will not see this.
#define chunk (sizeof(value_type)/sizeof(alloc_value_type)+(sizeof(value_type)%sizeof(alloc_value_type)>0))
#endif
  static value_type *allocate(vcl_size_t n) { return 0 == n? 0 : (value_type*) alloc_type::allocate(n * chunk); }
  static value_type *allocate(void) { return (value_type*) alloc_type::allocate(chunk); }
  static void deallocate(value_type *p, vcl_size_t n) { if (0 != n) alloc_type::deallocate(p, n * chunk); }
  static void deallocate(value_type *p) { alloc_type::deallocate(p, chunk); }
#undef chunk
};


// New-based allocator.  Typically slower than default alloc below.
// Typically thread-safe and more storage efficient.
template <int inst>
class __new_alloc
{
 public:
  // this one is needed for proper vcl_simple_alloc wrapping
  typedef char value_type;
  static void*  allocate(vcl_size_t n) { return 0 == n ? 0 : ::operator new(n);}
  static void*  reallocate(void *p, vcl_size_t old_sz, vcl_size_t new_sz)
  {
    void* result = allocate(new_sz);
    vcl_size_t copy_sz = new_sz > old_sz? old_sz : new_sz;
    vcl_memcpy(result, p, copy_sz);
    deallocate(p, old_sz);
    return result;
  }
  static void deallocate(void* p) { ::operator delete(p); }
  static void deallocate(void* p, vcl_size_t) { ::operator delete(p); }
};

typedef __new_alloc<0> new_alloc;

// Malloc-based allocator.  Typically slower than default alloc below.
// Typically thread-safe and more storage efficient.

typedef void (* __oom_handler_type)();

template <int inst>
class __malloc_alloc
{
 private:
  static void *oom_malloc(vcl_size_t);
  static void *oom_realloc(void *, vcl_size_t);
  static __oom_handler_type oom_handler;

 public:
  // this one is needed for proper vcl_simple_alloc wrapping
  typedef char value_type;

  static void * allocate(vcl_size_t n)
  {
    void *result = malloc(n);
    if (0 == result) result = oom_malloc(n);
    return result;
  }

  static void deallocate(void *p, vcl_size_t /* n */) { free(p); }

  static void * reallocate(void *p, vcl_size_t /* old_sz */, vcl_size_t new_sz)
  {
    void * result = realloc(p, new_sz);
    if (0 == result) result = oom_realloc(p, new_sz);
    return result;
  }

  static __oom_handler_type set_malloc_handler(__oom_handler_type f)
  {
    __oom_handler_type old = oom_handler;
    oom_handler = f;
    return old;
  }
};

// malloc_alloc out-of-memory handling
# if ( __STL_STATIC_TEMPLATE_DATA > 0 )
template <int inst>
__oom_handler_type __malloc_alloc<inst>::oom_handler=(__oom_handler_type)0;
#  else
__DECLARE_INSTANCE(__oom_handler_type, __malloc_alloc<0>::oom_handler,0);
# endif /* ( __STL_STATIC_TEMPLATE_DATA > 0 ) */

template <int inst>
void * __malloc_alloc<inst>::oom_malloc(vcl_size_t n)
{
  __oom_handler_type my_malloc_handler;
  void *result = 0;

  while (!result) {
    my_malloc_handler = oom_handler;
    if (0 == my_malloc_handler) { __THROW_BAD_ALLOC; }
    (*my_malloc_handler)();
    result = malloc(n);
  }
  return result;
}

template <int inst>
void * __malloc_alloc<inst>::oom_realloc(void *p, vcl_size_t n)
{
  __oom_handler_type my_malloc_handler;
  void *result = 0;

  while (!result) {
    my_malloc_handler = oom_handler;
    if (0 == my_malloc_handler) { __THROW_BAD_ALLOC; }
    (*my_malloc_handler)();
    result = realloc(p, n);
  }
  return result;
}

typedef __malloc_alloc<0> vcl_malloc_alloc;

# if defined ( __STL_USE_NEWALLOC )
#  if defined ( __STL_DEBUG_ALLOC )
    typedef debug_alloc<new_alloc> vcl_alloc;
#  else
    typedef new_alloc vcl_alloc;
#  endif
   typedef new_alloc single_client_alloc;
   typedef new_alloc multithreaded_alloc;
# else /* ! __STL_USE_NEWALLOC */
#  ifdef __STL_USE_MALLOC
#   if defined ( __STL_DEBUG_ALLOC )
     typedef debug_alloc<vcl_malloc_alloc> vcl_alloc;
#   else
     typedef vcl_malloc_alloc vcl_alloc;
#   endif
typedef vcl_malloc_alloc single_client_alloc;
typedef vcl_malloc_alloc multithreaded_alloc;
#  else /* ! __STL_USE_MALLOC */
// global-level stuff

// fbp : put all this stuff here
#   ifdef _NOTHREADS
//  Thread-unsafe
#    define __NODE_ALLOCATOR_LOCK
#    define __NODE_ALLOCATOR_UNLOCK
#    define __NODE_ALLOCATOR_THREADS false
#    define __VOLATILE
#   else /* ! _NOTHREADS */
#    ifdef _PTHREADS
       // POSIX Threads
       // This is dubious, since this is likely to be a high contention
       // lock.  The Posix standard appears to require an implemention
       // that makes convoy effects likely.  Performance may not be
       // adequate.
#      include <pthread.h>
//     pthread_mutex_t __node_allocator_lock = PTHREAD_MUTEX_INITIALIZER;
#      define __NODE_ALLOCATOR_LOCK \
                  if (threads) pthread_mutex_lock(&__node_allocator_lock)
#      define __NODE_ALLOCATOR_UNLOCK \
                  if (threads) pthread_mutex_unlock(&__node_allocator_lock)
#      define __NODE_ALLOCATOR_THREADS true
#      define __VOLATILE volatile  // Needed at -O3 on SGI
#    endif /* _PTHREADS */
#    ifdef __STL_WIN32THREADS
#      if !defined  (__STL_WINDOWS_H_INCLUDED)
#        define NOMINMAX
//#      include <windows.h>
#        undef min
#        undef max
#      endif
#      ifndef WIN32_LEAN_AND_MEAN
#       define WIN32_LEAN_AND_MEAN
#      endif
       // include windows.h outside #if !defined (__STL_WINDOWS_H_INCLUDED)
       // because including windows.h can cause the #if/#endif nesting
       // to exceed the maximum supported by Visual C++ (and windows.h
       // has an #ifndef _WINDOWS_ / #endif guard)
#      include <windows.h>
//       CRITICAL_SECTION __node_allocator_lock;
//       bool __node_allocator_lock_initialized;
//     this one is needed to ensure correct initialization order
//     and to avoid excess instances
       struct __stl_critical_section_wrapper {
                 CRITICAL_SECTION section;
                 __stl_critical_section_wrapper() {
                     InitializeCriticalSection(&section);
                 }
       };
#      define __NODE_ALLOCATOR_LOCK \
                 EnterCriticalSection(&__node_allocator_lock.section)
#      define __NODE_ALLOCATOR_UNLOCK \
                 LeaveCriticalSection(&__node_allocator_lock.section)
#      define __NODE_ALLOCATOR_THREADS true
#      define __VOLATILE volatile  // may not be needed
#    endif /* __STL_WIN32THREADS */
#    ifdef __STL_SGI_THREADS
      // This should work without threads, with sproc threads, or with
      // pthreads.  It is suboptimal in all cases.
      // It is unlikely to even compile on nonSGI machines.
#     include <malloc.h>
#     define __NODE_ALLOCATOR_LOCK if (threads && __us_rsthread_malloc) \
                       { __lock(&__node_allocator_lock); }
#     define __NODE_ALLOCATOR_UNLOCK if (threads && __us_rsthread_malloc) \
                       { __unlock(&__node_allocator_lock); }
#     define __NODE_ALLOCATOR_THREADS true
#     define __VOLATILE volatile  // Needed at -O3 on SGI
#    endif /* __STL_SGI_THREADS */
#   endif /* _NOTHREADS */

    // Default node allocator.
    // With a reasonable compiler, this should be roughly as fast as the
    // original STL class-specific allocators, but with less fragmentation.
    // Default_alloc_template parameters are experimental and MAY
    // DISAPPEAR in the future.  Clients should just use vcl_alloc for now.
    //
    // Important implementation properties:
    // 1. If the client request an object of size > __MAX_BYTES, the resulting
    //    object will be obtained directly from malloc.
    // 2. In all other cases, we allocate an object of size exactly
    //    ROUND_UP(requested_size).  Thus the client has enough size
    //    information that we can return the object to the proper free vcl_list
    //    without permanently losing part of the object.
    //

    // The first template parameter specifies whether more than one thread
    // may use this allocator.  It is safe to allocate an object from
    // one instance of a default_alloc and deallocate it with another
    // one.  This effectively transfers its ownership to the second one.
    // This may have undesirable effects on reference locality.
    // The second parameter is unreferenced and serves only to allow the
    // creation of multiple default_alloc instances.
    // Node that containers built on different allocator instances have
    // different types, limiting the utility of this approach.
#    if defined ( __SUNPRO_CC ) || defined ( _AIX )
    // breaks if we make these template class members:
      enum {__ALIGN = 8};
      enum {__MAX_BYTES = 128};
      enum {__NFREELISTS = __MAX_BYTES/__ALIGN};
#    endif

    template <bool threads, int inst>
    class __alloc
    {
     __PRIVATE:
      // Really we should use static const int x = N
      // instead of enum { x = N }, but few compilers accept the former.
#     if ! (defined ( __SUNPRO_CC ) || defined ( _AIX ))
            enum {__ALIGN = 8};
            enum {__MAX_BYTES = 128};
            enum {__NFREELISTS = __MAX_BYTES/__ALIGN};
#     endif


     private:
      static vcl_size_t ROUND_UP(vcl_size_t bytes) { return (((bytes) + __ALIGN-1) & ~(__ALIGN - 1)); }
     __PRIVATE:
      union obj;
      friend union obj;
      union obj {
        union obj * free_list_link;
        char client_data[1];    /* The client sees this.        */
      };
     private:
#     if defined ( __SUNPRO_CC ) || defined ( _AIX )
      static obj * __VOLATILE free_list[];
      // Specifying a size results in duplicate def for 4.1
#     else
      static obj * __VOLATILE free_list[__NFREELISTS];
#     endif
      static  vcl_size_t FREELIST_INDEX(vcl_size_t bytes) { return (((bytes) + __ALIGN-1)/__ALIGN - 1); }

      // Returns an object of size n, and optionally adds to size n free vcl_list.
      static void *refill(vcl_size_t n);
      // Allocates a chunk for nobjs of size size.  nobjs may be reduced
      // if it is inconvenient to allocate the requested number.
      static char *chunk_alloc(vcl_size_t size, int &nobjs);

      // Chunk allocation state.
      static char *start_free;
      static char *end_free;
      static vcl_size_t heap_size;

#     ifdef __STL_SGI_THREADS
      static volatile unsigned long __node_allocator_lock;
      static void __lock(volatile unsigned long *);
      static inline void __unlock(volatile unsigned long *);
#     endif

#     ifdef _PTHREADS
      static pthread_mutex_t __node_allocator_lock;
#     endif

#     ifdef __STL_WIN32THREADS
      static __stl_critical_section_wrapper __node_allocator_lock;
#     endif

      class lock
      {
       public:
        lock() { __NODE_ALLOCATOR_LOCK; }
       ~lock() { __NODE_ALLOCATOR_UNLOCK; }
      };
      friend class lock;

     public:
      // this one is needed for proper vcl_simple_alloc wrapping
      typedef char value_type;

      /* n must be > 0      */
      static void * allocate(vcl_size_t n)
      {
        obj * __VOLATILE * my_free_list;
        obj * __RESTRICT result;

        if (n > __MAX_BYTES) {
            return vcl_malloc_alloc::allocate(n);
        }
        my_free_list = free_list + FREELIST_INDEX(n);
        // Acquire the lock here with a constructor call.
        // This ensures that it is released in exit or during stack
        // unwinding.
            /*REFERENCED*/
#     if !defined (_NOTHREADS)
        lock lock_instance;
#     endif
        result = *my_free_list;
        if (result == 0) {
            void *r = refill(ROUND_UP(n));
            return r;
        }
        *my_free_list = result -> free_list_link;
        return result;
      };

      /* p may not be 0 */
      static void deallocate(void *p, vcl_size_t n)
      {
        obj *q = (obj *)p;
        obj * __VOLATILE * my_free_list;

        if (n > __MAX_BYTES) {
            vcl_malloc_alloc::deallocate(p, n);
            return;
        }
        my_free_list = free_list + FREELIST_INDEX(n);
        // acquire lock
#     if !defined (_NOTHREADS)
        /*REFERENCED*/
        lock lock_instance;
#     endif
        q -> free_list_link = *my_free_list;
        *my_free_list = q;
        // lock is released here
      }

      static void * reallocate(void *p, vcl_size_t old_sz, vcl_size_t new_sz);
    };

    typedef __alloc<__NODE_ALLOCATOR_THREADS, 0> node_alloc;
#       if defined ( __STL_DEBUG_ALLOC )
    typedef debug_alloc<node_alloc> vcl_alloc;
#       else
    typedef node_alloc vcl_alloc;
#       endif
    typedef __alloc<false, 0> single_client_alloc;
    typedef __alloc<true, 0>  multithreaded_alloc;

    /* We allocate memory in large chunks in order to avoid fragmenting     */
    /* the malloc heap too much.                                            */
    /* We assume that size is properly aligned.                             */
    /* We hold the allocation lock.                                         */
    template <bool threads, int inst>
    char*
    __alloc<threads, inst>::chunk_alloc(vcl_size_t size, int& nobjs)
    {
      char * result;
      vcl_size_t total_bytes = size * nobjs;
      vcl_size_t bytes_left = end_free - start_free;

      if (bytes_left >= total_bytes)
      {
        result = start_free;
        start_free += total_bytes;
        return result;
      }
      else if (bytes_left >= size)
      {
          nobjs = bytes_left/size;
          total_bytes = size * nobjs;
          result = start_free;
          start_free += total_bytes;
          return result;
      }
      else
      {
        vcl_size_t bytes_to_get = 2 * total_bytes + ROUND_UP(heap_size >> 4);
        // Try to make use of the left-over piece.
        if (bytes_left > 0)
        {
          obj * __VOLATILE * my_free_list = free_list + FREELIST_INDEX(bytes_left);
          ((obj *)start_free) -> free_list_link = *my_free_list;
          *my_free_list = (obj *)start_free;
        }
        start_free = (char *)malloc(bytes_to_get);
        if (0 == start_free)
        {
          obj * __VOLATILE * my_free_list, *p;
          // Try to make do with what we have.  That can't
          // hurt.  We do not try smaller requests, since that tends
          // to result in disaster on multi-process machines.
          for (int i = size; i <= __MAX_BYTES; i += __ALIGN)
          {
            my_free_list = free_list + FREELIST_INDEX(i);
            p = *my_free_list;
            if (0 != p)
            {
              *my_free_list = p -> free_list_link;
              start_free = (char *)p;
              end_free = start_free + i;
              return chunk_alloc(size, nobjs);
              // Any leftover piece will eventually make it to the
              // right free vcl_list.
            }
          }
          start_free = (char *)vcl_malloc_alloc::allocate(bytes_to_get);
          // This should either throw an
          // exception or remedy the situation.  Thus we assume it
          // succeeded.
        }
        heap_size += bytes_to_get;
        end_free = start_free + bytes_to_get;
        return chunk_alloc(size, nobjs);
      }
    }

    /* Returns an object of size n, and optionally adds to size n free vcl_list.*/
    /* We assume that n is properly aligned.                                */
    /* We hold the allocation lock.                                         */
    template <bool threads, int inst>
    void* __alloc<threads, inst>::refill(vcl_size_t n)
    {
      int nobjs = 20;
      char * chunk = chunk_alloc(n, nobjs);
      obj * __VOLATILE * my_free_list;
      obj * result;
      obj * current_obj, * next_obj;
      int i;

      if (1 == nobjs) return chunk;
      my_free_list = free_list + FREELIST_INDEX(n);

      /* Build free vcl_list in chunk */
      result = (obj *)chunk;
      *my_free_list = next_obj = (obj *)(chunk + n);
      for (i = 1; true; i++)
      {
        current_obj = next_obj;
        next_obj = (obj *)((char *)next_obj + n);
        if (nobjs - 1 == i) { current_obj -> free_list_link = 0; break; }
        else { current_obj -> free_list_link = next_obj; }
      }
      return result;
    }

    template <bool threads, int inst>
    void*
    __alloc<threads, inst>::reallocate(void *p,
                                       vcl_size_t old_sz,
                                       vcl_size_t new_sz)
    {
      void * result;
      vcl_size_t copy_sz;

      if (old_sz > __MAX_BYTES && new_sz > __MAX_BYTES)
        return realloc(p, new_sz);
      if (ROUND_UP(old_sz) == ROUND_UP(new_sz)) return p;
      result = allocate(new_sz);
      copy_sz = new_sz > old_sz? old_sz : new_sz;
      vcl_memcpy(result, p, copy_sz);
      deallocate(p, old_sz);
      return result;
    }

#   ifdef __STL_SGI_THREADS
#    include <mutex.h>
#    include <vcl_ctime.h>
    // Somewhat generic lock implementations.  We need only test-and-set
    // and some way to sleep.  These should work with both SGI pthreads
    // and sproc threads.  They may be useful on other systems.
#    if __mips < 3 || !(defined (_ABIN32) || defined(_ABI64)) || defined(__GNUC__)
#       define __test_and_set(l,v) test_and_set(l,v)
#    endif

    template <bool threads, int inst>
    void
    __alloc<threads, inst>::__lock(volatile unsigned long *lock)
    {
      const unsigned low_spin_max = 30;  // spin cycles if we suspect uniprocessor
      const unsigned high_spin_max = 1000; // spin cycles for multiprocessor
      static unsigned spin_max = low_spin_max;
      unsigned my_spin_max;
      static unsigned last_spins = 0;
      unsigned my_last_spins;
      static struct timespec ts = {0, 1000};
      unsigned junk;
#       define __ALLOC_PAUSE junk *= junk; junk *= junk; junk *= junk; junk *= junk
      if (!__test_and_set((unsigned long *)lock, 1)) return;
      my_spin_max = spin_max;
      my_last_spins = last_spins;
      for (int i = 0; i < my_spin_max; i++)
      {
        if (i < my_last_spins/2 || *lock) {
          __ALLOC_PAUSE;
          continue;
        }
        if (!__test_and_set((unsigned long *)lock, 1)) {
          // got it!
          // Spinning worked.  Thus we're probably not being scheduled
          // against the other process with which we were contending.
          // Thus it makes sense to spin longer the next time.
          last_spins = i;
          spin_max = high_spin_max;
          return;
        }
      }
      // We are probably being scheduled against the other process.  Sleep.
      spin_max = low_spin_max;
      for (;;) {
        if (!__test_and_set((unsigned long *)lock, 1)) return;
        nanosleep(&ts, 0);
      }
    }

    template <bool threads, int inst>
    inline void
    __alloc<threads, inst>::__unlock(volatile unsigned long *lock)
    {
#       if defined(__GNUC__) && __mips >= 3
      asm("sync");
      *lock = 0;
#       elif __mips >= 3 && (defined (_ABIN32) || defined(_ABI64))
      __lock_release(lock);
#       else
      *lock = 0;
      // This is not sufficient on many multiprocessors, since
      // writes to protected variables and the lock may be reordered.
#       endif
    }
#   endif /* ! __STL_SGI_THREADS */

#   if ( __STL_STATIC_TEMPLATE_DATA > 0 )

#    ifdef _PTHREADS
    template <bool threads, int inst>
    pthread_mutex_t
    __alloc<threads, inst>::__node_allocator_lock
    = PTHREAD_MUTEX_INITIALIZER;
#    endif

#     ifdef __STL_SGI_THREADS
    template <bool threads, int inst>
    volatile unsigned long
    __alloc<threads, inst>::__node_allocator_lock = 0;
#     endif

    template <bool threads, int inst>
    char *__alloc<threads, inst>::start_free = 0;

    template <bool threads, int inst>
    char *__alloc<threads, inst>::end_free = 0;

    template <bool threads, int inst>
    vcl_size_t __alloc<threads, inst>::heap_size = 0;

    template <bool threads, int inst>
    typename __alloc<threads, inst>::obj * __VOLATILE
    __alloc<threads, inst>::free_list[
#       if ! (defined ( __SUNPRO_CC ) || defined ( _AIX ))
          __alloc<threads, inst>::__NFREELISTS]
#       else
    __NFREELISTS]
#       endif
     = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, };
    // The 16 zeros are necessary to make version 4.1 of the SunPro
    // compiler happy.  Otherwise it appears to allocate too little
    // space for the array.

#       ifdef __STL_WIN32THREADS
    template <bool threads, int inst>
    __stl_critical_section_wrapper
    __alloc<threads, inst>::__node_allocator_lock;
#       endif
#     else /* ( __STL_STATIC_TEMPLATE_DATA > 0 ) */
    __DECLARE_INSTANCE(char *, single_client_alloc::start_free,0);
    __DECLARE_INSTANCE(char *, single_client_alloc::end_free,0);
    __DECLARE_INSTANCE(vcl_size_t, single_client_alloc::heap_size,0);
#       if defined ( __SUNPRO_CC ) || defined ( _AIX )
    __DECLARE_INSTANCE(single_client_alloc::obj * __VOLATILE,
                       single_client_alloc::free_list[__NFREELISTS],
                       {0});
#       else
    __DECLARE_INSTANCE(single_client_alloc::obj * __VOLATILE,
                       single_client_alloc::free_list[single_client_alloc::__NFREELISTS],
                       {0});
#       endif
    __DECLARE_INSTANCE(char *, multithreaded_alloc::start_free,0);
    __DECLARE_INSTANCE(char *, multithreaded_alloc::end_free,0);
    __DECLARE_INSTANCE(vcl_size_t, multithreaded_alloc::heap_size,0);
#       if defined ( __SUNPRO_CC ) || defined ( _AIX )
    __DECLARE_INSTANCE(multithreaded_alloc::obj * __VOLATILE,
                       multithreaded_alloc::free_list[__NFREELISTS],
                       {0});
#       else
    __DECLARE_INSTANCE(multithreaded_alloc::obj * __VOLATILE,
                       multithreaded_alloc::free_list[multithreaded_alloc::__NFREELISTS],
                       {0});
#       endif
#       ifdef __STL_WIN32THREADS
          __DECLARE_INSTANCE(__stl_critical_section_wrapper,
                             single_client_alloc::__node_allocator_lock,
                             __stl_critical_section_wrapper());
          __DECLARE_INSTANCE(__stl_critical_section_wrapper,
                             multithreaded_alloc::__node_allocator_lock,
                             __stl_critical_section_wrapper());
#       endif
#       ifdef _PTHREADS
           __DECLARE_INSTANCE(pthread_mutex_t,
                              single_client_alloc::__node_allocator_lock,
                              PTHREAD_MUTEX_INITIALIZER);
           __DECLARE_INSTANCE(pthread_mutex_t,
                              multithreaded_alloc::__node_allocator_lock,
                              PTHREAD_MUTEX_INITIALIZER);
#       endif
#       ifdef __STL_SGI_THREADS
           __DECLARE_INSTANCE(volatile unsigned long,
                              single_client_alloc::__node_allocator_lock,
                              0);
           __DECLARE_INSTANCE(volatile unsigned long,
                              multithreaded_alloc::__node_allocator_lock,
                              0);
#       endif

#   endif /* __STL_STATIC_TEMPLATE_DATA */

#  endif /* ! __STL_USE_MALLOC */
# endif /* ! __STL_USE_NEWALLOC */

# if defined ( __STL_USE_DEFALLOC )
#  include "vcl_defalloc.h"
# endif

#endif // vcl_emulation_alloc_h
