// This is core/vnl/vnl_alloc.h
#ifndef vnl_alloc_h_
#define vnl_alloc_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \author unknown
//
// \brief Default node allocator.
//
// With a reasonable compiler, this should be roughly as fast as the
// original STL class-specific allocators, but with less fragmentation.
// Default_alloc_template parameters are experimental and MAY
// DISAPPEAR in the future.  Clients should just use vcl_alloc for now.
//
// Important implementation properties:
// -  If the client request an object of size > __MAX_BYTES, the resulting
//    object will be obtained directly from malloc.
// -  In all other cases, we allocate an object of size exactly
//    ROUND_UP(requested_size).  Thus the client has enough size
//    information that we can return the object to the proper free li*st
//    without permanently losing part of the object.
//
// The first template parameter specifies whether more than one thread
// may use this allocator.  It is safe to allocate an object from
// one instance of a default_alloc and deallocate it with another
// one.  This effectively transfers its ownership to the second one.
// This may have undesirable effects on reference locality.
// The second parameter is unreferenced and serves only to allow the
// creation of multiple default_alloc instances.
//
// Note that containers built on different allocator instances have
// different types, limiting the utility of this approach.

#include <vcl_cstddef.h> // size_t lives here

const int VNL_ALLOC_ALIGN = 8;
const vcl_size_t VNL_ALLOC_MAX_BYTES = 256;
const vcl_size_t VNL_ALLOC_NFREELISTS = VNL_ALLOC_MAX_BYTES/VNL_ALLOC_ALIGN;

class vnl_alloc
{
  static vcl_size_t ROUND_UP(vcl_size_t bytes) {
    return (bytes + VNL_ALLOC_ALIGN-1) & ~(VNL_ALLOC_ALIGN - 1);
  }
  union obj;
  friend union obj;
  union obj {
    union obj * free_list_link;
    char client_data[1];    /* The client sees this.        */
  };
# if defined ( __SUNPRO_CC ) || defined ( _AIX )
  static obj * free_list[];
  // Specifying a size results in duplicate def for 4.1
# else
  static obj * free_list[VNL_ALLOC_NFREELISTS];
# endif
  static  vcl_size_t FREELIST_INDEX(vcl_size_t bytes) {
    return (bytes + VNL_ALLOC_ALIGN-1)/VNL_ALLOC_ALIGN - 1;
  }

  // Returns an object of size n, and optionally adds to size n free li*st.
  static void *refill(vcl_size_t n);
  // Allocates a chunk for nobjs of size size.  nobjs may be reduced
  // if it is inconvenient to allocate the requested number.
  static char *chunk_alloc(vcl_size_t size, int &nobjs);

  // Chunk allocation state.
  static char *start_free;
  static char *end_free;
  static vcl_size_t heap_size;

  class lock
  {
   public:
    lock() {}
    ~lock() {}
  };
  friend class lock;

 public:
  // this one is needed for proper vcl_simple_alloc wrapping
  typedef char value_type;

  /* n must be > 0      */
  static void * allocate(vcl_size_t n) {
    obj * * my_free_list;
    obj *  result;

    if (n > VNL_ALLOC_MAX_BYTES) {
      return (void*)new char[n];
    }
    my_free_list = free_list + FREELIST_INDEX(n);
    // Acquire the lock here with a constructor call.
    // This ensures that it is released in exit or during stack
    // unwinding.
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
    obj *  * my_free_list;

    if (n > VNL_ALLOC_MAX_BYTES) {
      delete [] (char*)p;
      return;
    }
    my_free_list = free_list + FREELIST_INDEX(n);
    q -> free_list_link = *my_free_list;
    *my_free_list = q;
  }

  static void * reallocate(void *p, vcl_size_t old_sz, vcl_size_t new_sz);
};

# endif // vnl_alloc_h_
