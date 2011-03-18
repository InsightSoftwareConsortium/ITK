// This is core/vnl/vnl_alloc.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif

#include "vnl_alloc.h"

#include <vcl_cstring.h>  // memcpy() lives here.
#include <vcl_cstdlib.h>

char*
vnl_alloc::chunk_alloc(vcl_size_t size, int& nobjs)
{
  char * result;
  vcl_size_t total_bytes = size * nobjs;
  vcl_size_t bytes_left = end_free - start_free;

  if (bytes_left >= total_bytes) {
    result = start_free;
    start_free += total_bytes;
    return result;
  } else if (bytes_left >= size) {
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
    if (bytes_left > 0) {
      obj *  * my_free_list =
        free_list + FREELIST_INDEX(bytes_left);
      ((obj *)start_free) -> free_list_link = *my_free_list;
      *my_free_list = (obj *)start_free;
    }
    start_free = (char*)vcl_malloc(bytes_to_get);
    if (0 == start_free)
    {
      obj *  * my_free_list, *p;
      // Try to make do with what we have.  That can't
      // hurt.  We do not try smaller requests, since that tends
      // to result in disaster on multi-process machines.
      for (vcl_size_t i = size; i <= VNL_ALLOC_MAX_BYTES; i += VNL_ALLOC_ALIGN)
      {
        my_free_list = free_list + FREELIST_INDEX(i);
        p = *my_free_list;
        if (0 != p) {
          *my_free_list = p -> free_list_link;
          start_free = (char *)p;
          end_free = start_free + i;
          return chunk_alloc(size, nobjs);
          // Any leftover piece will eventually make it to the
          // right free vcl_list.
        }
      }
      start_free = (char*)vcl_malloc(bytes_to_get);
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
void* vnl_alloc::refill(vcl_size_t n)
{
  int nobjs = 20;
  char * chunk = chunk_alloc(n, nobjs);
  obj *  * my_free_list;
  obj * result;
  obj * current_obj, * next_obj;
  int i;

  if (1 == nobjs) return chunk;
  my_free_list = free_list + FREELIST_INDEX(n);

  /* Build free vcl_list in chunk */
  result = (obj *)chunk;
  *my_free_list = next_obj = (obj *)(chunk + n);
  for (i = 1; ; i++) {
    current_obj = next_obj;
    next_obj = (obj *)((char *)next_obj + n);
    if (nobjs - 1 == i) {
      current_obj -> free_list_link = 0;
      break;
    } else {
      current_obj -> free_list_link = next_obj;
    }
  }
  return result;
}

void*
vnl_alloc::reallocate(void *p,
                      vcl_size_t old_sz,
                      vcl_size_t new_sz)
{
  void * result;
  vcl_size_t copy_sz;

  if (old_sz > VNL_ALLOC_MAX_BYTES && new_sz > VNL_ALLOC_MAX_BYTES) {
    return vcl_realloc(p, new_sz);
  }
  if (ROUND_UP(old_sz) == ROUND_UP(new_sz)) return p;
  result = allocate(new_sz);
  copy_sz = new_sz > old_sz? old_sz : new_sz;
  vcl_memcpy(result, p, copy_sz);
  deallocate(p, old_sz);
  return result;
}

char *vnl_alloc::start_free = 0;
char *vnl_alloc::end_free = 0;
vcl_size_t vnl_alloc::heap_size = 0;

vnl_alloc::obj *
vnl_alloc::free_list[VNL_ALLOC_NFREELISTS] =
{
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
};
// The 32 zeros are necessary to make version 4.1 of the SunPro
// compiler happy.  Otherwise it appears to allocate too little
// space for the array.

#ifdef TEST
#include <vcl_iostream.h>
int main()
{
  char* p = (char*)vnl_alloc::allocate(10);
  vcl_strcpy(p, "fred\n");
  vcl_cerr << p << '\n';
  vnl_alloc::deallocate(p,10);
}

#endif // TEST
