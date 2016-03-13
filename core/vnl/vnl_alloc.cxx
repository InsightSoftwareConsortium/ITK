// This is core/vnl/vnl_alloc.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif

#include <cstring>
#include <cstdlib>
#include <iostream>
#include "vnl_alloc.h"

#include <vcl_compiler.h>

char*
vnl_alloc::chunk_alloc(std::size_t size, int& nobjs)
{
  char * result;
  std::size_t total_bytes = size * nobjs;
  std::size_t bytes_left = end_free - start_free;

  if (bytes_left >= total_bytes) {
    result = start_free;
    start_free += total_bytes;
    return result;
  }
  else if (bytes_left >= size) {
    nobjs = int(bytes_left/size);
    total_bytes = size * nobjs;
    result = start_free;
    start_free += total_bytes;
    return result;
  }
  else
  {
    std::size_t bytes_to_get = 2 * total_bytes + ROUND_UP(heap_size >> 4);
    // Try to make use of the left-over piece.
    if (bytes_left > 0) {
      obj *  * my_free_list =
        free_list + FREELIST_INDEX(bytes_left);
      ((obj *)start_free) -> free_list_link = *my_free_list;
      *my_free_list = (obj *)start_free;
    }
    start_free = (char*)std::malloc(bytes_to_get);
    if (VXL_NULLPTR == start_free)
    {
      obj *  * my_free_list, *p;
      // Try to make do with what we have.  That can't
      // hurt.  We do not try smaller requests, since that tends
      // to result in disaster on multi-process machines.
      for (std::size_t i = size; i <= VNL_ALLOC_MAX_BYTES; i += VNL_ALLOC_ALIGN)
      {
        my_free_list = free_list + FREELIST_INDEX(i);
        p = *my_free_list;
        if (VXL_NULLPTR != p) {
          *my_free_list = p -> free_list_link;
          start_free = (char *)p;
          end_free = start_free + i;
          return chunk_alloc(size, nobjs);
          // Any leftover piece will eventually make it to the
          // right free std::list.
        }
      }
      start_free = (char*)std::malloc(bytes_to_get);
      // This should either throw an
      // exception or remedy the situation.  Thus we assume it
      // succeeded.
    }
    heap_size += bytes_to_get;
    end_free = start_free + bytes_to_get;
    return chunk_alloc(size, nobjs);
  }
}


/* Returns an object of size n, and optionally adds to size n free std::list.*/
/* We assume that n is properly aligned.                                */
/* We hold the allocation lock.                                         */
void* vnl_alloc::refill(std::size_t n)
{
  int nobjs = 20;
  char * chunk = chunk_alloc(n, nobjs);
  obj *  * my_free_list;
  obj * result;
  obj * current_obj, * next_obj;
  int i;

  if (1 == nobjs) return chunk;
  my_free_list = free_list + FREELIST_INDEX(n);

  /* Build free std::list in chunk */
  result = (obj *)chunk;
  *my_free_list = next_obj = (obj *)(chunk + n);
  for (i = 1; ; i++) {
    current_obj = next_obj;
    next_obj = (obj *)((char *)next_obj + n);
    if (nobjs - 1 == i) {
      current_obj -> free_list_link = VXL_NULLPTR;
      break;
    }
    else {
      current_obj -> free_list_link = next_obj;
    }
  }
  return result;
}

void*
vnl_alloc::reallocate(void *p,
                      std::size_t old_sz,
                      std::size_t new_sz)
{
  void * result;
  std::size_t copy_sz;

  if (old_sz > VNL_ALLOC_MAX_BYTES && new_sz > VNL_ALLOC_MAX_BYTES) {
    return std::realloc(p, new_sz);
  }
  if (ROUND_UP(old_sz) == ROUND_UP(new_sz)) return p;
  result = allocate(new_sz);
  copy_sz = new_sz > old_sz? old_sz : new_sz;
  std::memcpy(result, p, copy_sz);
  deallocate(p, old_sz);
  return result;
}

char *vnl_alloc::start_free = VXL_NULLPTR;
char *vnl_alloc::end_free = VXL_NULLPTR;
std::size_t vnl_alloc::heap_size = 0;

vnl_alloc::obj *
vnl_alloc::free_list[VNL_ALLOC_NFREELISTS] = { VXL_NULLPTR };

#ifdef TEST
int main()
{
  char* p = (char*)vnl_alloc::allocate(10);
  std::strcpy(p, "fred\n");
  std::cerr << p << '\n';
  vnl_alloc::deallocate(p,10);
}

#endif // TEST
