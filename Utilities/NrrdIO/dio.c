/*
  NrrdIO: stand-alone code for basic nrrd functionality
  Copyright (C) 2004, 2003, 2002, 2001, 2000, 1999, 1998 University of Utah
 
  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any
  damages arising from the use of this software.
 
  Permission is granted to anyone to use this software for any
  purpose, including commercial applications, and to alter it and
  redistribute it freely, subject to the following restrictions:
 
  1. The origin of this software must not be misrepresented; you must
     not claim that you wrote the original software. If you use this
     software in a product, an acknowledgment in the product
     documentation would be appreciated but is not required.
 
  2. Altered source versions must be plainly marked as such, and must
     not be misrepresented as being the original software.
 
  3. This notice may not be removed or altered from any source distribution.
*/


#include "NrrdIO.h"
#include "teemDio.h"

#if TEEM_DIO == 0
#else
/* HEY: these may be SGI-specific */
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#endif

#if TEEM_DIO == 0
const int airMyDio = 0;
#else
const int airMyDio = 1;
#endif

int airDisableDio = 0;

int
airDioInfo(int *mem, int *min, int *max, FILE *file) {
#if TEEM_DIO == 0
#else
  int fd;
  struct dioattr dioinfo;
#endif
  
#if TEEM_DIO == 0
  return 1;
#else
  if (!(mem && min && max && file))
    return 1;
  
  fd = fileno(file);
  if (-1 == fd) {
    /* couldn't get the underlying file descriptor */
    return 1;
  }
  if (0 != fcntl(fd, F_DIOINFO, &dioinfo)) {
    /* couldn't learn direct I/O specifics */
    return 1;
  }

  *mem = dioinfo.d_mem;
  *min = dioinfo.d_miniosz;
  *max = dioinfo.d_maxiosz;
  return 0;
#endif
}

int
airDioTest(size_t size, FILE *file, void *ptr) {
#if TEEM_DIO == 0
#else
  int fd;
  struct dioattr dioinfo;
  void *tmp;
#endif

#if TEEM_DIO == 0
  /* teem makefiles think no direct IO is possible on this architecture */
  return airNoDio_arch;
#else

  if (airDisableDio) {
    /* user turned direct I/O off */
    return airNoDio_disable;
  }

  if (!file) {
    /* didn't get a valid FILE * */
    return airNoDio_file;
  }

  fd = fileno(file);
  if (0 == fd || 1 == fd || 2 == fd) {
    /* This was added because I was noticing a problem with piping between
       unrrdu programs- sometimes the fread() of the receiving data through
       a unix pipe ("|") failed to read all the data.  If the body of this
       function was bypassed (with "return airNoDio_disable;", for instance),
       then the problem went away.  The problematic call seemed to be the
       fflush() below.  I don't think direct I/O is possible on stdin, stdout,
       or stdout, since the fcntl() call below fails on stdin and stdout.
       However, something about making that fcntl() call changes something
       which means that about half the time, the read() on a piped stdin 
       fails (on an irix6.n32 O2, at least). So, seems to be safest to just
       explicitly say that direct I/O is unavailable, based solely on the
       file descriptor number (0, 1, 2).  In other words, this is a hack.
       (this behavior was observed on the SGIs) */
    return airNoDio_std;
  }
  if (-1 == fd) {
    /* couldn't get the underlying file descriptor */
    return airNoDio_fd;
  }

  fflush(file);
  if (0 != fcntl(fd, F_DIOINFO, &dioinfo)) {
    /* couldn't learn direct I/O specifics */
    return airNoDio_fcntl;
  }
  
  /* 
  ** direct I/O requirements:
  ** 1) xfer size between d_miniosz and d_maxiosz
  ** 2) xfer size a multiple of d_miniosz
  ** 3) memory buffer on d_mem-byte boundary
  ** 4) file position on d_miniosz-byte boundary
  **
  ** As long as xfer size is >= d_miniosz and meets req. #2, then
  ** we can break the xfer into d_maxiosz-size pieces of need be.
  ** We can test #3 here if we're given non-NULL ptr
  ** We can always test #4
  */
  if (size < dioinfo.d_miniosz) {
    /* too small! */
    return airNoDio_small;
  }
  if (size % dioinfo.d_miniosz) {
    /* fails req. #2 above */
    return airNoDio_size;
  }
  if (ptr) {
    if ((unsigned long)(ptr) % dioinfo.d_mem) {
      /* fails req. #3 above */
      return airNoDio_ptr;
    }
  }
  if (lseek(fd, 0, SEEK_CUR) % dioinfo.d_miniosz) {
    /* fails req. #4 above */
    return airNoDio_fpos;
  }

  if (!ptr) {
    tmp = memalign(dioinfo.d_mem, dioinfo.d_miniosz);
    if (!tmp) {
      /* couldn't even alloc (via memalign) the minimum size */
      return airNoDio_test;
    }
    free(tmp);
  }
  
  /* seems that direct I/O is available */
  return airNoDio_okay;
#endif
}

const char
_airNoDioErr[AIR_NODIO_MAX+2][AIR_STRLEN_SMALL] = {
  "(invalid noDio value)",
  "CAN TOO do direct I/O!",
  "direct I/O apparently not available on this architecture",
  "direct I/O apparently not suitable for given file format",
  "got NULL pointer pointer",
  "won't do direct I/O on std{in|out|err}",
  "can't learn integral file descriptor from FILE pointer",
  "fcntl() call (to learn direct I/O specifics) failed",
  "requested transfer size is too small",
  "requested transfer size not a multiple of d_miniosz",
  "data memory address not multiple of d_mem",
  "current file position not multiple of d_miniosz",
  "memalign() test (on a small chuck of memory) failed",
  "direct I/O (in air library) has been disabled with airDisableDio"
};


const char *
airNoDioErr(int noDio) {

  if (AIR_IN_CL(0, noDio, AIR_NODIO_MAX)) {
    return _airNoDioErr[noDio+1];
  }
  else {
    return _airNoDioErr[0];
  }
}

size_t
airDioRead(FILE *file, void **ptrP, size_t size) {
  size_t red;
#if TEEM_DIO == 0
#else
  int fd, mem, min, max;
  struct dioattr dioinfo;
  size_t remain, part;
  char *ptr;
#endif
  
  if (!( ptrP && airNoDio_okay == airDioTest(size, file, NULL) ))
    return 0;

#if TEEM_DIO == 0
  red = 0;
#else
  fd = fileno(file);
  fcntl(fd, F_DIOINFO, &dioinfo);
  airDioInfo(&mem, &min, &max, file);
  *ptrP = memalign(mem, size);
  if (!*ptrP) {
    /* couldn't alloc (via memalign) "size" bytes */
    return 0;
  }
  ptr = *ptrP;
  remain = size;
  red = 0;
  do {
    if (remain >= max) {
      part = max;
    }
    else {
      part = remain;
    }
    red += read(fd, ptr, part);
    ptr += part;
    remain -= part;
  } while (remain);
#endif
  
  return red;
}

size_t
airDioWrite(FILE *file, void *_ptr, size_t size) {
  size_t rit;
#if TEEM_DIO == 0
#else
  int fd, mem, min, max;
  struct dioattr dioinfo;
  size_t remain, part;
  char *ptr;
#endif
  
  if (!( _ptr && (airNoDio_okay == airDioTest(size, file, _ptr)) ))
    return 0;

#if TEEM_DIO == 0
  rit = 0;
#else
  fd = fileno(file);
  fcntl(fd, F_DIOINFO, &dioinfo);
  airDioInfo(&mem, &min, &max, file);
  ptr = _ptr;
  remain = size;
  rit = 0;
  do {
    if (remain >= max) {
      part = max;
    }
    else {
      part = remain;
    }
    rit += write(fd, ptr, part);
    ptr += part;
    remain -= part;
  } while (remain);
#endif
  
  return rit;
}
