/*
  NrrdIO: stand-alone code for basic nrrd functionality
  Copyright (C) 2012, 2011, 2010, 2009  University of Chicago
  Copyright (C) 2008, 2007, 2006, 2005  Gordon Kindlmann
  Copyright (C) 2004, 2003, 2002, 2001, 2000, 1999, 1998  University of Utah

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

int airDisableDio = AIR_FALSE;

static const char
_airNoDioErr[AIR_NODIO_MAX+2][AIR_STRLEN_SMALL] = {
  "(invalid noDio value)",
  "CAN TOO do direct I/O!",
  "direct I/O apparently not available on this architecture",
  "direct I/O apparently not suitable for given file format",
  "won't do direct I/O on std{in|out|err}",
  "got -1 as file descriptor",
  "fcntl(F_DIOINFO) to learn direct I/O specifics failed",
  "requested transfer size is too small",
  "requested transfer size not a multiple of d_miniosz",
  "data memory address not multiple of d_mem",
  "current file position not multiple of d_miniosz",
  "fcntl(F_SETFL, FDIRECT) to turn on direct I/O failed",
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

/*
******** airDioTest
**
** does everything necessary to assess whether direct IO can be used
** to read a data segment of a given size, from a given file
** descriptor, into a given pointer.  The given pointer ptr can be
** NULL, and/or the size can be 0, in order to test the other aspects
** of direct IO. The return value of this is from the airNoDio_* enum.
** Note that airNoDio_okay means, "actually, direct IO *does* seem to
** be possible here".
*/
#if TEEM_DIO == 0
int
airDioTest(int fd, const void *ptr, size_t size) {
  AIR_UNUSED(fd);
  AIR_UNUSED(ptr);
  AIR_UNUSED(size);

  /* Teem makefiles think no direct IO is possible on this architecture */
  return airNoDio_arch;
}
#else
int
airDioTest(int fd, const void *ptr, size_t size) {
  struct dioattr dioinfo;
  void *tmp;
  int flags;

  if (airDisableDio) {
    /* user turned direct I/O off */
    return airNoDio_disable;
  }
  if (0 == fd || 1 == fd || 2 == fd) {
    /* This was added because I was noticing a problem with piping
       between unrrdu programs- sometimes the fread() of the receiving
       data through a unix pipe ("|") failed to read all the data.  If
       the body of this function was bypassed (with "return
       airNoDio_disable;", for instance), then the problem went away.
       The problematic call seemed to be the fflush() below (Tue Feb 1
       06:47:33 EST 2005: which has since been removed with the change
       of this function's argument from a FILE * to an integral file
       descriptor).  I don't think direct I/O is possible on stdin,
       stdout, or stdout, since the fcntl() call below fails on stdin
       and stdout.  However, something about making that fcntl() call
       changes something which means that about half the time, the
       read() on a piped stdin fails (on an irix6.n32 O2, at
       least). So, seems to be safest to just explicitly say that
       direct I/O is unavailable, based solely on the file descriptor
       number (0, 1, 2).  */
    return airNoDio_std;
  }
  if (-1 == fd) {
    /* caller probably couldn't get the underlying file descriptor */
    return airNoDio_fd;
  }
  if (0 != fcntl(fd, F_DIOINFO, &dioinfo)) {
    /* couldn't learn direct I/O specifics */
    return airNoDio_dioinfo;
  }

  if (size) {
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
      /* fails req. #1 above */
      return airNoDio_small;
    }
    /* we don't actually check for being too large, since we can always
       do IO on d_maxiosz-sized pieces */
    if (size % dioinfo.d_miniosz) {
      /* fails req. #2 above */
      return airNoDio_size;
    }
  }
  if (ptr) {
    if ((unsigned long)(ptr) % dioinfo.d_mem) {
      /* fails req. #3 above */
      return airNoDio_ptr;
    }
  } else {
    tmp = memalign(dioinfo.d_mem, dioinfo.d_miniosz);
    if (!tmp) {
      /* couldn't even alloc (via memalign) the minimum size */
      return airNoDio_test;
    }
    free(tmp);
  }
  if (lseek(fd, 0, SEEK_CUR) % dioinfo.d_miniosz) {
    /* fails req. #4 above */
    return airNoDio_fpos;
  }
  flags = fcntl(fd, F_GETFL);
  if (-1 == fcntl(fd, F_SETFL, flags | FDIRECT)) {
    /* couln't turn on direct I/O */
    return airNoDio_setfl;
  }
  /* put things back the way they were */
  fcntl(fd, F_SETFL, flags);

  /* as far as we know, direct I/O seems workable */
  return airNoDio_okay;
}
#endif

/*
******** airDioInfo
**
** does the fcntl stuff to learn the direct IO parameters:
** align: required alignment of memory (pointer must be multiple of this)
** min: minimum size of dio transfer
** max: maximum size of dio transfer
**
** NOTE: this does not try to do any error checking, because it assumes
** that you've already called airDioTest without incident.
*/
#if TEEM_DIO == 0
void
airDioInfo(int *align, int *min, int *max, int fd) {
  AIR_UNUSED(align);
  AIR_UNUSED(min);
  AIR_UNUSED(max);
  AIR_UNUSED(fd);
  return;
}
#else
void
airDioInfo(int *align, int *min, int *max, int fd) {
  struct dioattr dioinfo;

  if (align && min && max && !fcntl(fd, F_DIOINFO, &dioinfo)) {
    *align = dioinfo.d_mem;
    *min = dioinfo.d_miniosz;
    *max = dioinfo.d_maxiosz;
  }
  return;
}
#endif

/*
******** airDioMalloc
**
** does direct IO compatible memory allocation.
**
** NOTE: like airDioInfo, this assumes that you've called airDioTest
** without incident
*/
#if TEEM_DIO == 0
void *
airDioMalloc(size_t size, int fd) {
  AIR_UNUSED(size);
  AIR_UNUSED(fd);

  return NULL;
}
#else
void *
airDioMalloc(size_t size, int fd) {
  int align, min, max;

  airDioInfo(&align, &min, &max, fd);
  return memalign(align, size);
}
#endif

/*
******** airDioRead
**
** like read(), but for direct IO.  The idea is that you call this on as
** big a chunk of memory as possible.
**
** NOTE: like airDioInfo, this assumes that you've called airDioTest
** without incident
*/
#if TEEM_DIO == 0
size_t
airDioRead(int fd, void *_ptr, size_t size) {
  AIR_UNUSED(fd);
  AIR_UNUSED(_ptr);
  AIR_UNUSED(size);

  return 0;
}
#else
size_t
airDioRead(int fd, void *_ptr, size_t size) {
  size_t red, totalred;
  int align, min, max, flags;
  size_t remain, part;
  char *ptr;

  if (!( _ptr && airNoDio_okay == airDioTest(fd, _ptr, size) )) {
    return 0;
  }

  flags = fcntl(fd, F_GETFL);
  fcntl(fd, F_SETFL, flags | FDIRECT);
  airDioInfo(&align, &min, &max, fd);
  remain = size;
  totalred = 0;
  ptr = (char*)_ptr;
  do {
    part = AIR_MIN(remain, max);
    red = read(fd, ptr, part);
    totalred += red;
    if (red != part) {
      break;
    }
    ptr += red;
    remain -= red;
  } while (remain);
  fcntl(fd, F_SETFL, flags);

  return totalred;
}
#endif

/*
******** airDioWrite
**
** like write(), but for direct IO.  The idea is that you call this on as
** big a chunk of memory as possible.
**
** NOTE: like airDioInfo, this assumes that you've called airDioTest
** without incident
*/
#if TEEM_DIO == 0
size_t
airDioWrite(int fd, const void *_ptr, size_t size) {
  AIR_UNUSED(fd);
  AIR_UNUSED(_ptr);
  AIR_UNUSED(size);

  return 0;
}
#else
size_t
airDioWrite(int fd, const void *_ptr, size_t size) {
  size_t rit, totalrit;
  int align, min, max, flags;
  size_t remain, part;
  char *ptr;

  if (!( _ptr && (airNoDio_okay == airDioTest(fd, _ptr, size)) )) {
    return 0;
  }

  flags = fcntl(fd, F_GETFL);
  fcntl(fd, F_SETFL, flags | FDIRECT);
  airDioInfo(&align, &min, &max, fd);
  remain = size;
  totalrit = 0;
  ptr = (char*)_ptr;
  do {
    part = AIR_MIN(remain, max);
    rit = write(fd, ptr, part);
    totalrit += rit;
    if (rit != part) {
      break;
    }
    ptr += rit;
    remain -= rit;
  } while (remain);
  fcntl(fd, F_SETFL, flags);

  return totalrit;
}
#endif
