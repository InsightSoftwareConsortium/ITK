/*
 * Copyright (c) 1998
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 */



#include <fstream>
#ifdef __unix
#  include <sys/types.h>
#  include <sys/stat.h>         // For stat
#  include <sys/mman.h>         // For mmap
#  include <fcntl.h>            // For open, close
#include <unistd.h>

#elif _MSC_VER
#  include <windows.h>          // For GetFileType, etc.
#  include <io.h>               // For _get_osfhandle
#  include <fcntl.h>            // For _O_RDONLY, etc
#  include <sys/stat.h>         // For _fstat
#endif /* __unix */

#if defined(__unix)
#  define OPEN   open
#  define CLOSE  close
#  define READ   read
#  define WRITE  write
#  define STAT   stat
#  define FSTAT  fstat
#  define FILENO fileno
#  define DEFAULT_PERMISSIONS \
          (S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH)
#  define IS_REGULAR(mod) S_ISREG(mod)

   // All version of Unix have mmap and lseek system calls.  Some also have
   // longer versions of those system calls to accommodate 64-bit offsets.
#  ifdef __sgi /* IRIX */
#    define LSEEK lseek64
#    define MMAP  mmap64
#  else
#    define LSEEK lseek
#    define MMAP  mmap
#  endif

#elif _MSC_VER
#  define CLOSE(fd) !CloseHandle((HANDLE)fd)
#  define FILENO    fileno
#  define LSEEK(fd,off,whence) \
          (long)SetFilePointer((HANDLE)fd,(LONG)(off),0,(DWORD)whence)

// This default ensures that we use the Win32 cache.  If another
// process has the file open for reading, it may not see updates
// until the cache is flushed.
#  define DEFAULT_PERMISSIONS FILE_ATTRIBUTE_NORMAL
#  define MAP_FAILED 0

#  define LF '\n'      /* line feed */
#  define CR '\r'      /* carriage return */
#  define CTRLZ 26     /* ctrl-z */

#  ifndef O_RDONLY
#    define O_RDONLY _O_RDONLY
#    define O_WRONLY _O_WRONLY
#    define O_RDWR   _O_RDWR
#    define O_APPEND _O_APPEND
#    define O_CREAT  _O_CREAT
#    define O_TRUNC  _O_TRUNC
#    define O_TEXT   _O_TEXT
#    define O_BINARY _O_BINARY
#  endif
#  define O_ACCMODE (O_RDONLY|O_WRONLY|O_RDWR)

#  define _TEXTBUF_SIZE 0x400 // used in write TEXT conversion
#endif

// Helper functions for _Filebuf_base.

namespace SGI {

bool __is_regular_file(int fd) {
# ifdef __unix
  struct STAT buf;
  return FSTAT(fd, &buf) == 0 && IS_REGULAR(buf.st_mode);
# elif _MSC_VER
  return (GetFileType((HANDLE)fd) == FILE_TYPE_DISK);
# endif /* unix */
}

// Number of characters in the file.
std::streamoff __file_size(int fd) {
# ifdef __unix
  struct STAT buf;
  if(FSTAT(fd, &buf) == 0 && IS_REGULAR(buf.st_mode))
    return buf.st_size > 0 ? buf.st_size : 0;
  else
    return 0;
# elif _MSC_VER
  DWORD FileSizeHigh;
  DWORD FileSize = GetFileSize((HANDLE)fd, &FileSizeHigh);
  return (FileSize == ((DWORD)-1) ? 0 : FileSize);
# endif /* unix */
}

// Return the number of characters in f that follow the end of the
// internal buffer.  Returns 0 if no estimate is possible.
std::streamoff __remaining_characters(FILE* f)
{
  int fd = -1;
  fd = FILENO(f);

  std::streamoff size;
# ifdef __unix
  size = SGI::__file_size(fd);
# elif _MSC_VER
  struct _stat buf;
  if(_fstat(fd, &buf) == 0 && (buf.st_mode & _S_IFREG))
    size = (buf.st_size > 0 ? buf.st_size : 0);
  else
    size = 0;
# endif /* unix */
  long pos = ftell(f);

  return pos >= 0 && size > pos ? size - pos : 0;
}

#ifdef _MSC_VER
// fcntl(fileno, F_GETFL) for Microsoft
// 'semi-documented' defines:
#define IOINFO_L2E          5
#define IOINFO_ARRAY_ELTS   (1 << IOINFO_L2E)
#define _pioinfo(i) ( __pioinfo[(i) >> IOINFO_L2E] + \
              ((i) & (IOINFO_ARRAY_ELTS - 1)) )
#define FAPPEND         0x20    // O_APPEND flag
#define FTEXT           0x80    // O_TEXT flag
// end of 'semi-documented' defines

// fbp : empirical
#define F_WRITABLE      0x04

// 'semi-documented' internal structure
extern "C" {
  struct ioinfo {
    long osfhnd;    // the real os HANDLE
    char osfile;    // file handle flags
    char pipech;    // pipe buffer
#   ifdef _MT
    // multi-threaded locking
    int lockinitflag;
    CRITICAL_SECTION lock;
#   endif  /* _MT */
  };
  extern _CRTIMP ioinfo * __pioinfo[];
} // extern "C"
// end of 'semi-documented' declarations 

int _get_osfflags(int fd) {
  char dosflags = 0;
  HANDLE oshandle = (HANDLE)_get_osfhandle(fd);
  if ((long)oshandle == -1) // assume its a HANDLE
    oshandle = (HANDLE)fd;
  else // 'semi-documented' internal structure usage
    dosflags = _pioinfo(fd)->osfile;
    // end of 'semi-documented' stuff 
  int mode = 0;
  if (dosflags & FAPPEND)
    mode |= O_APPEND;
  if (dosflags & FTEXT)
    mode |= O_TEXT;
  else
    mode |= O_BINARY;

  // there seems to be no working O_WRONLY mode here
#ifdef __macintosh
  if (dosflags & F_WRITABLE)
    mode |= O_RDWR;
  else
    mode |= O_RDONLY;
#else
  DWORD dummy, dummy2;
  if (WriteFile(oshandle, &dummy2, 0, &dummy, 0))
    mode |= O_RDWR;
  else
    mode |= O_RDONLY;
#endif

  return mode;
}

#endif // _MSC_VER

} // Close namespace SGI

__STL_BEGIN_NAMESPACE

_Filebuf_base::_Filebuf_base()
  : _M_file_id(-1),
    _M_openmode(0),
    _M_page_size(512),          // Just a guess
    _M_is_open(false),
    _M_should_close(false)
{
# ifdef __unix
  _M_page_size = sysconf(_SC_PAGESIZE);
# elif _MSC_VER
  _M_hFileMappingObject = 0;
  SYSTEM_INFO SystemInfo;
  GetSystemInfo(&SystemInfo);
  _M_page_size = SystemInfo.dwPageSize;
  // might be .dwAllocationGranularity
# endif /* unix */
}

bool _Filebuf_base::_M_open(const char* name, ios_base::openmode openmode,
                            long permission)
{
  if (_M_is_open)
    return false;

#ifdef __unix
  int flags = 0;

  switch(openmode & (~ios_base::ate & ~ios_base::binary)) {
  case ios_base::out:
  case ios_base::out | ios_base::trunc:
    flags = O_WRONLY | O_CREAT | O_TRUNC;
    break;
  case ios_base::out | ios_base::app:
    flags = O_WRONLY | O_CREAT | O_APPEND;
    break;
  case ios_base::in:
    flags = O_RDONLY;
    permission = 0;             // Irrelevant unless we're writing.
    break;
  case ios_base::in | ios_base::out:
    flags = O_RDWR;
    break;
  case ios_base::in | ios_base::out | ios_base::trunc:
    flags = O_RDWR | O_CREAT | O_TRUNC;
    break;
  default:                      // The above are the only combinations of
    return false;               // flags allowed by the C++ standard.
  }

  int fileno = OPEN(name, flags, permission);
  if (fileno < 0)
    return false;

  _M_is_open = true;
  _M_file_id = fileno;

#elif _MSC_VER
  DWORD dwDesiredAccess, dwShareMode, dwCreationDisposition;
  switch(openmode & (~ios_base::ate & ~ios_base::binary)) {
  case ios_base::out:
  case ios_base::out | ios_base::trunc:
    dwDesiredAccess = GENERIC_WRITE;
    dwShareMode = FILE_SHARE_READ;
    dwCreationDisposition = TRUNCATE_EXISTING;
    break;
  case ios_base::out | ios_base::app:
    dwDesiredAccess = GENERIC_WRITE;
    dwShareMode = FILE_SHARE_READ;
    dwCreationDisposition = OPEN_ALWAYS;
    break;
  case ios_base::in:
    dwDesiredAccess = GENERIC_READ;
    dwShareMode = FILE_SHARE_READ | FILE_SHARE_WRITE;
    dwCreationDisposition = OPEN_EXISTING;
    permission = 0;             // Irrelevant unless we're writing.
    break;
  case ios_base::in | ios_base::out:
    dwDesiredAccess = GENERIC_READ | GENERIC_WRITE;
    dwShareMode = FILE_SHARE_READ;
    dwCreationDisposition = OPEN_ALWAYS;
    break;
  case ios_base::in | ios_base::out | ios_base::trunc:
    dwDesiredAccess = GENERIC_READ | GENERIC_WRITE;
    dwShareMode = FILE_SHARE_READ;
    dwCreationDisposition = TRUNCATE_EXISTING;
    break;
  default:                      // The above are the only combinations of
    return false;               // flags allowed by the C++ standard.
  }

  HANDLE fileno = CreateFile(name, dwDesiredAccess, dwShareMode, 0,
                         dwCreationDisposition, permission, 0);
  if (fileno == INVALID_HANDLE_VALUE && 
              dwCreationDisposition == TRUNCATE_EXISTING) {
    // might have failed because file did not exist:
    fileno = CreateFile(name, dwDesiredAccess, dwShareMode, 0, 
                         OPEN_ALWAYS, permission, 0);
  }
  if (fileno == INVALID_HANDLE_VALUE)
    return false;

  _M_is_open = true;
  _M_file_id = (int)fileno;

#endif /* __unix */

  if (openmode & ios_base::ate)
    if (LSEEK(fileno, 0, SEEK_END) == -1)
      _M_is_open = false;

  _M_should_close = _M_is_open;
  _M_openmode = openmode;
  if (_M_is_open)
    _M_regular_file = SGI::__is_regular_file(_M_file_id);

  return _M_is_open;
}

bool _Filebuf_base::_M_open(const char* name, ios_base::openmode openmode)
{
  // This doesn't really grant everyone in the world read/write
  // access.  On Unix, file-creation system calls always clear
  // bits that are set in the umask from the permissions flag.
  return this->_M_open(name, openmode, DEFAULT_PERMISSIONS);
}

// Associate the filebuf with a file descriptor pointing to an already-
// open file.  Mode is set to be consistent with the way that the file
// was opened.
bool _Filebuf_base::_M_open(int fileno) {

# ifdef __unix
  if (_M_is_open || fileno < 0)
    return false;

  int mode = fcntl(fileno, F_GETFL);
# elif _MSC_VER
  if (_M_is_open || fileno == -1)
    return false;

  int mode = SGI::_get_osfflags(fileno);
  HANDLE oshandle = (HANDLE)_get_osfhandle(fileno);
  if ((long)oshandle != -1)
	fileno = (int)oshandle;
# endif // __unix

  if (mode == -1)
    return false;

  switch(mode & O_ACCMODE) {
  case O_RDONLY:
    _M_openmode = ios_base::in; break;
  case O_WRONLY:
    _M_openmode = ios_base::out; break;
  case O_RDWR:
    _M_openmode = ios_base::in | ios_base::out; break;
  default:
    return false;
  }

  if (mode & O_APPEND)
    _M_openmode |= ios_base::app;

# ifdef _MSC_VER
  if (mode & O_BINARY)
    _M_openmode |= ios_base::binary;
# endif // _MSC_VER

  _M_file_id = fileno;
  _M_is_open = true;
  _M_should_close = false;
  _M_regular_file = SGI::__is_regular_file(_M_file_id);

  return true;
}

bool _Filebuf_base::_M_close() {
  if (!_M_is_open)
    return false;

  bool ok = false;

  if (!_M_should_close)
    ok = true;
  else {
    ok = CLOSE(_M_file_id) == 0;
  }

  _M_is_open = _M_should_close = false;
  _M_openmode = 0;
  return ok;
}

// Read up to n characters into a buffer.  Return value is number of
// characters read.
ptrdiff_t _Filebuf_base::_M_read(char* buf, ptrdiff_t n) {
# ifdef __unix
  return READ(_M_file_id, buf, n);
# elif _MSC_VER
  DWORD NumberOfBytesRead;
  ReadFile((HANDLE)_M_file_id, (LPVOID)buf, (DWORD)n, 
                       &NumberOfBytesRead, 0);

  if (! (_M_openmode & ios_base::binary) && NumberOfBytesRead) { 
    // translate CR-LFs to LFs in the buffer
    char * to = buf, * last = buf + NumberOfBytesRead - 1;
    char * from;
    for (from = buf; from <= last && * from != CTRLZ; ++ from ) {
      if (* from != CR)
        * to ++ = * from;
      else { // found CR
        if (from < last) { // not at buffer end
          if (* (from + 1) != LF)
            * to ++ = CR;
        }
        else { // last char is CR, peek for LF
          char peek = ' ';
          DWORD NumberOfBytesPeeked;
          ReadFile((HANDLE)_M_file_id, (LPVOID)&peek, 
                        1, &NumberOfBytesPeeked, 0);
          if (NumberOfBytesPeeked)
            LSEEK(_M_file_id, -1, SEEK_CUR);
          if (peek != LF)
            * to ++ = CR;
        }
      } // found CR
    } // for
    // seek back to TEXT end of file if hit CTRL-Z
    if (from <= last) // terminated due to CTRLZ
      LSEEK(_M_file_id, ((last+1) - from) , SEEK_CUR);
    NumberOfBytesRead = to - buf;
  }
  return (ptrdiff_t)NumberOfBytesRead;
# endif /* __unix */
}

// Write n characters from a buffer.  Return value: true if we managed
// to write the entire buffer, false if we didn't.
bool _Filebuf_base::_M_write(char* buf, ptrdiff_t n) {
  while (true) {
    ptrdiff_t written = 0;
#   ifdef __unix
    written = WRITE(_M_file_id, buf, n);
#   elif _MSC_VER
    // In append mode, every write does an implicit seek to the end
    // of the file.
    if (_M_openmode & ios_base::app)
      LSEEK(_M_file_id, 0, SEEK_END);
    if (_M_openmode & ios_base::binary) { 
      // binary mode
      DWORD NumberOfBytesWritten;
      WriteFile((HANDLE)_M_file_id, (LPVOID)buf, (DWORD)n, 
                          &NumberOfBytesWritten, 0);
      written = (ptrdiff_t)NumberOfBytesWritten;
    }
    else {
      // text mode
      char textbuf[_TEXTBUF_SIZE + 1]; // extra 1 in case LF at end
      char * nextblock = buf, * ptrtextbuf = textbuf;
      char * endtextbuf = textbuf + _TEXTBUF_SIZE;
      char * endblock = buf + n;
      ptrdiff_t nextblocksize = (std::min)(n, ptrdiff_t(_TEXTBUF_SIZE));
      char * nextlf;
      while (nextblocksize && 
            (nextlf = (char *)memchr(nextblock, LF, nextblocksize)) != 0) {
        ptrdiff_t linelength = nextlf - nextblock;
        memcpy(ptrtextbuf, nextblock, linelength);
        ptrtextbuf += linelength;
        nextblock += (linelength + 1);
        * ptrtextbuf ++ = CR;
        * ptrtextbuf ++ = LF;
        nextblocksize = (std::min)(endblock - nextblock, 
                                   endtextbuf - ptrtextbuf);
      }
      memcpy(ptrtextbuf, nextblock, nextblocksize);
      ptrtextbuf += nextblocksize;
      nextblock += nextblocksize;
      // now write out the translated buffer
      char * writetextbuf = textbuf;
      for (ptrdiff_t NumberOfBytesToWrite = ptrtextbuf - textbuf; 
                     NumberOfBytesToWrite;) {
        DWORD NumberOfBytesWritten;
        WriteFile((HANDLE)_M_file_id, (LPVOID)writetextbuf, 
                  NumberOfBytesToWrite, &NumberOfBytesWritten, 0);
        if (NumberOfBytesWritten == NumberOfBytesToWrite)
          break;
        if (!NumberOfBytesWritten) // write shortfall
          return false;
        writetextbuf += NumberOfBytesWritten;
        NumberOfBytesToWrite -=	NumberOfBytesWritten;
      }
      written = nextblock - buf;
    }
#   endif /* __unix */
    if (n == written)
      return true;
    else if (written > 0 && written < n) {
      n -= written;
      buf += written;
    }
    else
      return false;
  }
}

// Wrapper for lseek or the like.
streamoff _Filebuf_base::_M_seek(streamoff offset, ios_base::seekdir dir)
{
  streamoff result = -1;

  int whence;

  switch(dir) {
  case ios_base::beg:
    if (offset < 0 || offset > _M_file_size())
      return -1;
    whence = SEEK_SET;
    break;
  case ios_base::cur:
    whence = SEEK_CUR;
    break;
  case ios_base::end:
    if (offset > 0 || -offset > _M_file_size())
      return -1;
    whence = SEEK_END;
    break;
  default:
    return streamoff(-1);
  }

  result = LSEEK(_M_file_id, offset, whence);

  return result;
}

// Return the size of the file.  In Unix, this is a wrapper for stat.
// Returns zero if the size cannot be determined or is ill-defined.
streamoff _Filebuf_base::_M_file_size()
{
  return SGI::__file_size(_M_file_id);
}

// Attempts to memory-map len bytes of the current file, starting
// at position offset.  Precondition: offset is a multiple of the
// page size.  Postcondition: return value is a null pointer if the
// memory mapping failed.  Otherwise the return value is a pointer to
// the memory-mapped file and the file position is set to offset.
void* _Filebuf_base::_M_mmap(streamoff offset, streamoff len) {
# ifdef __unix
  void* base = MMAP(0, len, PROT_READ, MAP_PRIVATE, _M_file_id, offset);
# elif _MSC_VER
  _M_hFileMappingObject = (int)CreateFileMapping((HANDLE)_M_file_id,
           0, PAGE_READONLY, 0, len, 0);
  void* base = MAP_FAILED;
  if (_M_hFileMappingObject)
    base = MapViewOfFile((HANDLE)_M_hFileMappingObject, FILE_MAP_READ, 
                           0, offset, len);
# endif	// __unix
  if (base != MAP_FAILED) {
    if (LSEEK(_M_file_id, offset + len, SEEK_SET) < 0) {
      this->_M_unmap(base, len);
      base = 0;
    }
  }
  return base;
}

void _Filebuf_base::_M_unmap(void* base, streamoff len) {
# ifdef __unix
  munmap((caddr_t) base, len);
# elif _MSC_VER
  UnmapViewOfFile((LPCVOID)base);
  CloseHandle((HANDLE)_M_hFileMappingObject);
  _M_hFileMappingObject = 0;
# endif	// __unix
}

// In Unix, writing n characters always bumps the file position by n.
// In Windows text mode, however, it bumps the file position by n + m,
// where m is the number of newlines in the range.  That's because an
// internal \n corresponds to an external two-character sequence.
streamoff _Filebuf_base::_M_get_offset(char* first, char* last)
{
# ifdef __unix
  return last - first;
# elif _MSC_VER
  if (_M_openmode & ios_base::binary)
    return last - first;
  // we only count if necessary:
  int n;
  count(first, last, '\n', n);
  return (last - first) + n;
# endif
}

// In Unix there is no distinction between text and binary mode.  In Windows
// the difference is that text mode converts \n to an external two-character
// sequence.
bool _Filebuf_base::_M_in_binary_mode() const
{
# ifdef __unix
  return true;
# elif _MSC_VER
  return (_M_openmode & ios_base::binary) != 0;
# endif
}

// Specialization of underflow: if the character type is char, maybe
// we can use mmap instead of read.
template<>
class _Underflow< basic_filebuf<char, char_traits<char> > > {
public:
  typedef basic_filebuf<char, char_traits<char> >::int_type int_type;
  typedef char_traits<char> traits_type;

  static inline int_type doit(basic_filebuf<char, traits_type >* __this)
  {

    if (!__this->_M_in_input_mode) {
      if (!__this->_M_switch_to_input_mode())
	return traits_type::eof();
    }

    else if (__this->_M_in_putback_mode) {
      __this->_M_exit_putback_mode();
      if (__this->gptr() != __this->egptr()) {
	int_type __c = traits_type::to_int_type(*__this->gptr());
	return __c;
      }
    }

    // If it's a disk file, and if the internal and external character
    // sequences are guaranteed to be identical, then try to use memory
    // mapped I/O.  Otherwise, revert to ordinary read.
    if (__this->_M_regular_file
	&& __this->_M_always_noconv
	&& __this->_M_in_binary_mode()) {
      // If we've mmapped part of the file already, then unmap it.
      if (__this->_M_mmap_base)
	__this->_M_unmap(__this->_M_mmap_base, __this->_M_mmap_len);

      // Determine the position where we start mapping.  It has to be
      // a multiple of the page size.
      streamoff __cur = __this->_M_seek(0, ios_base::cur);
      streamoff __size = __this->_M_file_size();
      if (__size > 0 && __cur >= 0 && __cur < __size) {
	streamoff __offset    = (__cur / __this->_M_page_size)
	  * __this->_M_page_size;
	streamoff __remainder = __cur - __offset;

	__this->_M_mmap_len = __size - __offset;
	if ((__this->_M_mmap_base =
	     __this->_M_mmap(__offset, __this->_M_mmap_len)) != 0) {
	  __this->setg((char*) __this->_M_mmap_base,
		       (char*) __this->_M_mmap_base + __remainder,
		       (char*) __this->_M_mmap_base + __this->_M_mmap_len);
	  return traits_type::to_int_type(*__this->gptr());
	}
      }
    }

    return __this->_M_underflow_aux();
  }
};

//----------------------------------------------------------------------
// Force instantiation of filebuf and fstream classes.

template class basic_filebuf<char>;
template class basic_ifstream<char>;
template class basic_ofstream<char>;
template class basic_fstream<char>;

#ifdef INSTANTIATE_WIDE_STREAMS
template class basic_filebuf<wchar_t>;
template class basic_ifstream<wchar_t>;
template class basic_ofstream<wchar_t>;
template class basic_fstream<wchar_t>;
#endif /* INSTANTIATE_WIDE_STREAMS */


__STL_END_NAMESPACE

// Local Variables:
// mode:C++
// End:

