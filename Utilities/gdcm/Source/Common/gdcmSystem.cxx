/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library
  Module:  $URL$

  Copyright (c) 2006-2010 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmSystem.h"
#include "gdcmTrace.h"
#include "gdcmFilename.h"
#include "gdcmException.h"

#include <iostream>
#include <string>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include <sys/stat.h>
#include <limits.h> // PATH_MAX

// gettimeofday
#ifdef GDCM_HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <time.h>
#ifdef GDCM_HAVE_WINSOCK_H
#include <winsock.h>
#endif
#include <stdio.h> // snprintf
#if defined(GDCM_HAVE_SNPRINTF)
// ok nothing to do
#elif defined(GDCM_HAVE__SNPRINTF)
#define snprintf _snprintf
#endif
#ifdef __APPLE__
#include <CoreFoundation/CFBase.h>
#include <CoreFoundation/CFBundle.h>
#include <CoreFoundation/CFURL.h>
#endif // __APPLE__

#if defined(_WIN32) && (defined(_MSC_VER) || defined(__WATCOMC__) ||defined(__BORLANDC__) || defined(__MINGW32__))
#include <io.h>
#include <direct.h>
#define _unlink unlink
#else
//#include <features.h>	// we want GNU extensions
#include <dlfcn.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h> /* gethostname */
#include <strings.h> // strncasecmp
#endif

// TODO: WIN32 replacement for C99 stuff:
// #if defined(_WIN32) || defined(_WIN64)
// #define snprintf _snprintf
// #define vsnprintf _vsnprintf
// #define strcasecmp _stricmp
// #define strncasecmp _strnicmp
// #endif

namespace gdcm
{

#if defined(_WIN32) && (defined(_MSC_VER) || defined(__WATCOMC__) || defined(__BORLANDC__) || defined(__MINGW32__))
inline int Mkdir(const char* dir)
{
  return _mkdir(dir);
}
inline int Rmdir(const char* dir)
{
  return _rmdir(dir);
}
inline const char* Getcwd(char* buf, unsigned int len)
{
  const char* ret = _getcwd(buf, len);
  return ret;
}

#else
inline int Mkdir(const char* dir)
{
  return mkdir(dir, 00777);
}
inline int Rmdir(const char* dir)
{
  return rmdir(dir);
}
inline const char* Getcwd(char* buf, unsigned int len)
{
  const char* ret = getcwd(buf, len);
  return ret;
}
#endif

/*
// 1.14 How can I find a process' executable file?
// http://www.faqs.org/faqs/unix-faq/programmer/faq/
static std::string Argv0;

void System::SetArgv0(const char *argv0)
{
  Argv0 = argv0;
//std::cout << "Set:" << Argv0 << std::endl;
}

const char* System::GetArgv0()
{
//std::cout << "Get:" << Argv0 << std::endl;
  return Argv0.c_str();
}
*/

const char * System::GetCWD()
{
  static char buf[2048];
  const char* cwd = Getcwd(buf, 2048);
  return cwd;
/*
  std::string path;
  if ( cwd )
    {
    path = cwd;
    }
  return path;
*/
}

bool System::MakeDirectory(const char *path)
{
  if(System::FileExists(path))
    {
    return true;
    }
  Filename fn(path);
  std::string dir = fn.ToUnixSlashes();

  std::string::size_type pos = dir.find(':');
  if(pos == std::string::npos)
    {
    pos = 0;
    }
  std::string topdir;
  while((pos = dir.find('/', pos)) != std::string::npos)
    {
    topdir = dir.substr(0, pos);
    Mkdir(topdir.c_str());
    pos++;
    }
  if(dir[dir.size()-1] == '/')
    {
    topdir = dir.substr(0, dir.size());
    }
  else
    {
    topdir = dir;
    }
  if(Mkdir(topdir.c_str()) != 0)
    {
    // There is a bug in the Borland Run time library which makes MKDIR
    // return EACCES when it should return EEXISTS
    // if it is some other error besides directory exists
    // then return false
    if( (errno != EEXIST)
#ifdef __BORLANDC__
        && (errno != EACCES)
#endif
      )
      {
      return false;
      }
    }
  return true;
}

// return true if the file exists
bool System::FileExists(const char* filename)
{
#ifdef _MSC_VER
# define access _access
#endif
#ifndef R_OK
# define R_OK 04
#endif
  if ( access(filename, R_OK) != 0 )
    {
    return false;
    }
  else
    {
    //assert( !FileIsDirectory(filename) );
    return true;
    }
}

bool System::FileIsDirectory(const char* name)
{
  struct stat fs;
  if(stat(name, &fs) == 0)
    {
#if _WIN32
    return ((fs.st_mode & _S_IFDIR) != 0);
#else
    return S_ISDIR(fs.st_mode);
#endif
    }
  else
    {
    return false;
    }
}

// TODO st_mtimensec
time_t System::FileTime(const char* filename)
{
  struct stat fs;
  if(stat(filename, &fs) == 0)
    {
    // man 2 stat
    // time_t    st_atime;   /* time of last access */
    // time_t    st_mtime;   /* time of last modification */
    // time_t    st_ctime;   /* time of last status change */
    return fs.st_mtime;

    // Since  kernel 2.5.48, the stat structure supports nanosecond resolution
    // for the three file timestamp fields.  Glibc exposes the nanosecond com-
    // ponent of each field using names either of the form st_atim.tv_nsec, if
    // the _BSD_SOURCE or _SVID_SOURCE feature test macro is  defined,  or  of
    // the  form st_atimensec, if neither of these macros is defined.  On file
    // systems that do not support  sub-second  timestamps,  these  nanosecond
    // fields are returned with the value 0.
    }
  return 0;
}

const char *System::GetLastSystemError()
{
  int e = errno;
  return strerror(e);
}

bool System::GetPermissions(const char* file, unsigned short& mode)
{
  if ( !file )
    {
    return false;
    }

  struct stat st;
  if ( stat(file, &st) < 0 )
    {
    return false;
    }
  mode = st.st_mode;
  return true;
}

bool System::SetPermissions(const char* file, unsigned short mode)
{
  if ( !file )
    {
    return false;
    }
  if ( !System::FileExists(file) )
    {
    return false;
    }
  if ( chmod(file, mode) < 0 )
    {
    return false;
    }

  return true;
}

bool System::RemoveFile(const char* source)
{
#ifdef _WIN32
  unsigned short mode;
  if ( !System::GetPermissions(source, mode) )
    {
    return false;
    }
  /* Win32 unlink is stupid --- it fails if the file is read-only  */
  System::SetPermissions(source, S_IWRITE);
#endif
  bool res = unlink(source) != 0 ? false : true;
#ifdef _WIN32
  if ( !res )
    {
    System::SetPermissions(source, mode);
    }
#endif
  return res;
}

// return size of file; also returns zero if no file exists
size_t System::FileSize(const char* filename)
{
#if 0
       All of these system calls return a stat structure, which  contains  the
       following fields:

          struct stat {
              dev_t     st_dev;     /* ID of device containing file */
              ino_t     st_ino;     /* inode number */
              mode_t    st_mode;    /* protection */
              nlink_t   st_nlink;   /* number of hard links */
              uid_t     st_uid;     /* user ID of owner */
              gid_t     st_gid;     /* group ID of owner */
              dev_t     st_rdev;    /* device ID (if special file) */
              off_t     st_size;    /* total size, in bytes */
              blksize_t st_blksize; /* blocksize for filesystem I/O */
              blkcnt_t  st_blocks;  /* number of blocks allocated */
              time_t    st_atime;   /* time of last access */
              time_t    st_mtime;   /* time of last modification */
              time_t    st_ctime;   /* time of last status change */
          };
#endif
  struct stat fs;
  if (stat(filename, &fs) != 0)
    {
    return 0;
    }
  off_t size = fs.st_size;
  size_t size2 = size;
  // off_t can be larger than size_t
  if( size != (off_t)size2 ) return 0;
  return size2;
}

#if 0
const char *System::GetCurrentDataDirectory()
{
#ifdef _WIN32
  static char path[MAX_PATH];
  gdcm::Filename fn( GetCurrentProcessFileName() );
  if ( !fn.IsEmpty() )
    {
    std::string str = fn.GetPath();
    str += "/../" GDCM_INSTALL_DATA_DIR;
    strcpy(path, str.c_str());
    return path;
    }
#else

  static char path[PATH_MAX];

#ifdef __APPLE__
  Boolean success = false;
  CFURLRef pathURL = CFBundleCopyResourcesDirectoryURL(CFBundleGetMainBundle());
  if (pathURL != NULL)
    {
    success = CFURLGetFileSystemRepresentation(pathURL, true /*resolveAgainstBase*/, (unsigned char*) path, PATH_MAX);
    CFRelease(pathURL);
    }
  if (success)
    {
    strncat(path, "/" GDCM_INSTALL_DATA_DIR, PATH_MAX);
    return path;
    }
#endif

  gdcm::Filename fn( GetCurrentProcessFileName() );
  if ( !fn.IsEmpty() )
    {
    std::string str = fn.GetPath();
    str += "/../" GDCM_INSTALL_DATA_DIR;
    strcpy(path, str.c_str());
    return path;
    }
#endif
  return 0;
}
#endif

/*
 * TODO:
 * check cygwin
 * check beos : get_next_image_info
 * check solaris
 * check hpux
 * check os2: DosGetInfoBlocks / DosQueryModuleName
 * check macosx :
 *  ProcessSerialNumber psn = {kNoProcess, kCurrentProcess};
 *  GetProcessInformation -> FSMakeFSSpec
 * ...
 */
const char *System::GetCurrentProcessFileName()
{
#ifdef _WIN32
  static char buf[MAX_PATH];
  if ( ::GetModuleFileName(0, buf, sizeof(buf)) )
    {
    return buf;
    }
#elif defined(__APPLE__)
  static char buf[PATH_MAX];
  Boolean success = false;
  CFURLRef pathURL = CFBundleCopyExecutableURL(CFBundleGetMainBundle());
  if ( pathURL)
    {
    success = CFURLGetFileSystemRepresentation(pathURL, true /*resolveAgainstBase*/, (unsigned char*) buf, PATH_MAX);
    CFRelease(pathURL);
    }
  if (success)
    {
    return buf;
    }
#else
  static char path[PATH_MAX];
  if ( readlink ("/proc/self/exe", path, sizeof(path)) > 0) // Technically 0 is not an error, but that would mean
                                                            // 0 byte were copied ... thus considered it as an error
    {
    return path;
    }
#endif
   return 0;
}

#ifdef __USE_GNU
static void where_am_i() {}
#endif

const char *System::GetCurrentModuleFileName()
{
#ifdef __USE_GNU
  static char path[PATH_MAX];
  Dl_info info;
  if (dladdr( (void*)&where_am_i, &info ) == 0)
    {
    size_t len = strlen(info.dli_fname);
    if( len >= PATH_MAX ) return 0; // throw error ?
    // else
    strcpy(path,info.dli_fname);
    return path;
    }
#elif defined(_WIN32)
  // GetModuleFileName works the same on Win32 for library AFAIK
  return System::GetCurrentProcessFileName();
#endif

  return 0;
}

const char *System::GetCurrentResourcesDirectory()
{
#ifdef __APPLE__
  static char path[PATH_MAX];
  Boolean success = false;
  CFURLRef pathURL = CFBundleCopyResourcesDirectoryURL(CFBundleGetMainBundle());
  if (pathURL != NULL)
    {
    success = CFURLGetFileSystemRepresentation(pathURL, true /*resolveAgainstBase*/, (unsigned char*) path, PATH_MAX);
    CFRelease(pathURL);
    }
  if (success)
    {
    strncat(path, "/" GDCM_INSTALL_DATA_DIR, PATH_MAX);
    return path;
    }
#endif
  // Is there such beast on *any* other system but APPLE ?
  return 0;
}

/**
 * \brief Encode the mac address on a fixed length string of 15 characters.
 * we save space this way.
 */
inline int getlastdigit(unsigned char *data, unsigned long size)
{
  int extended, carry = 0;
  for(unsigned int i=0;i<size;i++)
    {
    extended = (carry << 8) + data[i];
    data[i] = extended / 10;
    carry = extended % 10;
    }
  return carry;
}

size_t System::EncodeBytes(char *out, const unsigned char *data, int size)
{
  bool zero = false;
  int res;
  std::string sres;
  unsigned char buffer[32];
  unsigned char *addr = buffer;
  memcpy(addr, data, size);
  while(!zero)
    {
    res = getlastdigit(addr, size);
    sres.insert(sres.begin(), '0' + res);
    zero = true;
    for(int i = 0; i < size; ++i)
      {
      zero = zero && (addr[i] == 0);
      }
    }

  //return sres;
  strcpy(out, sres.c_str()); //, sres.size() );
  return sres.size();
}

bool System::GetHardwareAddress(unsigned char addr[6])
{
  int stat = 0; //uuid_get_node_id(addr);
  memset(addr,0,6);
  /*
  // For debugging you need to consider the worse case where hardware addres is max number:
  addr[0] = 255;
  addr[1] = 255;
  addr[2] = 255;
  addr[3] = 255;
  addr[4] = 255;
  addr[5] = 255;
  */
  if (stat == 1) // success
    {
    return true;
    }
  // else
  //gdcmWarningMacro("Problem in finding the MAC Address");
  return false;
}

#if defined(_WIN32) && !defined(GDCM_HAVE_GETTIMEOFDAY)
#include <stdio.h>

// http://www.openasthra.com/c-tidbits/gettimeofday-function-for-windows/
// http://www.sisvia.com/blog/?p=24
// -> srand + gettimeofday
// http://csl.sublevel3.org/c++/
static int gettimeofday2(struct timeval *tv, struct timezone *tz)
{
  FILETIME ft;
  const uint64_t c1 = 27111902;
  const uint64_t c2 = 3577643008UL;
  const uint64_t OFFSET = (c1 << 32) + c2;
  uint64_t filetime = 0;
  GetSystemTimeAsFileTime(&ft);

  filetime |= ft.dwHighDateTime;
  filetime <<= 32;
  filetime |= ft.dwLowDateTime;
  filetime -= OFFSET;

  tv->tv_sec = (long)(filetime / 10000000); /* seconds since epoch */
  tv->tv_usec = (uint32_t)((filetime % 10000000) / 10);

  return 0;
}
#if defined(_MSC_VER) || defined(_MSC_EXTENSIONS)
  #define DELTA_EPOCH_IN_MICROSECS  11644473600000000Ui64
#else
  #define DELTA_EPOCH_IN_MICROSECS  11644473600000000ULL
#endif

//struct timezone
//{
//  int  tz_minuteswest; /* minutes W of Greenwich */
//  int  tz_dsttime;     /* type of dst correction */
//};

int gettimeofday(struct timeval *tv, struct timezone *tz)
{
/*
       The use of the timezone structure is obsolete; the tz  argument  should
       normally  be  specified  as  NULL.  The tz_dsttime field has never been
       used under Linux; it has not been and will not be supported by libc  or
       glibc.   Each  and  every occurrence of this field in the kernel source
       (other than the declaration) is a bug. Thus, the following is purely of
       historic interest.
*/
  assert( tz == 0 );
  FILETIME ft;
  unsigned __int64 tmpres = 0;
  //static int tzflag;

  if (NULL != tv)
  {
    GetSystemTimeAsFileTime(&ft);

    tmpres |= ft.dwHighDateTime;
    tmpres <<= 32;
    tmpres |= ft.dwLowDateTime;

    /*converting file time to unix epoch*/
    tmpres /= 10;  /*convert into microseconds*/
    tmpres -= DELTA_EPOCH_IN_MICROSECS;
    tv->tv_sec = (long)(tmpres / 1000000UL);
    tv->tv_usec = (long)(tmpres % 1000000UL);
  }

//  if (NULL != tz)
//  {
//    if (!tzflag)
//    {
//      _tzset();
//      tzflag++;
//    }
//    tz->tz_minuteswest = _timezone / 60;
//    tz->tz_dsttime = _daylight;
//  }

  return 0;
}
#endif

/**
 Implementation note. We internally use mktime which seems to be quite relaxed when it
 comes to invalid date. It handles :
 "17890714172557";
 "19891714172557";
 "19890014172557";
 While the DICOM PS 3.5-2008 would prohibit them.
 I leave it this way so that we correctly read in /almost/ valid date. What we write out is
 always valid anyway which is what is important.
*/
bool System::ParseDateTime(time_t &timep, const char date[22])
{
  long milliseconds;
  return ParseDateTime(timep, milliseconds, date);
}

bool System::ParseDateTime(time_t &timep, long &milliseconds, const char date[22])
{
  if(!date) return false;
  assert( strlen(date) <= 22 );
  size_t len = strlen(date);
  if( len < 4 ) return false; // need at least the full year

  struct tm ptm;
  // No such thing as strptime on some st*$^% platform
  //char *ptr = strptime(date, "%Y%m%d%H%M%S", &ptm);
  // instead write our own:
  int year, mon, day, hour, min, sec, n;
  if ((n = sscanf(date, "%4d%2d%2d%2d%2d%2d",
        &year, &mon, &day, &hour, &min, &sec)) >= 1)
    {
    switch (n)
      {
    case 1: mon = 1;
    case 2: day = 1;
    case 3: hour = 0;
    case 4: min = 0;
    case 5: sec = 0;
      }
    ptm.tm_year = year - 1900;
    ptm.tm_mon = mon - 1;
    ptm.tm_mday = day;
    ptm.tm_hour = hour;
    ptm.tm_min = min;
    ptm.tm_sec = sec;
    ptm.tm_wday = -1;
    ptm.tm_yday = -1;
    ptm.tm_isdst = -1;
    }
  else
    {
    return false;
    }
  timep = mktime(&ptm);
  if( timep == (time_t)-1) return false;

  milliseconds = 0;
  if( len > 14 ) // more data to process
    {
    const char *ptr = date + 14;
    if( *ptr != '.' ) return false;
    ++ptr;
    if( !*ptr || sscanf( ptr, "%06ld", &milliseconds ) != 1 )
      {
      // Could not parse milliseconds but date looks ok, should I return false anyway ?
      // -> yes this is an error !
      return false;
      }
    }

  return true;
}

bool System::FormatDateTime(char date[22], time_t timep, long milliseconds)
{
  // \precondition
  assert( milliseconds >= 0 && milliseconds < 1000000 );

  // YYYYMMDDHHMMSS.FFFFFF&ZZXX
  if(!date) return false;
  const size_t maxsize = 40;
  char tmp[maxsize];
  // Obtain the time of day, and convert it to a tm struct.
  struct tm *ptm = localtime (&timep);
  if(!ptm) return false;
  // Format the date and time, down to a single second.
  size_t ret = strftime (tmp, sizeof (tmp), "%Y%m%d%H%M%S", ptm);
  assert( ret == 14 );
  if( ret == 0 || ret >= maxsize )
    {
    return false;
    }

  // Add milliseconds
  const size_t maxsizall = 22;
  //char tmpAll[maxsizall];
  int ret2 = snprintf(date,maxsizall,"%s.%06ld",tmp,milliseconds);
  assert( ret2 >= 0 );
  if( (unsigned int)ret2 >= maxsizall )
    {
    return false;
    }

  // Ok !
  return true;
}

bool System::GetCurrentDateTime(char date[22])
{
  long milliseconds;
  time_t timep;

#if 0
       The functions gettimeofday() and settimeofday() can  get  and  set  the
       time  as  well  as a timezone.  The tv argument is a struct timeval (as
       specified  in <sys/time.h>):

         struct timeval {
             time_t      tv_sec;     /* seconds */
             suseconds_t tv_usec;    /* microseconds */
         };

       and gives the number of seconds and microseconds since the  Epoch  (see
       time(2)).  The tz argument is a struct timezone:

         struct timezone {
             int tz_minuteswest;     /* minutes west of Greenwich */
             int tz_dsttime;         /* type of DST correction */
         };

       If  either  tv or tz is NULL, the corresponding structure is not set or
       returned.

       The use of the timezone structure is obsolete; the tz  argument  should
       normally  be  specified  as  NULL.  The tz_dsttime field has never been
       used under Linux; it has not been and will not be supported by libc  or
       glibc.   Each  and  every occurrence of this field in the kernel source
       (other than the declaration) is a bug. Thus, the following is purely of
       historic interest.
#endif

  // Apparently suseconds_t is defined as long on linux system... why would this be signed ?

  struct timeval tv;
  gettimeofday (&tv, NULL);
  timep = tv.tv_sec;
  // A concatenated date-time character string in
  // the format:
  // YYYYMMDDHHMMSS.FFFFFF&ZZXX
  // The components of this string, from left to
  // right, are YYYY = Year, MM = Month, DD =
  // Day, HH = Hour (range "00" - "23"), MM =
  // Minute (range "00" - "59"), SS = Second
  // (range "00" - "60").
  // FFFFFF = Fractional Second contains a
  // fractional part of a second as small as 1
  // millionth of a second (range ¿000000¿ -
  // ¿999999¿).
  assert( tv.tv_usec >= 0 && tv.tv_usec < 1000000 );
  milliseconds = tv.tv_usec;

  return FormatDateTime(date, timep, milliseconds);
}

int System::StrNCaseCmp(const char *s1, const char *s2, size_t n)
{
#if defined(GDCM_HAVE_STRNCASECMP)
  return strncasecmp(s1,s2,n);
#elif defined(GDCM_HAVE__STRNICMP)
  return _strnicmp(s1,s2,n);
#else // default implementation
#error
  assert( n ); // TODO
  while (--n && *s1 && (tolower(*s1) == tolower(*s2)))
    {
    s1++;
    s2++;
    }

 return tolower(*s1) - tolower(*s2);
#endif
}

int System::StrCaseCmp(const char *s1, const char *s2)
{
#if defined(GDCM_HAVE_STRCASECMP)
  return strcasecmp(s1,s2);
#elif defined(GDCM_HAVE__STRNICMP)
  return _stricmp(s1,s2);
#else // default implementation
#error
  while (*s1 && (tolower(*s1) == tolower(*s2)))
    {
    s1++;
    s2++;
    }

 return tolower(*s1) - tolower(*s2);
#endif
}

bool System::GetHostName(char name[255])
{
// http://msdn.microsoft.com/en-us/library/ms738527.aspx
// WSANOTINITIALISED A successful WSAStartup call must occur before using this function.
#if _WIN32
  // Get the hostname
  WORD wVersionRequested;
  WSADATA wsaData;
  wVersionRequested = MAKEWORD(2,0);

  if ( WSAStartup( wVersionRequested, &wsaData ) == 0 )
    {
    bool ret = false;
    if( gethostname(name,255) == 0 )
      {
      ret = true;
      }
    else
      {
      *name = 0;
      }
    WSACleanup( );
    return ret;
    }
#else
  if( gethostname(name, 255) == 0 )
    {
    return true;
    }
#endif
  // If reach here gethostname failed, uninit name just in case
  *name = 0;
  return false;
}

} // end namespace gdcm
