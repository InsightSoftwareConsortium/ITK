/* Name of package */
#define PACKAGE "log4cxx"

/* Version number of package */
#define VERSION "0.9.6"

/* Define to 1 if you have the `ftime' function. */
#define HAVE_FTIME 1

/* ODBC support through Microsoft ODBC. */
#define HAVE_MS_ODBC 1

/* thread support through Microsoft threads. */
#define HAVE_MS_THREAD 1

/* XML support through Microsoft XML. */
#define HAVE_MS_XML 1

/* ODBC support */
#define HAVE_ODBC 1

/* thread support */
#define HAVE_THREAD 1

/* XML support */
#define HAVE_XML 1

typedef __int64 int64_t;

#ifdef WIN32
#pragma warning(disable : 4250 4251 4786 4290)
#endif

//#define LOG4CXX_STATIC

#ifdef LOG4CXX_STATIC
#define LOG4CXX_EXPORT
// cf. file msvc/static/static.cpp
// #pragma comment(linker, "/include:?ForceSymbolReferences@@YAXXZ")
#else // DLL
#ifdef LOG4CXX
  #define LOG4CXX_EXPORT __declspec(dllexport)
#else
  #define LOG4CXX_EXPORT __declspec(dllimport)
#endif
#endif

#define _WIN32_WINNT 0x0400
