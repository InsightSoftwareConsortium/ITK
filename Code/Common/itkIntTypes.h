#ifndef __itkIntTypes_h
#define	__itkIntTypes_h

#ifdef __cplusplus
extern "C" {
#endif

typedef char			ITK_INT8;
typedef int 			ITK_INT32;

#ifndef WIN32
typedef	long long		ITK_INT64;
#endif

#ifdef WIN32
typedef long			ITK_INT64;
#endif

typedef unsigned char	ITK_UINT8;
typedef unsigned short	ITK_UINT16;
typedef unsigned 		ITK_UINT32;

#ifndef WIN32
typedef unsigned long long	ITK_UINT64;
#endif

#ifdef WIN32
typedef unsigned long	ITK_UINT64;
#endif

typedef int  			ITK_INTPTR;
typedef unsigned 		ITK_UINTPTR;

#ifdef __cplusplus
}
#endif

#endif	/* __itkIntTypes_h */

