/* f2c.h  --  Standard Fortran to C header file */

/**  barf  [ba:rf]  2.  "He suggested using FORTRAN, and everybody barfed."

    - From The Shogakukan DICTIONARY OF NEW ENGLISH (Second edition) */

#ifndef F2C_INCLUDE
#define F2C_INCLUDE

/* JVM - turned off warnings in f2c generated code */
#if defined(_MSC_VER)
#if defined(__ICL)
#pragma warning(disable: 239 264 1011 )
#else
#pragma warning(disable: 4101 4244 4554 4756 4723)
#endif
#endif

typedef long int integer;
typedef long int logical;
typedef float real;
typedef double doublereal;


typedef long int flag;
typedef long int ftnlen;
typedef long int ftnint;

#define TRUE_ (1)
#define FALSE_ (0)

#define max(a,b) ((a) >= (b) ? (a) : (b))
#define min(a,b) ((a) <= (b) ? (a) : (b))
#define abs(x) ((x) >= 0 ? (x) : -(x))

/*external read, write*/
typedef struct
{   flag cierr;
    ftnint ciunit;
    flag ciend;
    char *cifmt;
    ftnint cirec;
} cilist;


#endif
