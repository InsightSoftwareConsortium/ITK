

#ifdef _WIN32
#include <io.h>
#include <fcntl.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if NRRD_RESAMPLE_FLOAT
#  define nrrdResample_nrrdType nrrdTypeFloat
#  define EVALN evalN_f               /* NrrdKernel method */
#else
#  define nrrdResample_nrrdType nrrdTypeDouble
#  define EVALN evalN_d               /* NrrdKernel method */
#endif

#define _NRRD_TEXT_INCR 1024

/* to access whatever nrrd there may be in in a NrrdIter */
#define _NRRD_ITER_NRRD(iter) ((iter)->nrrd ? (iter)->nrrd : (iter)->ownNrrd)

/*
** _NRRD_SPACING
**
** returns nrrdDefSpacing if the argument doesn't exist, otherwise
** returns the argument
*/
#define _NRRD_SPACING(spc) (AIR_EXISTS(spc) ? spc: nrrdDefSpacing)

typedef union {
  char **CP;
  int *I;
  unsigned int *UI;
  double *D;
  const void *P;
} _nrrdAxisInfoSetPtrs;

typedef union {
  char **CP;
  int *I;
  unsigned int *UI;
  double *D;
  void *P;
} _nrrdAxisInfoGetPtrs;

/* keyvalue.c */
extern int _nrrdKeyValueFwrite(FILE *file, const char *prefix,
                               const char *key, const char *value);

/* formatXXX.c */
extern const NrrdFormat _nrrdFormatNRRD;
extern const NrrdFormat _nrrdFormatPNM;
extern const NrrdFormat _nrrdFormatPNG;
extern const NrrdFormat _nrrdFormatVTK;
extern const NrrdFormat _nrrdFormatText;
extern const NrrdFormat _nrrdFormatEPS;

/* encodingXXX.c */
extern const NrrdEncoding _nrrdEncodingRaw;
extern const NrrdEncoding _nrrdEncodingAscii;
extern const NrrdEncoding _nrrdEncodingHex;
extern const NrrdEncoding _nrrdEncodingGzip;
extern const NrrdEncoding _nrrdEncodingBzip2;

/* read.c */
extern int _nrrdOneLine (int *lenP, NrrdIoState *nio, FILE *file);
extern int _nrrdCalloc (Nrrd *nrrd, NrrdIoState *nio);

/* arrays.c */
extern int _nrrdFieldValidInImage[NRRD_FIELD_MAX+1];
extern int _nrrdFieldValidInText[NRRD_FIELD_MAX+1];
extern int _nrrdFieldOnePerAxis[NRRD_FIELD_MAX+1];
extern char _nrrdEnumFieldStr[NRRD_FIELD_MAX+1][AIR_STRLEN_SMALL];
extern int _nrrdFieldRequired[NRRD_FIELD_MAX+1];

/* simple.c */
extern char *_nrrdContentGet(const Nrrd *nin);
extern int _nrrdContentSet_nva(Nrrd *nout, const char *func,
                               char *content, const char *format,
                               va_list arg);
extern int _nrrdContentSet(Nrrd *nout, const char *func,
                           char *content, const char *format, ...);


/* axis.c */
extern int _nrrdKindAltered(int kindIn);
extern void _nrrdAxisInfoCopy(NrrdAxisInfo *dest, const NrrdAxisInfo *src,
                              int bitflag);
extern void _nrrdAxisInfoInit(NrrdAxisInfo *axis);
extern int _nrrdCenter(int center);
extern int _nrrdCenter2(int center, int def);

/* convert.c */
extern void (*_nrrdConv[][NRRD_TYPE_MAX+1])(void *, const void *, size_t);

/* read.c */
extern char _nrrdFieldStr[NRRD_FIELD_MAX+1][AIR_STRLEN_SMALL];
extern char _nrrdRelativePathFlag[];
extern char _nrrdFieldSep[];
extern char _nrrdTextSep[];
extern int _nrrdReshapeUpGrayscale(Nrrd *nimg);
extern void _nrrdSplitName(char **dirP, char **baseP, const char *name);

/* write.c */
extern int _nrrdFieldInteresting (const Nrrd *nrrd, NrrdIoState *nio,
                                  int field);
extern void _nrrdSprintFieldInfo(char **strP, char *prefix,
                                 const Nrrd *nrrd, NrrdIoState *nio,
                                 int field);
extern void _nrrdFprintFieldInfo(FILE *file, char *prefix,
                                 const Nrrd *nrrd, NrrdIoState *nio,
                                 int field);
extern int _nrrdReshapeDownGrayscale(Nrrd *nimg);

/* parseNrrd.c */
extern int (*_nrrdReadNrrdParseInfo[NRRD_FIELD_MAX+1])(Nrrd *nrrd,
                                                       NrrdIoState *nio,
                                                       int useBiff);
extern int _nrrdReadNrrdParseField(Nrrd *nrrd, NrrdIoState *nio, int useBiff);

/* methods.c */
extern int _nrrdSizeCheck(int dim, const int *size, int useBiff);
extern void _nrrdTraverse(Nrrd *nrrd);
extern int _nrrdCopyShallow (Nrrd *nout, const Nrrd *nin);

#if TEEM_ZLIB
#include <zlib.h>

/* gzio.c */
extern gzFile _nrrdGzOpen(FILE* fd, const char *mode);
extern int _nrrdGzClose(gzFile file);
extern int _nrrdGzRead(gzFile file, voidp buf, unsigned int len,
                       unsigned int* read);
extern int _nrrdGzWrite(gzFile file, const voidp buf, unsigned int len,
                        unsigned int* written);
#endif

/* ccmethods.c */
extern int _nrrdCC_settle(int *map, int len);
extern int _nrrdCC_eclass(int *map, int len, airArray *eqvArr);

#ifdef __cplusplus
}
#endif

