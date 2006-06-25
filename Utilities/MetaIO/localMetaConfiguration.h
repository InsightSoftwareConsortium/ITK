#ifdef METAIO_USE_NAMESPACE
  #undef METAIO_USE_NAMESPACE 
#endif
#ifdef METAIO_NAMESPACE
  #undef METAIO_NAMESPACE 
#endif
#ifdef METAIO_STD
  #undef METAIO_STD 
#endif
#ifdef METAIO_EXPORT
  #undef METAIO_EXPORT
#endif 

#define METAIO_USE_NAMESPACE  0
#define METAIO_NAMESPACE      ITKMetaIO

#define METAIO_STL    std

#define METAIO_STREAM std

#include <iostream>
#include <fstream>

#define METAIO_EXPORT 

