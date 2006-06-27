#include "metaTypes.h"

#ifndef ITKMetaIO_METAIMAGEUTILS_H
#define ITKMetaIO_METAIMAGEUTILS_H

#include "metaImageTypes.h"

#if (METAIO_USE_NAMESPACE)
namespace METAIO_NAMESPACE {
#endif

METAIO_EXPORT bool MET_StringToImageModality(const char * _str,
                                      MET_ImageModalityEnumType * _type);

METAIO_EXPORT bool MET_ImageModalityToString(MET_ImageModalityEnumType _type,
                                      char * _str);

#if (METAIO_USE_NAMESPACE)
};
#endif

#endif
