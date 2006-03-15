#ifndef METAIMAGEUTILS_H
#define METAIMAGEUTILS_H

#include "metaImageTypes.h"

extern bool MET_StringToImageModality(const char * _str,
                                      MET_ImageModalityEnumType * _type);

extern bool MET_ImageModalityToString(MET_ImageModalityEnumType _type,
                                      char * _str);

#endif
