#include <stdio.h>

#include "metaImageTypes.h"

#if (METAIO_USE_NAMESPACE)
namespace METAIO_NAMESPACE {
#endif


bool MET_StringToImageModality(const char * _str,
                               MET_ImageModalityEnumType * _type)
  {
  int i;

  for(i=0; i<MET_NUM_IMAGE_MODALITY_TYPES; i++)
    if(!strcmp(MET_ImageModalityTypeName[i], _str))
      {
      *_type = (MET_ImageModalityEnumType)i;
      return true;
      }

  *_type = MET_MOD_UNKNOWN;

  return false;
  }

bool MET_ImageModalityToString(MET_ImageModalityEnumType _type,
                               char * _str)
  {
  strcpy(_str, MET_ImageModalityTypeName[(int)_type]);
  return true;
  }

#if (METAIO_USE_NAMESPACE)
};
#endif

