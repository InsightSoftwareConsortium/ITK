#ifdef CABLE_CONFIGURATION
#include "wrap_ITKCommon.h"
namespace _cable_
{
  const char* const package = ITK_WRAP_PACKAGE;
  const char* const groups[] =
  {
    ITK_WRAP_GROUP(ITKBase),
    ITK_WRAP_GROUP(itkImage)
  };
}
#endif
