#ifdef _MSC_VER
#  pragma warning(disable : 4996) /* deprecation */
#endif

#include "itkTestDriverIncludeRequiredIOFactories.h"

extern int
itkMorphologicalContourInterpolationTest(int argc, char * argv[]);

int
main(int argc, char * argv[])
{
  RegisterRequiredFactories();
  itkMorphologicalContourInterpolationTest(argc, argv);
}
