#include "RLEImageScanlineIterator.h"

extern int
itkImageScanlineIteratorTest1(int argc, char * argv[]);
extern int
itkIteratorTests(int argc, char * argv[]);
extern int
itkImageIteratorTest(int argc, char * argv[]);
extern int
itkImageIteratorsForwardBackwardTest(int argc, char * argv[]);
extern int
itkImageIteratorWithIndexTest(int argc, char * argv[]);
extern int
itkImageRegionConstIteratorWithOnlyIndexTest(int argc, char * argv[]);
extern int
itkImageRegionIteratorTest(int argc, char * argv[]);
extern int
itkRegionOfInterestImageFilterTest(int argc, char * argv[]);

int
main(int argc, char * argv[])
{
  itkImageRegionIteratorTest(argc, argv);
  itkImageScanlineIteratorTest1(argc, argv);
  itkIteratorTests(argc, argv);
  itkImageIteratorTest(argc, argv);
  itkImageIteratorsForwardBackwardTest(argc, argv);
  itkImageIteratorWithIndexTest(argc, argv);
  itkImageRegionConstIteratorWithOnlyIndexTest(argc, argv);
  itkRegionOfInterestImageFilterTest(argc, argv);
  typedef RLEImage<char, 2, char> charred2dType; // test size 256
  typedef RLEImage<char, 4>       charred4dType; // test size 65536
}
