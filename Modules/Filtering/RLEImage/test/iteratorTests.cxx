/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkRLEImage.h"

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
  using charred2dType = itk::RLEImage<char, 2, char>; // test size 256
  charred2dType::Pointer t2 = charred2dType::New();
  using charred4dType = itk::RLEImage<char, 4>; // test size 65536
  charred4dType::Pointer t4 = charred4dType::New();
}
