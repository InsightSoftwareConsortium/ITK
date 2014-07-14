/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
/*
  This tests the classes ImageRandomNonRepeatingIteratorWithIndex and
  ImageRandomNonRepeatingConstIteratorWithIndex.  This was contributed
  by Rupert Brooks, McGill Centre for Intelligent
  Machines, Montreal, Canada.  It is heavily based on the
  ImageRandomIterator test program.
*/

#include <iostream>
#include "itkImageRandomNonRepeatingConstIteratorWithIndex.h"

int itkImageRandomNonRepeatingIteratorWithIndexTest2(int, char* [])
{
  const unsigned int ImageDimension = 2;
  typedef itk::Index< ImageDimension >             PixelType;
  typedef itk::Image< PixelType, ImageDimension >  ImageType;

  typedef itk::ImageRandomNonRepeatingConstIteratorWithIndex< ImageType >
    RandomConstIteratorType;
  const unsigned long N = 10;
  const int Seed = 42;
  ImageType::SizeType size;
  size.Fill(N);
  ImageType::IndexType start;
  start.Fill(0);
  ImageType::RegionType region;
  region.SetIndex( start );
  region.SetSize( size );
  ImageType::Pointer myImage = ImageType::New();
  myImage->SetLargestPossibleRegion( region );
  myImage->SetBufferedRegion( region );
  myImage->SetRequestedRegion( region );
  myImage->Allocate();
  typedef std::vector<ImageType::IndexType>            WalkType;
  typedef WalkType::iterator                           WalkIteratorType;
  WalkType firstWalk(N);
  RandomConstIteratorType firstIt(myImage, region);
  firstIt.ReinitializeSeed( Seed );
  firstIt.SetNumberOfSamples( region.GetNumberOfPixels() );
  for (firstIt.GoToBegin(); !firstIt.IsAtEnd(); ++firstIt)
    {
    firstWalk.push_back( firstIt.GetIndex() );
    }
  WalkType secondWalk(N);
  RandomConstIteratorType secondIt(myImage, region);
  secondIt.ReinitializeSeed( Seed );
  secondIt.SetNumberOfSamples( region.GetNumberOfPixels() );
  for (secondIt.GoToBegin(); !secondIt.IsAtEnd(); ++secondIt)
    {
    secondWalk.push_back( secondIt.GetIndex() );
    }
  std::pair<WalkIteratorType,WalkIteratorType> mismatchTest;
  mismatchTest =
    std::mismatch( firstWalk.begin(),
                   firstWalk.end(),secondWalk.begin() );
  if ( mismatchTest.first != firstWalk.end() )
    {
    std::cerr << "Two iterations with the same seed do not"
              << " walk over the same pixels" << std::endl
              << "First mismatch found after "
              << mismatchTest.first - firstWalk.begin()
              << " iterations." << std::endl
              << "First walk index  : " << *(mismatchTest.first) << std::endl
              << "Second walk index : " << *(mismatchTest.second) << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;
}
