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

#include <iostream>
#include "itkImageRegionIterator.h"
#include "itkNonThreadedShrinkImageFilter.h"
#include "itkFileOutputWindow.h"

int itkNonThreadedShrinkImageTest(int, char* [] )
{
  itk::FileOutputWindow::Pointer fow = itk::FileOutputWindow::New();
  fow->SetInstance(fow);

  std::cout << "Shrink an image by (2,3)" << std::endl;

  // typedefs to simplify the syntax
  typedef itk::Image<short, 2>   SimpleImage;
  SimpleImage::Pointer simpleImage = SimpleImage::New();
  std::cout << "Simple image spacing: " << simpleImage->GetSpacing()[0] << ", "
            << simpleImage->GetSpacing()[1] << std::endl;
  std::cout << std::flush;

  // typedefs to simplify the syntax
  typedef itk::Image<short, 2>   ShortImage;

  // Test the creation of an image with native type
  ShortImage::Pointer if2 = ShortImage::New();

  // fill in an image
  ShortImage::IndexType  index = {{0, 0}};
  ShortImage::SizeType   size = {{8, 12}};
  ShortImage::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );
  if2->SetLargestPossibleRegion( region );
  if2->SetBufferedRegion( region );
  if2->Allocate();

  itk::ImageRegionIterator<ShortImage> iterator(if2, region);

  short i=0;
  for (; !iterator.IsAtEnd(); ++iterator, ++i)
    {
    iterator.Set( i );
    }

  // Create a filter, shrink by 2,3
  itk::NonThreadedShrinkImageFilter< ShortImage, ShortImage >::Pointer shrink;
  shrink = itk::NonThreadedShrinkImageFilter< ShortImage, ShortImage >::New();
  shrink->SetInput( if2 );
  shrink->SetNumberOfThreads(4);

  unsigned int factors[2] = { 2, 3 };
  shrink->SetShrinkFactors(factors);
  shrink->UpdateLargestPossibleRegion();

  std::cout << "Input spacing: " << if2->GetSpacing()[0] << ", "
            << if2->GetSpacing()[1] << std::endl;
  std::cout << std::flush;
  std::cout << "Output spacing: " << shrink->GetOutput()->GetSpacing()[0]
            << ", "
            << shrink->GetOutput()->GetSpacing()[1] << std::endl;
  std::cout << std::flush;
  std::cout << "Input Requested region: " << shrink->GetInput()->GetRequestedRegion() << std::endl;
  std::cout << std::flush;
  std::cout << "Output Requested region: " << shrink->GetOutput()->GetRequestedRegion() << std::endl;
  std::cout << std::flush;

  //
  // This code determines whether the shrink code produced
  // the image we expected.
  //
  ShortImage::RegionType requestedRegion;
  requestedRegion = shrink->GetOutput()->GetRequestedRegion();

  itk::ImageRegionIterator<ShortImage>
    iterator2(shrink->GetOutput(), requestedRegion);

  // If size is not a multiple of the shrink factors, then adjust the
  // row/col indices
  short rowOffset = 0;
  short colOffset = 0;
  if (region.GetSize()[1] % shrink->GetShrinkFactors()[1])
    {
    rowOffset = static_cast<short>(
      region.GetSize()[1] / 2.0 -
      ((region.GetSize()[1] / shrink->GetShrinkFactors()[1]) / 2.0 *
       shrink->GetShrinkFactors()[1])
      );
    }
  if (region.GetSize()[0] % shrink->GetShrinkFactors()[0])
    {
    colOffset = static_cast<short>(
      region.GetSize()[0] / 2.0 -
      ((region.GetSize()[0] / shrink->GetShrinkFactors()[0]) / 2.0 *
       shrink->GetShrinkFactors()[0])
      );
    }

  bool passed = true;
 for (; !iterator2.IsAtEnd(); ++iterator2)
    {
    short col = itk::Math::RoundHalfIntegerUp<short>(shrink->GetShrinkFactors()[0] * iterator2.GetIndex()[0] +
                                              (shrink->GetShrinkFactors()[0]-1.0) / 2.0);
    col += colOffset;

    short row = itk::Math::RoundHalfIntegerUp<short>(shrink->GetShrinkFactors()[1] * iterator2.GetIndex()[1] +
                                              (shrink->GetShrinkFactors()[1] - 1.0) / 2.0);
    row += rowOffset;
    short trueValue = col + region.GetSize()[0] * row;

    if ( iterator2.Get() != trueValue )
      {
      passed = false;
      std::cout << "Pixel " << iterator2.GetIndex()
                << " expected " << trueValue
                << " but got " << iterator2.Get()
                << std::endl;
      }
    }

  // Now test shrinking by 2x2
  std::cout << "Shrink the image by (2,2) instead." << std::endl;
  std::cout << std::flush;

  factors[1] = 2;
  shrink->SetShrinkFactors(factors);

  // ask for an invalid requested region to force an exception
  std::cout << "Assign an invalid requested region which should throw an exception." << std::endl;
  std::cout << std::flush;
  itk::Index<2> foo;
  itk::ImageRegion<2>::IndexValueType fooindex[] = {100, 100};
  foo.SetIndex(fooindex);
  itk::ImageRegion<2> fooregion;
  fooregion = shrink->GetOutput()->GetRequestedRegion();
  fooregion.SetIndex(foo);
  shrink->GetOutput()->SetRequestedRegion( fooregion );

  try
    {
    // this should fail due to a bad requested region
    shrink->Update();
    }
  catch (itk::InvalidRequestedRegionError & e)
    {
    std::cout << e << std::endl;
    std::cout << std::endl << std::endl
              << "Exception caught, updating largest possible region instead."
              << std::endl;
    std::cout << std::flush;
    shrink->ResetPipeline();
    shrink->UpdateLargestPossibleRegion();
    }
  catch (...)
    {
    std::cout << "Exception missed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << std::endl << std::endl;
  std::cout << std::flush;
  std::cout << "Input spacing: " << if2->GetSpacing()[0] << ", "
            << if2->GetSpacing()[1] << std::endl;
  std::cout << std::flush;
  std::cout << "Shrink filter: " << shrink;
  std::cout << "Shrink filter output: " << shrink->GetOutput();
  std::cout << "Output spacing: " << std::flush << shrink->GetOutput()->GetSpacing()[0]
            << ", "
            << shrink->GetOutput()->GetSpacing()[1] << std::endl;
  std::cout << std::flush;
  std::cout << "Input Requested region: " << std::flush << shrink->GetInput()->GetRequestedRegion() << std::endl;
  std::cout << std::flush;
  std::cout << "Output Requested region: " << shrink->GetOutput()->GetRequestedRegion() << std::endl;
  std::cout << std::flush;

  std::cout << shrink << std::endl;
  std::cout << std::flush;
  std::cout << "Input" << std::endl << shrink->GetInput() << std::endl;
  std::cout << std::flush;
  std::cout << "Output" << std::endl << shrink->GetOutput() << std::endl;
  std::cout << std::flush;

  requestedRegion = shrink->GetOutput()->GetRequestedRegion();
  iterator2 = itk::ImageRegionIterator<ShortImage>(shrink->GetOutput(), requestedRegion);

  std::cout << "Output image" << std::endl;
  std::cout << std::flush;
  for (; !iterator2.IsAtEnd(); ++iterator2)
    {
    std::cout << "Pixel " << iterator2.GetIndex() << " = " << iterator2.Get()
              << std::endl;
    std::cout << std::flush;

    short trueValue =
      itk::Math::RoundHalfIntegerUp<short>((shrink->GetShrinkFactors()[0] * iterator2.GetIndex()[0] +
                                      (shrink->GetShrinkFactors()[0] - 1.0) / 2.0)) +
      (region.GetSize()[0] * itk::Math::RoundHalfIntegerUp<short>((shrink->GetShrinkFactors()[1] * iterator2.GetIndex()[1] +
                                                            (shrink->GetShrinkFactors()[1] - 1.0) / 2.0)));
    if ( iterator2.Get() != trueValue )
      {
      std::cout << "B) Pixel " << iterator2.GetIndex()
                << " expected " << trueValue
                << " but got " << iterator2.Get()
                << std::endl;
      passed = false;
      }
    }

  std::cout << std::endl;
  std::cout << std::flush;
  if (passed)
    {
    std::cout << "Recovered from the exception." << std::endl;
    std::cout << "NonThreadedShrinkImageFilter test passed." << std::endl;
    std::cout << std::flush;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "NonThreadedShrinkImageFilter test failed." << std::endl;
    std::cout << std::flush;
    return EXIT_FAILURE;
    }

}
