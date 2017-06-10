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
#include "itkBinShrinkImageFilter.h"
#include "itkShrinkImageFilter.h"
#include "itkPipelineMonitorImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkTestingMacros.h"

int itkBinShrinkImageFilterTest1( int , char *[] )
{

  // typedefs to simplify the syntax
  typedef itk::Image< int, 2 >      InputImageType;
  typedef itk::Image< long int, 2 > OutputImageType;
  InputImageType::Pointer sourceImage = InputImageType::New();

  // fill in an image
  InputImageType::IndexType  index = {{100, 100}};
  InputImageType::SizeType   size = {{12, 20}};
  InputImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );
  sourceImage->SetRegions( region );
  sourceImage->Allocate();

    {
    itk::ImageRegionIteratorWithIndex<InputImageType> outIt(sourceImage, region);
    for ( outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt )
      {
      // we multiply by ten so for more precision
      outIt.Set( outIt.GetIndex()[0]*10 );
      }
    }

  // assemple pipeline
  typedef itk::PipelineMonitorImageFilter<InputImageType>
    InputMonitorFilterType;
  InputMonitorFilterType::Pointer monitor1 = InputMonitorFilterType::New();
  monitor1->SetInput( sourceImage );

  typedef itk::BinShrinkImageFilter< InputImageType, OutputImageType >
    BinShrinkFilterType;
  BinShrinkFilterType::Pointer bin = BinShrinkFilterType::New();

  // Exercise some methods for coverage
  EXERCISE_BASIC_OBJECT_METHODS( bin, BinShrinkImageFilter,
    ImageToImageFilter );

  bin->SetInput( monitor1->GetOutput() );

  itk::ModifiedTimeType t = bin->GetMTime();

  std::cout << bin;
  TEST_EXPECT_EQUAL(bin->GetShrinkFactors()[0], 1 );

  // test no change
  bin->SetShrinkFactors(1);
  TEST_EXPECT_EQUAL(bin->GetShrinkFactors()[0], 1 );
  TEST_EXPECT_EQUAL(t,  bin->GetMTime());

  // test zero value
  bin->SetShrinkFactors(0);
  TEST_EXPECT_EQUAL(bin->GetShrinkFactors()[0], 1 );
  TEST_EXPECT_TRUE( t != bin->GetMTime() );
  t = bin->GetMTime();

  // no change
  bin->SetShrinkFactor(0,1);
  TEST_EXPECT_EQUAL(bin->GetShrinkFactors()[0], 1 );
  TEST_EXPECT_EQUAL(t,  bin->GetMTime());

  bin->SetShrinkFactor(0,2);
  TEST_EXPECT_EQUAL(bin->GetShrinkFactors()[0], 2 );
  TEST_EXPECT_TRUE( t != bin->GetMTime() );


  typedef itk::PipelineMonitorImageFilter<OutputImageType> OutputMonitorFilterType;
  OutputMonitorFilterType::Pointer monitor2 = OutputMonitorFilterType::New();
  monitor2->SetInput(bin->GetOutput());

  bool failed = false;


  try
    {
    // update with 1,1 shrink factor
    unsigned int factors[2] = { 1, 1 };
    std::cout << "== Testing with shrink factors " << factors[0] << " " << factors[1] << " == " << std::endl;
    bin->SetShrinkFactors(factors);
    monitor2->Update();

    // check values
    itk::ImageRegionConstIteratorWithIndex<OutputImageType> inIt( monitor2->GetOutput(),
                                                            monitor2->GetOutput()->GetLargestPossibleRegion() );
    for (inIt.GoToBegin(); !inIt.IsAtEnd(); ++inIt)
      {
      if (inIt.Get() != inIt.GetIndex()[0]*10)
        {
        std::cout << "Wrong pixel value at " << inIt.GetIndex() << " of " << inIt.Get() << std::endl;
        failed = true;
        }
      }
    }
  catch (itk::ExceptionObject &e)
    {
    std::cout << "Exception: " << e << std::endl;
    failed = true;
    }

  try
    {
    // update with 2,1 shrink factor
    unsigned int factors[2] = { 2, 1 };
    std::cout << "== Testing with shrink factors " << factors[0] << " " << factors[1] << " == " << std::endl;
    bin->SetShrinkFactors(factors);
    monitor2->UpdateLargestPossibleRegion();

    // check values
    itk::ImageRegionConstIteratorWithIndex<OutputImageType> inIt( monitor2->GetOutput(),
                                                            monitor2->GetOutput()->GetLargestPossibleRegion() );
    for (inIt.GoToBegin(); !inIt.IsAtEnd(); ++inIt)
      {
      if (inIt.Get() != static_cast< int >( inIt.GetIndex()[0]*factors[0] )*10 + 5 )
        {
        std::cout << "Wrong pixel value at " << inIt.GetIndex() << " of " << inIt.Get() << std::endl;
        failed = true;
        }
      }
    }
  catch (itk::ExceptionObject &e)
    {
    std::cout << "Exception: " << e << std::endl;
    failed = true;
    }

  try
    {
    // update with 2,2 shrink factor
    unsigned int factors[2] = { 2, 2 };
    std::cout << "== Testing with shrink factors " << factors[0] << " " << factors[1] << " == " << std::endl;
    bin->SetShrinkFactors(factors);
    monitor2->UpdateLargestPossibleRegion();

    // check values
    itk::ImageRegionConstIteratorWithIndex<OutputImageType> inIt( monitor2->GetOutput(),
                                                            monitor2->GetOutput()->GetLargestPossibleRegion() );
    for (inIt.GoToBegin(); !inIt.IsAtEnd(); ++inIt)
      {
      if (inIt.Get() != static_cast< int >( inIt.GetIndex()[0]*factors[0] )*10 + 5 )
        {
        std::cout << "Wrong pixel value at " << inIt.GetIndex() << " of " << inIt.Get() << std::endl;
        failed = true;
        }
      }
    }
  catch (itk::ExceptionObject &e)
    {
    std::cout << "Exception: " << e << std::endl;
    failed = true;
    }

  try
    {
    // update with 5,2 shrink factor
    unsigned int factors[2] = { 5, 2 };
    std::cout << "== Testing with shrink factors " << factors[0] << " " << factors[1] << " == " << std::endl;
    bin->SetShrinkFactors(factors);
    monitor2->UpdateLargestPossibleRegion();

    // check values
    itk::ImageRegionConstIteratorWithIndex<OutputImageType> inIt( monitor2->GetOutput(),
                                                            monitor2->GetOutput()->GetLargestPossibleRegion() );
    for (inIt.GoToBegin(); !inIt.IsAtEnd(); ++inIt)
      {
      if (inIt.Get() != static_cast< int >( inIt.GetIndex()[0]*factors[0]+2)*10 )
        {
        std::cout << "Wrong pixel value at " << inIt.GetIndex() << " of " << inIt.Get() << std::endl;
        failed = true;
        }
      }
    }
  catch (itk::ExceptionObject &e)
    {
    std::cout << "Exception: " << e << std::endl;
    failed = true;
    }

  std::cout << "-- Changing starting index to odd numer --" << std::endl;

  // set to odd index then reallocate and set values
  index[0] = 101;
  region.SetIndex( index );
  sourceImage->SetRegions( region );
  sourceImage->Allocate();

    {
    itk::ImageRegionIteratorWithIndex<InputImageType> outIt(sourceImage, region);
    for ( outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt )
      {
      // we multiply by ten for more precision
      outIt.Set( outIt.GetIndex()[0]*10 );
      }
    }
  monitor1->SetInput( sourceImage );

  try
    {
    // update with 2,1 shrink factor
    unsigned int factors[2] = { 2, 1 };
    std::cout << "== Testing with shrink factors " << factors[0] << " " << factors[1] << " == " << std::endl;
    bin->SetShrinkFactors(factors);
    monitor2->UpdateLargestPossibleRegion();

    // check values
    itk::ImageRegionConstIteratorWithIndex<OutputImageType> inIt( monitor2->GetOutput(),
                                                            monitor2->GetOutput()->GetLargestPossibleRegion() );
    for (inIt.GoToBegin(); !inIt.IsAtEnd(); ++inIt)
      {
      if (inIt.Get() != static_cast< int >( inIt.GetIndex()[0]*factors[0] )*10 + 5 )
        {
        std::cout << "Wrong pixel value at " << inIt.GetIndex() << " of " << inIt.Get() << std::endl;
        failed = true;
        }
      }
    }
  catch (itk::ExceptionObject &e)
    {
    std::cout << "Exception: " << e << std::endl;
    failed = true;
    }

  try
    {
    // update with 3,1 shrink factor
    unsigned int factors[2] = { 3, 1 };
    std::cout << "== Testing with shrink factors " << factors[0] << " " << factors[1] << " == " << std::endl;
    bin->SetShrinkFactors(factors);
    monitor2->UpdateLargestPossibleRegion();

    // check values
    itk::ImageRegionConstIteratorWithIndex<OutputImageType> inIt( monitor2->GetOutput(),
                                                            monitor2->GetOutput()->GetLargestPossibleRegion() );
    for (inIt.GoToBegin(); !inIt.IsAtEnd(); ++inIt)
      {
      if (inIt.Get() != static_cast< int >(inIt.GetIndex()[0]*factors[0]+1 )*10 )
        {
        std::cout << "Wrong pixel value at " << inIt.GetIndex() << " of " << inIt.Get() << std::endl;
        failed = true;
        }
      }
    }
  catch (itk::ExceptionObject &e)
    {
    std::cout << "Exception: " << e << std::endl;
    failed = true;
    }

  index[0] = 0;
  index[1] = 3;
  region.SetIndex( index );

  for (unsigned int shrink = 1; shrink < 12; ++shrink)
    {
    // update with 3,1 shrink factor
    unsigned int factors[2] = { 1, shrink };
    std::cout << "== Testing with shrink factors " << factors[0] << " " << factors[1] << " == " << std::endl;

    for (unsigned int x = 1; x < 10; ++x )
      for (unsigned int y = 2*shrink; y < 20; ++y )
        {
        // fill in an image
        size[0] = x;
        size[1] = y;
        region.SetSize( size );
        sourceImage->SetRegions( region );
        sourceImage->Allocate();

        std::cout << "--Resolution " << x << " " << y << "--" << std::endl;

        itk::ImageRegionIteratorWithIndex<InputImageType> outIt(sourceImage, region);
        for ( outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt )
          {
          // we multiply by ten so for more precision
          outIt.Set( outIt.GetIndex()[0]*10 );
          }

        try
          {
          bin->SetShrinkFactors(factors);
          monitor2->UpdateLargestPossibleRegion();
          // check values
          itk::ImageRegionConstIteratorWithIndex<OutputImageType> inIt( monitor2->GetOutput(),
                                                                  monitor2->GetOutput()->GetLargestPossibleRegion() );
          bool lfailed = false;
          for (inIt.GoToBegin(); !inIt.IsAtEnd(); ++inIt)
            {
            int expectedValue = inIt.GetIndex()[0]*10;
            if (inIt.Get() != expectedValue )
              {
              if (!lfailed)
                {
                std::cout << "--Resolution " << x << " " << y << "--" << std::endl;
                }
              std::cout << "Wrong pixel value at " << inIt.GetIndex() << " of " << inIt.Get() << ", expected: " <<
              expectedValue << std::endl;
              lfailed = failed = true;
              }
            }
          if (lfailed)
            {
            monitor2->GetOutput()->Print(std::cout);
            exit(1);
            }
          }
        catch (itk::ExceptionObject &e)
          {
          std::cout << "Exception: " << e << std::endl;
          failed = true;
          }

        }
    }

  if ( failed )
    {
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
