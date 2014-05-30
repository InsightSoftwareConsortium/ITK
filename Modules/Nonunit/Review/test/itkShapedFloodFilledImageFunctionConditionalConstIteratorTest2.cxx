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
// Test itk::ShapedFloodFilledImageFunctionConditionalConstIterator on a 2D
// image using 4 connectivity.

#include "itkBinaryThresholdImageFunction.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkFloodFilledImageFunctionConditionalConstIterator.h"

#include "itkShapedFloodFilledImageFunctionConditionalConstIterator.h"

int itkShapedFloodFilledImageFunctionConditionalConstIteratorTest2( int, char * [] )
{
  try
    {
    const unsigned int ImageDimension = 2;
    typedef unsigned char PixelType;

    typedef itk::Image<PixelType, ImageDimension> ImageType;
    typedef ImageType::RegionType                 RegionType;
    typedef ImageType::IndexType                  IndexType;

    typedef itk::BinaryThresholdImageFunction<ImageType> FunctionType;
    typedef itk::FloodFilledImageFunctionConditionalConstIterator<
      ImageType, FunctionType> FloodFilledIteratorType;
    typedef itk::ShapedFloodFilledImageFunctionConditionalConstIterator<
      ImageType, FunctionType> ShapedFloodFilledIteratorType;

    std::vector<IndexType> seedList;

    RegionType region;
    region.SetSize(0, 128);
    region.SetSize(1, 128);
    region.SetIndex(0, 0);
    region.SetIndex(1, 0);

    ImageType::Pointer inputImage = ImageType::New();
    inputImage->SetRegions(region);
    inputImage->Allocate(true); // initialize
                                                       // buffer to zero

    itk::ImageLinearIteratorWithIndex<ImageType> it( inputImage, region );

    // make sure that we create a 4-connected image!
    for( unsigned int dir = 0; dir < ImageDimension; ++dir )
      {
      it.SetDirection(dir);
      it.GoToBegin();
      while (!it.IsAtEnd())
        {
        while (!it.IsAtEndOfLine())
          {
          // add a seed
          if( seedList.empty() )
            {
            seedList.push_back(it.GetIndex());
            }

          it.Set(255);
          ++it;
          }

        // and jump over every
        it.NextLine();
        if (!it.IsAtEnd())
          {
          it.NextLine();
          }
        if (!it.IsAtEnd())
          {
          it.NextLine();
          }
        }
      }

    FunctionType::Pointer function = FunctionType::New();

    function->SetInputImage ( inputImage );
    function->ThresholdAbove ( 1 ); // >= 1

    FloodFilledIteratorType floodIt(inputImage, function, seedList);
    ShapedFloodFilledIteratorType shapedFloodIt(inputImage, function, seedList);

    shapedFloodIt.SetFullyConnected(false); // 4-connected, default

    for (unsigned short i = 1; !floodIt.IsAtEnd(); ++floodIt,
                                                   ++shapedFloodIt,
                                                   ++i)
      {
      if (floodIt.GetIndex() != shapedFloodIt.GetIndex())
        {
        return EXIT_FAILURE;
        }
      }

    if (!floodIt.IsAtEnd() || !shapedFloodIt.IsAtEnd())
      {
      return EXIT_FAILURE;
      }
    }
  catch (itk::ExceptionObject& e)
    {
    e.Print(std::cerr);
    return EXIT_FAILURE;
    }
  catch (...)
    {
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
