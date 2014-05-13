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
// image using the fully connected option.

#include "itkImageFileReader.h"
#include "itkBinaryThresholdImageFunction.h"

#include "itkShapedFloodFilledImageFunctionConditionalConstIterator.h"

int itkShapedFloodFilledImageFunctionConditionalConstIteratorTest1(int argc, char *argv [] )
{
  if( argc < 2 )
    {
    std::cerr << "Error: missing arguments" << std::endl;
    std::cerr << argv[0] << " filename " << std::endl;
    return EXIT_FAILURE;
    }

  try
    {
    const unsigned int ImageDimension = 2;
    typedef unsigned char PixelType;

    typedef itk::Image<PixelType, ImageDimension> ImageType;
    typedef ImageType::RegionType                 RegionType;
    typedef ImageType::IndexType                  IndexType;

    typedef itk::BinaryThresholdImageFunction<ImageType> FunctionType;
    typedef itk::ShapedFloodFilledImageFunctionConditionalConstIterator<
                  ImageType, FunctionType> ShapedFloodFilledIteratorType;

    typedef itk::ImageFileReader<ImageType> ReaderType;

    ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName( argv[1] );
    reader->Update();

    IndexType index;
    index[0] = 29;
    index[1] = 47;

    std::vector<IndexType> seedList;
    seedList.push_back(index);

    RegionType region = reader->GetOutput()->GetBufferedRegion();

    FunctionType::Pointer function = FunctionType::New();

    function->SetInputImage ( reader->GetOutput() );
    function->ThresholdAbove ( 1 ); // >= 1

    ShapedFloodFilledIteratorType shapedFloodIt(
        reader->GetOutput(), function, seedList);
    shapedFloodIt.SetFullyConnected(true); // 8-connected, default
    //
    // get the seeds and display them.
    const ShapedFloodFilledIteratorType::SeedsContainerType &seeds(shapedFloodIt.GetSeeds());
    std::cout << "Iterator seeds";
    for(ShapedFloodFilledIteratorType::SeedsContainerType::const_iterator it =
          seeds.begin(); it != seeds.end(); it++)
      {
      std::cout << " " << (*it);
      }
    std::cout << std::endl;

    ImageType::Pointer visitedImage = ImageType::New();
    visitedImage->SetRegions(region);
    visitedImage->Allocate(true); // initialize
                                                         // buffer to zero

    for (; !shapedFloodIt.IsAtEnd(); ++shapedFloodIt)
      {
      visitedImage->SetPixel( shapedFloodIt.GetIndex(), 255);
      }

    typedef itk::ImageRegionConstIterator<ImageType> ConstIteratorType;

    ConstIteratorType inIt(reader->GetOutput(), region);
    ConstIteratorType outIt(visitedImage, region);

    for (; !inIt.IsAtEnd(); ++inIt, ++outIt)
      {
      if (inIt.Get() != outIt.Get())
        {
        return EXIT_FAILURE;
        }
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
