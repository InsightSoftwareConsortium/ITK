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

#include "itkBinaryMaskToNarrowBandPointSetFilter.h"
#include "itkPointSet.h"
#include "itkImageRegionIterator.h"

int itkBinaryMaskToNarrowBandPointSetFilterTest(int , char *[] )
{
  constexpr unsigned int Dimension = 2;

  using BinaryMaskPixelType = unsigned char;

  using BinaryMaskImageType = itk::Image<
                        BinaryMaskPixelType,
                        Dimension  >;


  //
  //  Initialize an image with a white square in a black background
  //
  BinaryMaskImageType::Pointer binaryMask = BinaryMaskImageType::New();

  BinaryMaskImageType::SizeType     size;
  BinaryMaskImageType::IndexType    index;
  BinaryMaskImageType::RegionType   region;

  size[0] = 100;
  size[1] = 100;

  index[0] = 0;
  index[1] = 0;

  region.SetIndex( index );
  region.SetSize(  size );

  binaryMask->SetRegions( region );
  binaryMask->Allocate(true); // initialize buffer to zero

  size[0] = 60;
  size[1] = 60;

  index[0] = 20;
  index[1] = 20;

  region.SetIndex( index );
  region.SetSize(  size );

  itk::ImageRegionIterator< BinaryMaskImageType > it( binaryMask, region );

  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    it.Set( 255 );
    ++it;
    }

  //
  //  Set up the filter
  //
  using PointSetType = itk::PointSet< float, Dimension >;

  using GeneratorType = itk::BinaryMaskToNarrowBandPointSetFilter<
                                BinaryMaskImageType,
                                PointSetType
                                            >;

  GeneratorType::Pointer narrowBandGenerator = GeneratorType::New();

  narrowBandGenerator->SetInput( binaryMask );

  try
    {
    narrowBandGenerator->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown during the execution of the generator " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  //
  //  Checking the output
  //
  using PointType = PointSetType::PointType;

  using PointsContainer = PointSetType::PointsContainer;
  using PointsContainerPointer = PointsContainer::Pointer;
  using PointsIterator = PointsContainer::ConstIterator;

  using PointDataContainer = PointSetType::PointDataContainer;
  using PointDataContainerPointer = PointDataContainer::Pointer;
  using PointDataIterator = PointDataContainer::ConstIterator;

  PointSetType::Pointer                     pointSet  = narrowBandGenerator->GetOutput();

  PointsContainerPointer      points    = pointSet->GetPoints();
  PointDataContainerPointer   pointData = pointSet->GetPointData();

  PointsIterator point     = points->Begin();
  PointsIterator lastPoint = points->End();

  PointDataIterator  data     = pointData->Begin();
  PointDataIterator  lastData = pointData->End();

  while( point != lastPoint  && data != lastData )
    {

    const PointType & p = point.Value();

    binaryMask->TransformPhysicalPointToIndex( p, index );

    if( ( !binaryMask->GetPixel( index ) && data.Value() > 0 ) ||
        (  binaryMask->GetPixel( index ) && data.Value() < 0 )    )
      {
      std::cerr << "Pixel " << index << " shouldn't be in the narrow band" << std::endl;
      return EXIT_FAILURE;
      }

    ++point;
    ++data;
    }

  return EXIT_SUCCESS;
}
