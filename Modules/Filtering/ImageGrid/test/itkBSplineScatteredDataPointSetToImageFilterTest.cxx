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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkPointSet.h"
#include "itkBSplineScatteredDataPointSetToImageFilter.h"
#include "itkTestingMacros.h"

/**
 * In this test, we approximate a 2-D scalar field.
 * The scattered data is derived from a segmented
 * image. We write the output to an image for
 * comparison.
 */
int itkBSplineScatteredDataPointSetToImageFilterTest( int argc, char * argv [] )
{

  if ( argc != 3 )
    {
    std::cout << "Usage: " << argv[0] << " inputImage outputImage" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int ParametricDimension = 2;
  const unsigned int DataDimension = 1;

  typedef int                                           PixelType;
  typedef itk::Image<PixelType, ParametricDimension>    InputImageType;
  typedef float                                         RealType;
  typedef itk::Vector<RealType, DataDimension>          VectorType;
  typedef itk::Image<VectorType, ParametricDimension>   VectorImageType;
  typedef itk::PointSet <VectorImageType::PixelType,
    ParametricDimension>                                PointSetType;

  PointSetType::Pointer pointSet = PointSetType::New();

  typedef itk::ImageFileReader<InputImageType> ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  reader->Update();

  itk::ImageRegionIteratorWithIndex<InputImageType>
    It( reader->GetOutput(), reader->GetOutput()->GetLargestPossibleRegion() );

  // Iterate through the input image which consists of multivalued
  // foreground pixels (=nonzero) and background values (=zero).
  // The foreground pixels comprise the input point set.

  for ( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    if ( It.Get() != itk::NumericTraits<PixelType>::ZeroValue() )
      {
      // We extract both the 2-D location of the point
      // and the pixel value of that point.

      PointSetType::PointType point;
      reader->GetOutput()->TransformIndexToPhysicalPoint( It.GetIndex(), point );

      unsigned long i = pointSet->GetNumberOfPoints();
      pointSet->SetPoint( i, point );

      PointSetType::PixelType V( DataDimension );
      V[0] = static_cast<RealType>( It.Get() );
      pointSet->SetPointData( i, V );
      }
    }

  // Instantiate the B-spline filter and set the desired parameters.
  typedef itk::BSplineScatteredDataPointSetToImageFilter
    <PointSetType, VectorImageType> FilterType;

  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, BSplineScatteredDataPointSetToImageFilter,
    PointSetToImageFilter );


  unsigned int splineOrder = 0u;
  TRY_EXPECT_EXCEPTION( filter->SetSplineOrder( splineOrder ) );

  FilterType::ArrayType splineOrderArray;
  splineOrderArray.Fill( 4u );
  filter->SetSplineOrder( splineOrderArray );
  TEST_SET_GET_VALUE( splineOrderArray, filter->GetSplineOrder() );

  splineOrder = 3u;
  filter->SetSplineOrder( splineOrder );
  splineOrderArray.Fill( splineOrder );
  TEST_SET_GET_VALUE( splineOrderArray, filter->GetSplineOrder() );


  unsigned numberOfLevels = 0u;
  TRY_EXPECT_EXCEPTION( filter->SetNumberOfLevels( numberOfLevels ) );

  FilterType::ArrayType numberOfLevelsArray;
  numberOfLevelsArray.Fill( 4u );
  filter->SetNumberOfLevels( numberOfLevelsArray );
  TEST_SET_GET_VALUE( numberOfLevelsArray, filter->GetNumberOfLevels() );

  numberOfLevels = 3u;
  filter->SetNumberOfLevels( numberOfLevels );
  numberOfLevelsArray.Fill( numberOfLevels );
  TEST_SET_GET_VALUE( numberOfLevelsArray, filter->GetNumberOfLevels() );


  FilterType::ArrayType ncps;
  ncps.Fill( 4u );
  filter->SetNumberOfControlPoints( ncps );
  TEST_SET_GET_VALUE( ncps, filter->GetNumberOfControlPoints() );


  FilterType::ArrayType close;
  close.Fill( 0u );
  filter->SetCloseDimension( close );
  TEST_SET_GET_VALUE( close, filter->GetCloseDimension() );


  // Define the parametric domain.
  filter->SetOrigin( reader->GetOutput()->GetOrigin() );
  filter->SetSpacing( reader->GetOutput()->GetSpacing() );
  filter->SetSize( reader->GetOutput()->GetLargestPossibleRegion().GetSize() );
  filter->SetDirection( reader->GetOutput()->GetDirection() );

  filter->SetInput( pointSet );

  TRY_EXPECT_NO_EXCEPTION( filter->Update() );

  // Get the current number of control points to increase coverage
  std::cout << "Current number of control points: "
    << filter->GetCurrentNumberOfControlPoints() << std::endl;

  // Get the control point lattice produced by the fitting process to increase
  // coverage
  std::cout << "Control point lattice produced by the fitting process: " << std::endl;
  std::cout << filter->GetPhiLattice() << std::endl;

  VectorImageType *outputImage = filter->GetOutput();

  // Write the output to an image.
  typedef itk::Image<RealType, ParametricDimension> RealImageType;
  RealImageType::Pointer image = RealImageType::New();
  image->SetRegions( reader->GetOutput()->GetLargestPossibleRegion() );
  image->Allocate();
  itk::ImageRegionIteratorWithIndex<RealImageType>
    Itt( image, image->GetLargestPossibleRegion() );

  for ( Itt.GoToBegin(); !Itt.IsAtEnd(); ++Itt )
    {
    Itt.Set( outputImage->GetPixel( Itt.GetIndex() )[0] );
    }

  typedef itk::ImageFileWriter<RealImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( image );
  writer->SetFileName( argv[2] );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}
