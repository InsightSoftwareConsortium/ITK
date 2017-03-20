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

#include "itkWatershedImageFilter.h"
#include "itkWatershedEquivalenceRelabeler.h"
#include "itkWatershedBoundaryResolver.h"
#include "itkFilterWatcher.h"
#include "itkTestingMacros.h"


int itkWatershedImageFilterTest( int, char* [] )
{

  const unsigned int Dimension = 2;

  typedef float                                         PixelType;
  typedef itk::Image< PixelType, Dimension >            ImageType2D;
  typedef itk::Image< itk::IdentifierType, Dimension >  LongImageType2D;


  itk::ImageRegion< Dimension > region;

  itk::Size< Dimension > size;
  size[0] = 314;
  size[1] = 314;

  itk::Index< Dimension > origin;
  origin[0] = 0;
  origin[1] = 0;

  region.SetSize( size );
  region.SetIndex( origin );

  ImageType2D::Pointer image2D = ImageType2D::New();
  image2D->SetLargestPossibleRegion( region);
  image2D->SetBufferedRegion( region );
  image2D->SetRequestedRegion( region );
  image2D->Allocate();

  LongImageType2D::Pointer longimage2D = LongImageType2D::New();
  longimage2D->SetRegions( region );
  longimage2D->Allocate( true ); // initialize buffer to zero

  itk::ImageRegionIterator< ImageType2D > it2D( image2D,
    image2D->GetRequestedRegion() );

  for( float q = 0.00f; !it2D.IsAtEnd(); ++it2D )
    {
    it2D.Value() = std::sin( q );
    q += 0.10f;
    }

  // Test various objects associated to itk::WatershedImageFilter
  //

  // Test EquivalenceRelabeler
  itk::EquivalencyTable::Pointer table = itk::EquivalencyTable::New();

  itk::watershed::EquivalenceRelabeler<
    LongImageType2D::PixelType, Dimension >::Pointer eq =
    itk::watershed::EquivalenceRelabeler<
    LongImageType2D::PixelType, Dimension >::New();
  eq->SetInputImage( longimage2D );

  eq->SetEquivalencyTable( table );
  TEST_SET_GET_VALUE( table, eq->GetEquivalencyTable() );


  TRY_EXPECT_NO_EXCEPTION( eq->Update() );

  // Test WatershedMiniPipelineProgressCommand
  // Forcing the execution of the const Execute method which is not normally called.
  itk::WatershedMiniPipelineProgressCommand::Pointer wmppc =
    itk::WatershedMiniPipelineProgressCommand::New();

  EXERCISE_BASIC_OBJECT_METHODS( wmppc, WatershedMiniPipelineProgressCommand,
    Command );

  double count = 2.0;
  wmppc->SetCount( count );
  TEST_SET_GET_VALUE( count, wmppc->GetCount() );

  unsigned int numberOfFilters = 2;
  wmppc->SetNumberOfFilters( numberOfFilters );
  TEST_SET_GET_VALUE( numberOfFilters, wmppc->GetNumberOfFilters() );

  wmppc->SetFilter( eq );
  TEST_SET_GET_VALUE( eq, wmppc->GetFilter() );

  const itk::ProcessObject *constp = eq.GetPointer();
  wmppc->Execute( constp, itk::ProgressEvent() );
  wmppc->Execute( eq.GetPointer(), itk::ProgressEvent() );

  // Test watershed::BoundaryResolver
  itk::watershed::BoundaryResolver< PixelType, Dimension >::Pointer br =
    itk::watershed::BoundaryResolver< PixelType, Dimension >::New();
  if( br.IsNull() )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cout << "Null itk::watershed::BoundaryResolver." << std::endl;
    return EXIT_FAILURE;
    }
  itk::watershed::Boundary< PixelType, 1 >::Pointer boundaryA =
    itk::watershed::Boundary< PixelType, 1 >::New();
  if( boundaryA.IsNull() )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cout << "Null itk::watershed::Boundary." << std::endl;
    return EXIT_FAILURE;
    }
  itk::watershed::Boundary< PixelType, 1 >::Pointer boundaryB =
    itk::watershed::Boundary< PixelType, 1 >::New();
  if( boundaryB.IsNull() )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cout << "Null itk::watershed::Boundary." << std::endl;
    return EXIT_FAILURE;
    }


  itk::WatershedImageFilter< ImageType2D >::Pointer watershedFilter =
    itk::WatershedImageFilter< ImageType2D >::New();

  EXERCISE_BASIC_OBJECT_METHODS( watershedFilter, WatershedImageFilter,
    ImageToImageFilter );

  FilterWatcher watchIt( watershedFilter, "WatershedImageFilter" );

  double threshold = .05;
  watershedFilter->SetThreshold( threshold );
  TEST_SET_GET_VALUE( threshold, watershedFilter->GetThreshold() );

  double level = 1.0;
  watershedFilter->SetLevel( level );
  TEST_SET_GET_VALUE( level, watershedFilter->GetLevel() );

  watershedFilter->SetInput( image2D );

  TRY_EXPECT_NO_EXCEPTION( watershedFilter->Update() );


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
