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

#include "itkImageRegionIteratorWithIndex.h"
#include "itkTimeProbesCollectorBase.h"

int itkOrientedImageProfileTest1( int, char *[] )
{

  const unsigned int Dimension = 3;
  typedef unsigned char PixelType;

  typedef itk::Image<PixelType, Dimension>    ImageType;

  typedef ImageType::IndexType                        IndexType;
  typedef ImageType::SizeType                         SizeType;
  typedef ImageType::PointType                        PointType;
  typedef ImageType::RegionType                       RegionType;
  typedef ImageType::SpacingType                      SpacingType;

  IndexType start;
  SizeType  size;

  start.Fill( 0 );
  size.Fill( 300 );

  RegionType region;

  region.SetIndex( start );
  region.SetSize( size );

  ImageType::Pointer image = ImageType::New();

  image->SetRegions( region );
  image->Allocate();

  SpacingType spacing;

  spacing.Fill( 1.5 );

  image->SetSpacing( spacing );

  PointType origin;

  origin.Fill( 1.3 );

  image->SetOrigin( origin );

  typedef itk::ImageRegionConstIteratorWithIndex< ImageType > IteratorType;

  IteratorType itr( image, region );

  itr.GoToBegin();

  itk::TimeProbesCollectorBase  chronometer;

  chronometer.Start("Transform");

  IndexType index;
  PointType point;

  while( !itr.IsAtEnd() )
    {
    image->TransformIndexToPhysicalPoint( itr.GetIndex(), point );
    image->TransformPhysicalPointToIndex( point, index );
    ++itr;
    }

  chronometer.Stop("Transform");

  chronometer.Report( std::cout );

  return EXIT_SUCCESS;
}
