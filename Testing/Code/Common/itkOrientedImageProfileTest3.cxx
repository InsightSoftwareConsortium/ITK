/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOrientedImageProfileTest3.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkVectorImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkTimeProbesCollectorBase.h"

int itkOrientedImageProfileTest3( int , char *[] )
{

  const unsigned int Dimension = 3;
  typedef unsigned char PixelType;

  //
  //  Yes, on purpose we are using here the itk::VectorImage, so we can compare it
  //  against the itk::Image used in itkOrientedImageProfileTest1.
  //
  typedef itk::VectorImage<PixelType, Dimension>      ImageType;

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

  image->SetVectorLength( 2 );
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
