/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkPointSetToImageTranslationNormalizedCorrelationGradientDescentRegistrationTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
s

Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkPhysicalImage.h"
#include "itkSimpleImageRegionIterator.h"
#include "itkPointSetToImageTranslationNormalizedCorrelationGradientDescentRegistration.h"

/** 
 *  This test uses two 2D-Gaussians (standard deviation RegionSize/2)
 * 
 *  One of the images is subsampled in order to obtain a PoinSet.
 *  The PointSet is considered the Target and the other image is
 *  considered the Reference.
 */ 

int main()
{

  // Number of pixels to take from the target image
  unsigned long  numberOfSamples = 10000L;

  /*Allocate Images*/
  const unsigned int ImageDimension  = 2;

  typedef itk::PhysicalImage<unsigned char,ImageDimension>  ReferenceType;

  typedef itk::DefaultStaticMeshTraits<unsigned char,   // pixel type
                                       ImageDimension,  // point dimension
                                       1,               // max topological dim
                                       double,          // coordinates type
                                       double           // interpolation weight
                                         >  DefaultPointSetTraits;

  typedef itk::PointSet<unsigned char,DefaultPointSetTraits>    PointSetType;

  typedef PointSetType                                          TargetType;

  typedef itk::PointSetToImageTranslationNormalizedCorrelationGradientDescentRegistration<
                                                                ReferenceType,
                                                                TargetType> RegistrationType;

  ReferenceType::SizeType size = {{100,100}};
  ReferenceType::IndexType index = {{0,0}};
  ReferenceType::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );

  ReferenceType::Pointer imgReference = ReferenceType::New();
  imgReference->SetLargestPossibleRegion( region );
  imgReference->SetBufferedRegion( region );
  imgReference->SetRequestedRegion( region );
  imgReference->Allocate();

  ReferenceType::Pointer imgTarget = ReferenceType::New(); // type=Reference but this 
                                                           // is the target image to subsample
  imgTarget->SetLargestPossibleRegion( region );
  imgTarget->SetBufferedRegion( region );
  imgTarget->SetRequestedRegion( region );
  imgTarget->Allocate();

  /* Fill images with a 2D gaussian*/
  typedef  itk::SimpleImageRegionIterator<ReferenceType> ReferenceIteratorType;


  itk::Point<double,2> center;
  center[0] = (double)region.GetSize()[0]/2.0;
  center[1] = (double)region.GetSize()[1]/2.0;

  const double s = (double)region.GetSize()[0]/2.0;

  itk::Point<double,2>  p;
  itk::Vector<double,2> d;

  /* Set the displacement */
  itk::Vector<double,2> displacement;
  displacement[0] = 7;
  displacement[1] =  3;

  ReferenceIteratorType ri(imgReference,region);
  ReferenceIteratorType ti(imgTarget,region);
  ri.Begin();
  while(!ri.IsAtEnd())
  {
    p[0] = ri.GetIndex()[0];
    p[1] = ri.GetIndex()[1];
    d = p-center;
    d += displacement;
    const double x = d[0];
    const double y = d[1];
    ri.Set( 200.0 * exp( - ( x*x + y*y )/(s*s) ) );
    ++ri;
  }


  ti.Begin();
  while(!ti.IsAtEnd())
  {
    p[0] = ti.GetIndex()[0];
    p[1] = ti.GetIndex()[1];
    d = p-center;
    const double x = d[0];
    const double y = d[1];
    ti.Set( 200.0 * exp( - ( x*x + y*y )/(s*s) ) );
    ++ti;
  }

  
  // Subsample the target image to produce a point set
  PointSetType::Pointer pointSetTarget = PointSetType::New();

  PointSetType::PointsContainer::Pointer    points = pointSetTarget->GetPoints();
  PointSetType::PointDataContainer::Pointer data   = PointSetType::PointDataContainer::New();
  pointSetTarget->SetPointData( data );

  points->Reserve( numberOfSamples );
  data->Reserve(   numberOfSamples );

  const unsigned int numPixelsToSkip =
            imgTarget->GetOffsetTable()[ImageDimension] / numberOfSamples;

  unsigned int counter   = 0;
  unsigned int numPoints = 0;

  ti.Begin();
  while(!ti.IsAtEnd() && numPoints < numberOfSamples )
  {
    if( counter < numPixelsToSkip ) 
    {
      PointSetType::PointType point;
      point[0] = ti.GetIndex()[0];
      point[1] = ti.GetIndex()[1];
      points->SetElement( counter, point    );
      data->SetElement(   counter, ti.Get() );
      numPoints++;
    }
    ++ti;
    ++counter;
  }






  
  RegistrationType::Pointer registrationMethod = RegistrationType::New();

  registrationMethod->SetReference(imgReference);
  registrationMethod->SetTarget(pointSetTarget);

  registrationMethod->StartRegistration();


  std::cout << "The correct answer should be : " << std::endl;
  std::cout << -displacement << std::endl;
  

  return EXIT_SUCCESS;

}



