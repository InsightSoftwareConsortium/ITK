/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBSplineScatteredDataPointSetToImageFilterTest2.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkPointSet.h"
#include "itkBSplineScatteredDataPointSetToImageFilter.h"

/**
 * In this test, we approximate a sequence of 3D points with a parametric curve described by B-Splines
 */
int itkBSplineScatteredDataPointSetToImageFilterTest2( int , char * [] )
{
  const unsigned int ParametricDimension = 1;
  const unsigned int DataDimension = 3;

  typedef double                                         RealType;
  typedef itk::Vector<RealType, DataDimension>           VectorType;
  typedef itk::Image<VectorType, ParametricDimension>    ImageType;  

  typedef itk::PointSet<VectorType, ParametricDimension> PointSetType;

  PointSetType::Pointer pointSet = PointSetType::New();  

  // Sample the helix.
  for ( RealType t = 0.0; t <= 1.0+1e-10; t += 0.05 )
    {
    unsigned long i = pointSet->GetNumberOfPoints();

    PointSetType::PointType point;
    point[0] = t;
    pointSet->SetPoint( i, point );        

    VectorType V;
    V[0] = 0.25*cos(t*6.0*3.141);
    V[1] = 0.25*sin(t*6.0*3.141);
    V[2] = 4.0*t;

    pointSet->SetPointData( i, V );
    }

  // Instantiate the filter and set the parameters
  typedef itk::BSplineScatteredDataPointSetToImageFilter<PointSetType, ImageType>  FilterType;
  FilterType::Pointer filter = FilterType::New();
  
  // Define the parametric domain
  ImageType::SpacingType spacing;  
  spacing.Fill( 0.001 );
  ImageType::SizeType size;  
  size.Fill( static_cast<unsigned int>( 1.0/spacing[0] ) + 1 );
  ImageType::PointType origin;  
  origin.Fill( 0.0 );

  filter->SetSize( size );
  filter->SetOrigin( origin );
  filter->SetSpacing( spacing );
  filter->SetInput( pointSet );

  filter->SetSplineOrder( 3 );  
  FilterType::ArrayType ncps;
  ncps.Fill( 4 );  
  filter->SetNumberOfControlPoints( ncps );
  filter->SetNumberOfLevels( 5 );
  filter->SetGenerateOutputImage( false );

  try 
    {
    filter->Update();
    
    for ( RealType t = 0.0; t <= 1.0+1e-10; t += 0.01 )
      {
      PointSetType::PointType point;
      point[0] = t;

      VectorType V; 
      filter->Evaluate( point, V );

      FilterType::GradientType G;
      filter->EvaluateGradient( point, G );

      std::cout << V << " :  " << G << std::endl;
      }
    }
  catch ( itk::ExceptionObject & excp ) 
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
};
