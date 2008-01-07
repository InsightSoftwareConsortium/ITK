/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLinearInterpolateImageFunctionTest.cxx
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

#include <iostream>

#include "itkImage.h"
#include "itkTranslationTransform.h"
#include "itkLinearInterpolateImageFunction.h"


int itkLinearInterpolateImageFunctionTest( int , char*[] )
 {

 const   unsigned int                                  Dimension = 2;
 typedef float                                         PixelType;
 typedef itk::Image< PixelType, Dimension >            ImageType;
 typedef ImageType::RegionType                         RegionType;
 typedef RegionType::SizeType                          SizeType;
 typedef ImageType::IndexType                          IndexType;

 typedef itk::ContinuousIndex<float, 2>                ContinuousIndexType;
 typedef itk::Point<float,2>                           PointType;

 typedef itk::LinearInterpolateImageFunction< ImageType >  InterpolatorType;

 ImageType::Pointer image = ImageType::New();

 IndexType start;
 start.Fill( 0 );

 SizeType size;
 size.Fill( 3 );

 RegionType region;
 region.SetSize( size );
 region.SetIndex( start );

 image->SetRegions( region );
 image->Allocate();

 ImageType::PointType     origin;
 ImageType::SpacingType   spacing;

 origin.Fill( 0.0 );
 spacing.Fill( 1.0 );

 image->SetOrigin( origin );
 image->SetSpacing( spacing );

 image->Print( std::cout );

 unsigned int maxx = 3;
 unsigned int maxy = 3;

 //
 // Fill up the image values with the function
 //
 //   Intensity = f(x,y) = x + 3 * y
 //
 //
 for (unsigned int y = 0; y < maxy; y++)
   {
   for (unsigned int x = 0; x < maxx; x++)
     {
     IndexType index;
     index[0] = x;
     index[1] = y;

     PixelType value = x + y * maxx;
     image->SetPixel( index, value );
     std::cout << value << " ";
     }
   std::cout << std::endl;
   }

 InterpolatorType::Pointer interpolator = InterpolatorType::New();
 interpolator->SetInputImage( image );

 const double incr = 0.1;
 
 const double tolerance = 1e-6;

 PointType point;

 for (double yy = 0; yy < static_cast<double>(maxy-1); yy++)
   {
   for (double xx = 0; xx < static_cast<double>(maxx-1); xx++)
     {
     for (double yyy = yy; yyy < yy + 1.01; yyy += incr)
       {
       for (double xxx = xx; xxx < xx + 1.01; xxx += incr)
         {
         point[0] = xxx;
         point[1] = yyy;

         if( interpolator->IsInsideBuffer( point ) )
           {
           const double expectedValue = xxx + 3.0 * yyy;
           
           const double computedValue = interpolator->Evaluate( point );

           const double difference = expectedValue - computedValue;

           if( vcl_fabs( difference ) > tolerance )
             {
             std::cerr << "Error found while computing interpolation " << std::endl;
             std::cerr << "Point = " << point << std::endl; 
             std::cerr << "Expected value = " << expectedValue << std::endl;
             std::cerr << "Computed value = " << computedValue << std::endl;
             std::cerr << "Difference     = " << difference << std::endl;
             return EXIT_FAILURE;
             }
           }
         }
       }
     }
   }


  return EXIT_SUCCESS;
}
