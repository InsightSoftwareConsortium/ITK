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

  const    unsigned int                                  Dimension = 2;
  typedef  float                                         PixelType;
  typedef  itk::Image< PixelType, Dimension >            ImageType;

 typedef itk::Image< PixelType, Dimension >                ImageType;
 typedef ImageType::RegionType                             RegionType;
 typedef RegionType::SizeType                              SizeType;
 typedef ImageType::IndexType                              IndexType;
 //typedef ImageType::ContinuousIndexType                    ContinuousIndexType;
 typedef itk::ContinuousIndex<float, 2>                     ContinuousIndexType;
 typedef itk::LinearInterpolateImageFunction< ImageType >  InterpolatorType;

 ImageType::Pointer image = ImageType::New();
 SizeType size;
 size.Fill( 3 );
 RegionType region( size );

 image->SetLargestPossibleRegion( region );
 image->Allocate();

 unsigned int maxx = 3;
 unsigned int maxy = 3;

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

 double incr = 0.1;

 for (double yy = 0; yy < static_cast<double>(maxy); yy++)
   {
   for (double xx = 0; xx < static_cast<double>(maxx); xx++)
     {
     for (double yyy = 0; yyy <= yy + 1.01; yyy += incr)
       {
       for (double xxx = 0; xxx <= xx + 1.01; xxx += incr)
         {
         ContinuousIndexType cindex;
         cindex[0] = xxx;
         cindex[1] = yyy;
         double val = interpolator->EvaluateAtContinuousIndex( cindex );
         std::cout << cindex << " Value : " << val << std::endl;
         }
       std::cout << " -- " << std::endl;
       }
     }
   }


  return EXIT_SUCCESS;
}
