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

#include <iostream>

#include "itkImage.h"
#include "itkLabelImageGaussianInterpolateImageFunction.h"


int itkLabelImageGaussianInterpolateImageFunctionTest( int , char*[] )
 {
 int result = EXIT_SUCCESS;

 const   unsigned int                                  Dimension = 2;
 typedef float                                         PixelType;
 typedef itk::Image< PixelType, Dimension >            ImageType;
 typedef ImageType::RegionType                         RegionType;
 typedef RegionType::SizeType                          SizeType;
 typedef ImageType::IndexType                          IndexType;

 typedef itk::ContinuousIndex<float, 2>                ContinuousIndexType;
 typedef itk::Point<float,2>                           PointType;

 typedef float                                         CoordRepType;
 typedef itk::LabelImageGaussianInterpolateImageFunction< ImageType, CoordRepType >
                                                       InterpolatorType;
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

     const PixelType value = x + y * maxx;
     image->SetPixel( index, value );

     std::cout << value << " ";
     }
   std::cout << std::endl;
   }

 InterpolatorType::Pointer interpolator = InterpolatorType::New();
 interpolator->SetInputImage( image );

 double sigma[Dimension];
 for( unsigned int d = 0; d < Dimension; d++ )
   {
   sigma[d] = 1.0;
   }
 double alpha = 1.0;

 interpolator->SetParameters( sigma, alpha );
 interpolator->Print( std::cout, 3 );

 if( interpolator->GetSigma()[0] != 1.0 ||
   interpolator->GetAlpha() != 1.0 )
   {
   std::cerr << "Parameters were not returned correctly." << std::endl;
   }

 const double incr = 0.1;
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
           //test scalar image
           const double computedValue = interpolator->Evaluate( point );
           std::cout << "computed value =  " << computedValue << std::endl;
           }
         }
       }
     }
   }

  return result;
}
