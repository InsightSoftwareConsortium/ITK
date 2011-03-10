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
 const   unsigned int                                  VectorDimension = 4;
 typedef itk::Vector< PixelType, VectorDimension >     VectorPixelType;
 typedef itk::Image< PixelType, Dimension >            ImageType;
 typedef itk::Image< VectorPixelType, Dimension >      VectorImageType;
 typedef ImageType::RegionType                         RegionType;
 typedef RegionType::SizeType                          SizeType;
 typedef ImageType::IndexType                          IndexType;

 typedef itk::ContinuousIndex<float, 2>                ContinuousIndexType;
 typedef itk::Point<float,2>                           PointType;

 typedef float                                         CoordRepType;
 typedef itk::LinearInterpolateImageFunction<
    ImageType, CoordRepType >                          InterpolatorType;
 typedef itk::LinearInterpolateImageFunction<
    VectorImageType, CoordRepType >                    VectorInterpolatorType;

 typedef VectorInterpolatorType::OutputType            InterpolatedVectorType;

 ImageType::Pointer image = ImageType::New();
 VectorImageType::Pointer vectorimage = VectorImageType::New();

 IndexType start;
 start.Fill( 0 );

 SizeType size;
 size.Fill( 3 );

 RegionType region;
 region.SetSize( size );
 region.SetIndex( start );

 image->SetRegions( region );
 image->Allocate();

 vectorimage->SetRegions( region );
 vectorimage->Allocate();

 ImageType::PointType     origin;
 ImageType::SpacingType   spacing;

 origin.Fill( 0.0 );
 spacing.Fill( 1.0 );

 image->SetOrigin( origin );
 image->SetSpacing( spacing );

 vectorimage->SetOrigin( origin );
 vectorimage->SetSpacing( spacing );

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

     VectorPixelType & vectorpixel = vectorimage->GetPixel( index );
     vectorpixel.Fill( value );

     std::cout << value << " ";
     }
   std::cout << std::endl;
   }

 InterpolatorType::Pointer interpolator = InterpolatorType::New();
 interpolator->SetInputImage( image );

 VectorInterpolatorType::Pointer vectorinterpolator = VectorInterpolatorType::New();
 vectorinterpolator->SetInputImage( vectorimage );

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

           const InterpolatedVectorType vectorpixel = vectorinterpolator->Evaluate( point );

           const InterpolatedVectorType expectedvector(expectedValue);

           const double errornorm = (expectedvector - vectorpixel).GetNorm();

           if( errornorm > tolerance )
             {
             std::cerr << "Error found while computing vector interpolation " << std::endl;
             std::cerr << "Point = " << point << std::endl;
             std::cerr << "Expected vector = " << expectedvector << std::endl;
             std::cerr << "Computed vector = " << vectorpixel << std::endl;
             std::cerr << "Difference     = " << (expectedvector - vectorpixel) << std::endl;
             return EXIT_FAILURE;
             }
           }
         }
       }
     }
   }


  return EXIT_SUCCESS;
}
