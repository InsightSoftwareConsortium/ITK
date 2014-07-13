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
 int test_status = EXIT_SUCCESS;
 const   unsigned int                       Dimension = 2;
 typedef unsigned short int                 PixelType; //Label images should be integer value types
 typedef itk::Image< PixelType, Dimension > ImageType;
 typedef ImageType::RegionType              RegionType;
 typedef RegionType::SizeType               SizeType;
 typedef ImageType::IndexType               IndexType;

 typedef float                              CoordRepType;

 //The ImageSizeToCompute
 const double FOV=10.0;
 const itk::IndexValueType small_xSize = 3;
 const itk::IndexValueType small_ySize = 3;
 ImageType::Pointer small_image = ImageType::New();
   {
   RegionType region;
     {
     IndexType start;
     start.Fill( 0 );

     SizeType size;
     size[0]= small_xSize;
     size[1]= small_ySize;
     region.SetSize( size );
     region.SetIndex( start );
     }

   small_image->SetRegions( region );
   small_image->Allocate();

     {
     ImageType::SpacingType   spacing;
     spacing.Fill( FOV/static_cast<double>(small_ySize) );
     ImageType::PointType     origin;
     origin.Fill( 0.5* FOV/static_cast<double>(small_ySize) );

     small_image->SetOrigin( origin );
     small_image->SetSpacing( spacing );
     }
   small_image->Print( std::cout );
   //
   // Fill up the small_image values with the function
   //
   //   Intensity = f(x,y) = x + 3 * y
   //
   //
   PixelType valarray[small_xSize][small_ySize] = {
       {255,255,255},
       {255,171,  7},
       {  7,  7,  7}
   };

   for (itk::IndexValueType y = 0; y < small_ySize; y++)
     {
     for (itk::IndexValueType x = 0; x < small_xSize; x++)
       {
       const IndexType index = { { x, y } };
       const PixelType value = valarray[x][y];
       small_image->SetPixel( index, value );
       std::cout << value << " ";
       }
     std::cout << std::endl;
     }
   }

 typedef itk::LabelImageGaussianInterpolateImageFunction< ImageType, CoordRepType >
                                                       InterpolatorType;
 InterpolatorType::Pointer interpolator = InterpolatorType::New();
 interpolator->SetInputImage( small_image );
   {
   double sigma[Dimension];
   for( unsigned int d = 0; d < Dimension; d++ )
     {
     sigma[d] = 1.0;
     }
   const double alpha = 1.0;
   interpolator->SetParameters( sigma, alpha );
   }
 interpolator->Print( std::cout, 3 );

 if( interpolator->GetSigma()[0] != 1.0 ||
     interpolator->GetSigma()[1] != 1.0 ||
     interpolator->GetAlpha() != 1.0 )
   {
   std::cerr << "Parameters were not returned correctly." << std::endl;
   }

 //########################
 //Now check the results
 //The ImageSizeToCompute
 const unsigned char default_background_value=17;
 const itk::IndexValueType large_xSize = 5+1;
 const itk::IndexValueType large_ySize = 5;
 ImageType::Pointer large_image = ImageType::New();
   {
   RegionType region;
     {
     IndexType start;
     start.Fill( 0 );

     SizeType size;
     size[0]= large_xSize;
     size[1]= large_ySize;
     region.SetSize( size );
     region.SetIndex( start );
     }

   large_image->SetRegions( region );
   large_image->Allocate();

     {
     ImageType::SpacingType   spacing;
     spacing.Fill( FOV/static_cast<double>(large_ySize) );
     ImageType::PointType     origin;
     origin.Fill( 0.5* FOV/static_cast<double>(large_ySize) );

     large_image->SetOrigin( origin );
     large_image->SetSpacing( spacing );
     }
   large_image->Print( std::cout );
   //
   // Fill up the large_image values with the function
   //
   //   Intensity = f(x,y) = x + 3 * y
   //
   //
   /*
At: [0, 0] computed value =  255 known_value = 255
At: [1, 0] computed value =  255 known_value = 255
At: [2, 0] computed value =  255 known_value = 255
At: [3, 0] computed value =  7 known_value = 255
At: [4, 0] computed value =  7 known_value = 7
At: [5, 0] computed value =  17 known_value = 17

At: [0, 1] computed value =  255 known_value = 255
At: [1, 1] computed value =  255 known_value = 255
At: [2, 1] computed value =  255 known_value = 255
At: [3, 1] computed value =  7 known_value = 7
At: [4, 1] computed value =  7 known_value = 7
At: [5, 1] computed value =  17 known_value = 17

At: [0, 2] computed value =  255 known_value = 255
At: [1, 2] computed value =  255 known_value = 255
At: [2, 2] computed value =  171 known_value = 171
At: [3, 2] computed value =  7 known_value = 7
At: [4, 2] computed value =  7 known_value = 7
At: [5, 2] computed value =  17 known_value = 17

At: [0, 3] computed value =  255 known_value = 255
At: [1, 3] computed value =  255 known_value = 255
At: [2, 3] computed value =  7 known_value = 7
At: [3, 3] computed value =  7 known_value = 7
At: [4, 3] computed value =  7 known_value = 7
At: [5, 3] computed value =  17 known_value = 17

At: [0, 4] computed value =  255 known_value = 255
At: [1, 4] computed value =  255 known_value = 7
At: [2, 4] computed value =  7 known_value = 7
At: [3, 4] computed value =  7 known_value = 7
At: [4, 4] computed value =  7 known_value = 7
At: [5, 4] computed value =  17 known_value = 17
    */

   PixelType valarray[large_xSize][large_ySize] = {
       {255,255,255,255,255},
       {255,255,255,255,255},
       {255,255,171,  7,  7},
       {  7,  7,  7,  7,  7},
       {  7,  7,  7,  7,  7},
       { default_background_value,default_background_value,default_background_value,default_background_value,default_background_value }
   };

   for (itk::IndexValueType y = 0; y < large_ySize; y++)
     {
     for (itk::IndexValueType x = 0; x < large_xSize; x++)
       {
       const IndexType index = { { x, y } };
       const PixelType known_value = valarray[x][y];
       ImageType::PointType physPoint;
       large_image->TransformIndexToPhysicalPoint(index,physPoint);
       if( interpolator->IsInsideBuffer( physPoint ) )
           {
           //test scalar small_image
           const double computedValue = interpolator->Evaluate( physPoint );
           large_image->SetPixel( index, static_cast< PixelType >( computedValue ) );
           }
       else
         {
         large_image->SetPixel( index, default_background_value );
         }
         if(large_image->GetPixel(index) != known_value)
           {
           test_status=EXIT_FAILURE;

           }
         std::cout << "At: " << index << " computed value =  " << large_image->GetPixel(index) << " known_value = " << known_value << std::endl;
       }
     std::cout << std::endl;
     }
   }
#if 0
 const double incr = 0.1;
 PointType point;
 for (double yy = 0; yy < static_cast<double>(small_ySize-1); yy++)
   {
   for (double xx = 0; xx < static_cast<double>(small_xSize-1); xx++)
     {
     for (double yyy = yy; yyy < yy + 1.01; yyy += incr)
       {
       for (double xxx = xx; xxx < xx + 1.01; xxx += incr)
         {
         point[0] = xxx;
         point[1] = yyy;
         if( interpolator->IsInsideBuffer( point ) )
           {
           //test scalar small_image
           const double computedValue = interpolator->Evaluate( point );
           std::cout << "computed value =  " << computedValue << std::endl;
           }
         }
       }
     }
   }
#endif

 return test_status;
}
