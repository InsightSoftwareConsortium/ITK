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
#include "itkRayCastInterpolateImageFunction.h"
#include "itkTranslationTransform.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkTestingMacros.h"


int
itkRayCastInterpolateImageFunctionTest(
    int itkNotUsed(argc),
    char * itkNotUsed(argv) [] )
{
    std::cout << "Testing RayCastInterpolateImageFunction:\n";

    typedef unsigned char PixelType;
    const unsigned int ImageDimension = 3;

    typedef itk::Image< PixelType, ImageDimension > ImageType;

    typedef ImageType::IndexType    IndexType;
    typedef ImageType::PointType    PointType;
    typedef ImageType::SpacingType  SpacingType;
    typedef ImageType::SizeType     SizeType;
    typedef ImageType::RegionType   RegionType;

    /* Allocate a simple test image */
    ImageType::Pointer image = ImageType::New();
    IndexType start;
    start.Fill(0);
    SizeType size;
    size[0] = 30;
    size[1] = 30;
    size[2] = 30;

    RegionType region;
    region.SetIndex(start);
    region.SetSize(size);
    image->SetRegions(region);
    image->Allocate();

    PointType origin;
    origin.Fill(0.0);

    SpacingType spacing;
    spacing.Fill(1.0);

    /* Set origin and spacing of physical coordinates */
    image->SetOrigin(origin);
    image->SetSpacing(spacing);

    /* Initialize the image contents */
    IndexType index;
    for (unsigned int slice = 0; slice < size[2]; slice++)
      {
      index[2] = slice;
      for (unsigned int row = 0; row < size[1]; row++)
        {
        index[1] = row;
        for (unsigned int col = 0; col < size[0]; col++)
          {
          index[0] = col;
          PixelType value = (PixelType)(slice+row+col);
          image->SetPixel(index,value);
          }
        }
      }

    typedef itk::RayCastInterpolateImageFunction<
                      ImageType,double> RayCastInterpolatorType;

    /* Create and initialize the interpolator */
    RayCastInterpolatorType::Pointer interp = RayCastInterpolatorType::New();
    interp->SetInputImage(image);
    interp->Print( std::cout );

    PointType focus;
    focus[0] =  15.0;
    focus[1] =  15.0;
    focus[2] = 100.0;

    interp->SetFocalPoint( focus );


    /* Create the transform */
    typedef itk::TranslationTransform<
                    double,ImageDimension>  TransformType;

    TransformType::Pointer transform = TransformType::New();
    interp->SetTransform( transform );

    /* Create the auxiliary interpolator */
    typedef itk::LinearInterpolateImageFunction<
                    ImageType, double > InterpolatorType;

    InterpolatorType::Pointer auxInterpolator = InterpolatorType::New();

    interp->SetInterpolator( auxInterpolator );

    /* Exercise the SetThreshold() method */
    interp->SetThreshold( 1.0 );

    /* Evaluate the function */
    double integral;
    PointType query;
    query[0] = 15.;
    query[1] = 15.;
    query[2] = -2.;

    integral = interp->Evaluate(query);

    std::cout << "Integral = " << integral << std::endl;

    TEST_EXPECT_TRUE( itk::Math::FloatAlmostEqual( integral, 1247. ) );

    return EXIT_SUCCESS;
}
