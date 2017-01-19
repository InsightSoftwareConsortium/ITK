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
#include "itkVectorImage.h"
#include "itkGradientImageFilter.h"
#include "itkNullImageToImageFilterDriver.hxx"
#include "itkTestingMacros.h"

#include "itkPeriodicBoundaryCondition.h"

inline std::ostream& operator<<(std::ostream &o, const itk::CovariantVector<float, 3> &v)
{
  o << "["<< v[0] << " " << v[1] << " " << v[2] << "]";
  return o;
}

int itkGradientImageFilterTest(int , char * [] )
{
  try
    {
    typedef itk::Image<unsigned short, 2>                     ImageType;
    typedef itk::GradientImageFilter<ImageType, float, float> FilterType;
    typedef FilterType::OutputImageType                       OutputImageType;


    // Set up filter
    FilterType::Pointer filter = FilterType::New();

    EXERCISE_BASIC_OBJECT_METHODS( filter, GradientImageFilter, ImageToImageFilter );

    // Run test
    itk::Size<2> sz;
    sz[0] = 100;
    sz[1] = 100;
    itk::NullImageToImageFilterDriver< ImageType, OutputImageType > test1;
    test1.SetImageSize(sz);
    test1.SetFilter(filter.GetPointer());
    test1.Execute();
    }
  catch(itk::ExceptionObject &err)
    {
      (&err)->Print(std::cerr);
      return EXIT_FAILURE;
    }

  // Verify that we can run with VectorImages
  try
    {
    typedef itk::Image< float, 3 >     InputImageType;
    typedef itk::VectorImage<float, 3> OutputImageType;

    typedef itk::GradientImageFilter< InputImageType, float, float, OutputImageType> FilterType;

    FilterType::Pointer filter = FilterType::New();

    typedef itk::PeriodicBoundaryCondition<InputImageType> PeriodicBoundaryType;
    //Test the OverrideBoundaryCondition setting;
    filter->OverrideBoundaryCondition( new PeriodicBoundaryType );

    EXERCISE_BASIC_OBJECT_METHODS( filter, GradientImageFilter, ImageToImageFilter );

    // Run test
    itk::Size<3> sz;
    sz[0] = 25;
    sz[1] = 25;
    sz[2] = 25;
    itk::NullImageToImageFilterDriver< InputImageType, OutputImageType > test1;
    test1.SetImageSize(sz);
    test1.SetFilter(filter.GetPointer());
    test1.Execute();
    }
  catch(itk::ExceptionObject &err)
    {
      (&err)->Print(std::cerr);
      return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
