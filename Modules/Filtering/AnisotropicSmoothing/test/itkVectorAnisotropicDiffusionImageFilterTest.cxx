/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <iostream>
#include "itkVectorGradientAnisotropicDiffusionImageFilter.h"
#include "itkVectorCurvatureAnisotropicDiffusionImageFilter.h"
#include "itkNullImageToImageFilterDriver.hxx"
#include "itkTestingMacros.h"

inline std::ostream &
operator<<(std::ostream & o, const itk::Vector<float, 3> & v)
{
  o << "[" << v[0] << " " << v[1] << " " << v[2] << "]";
  return o;
}

int
itkVectorAnisotropicDiffusionImageFilterTest(int itkNotUsed(argc), char * itkNotUsed(argv)[])
{
  try
  {
    using ImageType = itk::Image<itk::Vector<float, 3>, 2>;

    // Set up Gradient diffusion filter
    itk::VectorGradientAnisotropicDiffusionImageFilter<ImageType, ImageType>::Pointer filter =
      itk::VectorGradientAnisotropicDiffusionImageFilter<ImageType, ImageType>::New();

    ITK_EXERCISE_BASIC_OBJECT_METHODS(
      filter, VectorGradientAnisotropicDiffusionImageFilter, AnisotropicDiffusionImageFilter);


    filter->SetNumberOfIterations(1);
    filter->SetConductanceParameter(3.0f);
    filter->SetTimeStep(0.125f);

    filter->GetNumberOfIterations();
    filter->GetConductanceParameter();
    filter->GetTimeStep();

    // Run Test
    itk::Size<2> sz;
    sz[0] = 100; // std::stoi(argv[1]);
    sz[1] = 100; // std::stoi(argv[2]);
    //      sz[2] = 10;//std::stoi(argv[3]);
    //      sz[3] = 5;//std::stoi(argv[4]);
    itk::NullImageToImageFilterDriver<ImageType, ImageType> test1;
    test1.SetImageSize(sz);
    test1.SetFilter(filter);
    test1.Execute();


    // Set up Curvature diffusion filter
    itk::VectorCurvatureAnisotropicDiffusionImageFilter<ImageType, ImageType>::Pointer filter2 =
      itk::VectorCurvatureAnisotropicDiffusionImageFilter<ImageType, ImageType>::New();

    ITK_EXERCISE_BASIC_OBJECT_METHODS(
      filter2, VectorCurvatureAnisotropicDiffusionImageFilter, AnisotropicDiffusionImageFilter);


    filter2->SetNumberOfIterations(1);
    filter2->SetConductanceParameter(3.0f);
    filter2->SetTimeStep(0.1f);

    filter2->GetNumberOfIterations();
    filter2->GetConductanceParameter();
    filter2->GetTimeStep();

    // Run Test
    itk::NullImageToImageFilterDriver<ImageType, ImageType> test2;
    test2.SetImageSize(sz);
    test2.SetFilter(filter2);
    test2.Execute();
  }
  catch (const itk::ExceptionObject & err)
  {
    err.Print(std::cerr);
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
