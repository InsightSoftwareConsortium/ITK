/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorAnisotropicDiffusionImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <iostream>
#include "itkImage.h"
#include "itkVectorGradientAnisotropicDiffusionImageFilter.h"
#include "itkVectorCurvatureAnisotropicDiffusionImageFilter.h"
#include "itkNullImageToImageFilterDriver.txx"
#include "itkVector.h"

inline std::ostream& operator<<(std::ostream &o, const itk::Vector<float, 3> &v)
{
  o << "["<< v[0] << " " << v[1] << " " << v[2] << "]";
  return o;
}

int itkVectorAnisotropicDiffusionImageFilterTest(int itkNotUsed(argc), char *itkNotUsed(argv) [] )
{
  try
    {
      typedef itk::Image<itk::Vector<float, 3>, 2> ImageType;
      
      // Set up Gradient diffusion filter
      itk::VectorGradientAnisotropicDiffusionImageFilter<ImageType, ImageType>
        ::Pointer  filter =
        itk::VectorGradientAnisotropicDiffusionImageFilter<ImageType,
        ImageType>
        ::New();
      filter->SetNumberOfIterations(1);
      filter->SetConductanceParameter(3.0f);
      filter->SetTimeStep(0.125f);

      filter->GetNumberOfIterations();
      filter->GetConductanceParameter();
      filter->GetTimeStep();

      // Run Test
      itk::Size<2> sz;
      sz[0] = 100 ; //atoi(argv[1]);
      sz[1] = 100 ; // atoi(argv[2]);
      //      sz[2] = 10;//atoi(argv[3]);
      //      sz[3] = 5;//atoi(argv[4]);
      itk::NullImageToImageFilterDriver< ImageType, ImageType > test1;
      test1.SetImageSize(sz);
      test1.SetFilter(filter.GetPointer());
      test1.Execute();

      
 // Set up Curvature diffusion filter
      itk::VectorCurvatureAnisotropicDiffusionImageFilter<ImageType, ImageType>
        ::Pointer  filter2 =
        itk::VectorCurvatureAnisotropicDiffusionImageFilter<ImageType, ImageType>
        ::New();
      filter2->SetNumberOfIterations(1);
      filter2->SetConductanceParameter(3.0f);
      filter2->SetTimeStep(0.1f);

      filter2->GetNumberOfIterations();
      filter2->GetConductanceParameter();
      filter2->GetTimeStep();

      // Run Test
      itk::NullImageToImageFilterDriver< ImageType, ImageType > test2;
      test2.SetImageSize(sz);
      test2.SetFilter(filter2.GetPointer());
      test2.Execute();
      
      
    }
  catch(itk::ExceptionObject &err)
    {
      (&err)->Print(std::cerr);
      return 1;
    } 
  return 0;   
}
