/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPathToImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkPolyLineParametricPath.h"
#include "itkPathToImageFilter.h"

int itkPathToImageFilterTest(int, char* [] )
{
  typedef  itk::PolyLineParametricPath<2>                 PathType;
  typedef  itk::Image<double, 2>                          ImageType;
  typedef  PathType::VertexType                           VertexType;
  
  // Setup the path
  std::cout << "Making a square Path with v0 at (30,30) and v2 at (33,33)" << std::endl;
  VertexType        v;
  PathType::Pointer path  = PathType::New();
  v.Fill(30);
  path->AddVertex(v);
  v[0]=33;
  v[1]=30;
  path->AddVertex(v);
  v.Fill(33);
  path->AddVertex(v);
  v[0]=30;
  v[1]=33;
  path->AddVertex(v);
  v.Fill(30);
  path->AddVertex(v);

  typedef itk::PathToImageFilter<PathType,ImageType> PathToImageFilterType;
  PathToImageFilterType::Pointer imageFilter = PathToImageFilterType::New();
  imageFilter->SetInput(path);
  imageFilter = PathToImageFilterType::New();
  imageFilter->SetInput(path);
  imageFilter->SetPathValue(1);
  imageFilter->GetPathValue();
  imageFilter->SetBackgroundValue(0);
  imageFilter->GetBackgroundValue();
  ImageType::SizeType size;
  size[0]=256;
  size[1]=256;
  imageFilter->SetSize(size);

  // Testing spacing 
  std::cout << "Testing Spacing: ";
  
  float spacing_float[2];
  double spacing_double[2];

  for(unsigned int i=0;i<2;i++)
  {
    spacing_float[i]=1.0;
    spacing_double[i]=1.0;
  }
  imageFilter->SetSpacing(spacing_float);
  imageFilter->SetSpacing(spacing_double);
  const double* spacing_result = imageFilter->GetSpacing();
  
  for(unsigned int i=0;i<2;i++)
  {
    if(spacing_result[i]!=1.0)
    {
      std::cout << "[FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }
  }

  std::cout << "[PASSED]" << std::endl;

  // Testing Origin 
  std::cout << "Testing Origin: ";
  
  float origin_float[2];
  double origin_double[2];

  for(unsigned int i=0;i<2;i++)
  {
    origin_float[i]=0.0;
    origin_double[i]=0.0;
  }
  imageFilter->SetOrigin(origin_float);
  imageFilter->SetOrigin(origin_double);
  const double* origin_result = imageFilter->GetOrigin();
  
  for(unsigned int i=0;i<2;i++)
  {
    if(origin_result[i]!=0.0)
    {
      std::cout << "[FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }
  }

  std::cout << "[PASSED]" << std::endl;

  // Testing PrintSelf
  std::cout << imageFilter << std::endl;

  //Update the filter
  imageFilter->Update();

  ImageType::Pointer image = imageFilter->GetOutput();

  std::cout << "Testing Output Image: ";

  ImageType::IndexType index;
  // Test only pixels on or in the path
  for(int i=0;i<=3;i++)
    {
    for(int j=0;j<=3;j++)
      {
      double targetValue;
      
      index[0] = 30+i;
      index[1] = 30+j;

      if( 0<i&&i<3 && 0<j&&j<3 )
        {
        // inside the closed path, but not on it
        targetValue=0;
        }
      else
        {
        // on the path
        targetValue=1;
        }
      if(image->GetPixel(index) != targetValue)
        {
        std::cout << "[FAILURE]" << std::endl;
        return EXIT_FAILURE;
        }
      }
    }
  
  std::cout << "[PASSED]" << std::endl;

  return EXIT_SUCCESS;
}
