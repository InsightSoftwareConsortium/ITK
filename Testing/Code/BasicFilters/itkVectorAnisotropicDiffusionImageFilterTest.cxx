/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorAnisotropicDiffusionImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#include <iostream>
#include "itkImage.h"
#include "itkVectorAnisotropicDiffusionImageFilter.h"
#include "itkNullImageToImageFilterDriver.txx"
#include "itkVector.h"

inline ostream& operator<<(ostream &o, const itk::Vector<float, 3> &v)
{
  o << "["<< v[0] << " " << v[1] << " " << v[2] << "]";
  return o;
}

int main(int argc, char *argv[])
{
  try
    {
      // Set up filter
      itk::VectorAnisotropicDiffusionImageFilter<itk::Vector<float, 3>,
        2>::Pointer 
        filter =
        itk::VectorAnisotropicDiffusionImageFilter<itk::Vector<float, 3>, 
        2>::New();
      filter->SetIterations(1);
      filter->SetConductanceParameter(3.0f);
      filter->SetTimeStep(0.125f);

      // Run Test
      itk::Size<2> sz;
      sz[0] = 100 ; //atoi(argv[1]);
      sz[1] = 100 ; // atoi(argv[2]);
      //      sz[2] = 10;//atoi(argv[3]);
      //      sz[3] = 5;//atoi(argv[4]);
      itk::NullImageToImageFilterDriver< itk::Image<itk::Vector<float, 3>, 2>,
                                        itk::Image<itk::Vector<float, 3>, 2> >
        test1;
      test1.SetImageSize(sz);
      test1.SetFilter(filter.GetPointer());
      test1.Execute();
    }
  catch(itk::ExceptionObject &err)
    {
      (&err)->Print(std::cerr);
      return 1;
    } 
  return 0;   
}
