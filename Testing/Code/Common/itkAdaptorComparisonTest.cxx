/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAdaptorComparisonTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include <time.h>
#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionIterator.h"
#include "itkVector.h"

void AdaptorSupportedIteratorSpeed(itk::Image<float, 3> *img)
{
  itk::ImageRegionIteratorWithIndex<itk::Image<float, 3> >
    it (img, img->GetRequestedRegion());

  while( ! it.IsAtEnd() )
    {
      ++it;
    }
  
}

void NoAdaptorSupportIteratorSpeed(itk::Image<float, 3> *img)
{
  itk::ImageRegionIterator<itk::Image<float, 3> >
    it (img, img->GetRequestedRegion());

  while( ! it.IsAtEnd() )
    {
      ++it;
    }
}

void AdaptorSupportedModifyScalars(itk::Image<float, 3> *img)
{
  itk::ImageRegionIteratorWithIndex<itk::Image<float, 3> >
    it (img, img->GetRequestedRegion());

  while( ! it.IsAtEnd() )
    {
      // *it += 3.435f;
      it.Set(it.Get() + 3.435f);
      ++it;
    }
}

void NoAdaptorSupportModifyScalars(itk::Image<float, 3> *img)
{
  itk::ImageRegionIterator<itk::Image<float, 3> >
    it (img, img->GetRequestedRegion());

  while( ! it.IsAtEnd() )
    {
      // *it += 3.435f;
      it.Set(it.Get() + 3.435f);
      ++it;
    }
}

void BypassAdaptorSupportModifyScalars(itk::Image<float, 3> *img)
{
  itk::ImageRegionIteratorWithIndex< itk::Image<float, 3> >
    it (img, img->GetRequestedRegion());

  while( ! it.IsAtEnd() )
    {
      it.Value() += 3.435f;
      ++it;
    }
}

void AdaptorSupportedModifyVectors(itk::Image<itk::Vector<float, 3>, 3> *img)
{
  typedef itk::Vector<float, 3> VectorType;
  const unsigned int N = 3;
  unsigned int i;
  VectorType temp_vector;
  
  itk::ImageRegionIteratorWithIndex<itk::Image<VectorType, 3> >
    it (img, img->GetRequestedRegion());
  
  while( ! it.IsAtEnd() )
    {
      temp_vector = it.Get();

      for (i = 0; i<N; ++i) temp_vector[i] += 3.435f;

      it.Set(temp_vector);
       ++it;
    }
}

void NoAdaptorSupportModifyVectors(itk::Image<itk::Vector<float, 3>, 3> *img)
{
  typedef itk::Vector<float, 3> VectorType;
  const unsigned int N = 3;
  unsigned int i;
  VectorType temp_vector;
  
  itk::ImageRegionIterator<itk::Image<VectorType, 3> >
    it (img, img->GetRequestedRegion());
  
  while( ! it.IsAtEnd() )
    {
      temp_vector = it.Get();

      for (i = 0; i<N; ++i) temp_vector[i] += 3.435f;

      it.Set(temp_vector);
      ++it;

      //      for (i = 0; i<N; ++i)  (*it)[i] += 3.435f;
      //       ++it;
    }
}

void BypassAdaptorSupportModifyVectors(itk::Image<itk::Vector<float, 3>, 3> *img)
{
  typedef itk::Vector<float, 3> VectorType;
  const unsigned int N = 3;
  unsigned int i;
  
  itk::ImageRegionIteratorWithIndex< itk::Image<VectorType, 3> >
    it (img, img->GetRequestedRegion());
  
  while( ! it.IsAtEnd() )
    {
      for (i = 0; i<N; ++i)  (it.Value())[i] += 3.435f;
       ++it;
    }
}


void BypassNoAdaptorSupportModifyVectors(itk::Image<itk::Vector<float, 3>, 3> *img)
{
  typedef itk::Vector<float, 3> VectorType;
  const unsigned int N = 3;
  unsigned int i;
  
  itk::ImageRegionIterator< itk::Image<VectorType, 3> >
    it (img, img->GetRequestedRegion());
  
  while( ! it.IsAtEnd() )
    {
      for (i = 0; i<N; ++i)  (it.Value())[i] += 3.435f;
       ++it;
    }
}


int itkAdaptorComparisonTest(int, char * [] )
{
  typedef itk::Image<float, 3> ScalarImageType;
  typedef itk::Image<itk::Vector<float, 3>, 3> VectorImageType;

  clock_t start, stop, no_adaptor_comp, adaptor_comp;
  
  // Set up some images
  itk::ImageRegion<3> region;
  itk::Size<3> size;
   size[0] = 100;
   size[1] = 100;
   size[2] = 100;
  itk::Index<3> index;
   index[0] = 0;
   index[1] = 0;
   index[2] = 0;
  region.SetSize(size);
  region.SetIndex(index);

  ScalarImageType::Pointer scalar_image = ScalarImageType::New();
  VectorImageType::Pointer vector_image = VectorImageType::New();

  scalar_image->SetLargestPossibleRegion(region);
  scalar_image->SetBufferedRegion(region);
  scalar_image->SetRequestedRegion(region);
  scalar_image->Allocate();
  scalar_image->FillBuffer(0);

  vector_image->SetLargestPossibleRegion(region);
  vector_image->SetBufferedRegion(region);
  vector_image->SetRequestedRegion(region);
  vector_image->Allocate();

  VectorImageType::PixelType initialVectorValue;
  initialVectorValue.Fill(1.2345);  // arbitrary value;
  vector_image->FillBuffer( initialVectorValue );
  
  // Time trials

  std::cout << "Speed of adaptor supporting interator (for reference) \t";
  start = clock();
  AdaptorSupportedIteratorSpeed(scalar_image);
  stop = clock();
  adaptor_comp = stop - start;
  std::cout << adaptor_comp << std::endl;

  std::cout
    << "Speed of interator that does not support adaptors (for reference) \t";
  start = clock();
  NoAdaptorSupportIteratorSpeed(scalar_image);
  stop = clock();
  no_adaptor_comp = stop - start;
  std::cout << no_adaptor_comp << std::endl;
  
  std::cout << "Modifying scalar image using adaptor iterator...\t";
  start = clock();
  AdaptorSupportedModifyScalars(scalar_image);
  stop = clock();
  std::cout << (stop - start) << "\t compensated = " << (stop-start) -
    adaptor_comp << std::endl;

  std::cout << "Modifying scalar image using non-adaptor iterator...\t";
  start = clock();
  NoAdaptorSupportModifyScalars(scalar_image);
  stop = clock();
  std::cout << (stop - start) << "\t compensated = " << (stop-start) -
    no_adaptor_comp << std::endl;

  std::cout << "Modifying vector image using adaptor iterator...\t";
  start = clock();
  AdaptorSupportedModifyVectors(vector_image);
  stop = clock();
  std::cout << (stop - start) << "\t compensated = " << (stop-start) -
    adaptor_comp << std::endl;

  std::cout << "Modifying vector image using non-adaptor iterator...\t";
  start = clock();
  NoAdaptorSupportModifyVectors(vector_image);
  stop = clock();
  std::cout << (stop - start) << "\t compensated = " << (stop-start) -
    no_adaptor_comp <<std::endl;

  std::cout << "Modifying scalar image bypassing adaptor api using"
            << " adaptor iterator...\t";
  start = clock();
  BypassAdaptorSupportModifyScalars(scalar_image);
  stop = clock();
  std::cout << (stop - start) << "\t compensated = " << (stop-start) -
    adaptor_comp <<std::endl;
  
  std::cout << "Modifying vector image bypassing adaptor api using"
            << " non-adaptor iterator...\t";
  start = clock();
  BypassNoAdaptorSupportModifyVectors(vector_image);
  stop = clock();
  std::cout << (stop - start) << "\t compensated = " << (stop-start) -
    adaptor_comp <<std::endl;

  std::cout << "Modifying vector image bypassing adaptor api using"
            << " adaptor iterator...\t";
  start = clock();
  BypassAdaptorSupportModifyVectors(vector_image);
  stop = clock();
  std::cout << (stop - start) << "\t compensated = " << (stop-start) -
    adaptor_comp <<std::endl;

  return 0;
}
