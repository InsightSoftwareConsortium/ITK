/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkAdaptorComparisonTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#include <iostream>
#include <time.h>
#include "itkImage.h"
#include "itkSimpleImageRegionIterator.h"
#include "itkImageRegionIterator.h"
#include "itkVector.h"

void AdaptorSupportedIteratorSpeed(itk::Image<float, 3> *img)
{
  itk::SimpleImageRegionIterator<itk::Image<float, 3> >
    it (img, img->GetRequestedRegion());

  it.Begin();
  while( ! it.IsAtEnd() )
    {
      ++it;
    }
  
}

void NoAdaptorSupportIteratorSpeed(itk::Image<float, 3> *img)
{
  itk::ImageRegionIterator<itk::Image<float, 3> >
    it (img, img->GetRequestedRegion());

  it.Begin();
  while( ! it.IsAtEnd() )
    {
      ++it;
    }
}

void AdaptorSupportedModifyScalars(itk::Image<float, 3> *img)
{
  itk::SimpleImageRegionIterator<itk::Image<float, 3> >
    it (img, img->GetRequestedRegion());

  it.Begin();
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

  it.Begin();
  while( ! it.IsAtEnd() )
    {
      // *it += 3.435f;
      it.Set(it.Get() + 3.435f);
      ++it;
    }
}

void BypassAdaptorSupportModifyScalars(itk::Image<float, 3> *img)
{
  itk::SimpleImageRegionIterator< itk::Image<float, 3> >
    it (img, img->GetRequestedRegion());

  it.Begin();
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
  
  itk::SimpleImageRegionIterator<itk::Image<VectorType, 3> >
    it (img, img->GetRequestedRegion());
  
  it.Begin();
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
  
  it.Begin();
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
  
  itk::SimpleImageRegionIterator< itk::Image<VectorType, 3> >
    it (img, img->GetRequestedRegion());
  
  it.Begin();
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
  
  it.Begin();
  while( ! it.IsAtEnd() )
    {
      for (i = 0; i<N; ++i)  (it.Value())[i] += 3.435f;
       ++it;
    }
}


int main()
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

  vector_image->SetLargestPossibleRegion(region);
  vector_image->SetBufferedRegion(region);
  vector_image->SetRequestedRegion(region);
  vector_image->Allocate();

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
