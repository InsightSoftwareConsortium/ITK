/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMRASlabIdentifier.txx
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
#ifndef __itkMRASlabIdentifier_txx
#define __itkMRASlabIdentifier_txx

#include <algorithm>
#include <vector>
#include "itkMRASlabIdentifier.h"
#include "itkImageRegionIterator.h"

namespace itk
{
  template<class TInputImage>
  MRASlabIdentifier<TInputImage>
  ::MRASlabIdentifier()
  {
    m_NumberOfMinimumsPerSlice = 10 ;
    // default slicing axis is z
    m_SlicingDirection = 2 ;
  }

  template<class TInputImage>
  void 
  MRASlabIdentifier<TInputImage>
  ::SetImage(ImagePointer image)
  {
    m_Image = image ;
  }

  template<class TInputImage>
  void 
  MRASlabIdentifier<TInputImage>
  ::SetNumberOfMinimumsPerSlice(int no) 
  {
    m_NumberOfMinimumsPerSlice = no ;
  }

  template<class TInputImage>
  void 
  MRASlabIdentifier<TInputImage>
  ::GenerateSlabRegions(void)
  {
    // this method only works with 3D MRI image
    if (ImageType::ImageDimension != 3)
      {
        throw ExceptionObject(__FILE__, __LINE__) ;
      }

    ImageSizeType size ;
    ImageRegionType region ;
    ImageIndexType index ;

    region = m_Image->GetLargestPossibleRegion() ;  
    size = region.GetSize() ;
    index = region.GetIndex() ;
    long firstSlice = index[m_SlicingDirection] ;
    long lastSlice = firstSlice + size[m_SlicingDirection] ;
    unsigned long totalSlices = size[m_SlicingDirection] ;

    double sum ;
    std::vector<double> avgMin(totalSlices) ;
    // calculate minimum intensities for each slice
    std::vector<ImagePixelType> mins(m_NumberOfMinimumsPerSlice, 1000) ;

    ImagePixelType pixel ;
    std::vector<ImagePixelType>::iterator target ;

    long currentSlice = firstSlice ;

    for (int i = 0 ; i < 3 ; i++)
      {
        if (i != m_SlicingDirection)
          {
            index[i] = 0 ;
          }
      }

    size[m_SlicingDirection] = 1 ;
    region.SetSize(size) ;

    unsigned long count = 0 ;
    while (currentSlice < lastSlice)
      {
        index[m_SlicingDirection] = currentSlice ;
        region.SetIndex(index) ;

        ImageRegionIterator<TInputImage> iter(m_Image, region) ;
        iter.GoToBegin() ;

        std::fill(mins.begin(), mins.end(), 1000) ;
        while (!iter.IsAtEnd())
          {
            pixel = iter.Get() ;
            if (pixel > 0)
              {
                target = 
                  std::find_if(mins.begin(), mins.end(), 
                          std::bind2nd(std::greater<ImagePixelType>(), pixel)) ;
                *target = pixel ;
              }
            ++iter ;
          }
        
        sum = 0 ;
        std::vector<ImagePixelType>::iterator m_iter = mins.begin() ;
        while (m_iter != mins.end())
          {
            sum += *m_iter ;
            ++m_iter ;
          }

        avgMin[count] 
          = sum / (double) m_NumberOfMinimumsPerSlice ;

        ++count ;
        ++currentSlice ;
      }

    // calculate overall average
    sum = 0 ;
    std::vector<double>::iterator am_iter = avgMin.begin() ;
    while (am_iter != avgMin.end())
      {
        sum += *am_iter ;
        ++am_iter ;
      }

    double average = sum / (double) totalSlices ; 
    

    // determine slabs
    am_iter = avgMin.begin() ;
    
    int slabCount = 0 ;
    double sign ;
    double prevSign = *am_iter - average ;
    double avgMinValue ;

    ImageIndexType slabIndex ;
    ImageRegionType slabRegion ;
    ImageSizeType slabSize ;

    long slabLength = 0 ;
    long slabBegin = firstSlice ;
    slabSize = size ;
    slabIndex = index ;
    while (am_iter != avgMin.end())
      {
        avgMinValue = *am_iter ;
        sign = avgMinValue - average ;
        if (sign * prevSign < 0)
          { 
            slabIndex[m_SlicingDirection] = slabBegin ;
            slabSize[m_SlicingDirection] = slabLength ;
            slabRegion.SetSize(slabSize) ;
            slabRegion.SetIndex(slabIndex) ;
            m_Slabs.push_back(slabRegion) ;

            prevSign = sign ;
            slabBegin += slabLength ;
            slabLength = 0 ;
          }
        am_iter++ ;
        slabLength++ ;
      }
    slabIndex[m_SlicingDirection] = slabBegin ;
    slabSize[m_SlicingDirection] = slabLength ;
    slabRegion.SetIndex(slabIndex) ;
    slabRegion.SetSize(slabSize) ;
    m_Slabs.push_back(slabRegion) ;
  }


  template<class TInputImage>
  MRASlabIdentifier<TInputImage>::SlabRegionVectorType 
  MRASlabIdentifier<TInputImage>
  ::GetSlabRegionVector(void)
  {
    return m_Slabs ;
  }

} // end namespace itk

#endif /* __itkMRASlabIdentifier_txx */
