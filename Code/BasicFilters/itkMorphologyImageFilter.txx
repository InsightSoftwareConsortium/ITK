/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMorphologyImageFilter.txx
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
#ifndef __itkMorphologyImageFilter_txx
#define __itkMorphologyImageFilter_txx

#include <limits.h>

#include "itkConstantBoundaryCondition.h"
#include "itkNumericTraits.h"
#include "itkMorphologyImageFilter.h"

namespace itk {

template<class TInputImage, class TOutputImage, class TKernel>
MorphologyImageFilter<TInputImage, TOutputImage, TKernel>
::MorphologyImageFilter()
{
  m_Kernel = 0;
}
  
template <class TInputImage, class TOutputImage, class TKernel>
void 
MorphologyImageFilter<TInputImage, TOutputImage, TKernel>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  InputImagePointer  input = this->GetInput();
  OutputImagePointer output = this->GetOutput();

  if ( !input || !output )
    {
      return;
    }

  InputImageRegionType inputRequestedRegion = input->GetRequestedRegion() ;
  OutputImageRegionType outputRequestedRegion = output->GetRequestedRegion() ;
  
  InputImageRegionType tempRegion ;
  InputImageRegionType tempRegion2 ;
  tempRegion = 
    EnlargeImageRegion(outputRequestedRegion, m_Kernel->GetRadius()) ;
  tempRegion2 = 
    EnlargeImageRegion(inputRequestedRegion, 
                       input->GetLargestPossibleRegion(),
                       tempRegion) ;

  input->SetRequestedRegion(tempRegion2) ;
}

template<class TInputImage, class TOutputImage, class TKernel>
void
MorphologyImageFilter<TInputImage, TOutputImage, TKernel>
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId) 
{
  NeighborhoodIteratorType n_iter(m_Kernel->GetRadius(), 
                                  this->GetInput(), outputRegionForThread) ;

  // Is this the right boundary condition for all morphology operations?
  ConstantBoundaryCondition<TInputImage> BC;
  BC.SetConstant( NumericTraits<PixelType>::Zero );

  n_iter.OverrideBoundaryCondition(&BC);

  ImageRegionIterator<TOutputImage>
    o_iter(this->GetOutput(), outputRegionForThread) ;

  n_iter.GoToBegin();
  o_iter.GoToBegin() ;
  KernelIteratorType kernelFirst = m_Kernel->Begin() ;
  ImageKernelIteratorType first ; 
  ImageKernelIteratorType last ;
  while ( ! n_iter.IsAtEnd() )
    {
      first = n_iter.GetNeighborhood().Begin() ;
      last = n_iter.GetNeighborhood().End() ;

      o_iter.Set ( this->Evaluate(first, last, 
                                  kernelFirst, 
                                  n_iter.GetCenterPixel()) );
      ++n_iter ;
      ++o_iter ;
    }
}

template<class TInputImage, class TOutputImage, class TKernel>
MorphologyImageFilter<TInputImage, TOutputImage, TKernel>::RegionType
MorphologyImageFilter<TInputImage, TOutputImage, TKernel>
::EnlargeImageRegion(RegionType region,
                     RadiusType radius)
{
  int i;
  SizeType size = region.GetSize();
  IndexType index = region.GetIndex();
  
  SizeType  tempSize;
  IndexType tempIndex;
  
  for (i = 0; i < ImageDimension; i++)
    {
     tempSize[i] = size[i] + radius[i] * 2 ;
     tempIndex[i] = index[i] - radius[i] ;
    }

  RegionType ret ;
  ret.SetIndex(tempIndex) ;
  ret.SetSize(tempSize) ;

  return ret ;
}

template<class TInputImage, class TOutputImage, class TKernel>
MorphologyImageFilter<TInputImage, TOutputImage, TKernel>::RegionType
MorphologyImageFilter<TInputImage, TOutputImage, TKernel>
::EnlargeImageRegion(RegionType current,
                     RegionType largest,
                     RegionType requested)
{
  int i ;

  SizeType currentSize = current.GetSize() ;
  SizeType largestSize = largest.GetSize() ;
  SizeType requestedSize = requested.GetSize() ;
  
  IndexType currentIndex = current.GetIndex() ;
  IndexType largestIndex = largest.GetIndex() ;
  IndexType requestedIndex = requested.GetIndex() ;

  // index for the bottom right pixel + 1  
  IndexType currentLastP1Index = currentIndex + currentSize ;
  IndexType largestLastP1Index = largestIndex + largestSize ;
  IndexType requestedLastP1Index = requestedIndex + requestedSize ;

  SizeType tempSize ;
  IndexType tempIndex ;

  for (i = 0 ; i < ImageDimension ; i++)
    {
      if (requestedIndex[i] < largestIndex[i])
        {
          tempIndex[i] = largestIndex[i] ;
          if (requestedLastP1Index[i] > largestLastP1Index[i])
            tempSize[i] = largestLastP1Index[i] - largestIndex[i]  ;
          else
            tempSize[i] = requestedLastP1Index[i] - largestIndex[i]  ;
        }
      else if (requestedIndex[i] < currentIndex[i]) 
        {
          tempIndex[i] = requestedIndex[i] ;
          
          if (requestedLastP1Index[i] > largestLastP1Index[i])
            tempSize[i] = largestLastP1Index[i] - largestIndex[i]  ;
          else
            tempSize[i] = requestedLastP1Index[i] - largestIndex[i]  ;
        }
      else if (requestedIndex[i] >= largestLastP1Index[i] - 1)
        {
          tempIndex[i] = largestLastP1Index[i] - 1 ;
          tempSize[i] = 0 ;
        }
      else
        {
          // requestedIndex is in the range [currentIndex, largestLastP1Index)
          tempIndex[i] = currentIndex[i] ;

          if (requestedLastP1Index[i] > currentLastP1Index[i])
            {
              if (requestedLastP1Index[i] > largestLastP1Index[i])
                tempSize[i] = largestLastP1Index[i] - largestIndex[i]  ;
              else
                tempSize[i] = requestedLastP1Index[i] - largestIndex[i]  ;
            }
          else
            {
              tempSize[i] = currentSize[i] ;
            }
        }
    }

  RegionType ret ;
  ret.SetIndex(tempIndex) ;
  ret.SetSize(tempSize) ;
  
  return ret ;
}


template<class TInputImage, class TOutputImage, class TKernel>
void
MorphologyImageFilter<TInputImage, TOutputImage, TKernel>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Kernel: " << (m_Kernel ? "(None)" : m_Kernel)
     << std::endl;
}

}// end namespace itk
#endif
