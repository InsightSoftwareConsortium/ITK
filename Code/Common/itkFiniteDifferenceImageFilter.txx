/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFiniteDifferenceImageFilter.txx
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
#ifndef __itkFiniteDifferenceImageFilter_txx_
#define __itkFiniteDifferenceImageFilter_txx_

#include "itkImageRegionIterator.h"
#include "itkExceptionObject.h"

namespace itk {

template <class TInputImage, class TOutputImage>
void
FiniteDifferenceImageFilter<TInputImage, TOutputImage>
::CopyInputToOutput()
{
  typename TInputImage::Pointer  input  = this->GetInput();
  typename TOutputImage::Pointer output = this->GetOutput();
  
  ImageRegionIterator<TInputImage>  in(input, output->GetRequestedRegion());
  ImageRegionIterator<TInputImage> out(output, output->GetRequestedRegion());

  in.Begin();
  out.Begin();
  while( ! out.IsAtEnd() )
    {
      out.Value() =  in.Get();  // Supports input image adaptors only
      ++in;
      ++out;
    }
}
  
template <class TInputImage, class TOutputImage>
void
FiniteDifferenceImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  // Allocate the output image
  typename TOutputImage::Pointer output = this->GetOutput();
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();

  // Copy the input image to the output image.  Algorithms will operate
  // directly on the output image and the update buffer.
  this->CopyInputToOutput();

  // Allocate the internal update buffer.  This takes place entirely within
  // the subclass, since this class cannot define an update buffer type.
  this->AllocateUpdateBuffer();

  // Iterative algorithm
  TimeStepType dt;
  m_ElapsedIterations = 0;
  while ( ! this->Halt() )
    {
      this->InitializeIteration(); // An optional method for precalculating
                                   // global values, or otherwise setting up
                                   // for the next iteration
      dt = this->CalculateChange();
      this->ApplyUpdate(dt);
      ++m_ElapsedIterations;

    }
}
  
template <class TInputImage, class TOutputImage>
typename FiniteDifferenceImageFilter<TInputImage, TOutputImage>::TimeStepType
FiniteDifferenceImageFilter<TInputImage, TOutputImage>
::ResolveTimeStep(const TimeStepType *list, const bool *valid, int size)
{  
  TimeStepType min;
  bool flag;
  
  // grab first valid value
  flag = false;
  for (int i = 0; i < size; ++i)
    {
      if (valid[i])
        {
          min = list[i];
          flag = true;
          break;
        }
    }
  
  if (!flag)
    {  // no values!
      throw ExceptionObject();
    }

  // find minimum value
  for (int i = 0; i < size; ++i)
    {      if ( valid[i] && (list[i] < min) )   min = list[i];      }

  return min;
}

template <class TInputImage, class TOutputImage>
void
FiniteDifferenceImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  os << indent << "FiniteDifferenceImageFilter";
  Superclass::PrintSelf(os, indent.GetNextIndent());
}


}// end namespace itk

#endif
