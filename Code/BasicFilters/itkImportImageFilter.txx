/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImportImageFilter.txx
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
#ifndef _itkImportImageFilter_txx
#define _itkImportImageFilter_txx

#include "itkImportImageFilter.h"
#include "itkObjectFactory.h"

namespace itk
{

/**
 *
 */
template <class TPixel, unsigned int VImageDimension>
ImportImageFilter<TPixel, VImageDimension>
::ImportImageFilter()
{
}

/**
 *
 */
template <class TPixel, unsigned int VImageDimension>
ImportImageFilter<TPixel, VImageDimension>
::~ImportImageFilter()
{
}


/**
 *
 */
template <class TPixel, unsigned int VImageDimension>
void 
ImportImageFilter<TPixel, VImageDimension>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

}


/**
 *
 */
template <class TPixel, unsigned int VImageDimension>
void 
ImportImageFilter<TPixel, VImageDimension>
::SetImportPointer(TPixel *ptr, unsigned long num, bool LetSourceManageMemory)
{
  OutputImagePointer outputPtr = this->GetOutput();

  // check to see if this will cause the filter to be modified
  if (ptr != outputPtr->GetPixelContainer()->GetImportPointer())
    {
    outputPtr->GetPixelContainer()
      ->SetImportPointer( ptr, num, LetSourceManageMemory );
    this->Modified();
    }
}


/**
 *
 */
template <class TPixel, unsigned int VImageDimension>
TPixel *
ImportImageFilter<TPixel, VImageDimension>
::GetImportPointer()
{
  OutputImagePointer outputPtr = this->GetOutput();

  return outputPtr->GetImportPointer();
}


/**
 *
 */
template <class TPixel, unsigned int VImageDimension>
void 
ImportImageFilter<TPixel, VImageDimension>
::EnlargeOutputRequestedRegion(DataObject *output) 
{
  // call the superclass' implementation of this method
  Superclass::EnlargeOutputRequestedRegion(output);

  // get pointer to the output
  OutputImagePointer outputPtr = this->GetOutput();

  // set the requested region to the largest possible region (in this case
  // the amount of data that we have)
  outputPtr->SetRequestedRegion( outputPtr->GetLargestPossibleRegion() );
}


/** 
 *
 */
template <class TPixel, unsigned int VImageDimension>
void 
ImportImageFilter<TPixel, VImageDimension>
::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // get pointer to the output
  OutputImagePointer outputPtr = this->GetOutput();

  // we need to compute the output spacing, the output origin, the
  // output image size, and the output image start index
  outputPtr->SetLargestPossibleRegion( m_Region );
}


/**
 *
 */
template <class TPixel, unsigned int VImageDimension>
void 
ImportImageFilter<TPixel, VImageDimension>
::GenerateData()
{
  // Normally, GenerateData() allocates memory.  However, the application
  // provides the memory for this filter via the SetImportPointer() method.
  // Therefore, this filter does not call outputPtr->Allocate().
  
  // get pointer to the output
  OutputImagePointer outputPtr = this->GetOutput();

  // the output buffer size is set to the size specified by the user via the
  // SetRegion() method.
  outputPtr->SetBufferedRegion( outputPtr->GetLargestPossibleRegion() );
}


} // end namespace itk

#endif
