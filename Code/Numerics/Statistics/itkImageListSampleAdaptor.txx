/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageListSampleAdaptor.txx
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
#ifndef _itkImageListSampleAdaptor_txx
#define _itkImageListSampleAdaptor_txx

#include "itkImageListSampleAdaptor.h"

namespace itk{ 
  namespace Statistics{

template < class TImage, class TMeasurement, 
           unsigned int VMeasurementVectorSize , 
           class TAccessor >
ImageListSampleAdaptor< TImage, TMeasurement, VMeasurementVectorSize, 
                               TAccessor >::IndexType
ImageListSampleAdaptor< TImage, TMeasurement, VMeasurementVectorSize,
                               TAccessor >
::GetIndex(const InstanceIdentifier id) 
{
  IndexType index = m_Image->ComputeIndex(id) ;
  return index ;
}

template < class TImage, class TMeasurement, 
           unsigned int VMeasurementVectorSize , 
           class TAccessor >
ImageListSampleAdaptor< TImage, TMeasurement, VMeasurementVectorSize,
                               TAccessor >::InstanceIdentifier
ImageListSampleAdaptor< TImage, TMeasurement, VMeasurementVectorSize,
                               TAccessor >
::GetInstanceIdentifier(const IndexType index) 
{
  return m_Image->ComputeOffset(index) ;
}


template < class TImage, class TMeasurement, 
           unsigned int VMeasurementVectorSize , 
           class TAccessor >
void
ImageListSampleAdaptor< TImage, TMeasurement, VMeasurementVectorSize,
                               TAccessor >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Image: " << m_Image << std::endl;
  //  os << indent << "Accesor: " << m_Accessor << std::endl;
}

  } // end of namespace Statistics 
} // end of namespace itk

#endif
