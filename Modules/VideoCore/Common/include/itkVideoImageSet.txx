/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef __itkVideoImageSet_txx
#define __itkVideoImageSet_txx

#include "itkVideoImageSet.h"
#include "itkPixelTraits.h"
#include "itkConvertPixelBuffer.h"

namespace itk
{
//-CONSTRUCTOR DESTRUCTOR PRINT------------------------------------------------

//
// Constructor
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
VideoImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::VideoImageSet()
{
  this->m_Allocated = false;
  this->Initialize();
}


//
// Destructor
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
VideoImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::~VideoImageSet()
{
  this->Deallocate();
}


//
// PrintSelf
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
void
VideoImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "VideoImageSet:" << std::endl;
  os << indent << "NumberOfBuffers" << this->GetNumberOfBuffers() << std::endl;
  if (this->m_BufferArray.size() != 0)
    {
    os << indent << "PixelContainer:" << std::endl;
    this->m_BufferArray[0]->Print(os, indent.GetNextIndent());
    }
}

//-VIDEO METHODS---------------------------------------------------------------

//
// GetCurrentPositionFrame
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
unsigned long
VideoImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::GetCurrentPositionFrame()
{
  if(this->m_VideoIO.GetPointer() == NULL)
    {
    itkWarningMacro("No VideoIO set");
    return 0;
    }
  return this->m_VideoIO->GetCurrentFrame();
}


//
// GetCurrentPositionRatio
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
double
VideoImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::GetCurrentPositionRatio()
{
  if(this->m_VideoIO.GetPointer() == NULL)
    {
    itkWarningMacro("No VideoIO set");
    return 0;
    }
  return this->m_VideoIO->GetRatio();
}


//
// GetCurrentPositionMSec
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
double
VideoImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::GetCurrentPositionMSec()
{
  if(this->m_VideoIO.GetPointer() == NULL)
    {
    itkWarningMacro("No VideoIO set");
    return 0;
    }
  return this->m_VideoIO->GetPositionInMSec();
}


//
// GetNumberOfFrames
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
unsigned long
VideoImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::GetNumberOfFrames()
{
  if(this->m_VideoIO.GetPointer() == NULL)
    {
    itkWarningMacro("No VideoIO set");
    return 0;
    }
  return this->m_VideoIO->GetFrameTotal();
}


//
// GetFpS
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
double
VideoImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::GetFpS()
{
  if(this->m_VideoIO.GetPointer() == NULL)
    {
    itkWarningMacro("No VideoIO set");
    return 0;
    }
  return this->m_VideoIO->GetFpS();
}


} // end namespace itk

#endif
