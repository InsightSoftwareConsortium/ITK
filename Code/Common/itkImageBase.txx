/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageBase.txx
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
#ifndef _itkImageBase_txx
#define _itkImageBase_txx
#include "itkImageBase.h"

namespace itk
{
  
/**
 *
 */
template<unsigned int VImageDimension>
ImageBase<VImageDimension>
::ImageBase()
{
  memset( m_OffsetTable, 0, (VImageDimension+1)*sizeof(unsigned long) );
}


/**
 *
 */
template<unsigned int VImageDimension>
ImageBase<VImageDimension>
::~ImageBase()
{
  this->Initialize();
}


/**
 *
 */
template<unsigned int VImageDimension>
void 
ImageBase<VImageDimension>
::Initialize()
{
  this->DataObject::Initialize();
}

//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
void
ImageBase<VImageDimension>
::ComputeOffsetTable()
{
  unsigned long num=1;
  const SizeType& bufferSize = m_BufferedRegion.GetSize();
  
  m_OffsetTable[0] = num;
  for (unsigned int i=0; i < VImageDimension; i++)
    {
    num *= bufferSize[i];
    m_OffsetTable[i+1] = num;
    }
}


//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
void 
ImageBase<VImageDimension>
::UpdateOutputInformation()
{
  if (this->GetSource())
    {
    this->GetSource()->UpdateOutputInformation();
    }
  // If we don't have a source, then let's make our Image
  // span our buffer
  else
    {
    m_LargestPossibleRegion = m_BufferedRegion;
    }
  
  // Now we should know what our largest possible region is. If our 
  // requested region was not set yet, (or has been set to something 
  // invalid - with no data in it ) then set it to the largest possible
  // region.
  if ( ! m_RequestedRegionInitialized)
    {
    this->SetRequestedRegionToLargestPossibleRegion();
    m_RequestedRegionInitialized = true;
    }
  
  m_LastRequestedRegionWasOutsideOfTheBufferedRegion = 0;
}


//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
void 
ImageBase<VImageDimension>
::SetRequestedRegionToLargestPossibleRegion()
{
  m_RequestedRegion = m_LargestPossibleRegion;
}

//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
void 
ImageBase<VImageDimension>
::CopyInformation(DataObject *data)
{
  ImageBase *imgData;
  
  imgData = dynamic_cast<ImageBase*>(data);

  if (imgData)
    {
    m_LargestPossibleRegion = imgData->GetLargestPossibleRegion();
    }
  else
    {
    // pointer could not be cast back down
    std::cerr << "itk::ImageBase::CopyInformation() cannot cast "
              << typeid(data).name() << " to "
              << typeid(ImageBase*).name() << std::endl;
    }
}




//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
bool 
ImageBase<VImageDimension>
::RequestedRegionIsOutsideOfTheBufferedRegion()
{
  unsigned int i;
  const IndexType &requestedRegionIndex = m_RequestedRegion.GetIndex();
  const IndexType &bufferedRegionIndex = m_BufferedRegion.GetIndex();

  const SizeType& requestedRegionSize = m_RequestedRegion.GetSize();
  const SizeType& bufferedRegionSize = m_BufferedRegion.GetSize();
  
  for (i=0; i< VImageDimension; i++)
    {
    if ( (requestedRegionIndex[i] < bufferedRegionIndex[i]) ||
         ((requestedRegionIndex[i] + requestedRegionSize[i])
          > (bufferedRegionIndex[i] + bufferedRegionSize[i])) )
      {
      return true;
      }
    }

  return false;
}


//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
bool 
ImageBase<VImageDimension>
::VerifyRequestedRegion()
{
  bool retval = true;
  unsigned int i;

  // Is the region within the image?
  const IndexType &requestedRegionIndex = m_RequestedRegion.GetIndex();
  const IndexType &largestPossibleRegionIndex
    = m_LargestPossibleRegion.GetIndex();

  const SizeType& requestedRegionSize = m_RequestedRegion.GetSize();
  const SizeType& largestPossibleRegionSize
    = m_LargestPossibleRegion.GetSize();
  
  for (i=0; i< VImageDimension; i++)
    {
    if ( (requestedRegionIndex[i] < largestPossibleRegionIndex[i]) ||
         ((requestedRegionIndex[i] + requestedRegionSize[i])
          > (largestPossibleRegionIndex[i]+largestPossibleRegionSize[i])))
      {
      itkErrorMacro( << "Region does not lie within the image" );
      retval = false;
      }
    }

  return retval;
}

//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
void
ImageBase<VImageDimension>
::SetBufferedRegion(const RegionType &region)
{
  if (m_BufferedRegion != region)
    {
    m_BufferedRegion = region;
    this->ComputeOffsetTable();
    this->Modified();
    }
}


//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
void
ImageBase<VImageDimension>
::SetRequestedRegion(const RegionType &region)
{
  if (m_RequestedRegion != region)
    {
    m_RequestedRegion = region;
    }
}

//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
void 
ImageBase<VImageDimension>
::SetRequestedRegion(DataObject *data)
{
  ImageBase *imgData;
  
  imgData = dynamic_cast<ImageBase*>(data);

  if (imgData)
    {
    m_RequestedRegion = imgData->GetRequestedRegion();
    }
  else
    {
    // pointer could not be cast back down
    std::cerr << "itk::ImageBase::SetRequestedRegion(DataObject*) cannot cast "
              << typeid(data).name() << " to "
              << typeid(ImageBase*).name() << std::endl;
    }
}

//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
void
ImageBase<VImageDimension>
::SetLargestPossibleRegion(const RegionType &region)
{
  if (m_LargestPossibleRegion != region)
    {
    m_LargestPossibleRegion = region;
    this->Modified();
    }
}



/**
 *
 */
template<unsigned int VImageDimension>
void 
ImageBase<VImageDimension>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "LargestPossibleRegion: " << m_LargestPossibleRegion
     << std::endl;
  os << indent << "BufferedRegion: " << m_BufferedRegion << std::endl;
  os << indent << "RequestedRegion: " << m_RequestedRegion << std::endl;
}

} // end namespace itk

#endif
