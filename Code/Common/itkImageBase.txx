/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageBase.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageBase_txx
#define _itkImageBase_txx
#include "itkImageBase.h"
#include "itkFastMutexLock.h"

namespace itk
{

/**
 * Helper class that will be used in GetSpacing() for an ImageBase.
 * ImageBase::GetSpacing() is defined to be 1,1,1. For efficiency, we
 * only want to fill a vector once with 1's for each possible dimension.
 */
template<unsigned int VImageDimension>
class StaticSpacing
{
 public:
  static void Initialize()
  {
    static bool initialized = false;
    static SimpleFastMutexLock StaticSpacingCriticalSection;

    // static variable, initialize only once
    StaticSpacingCriticalSection.Lock();
    if (!initialized)
      {
      initialized = true;
      for (unsigned int i=0; i < VImageDimension; i++)
        {
        m_Spacing[i] = 1.0;
        }
      }
    StaticSpacingCriticalSection.Unlock();
  }

  static double *GetSpacing()
  {
    return m_Spacing;
  }
 protected:
  static double m_Spacing[VImageDimension];
};

// Initialize static variable to zero.  It will be set to 1,1,1 by the first
// call to Initialize()
template<unsigned int VImageDimension>
double StaticSpacing<VImageDimension>::m_Spacing[VImageDimension] = {0.0};


  
/**
 *
 */
template<unsigned int VImageDimension>
ImageBase<VImageDimension>
::ImageBase()
{
  memset( m_OffsetTable, 0, (VImageDimension+1)*sizeof(unsigned long) );

  // Call a helper class that builds a static variable of the current dimension
  // in a thread safe manner for use by GetSpacing(). This ImageBase of the
  // prescribe dimension will initialize the variable for all to use.
  StaticSpacing<VImageDimension>::Initialize();
}


/**
 *
 */
template<unsigned int VImageDimension>
void
ImageBase<VImageDimension>
::Initialize()
{
  //
  // We don't modify ourselves because the "ReleaseData" methods depend upon
  // no modification when initialized.
  //

  // Call the superclass which should initialize the BufferedRegion ivar.
  Superclass::Initialize();

  // Clear the offset table
  memset( m_OffsetTable, 0, (VImageDimension+1)*sizeof(unsigned long) );

  // Clear the BufferedRegion ivar
  m_BufferedRegion = RegionType();
}


/**
 *
 */
template<unsigned int VImageDimension>
ImageBase<VImageDimension>
::~ImageBase()
{
}


/**
 *
 */
template<unsigned int VImageDimension>
const double * 
ImageBase<VImageDimension>
::GetSpacing() const
{
  return StaticSpacing<VImageDimension>::GetSpacing();
}


/**
 *
 */
template<unsigned int VImageDimension>
const double * 
ImageBase<VImageDimension>
::GetOrigin() const
{
  // Use a static local variable so the storage for the response is
  // always available
  static const double origin[VImageDimension] = {0.0};

  return origin;
}

//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
void
ImageBase<VImageDimension>
::ComputeOffsetTable()
{
  OffsetValueType num=1;
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
  m_RequestedRegionInitialized = true;
}

//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
void 
ImageBase<VImageDimension>
::CopyInformation(const DataObject *data)
{
  // Standard call to the superclass' method
  Superclass::CopyInformation(data);

  // Attempt to cast data to an ImageBase
  const ImageBase<VImageDimension> *imgData;
  
  imgData = dynamic_cast<const ImageBase<VImageDimension>*>(data);

  if (imgData)
    {
    // Copy the meta data for this data type
    m_LargestPossibleRegion = imgData->GetLargestPossibleRegion();
    }
  else
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::ImageBase::CopyInformation() cannot cast "
                   << typeid(data).name() << " to "
                   << typeid(ImageBase*).name() );
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
         ((requestedRegionIndex[i] + static_cast<OffsetValueType>(requestedRegionSize[i]))
          > (bufferedRegionIndex[i] + static_cast<OffsetValueType>(bufferedRegionSize[i]))) )
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

  // Is the requested region within the LargestPossibleRegion?
  // Note that the test is indeed against the largest possible region
  // rather than the buffered region; see DataObject::VerifyRequestedRegion.
  const IndexType &requestedRegionIndex = m_RequestedRegion.GetIndex();
  const IndexType &largestPossibleRegionIndex
    = m_LargestPossibleRegion.GetIndex();

  const SizeType& requestedRegionSize = m_RequestedRegion.GetSize();
  const SizeType& largestPossibleRegionSize
    = m_LargestPossibleRegion.GetSize();
  
  for (i=0; i< VImageDimension; i++)
    {
    if ( (requestedRegionIndex[i] < largestPossibleRegionIndex[i]) ||
         ((requestedRegionIndex[i] + static_cast<long>(requestedRegionSize[i]))
          > (largestPossibleRegionIndex[i]+static_cast<long>(largestPossibleRegionSize[i]))))
      {
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
    m_RequestedRegionInitialized = true;
    this->Modified();
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
    m_RequestedRegionInitialized = true;
    }
  else
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::ImageBase::SetRequestedRegion(DataObject*) cannot cast " << typeid(data).name() << " to " << typeid(ImageBase*).name() );
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

  os << indent << "LargestPossibleRegion: " << std::endl;
  m_LargestPossibleRegion.PrintSelf(os, indent.GetNextIndent());

  os << indent << "BufferedRegion: " << std::endl;
  m_BufferedRegion.PrintSelf(os, indent.GetNextIndent());

  os << indent << "RequestedRegion: " << std::endl;
  m_RequestedRegion.PrintSelf(os, indent.GetNextIndent());

}

} // end namespace itk

#endif
