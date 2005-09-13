/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageBase.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
#include "itkSpatialOrientation.h"

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
  m_Spacing.Fill(1.0);
  m_Origin.Fill(0.0);
  m_Direction.SetIdentity();
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



//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
void 
ImageBase<VImageDimension>
::SetSpacing(const double spacing[VImageDimension] )
{
  SpacingType s(spacing);
  this->SetSpacing(s);
}


//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
void 
ImageBase<VImageDimension>
::SetSpacing(const float spacing[VImageDimension] )
{
  Vector<float, VImageDimension> sf(spacing);
  SpacingType s;
  s.CastFrom( sf );
  this->SetSpacing(s);
}

//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
void 
ImageBase<VImageDimension>
::SetOrigin(const double origin[VImageDimension] )
{
  PointType p(origin);
  this->SetOrigin( p );
}


//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
void 
ImageBase<VImageDimension>
::SetOrigin(const float origin[VImageDimension] )
{
  Point<float, VImageDimension> of(origin);
  PointType p;
  p.CastFrom( of );
  this->SetOrigin( p );
}

//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
void 
ImageBase<VImageDimension>
::SetDirection(const DirectionType direction )
{
  bool modified = false;
  for (unsigned int r = 0; r < VImageDimension; r++)
    {
    for (unsigned int c = 0; c < VImageDimension; c++)
      {
      if (m_Direction[r][c] != direction[r][c])
        {
        m_Direction[r][c] = direction[r][c];
        modified = true;
        }
      }
    }
  if (modified)
    {
    this->Modified();
    }
}

//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
void
ImageBase<VImageDimension>
::ComputeOffsetTable()
{
  OffsetValueType num=1;
  const SizeType& bufferSize = this->GetBufferedRegion().GetSize();
  
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
  else
    {
    // If we don't have a source, we should set our Image to span our
    // buffer (by setting our LargestPossibleRegion to equal our
    // BufferedRegion). However, if the buffer is empty, we leave the
    // LargestPossibleRegion at its prior value.  This allows InPlace
    // filters to overwrite their inputs safely (taking ownership of
    // the pixel buffers), yet respond to subsequent requests for
    // information.
    if (m_BufferedRegion.GetNumberOfPixels() > 0)
      {
      m_LargestPossibleRegion = m_BufferedRegion;
      }
    }
  
  // Now we should know what our largest possible region is. If our 
  // requested region was not set yet, (or has been set to something 
  // invalid - with no data in it ) then set it to the largest possible
  // region.
  if ( m_RequestedRegion.GetNumberOfPixels() == 0)
    {
    this->SetRequestedRegionToLargestPossibleRegion();
    }
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
::CopyInformation(const DataObject *data)
{
  // Standard call to the superclass' method
  Superclass::CopyInformation(data);

  if (data)
    {
    // Attempt to cast data to an ImageBase
    const ImageBase<VImageDimension> *imgData;
  
    imgData = dynamic_cast<const ImageBase<VImageDimension>*>(data);

    if (imgData)
      {
      // Copy the meta data for this data type
      m_LargestPossibleRegion = imgData->GetLargestPossibleRegion();
      m_Spacing = imgData->m_Spacing;
      m_Origin = imgData->m_Origin;
      this->SetDirection(imgData->m_Direction);
      }
    else
      {
      // pointer could not be cast back down
      itkExceptionMacro( << "itk::ImageBase::CopyInformation() cannot cast "
                         << typeid(data).name() << " to "
                         << typeid(const ImageBase*).name() );
      }
    }
}

//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
void 
ImageBase<VImageDimension>
::Graft(const ImageBase<VImageDimension> *data)
{
  // Copy the meta-information
  this->CopyInformation(data);

  if (data)
    {
    // Copy the remaining region information. Subclasses are
    // responsible for copying the pixel container.
    this->SetBufferedRegion( data->GetBufferedRegion() );
    this->SetRequestedRegion( data->GetRequestedRegion() );
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
    // only copy the RequestedRegion if the parameter object is an image
    m_RequestedRegion = imgData->GetRequestedRegion();
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

  os << indent << "Spacing: " << m_Spacing << std::endl;

  os << indent << "Origin: " << m_Origin << std::endl;\

  os << indent << "Direction: " << std::endl << m_Direction << std::endl;
}

} // end namespace itk

#endif
