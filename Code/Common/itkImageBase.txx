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
#include "itkProcessObject.h"
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
  this->SetBufferedRegion( RegionType() );
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
  if( this->GetSource() )
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
    if( this->GetBufferedRegion().GetNumberOfPixels() > 0)
      {
      this->SetLargestPossibleRegion( this->GetBufferedRegion() );
      }
    }
  
  // Now we should know what our largest possible region is. If our 
  // requested region was not set yet, (or has been set to something 
  // invalid - with no data in it ) then set it to the largest possible
  // region.
  if ( this->GetRequestedRegion().GetNumberOfPixels() == 0)
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
  this->SetRequestedRegion( this->GetLargestPossibleRegion() );
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
  
     try
       {
       imgData = dynamic_cast<const ImageBase<VImageDimension>*>(data);
       }
     catch( ... )
       {
       return;
       }

    if( imgData )
      {
      // Copy the meta data for this data type
      this->SetLargestPossibleRegion( imgData->GetLargestPossibleRegion() );
      this->SetSpacing( imgData->GetSpacing() );
      this->SetOrigin( imgData->GetOrigin() );
      this->SetDirection(imgData->GetDirection() );
      this->SetNumberOfComponentsPerPixel( 
          imgData->GetNumberOfComponentsPerPixel() );
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
::Graft(const DataObject *data)
{
  typedef ImageBase<VImageDimension>  ImageBaseType;

  const ImageBaseType * image;
  
  try
    {
    image = dynamic_cast< const ImageBaseType * >( data );
    }
  catch( ... )
    {
    return;
    }

  if( !image )
    {
    return;
    }

  // Copy the meta-information
  this->CopyInformation( image );

  // Copy the remaining region information. Subclasses are
  // responsible for copying the pixel container.
  this->SetBufferedRegion( image->GetBufferedRegion() );
  this->SetRequestedRegion( image->GetRequestedRegion() );

}



//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
bool 
ImageBase<VImageDimension>
::RequestedRegionIsOutsideOfTheBufferedRegion()
{
  unsigned int i;
  const IndexType &requestedRegionIndex = this->GetRequestedRegion().GetIndex();
  const IndexType &bufferedRegionIndex = this->GetBufferedRegion().GetIndex();

  const SizeType& requestedRegionSize = this->GetRequestedRegion().GetSize();
  const SizeType& bufferedRegionSize = this->GetBufferedRegion().GetSize();
  
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
  const IndexType &requestedRegionIndex = this->GetRequestedRegion().GetIndex();
  const IndexType &largestPossibleRegionIndex
    = this->GetLargestPossibleRegion().GetIndex();

  const SizeType& requestedRegionSize = this->GetRequestedRegion().GetSize();
  const SizeType& largestPossibleRegionSize
    = this->GetLargestPossibleRegion().GetSize();
  
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
    this->SetRequestedRegion( imgData->GetRequestedRegion() );
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

//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
unsigned int 
ImageBase<VImageDimension>
::GetNumberOfComponentsPerPixel() const
{ 
  // Returns the number of components in the image. Note that for most images
  // this is 1. Even for Image< RGBPixel< T >, 3 >.
  // This is > 1 only for time-series images such as itk::VectorImage. 
  return 1;
}

//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
void
ImageBase<VImageDimension>
::SetNumberOfComponentsPerPixel( unsigned int )
{ // does nothing (always 1 )
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
  this->GetLargestPossibleRegion().PrintSelf(os, indent.GetNextIndent());

  os << indent << "BufferedRegion: " << std::endl;
  this->GetBufferedRegion().PrintSelf(os, indent.GetNextIndent());

  os << indent << "RequestedRegion: " << std::endl;
  this->GetRequestedRegion().PrintSelf(os, indent.GetNextIndent());

  os << indent << "Spacing: " << this->GetSpacing() << std::endl;

  os << indent << "Origin: " << this->GetOrigin() << std::endl;\

  os << indent << "Direction: " << std::endl << this->GetDirection() << std::endl;
}

} // end namespace itk

#endif
