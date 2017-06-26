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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkImageBase_hxx
#define itkImageBase_hxx

#include "itkImageBase.h"

#include "itkFastMutexLock.h"
#include "itkProcessObject.h"
#include "itkSpatialOrientation.h"
#include <cstring>
#include "itkMath.h"

namespace itk
{

template< unsigned int VImageDimension >
ImageBase< VImageDimension >
::ImageBase()
{
  memset(m_OffsetTable, 0, sizeof(m_OffsetTable));
  m_Spacing.Fill(1.0);
  m_Origin.Fill(0.0);
  m_Direction.SetIdentity();
  m_InverseDirection.SetIdentity();
  m_IndexToPhysicalPoint.SetIdentity();
  m_PhysicalPointToIndex.SetIdentity();
}


template< unsigned int VImageDimension >
void
ImageBase< VImageDimension >
::Allocate(bool)
{
}


template< unsigned int VImageDimension >
void
ImageBase< VImageDimension >
::Initialize()
{
  //
  // We don't modify ourselves because the "ReleaseData" methods depend upon
  // no modification when initialized. Otherwise BUG: 8490 will
  // reoccur.
  //
  // DO NOT CALL ANY METHODS WHICH MODIFY OURSELVES

  // Call the superclass which should initialize the BufferedRegion ivar.
  Superclass::Initialize();

  // Clear the offset table
  memset(m_OffsetTable, 0, sizeof(m_OffsetTable));

  // Clear the BufferedRegion ivar
  this->InitializeBufferedRegion();
}


template< unsigned int VImageDimension >
ImageBase< VImageDimension >
::~ImageBase()
{}


template< unsigned int VImageDimension >
void
ImageBase< VImageDimension >
::SetSpacing(const SpacingType & spacing)
{
  for ( unsigned int i = 0; i < VImageDimension; i++ )
    {
    if ( this->m_Spacing[i] < 0.0 )
      {
#if  defined(ITK_FUTURE_LEGACY_REMOVE)
      itkExceptionMacro("Negative spacing is not allowed: Spacing is " << this->m_Spacing);
#else
      itkWarningMacro("Negative spacing is not supported and may result in undefined behavior. Spacing is " << this->m_Spacing);
      break;
#endif
      }
    }

  itkDebugMacro("setting Spacing to " << spacing);
  if ( this->m_Spacing != spacing )
    {
    this->m_Spacing = spacing;
    this->ComputeIndexToPhysicalPointMatrices();
    this->Modified();
    }
}


template< unsigned int VImageDimension >
void
ImageBase< VImageDimension >
::SetSpacing(const double spacing[VImageDimension])
{
  this->InternalSetSpacing(spacing);
}


template< unsigned int VImageDimension >
void
ImageBase< VImageDimension >
::SetSpacing(const float spacing[VImageDimension])
{
  this->InternalSetSpacing(spacing);
}


template< unsigned int VImageDimension >
void
ImageBase< VImageDimension >
::SetOrigin(const double origin[VImageDimension])
{
  PointType p(origin);

  this->SetOrigin(p);
}


template< unsigned int VImageDimension >
void
ImageBase< VImageDimension >
::SetOrigin(const float origin[VImageDimension])
{
  Point< float, VImageDimension > of(origin);
  PointType                       p;
  p.CastFrom(of);
  this->SetOrigin(p);
}


template< unsigned int VImageDimension >
void
ImageBase< VImageDimension >
::SetDirection(const DirectionType & direction)
{
  bool modified = false;

  for ( unsigned int r = 0; r < VImageDimension; r++ )
    {
    for ( unsigned int c = 0; c < VImageDimension; c++ )
      {
      if ( Math::NotExactlyEquals(m_Direction[r][c], direction[r][c]) )
        {
        m_Direction[r][c] = direction[r][c];
        modified = true;
        }
      }
    }

  if ( modified )
    {
    this->ComputeIndexToPhysicalPointMatrices();
    this->m_InverseDirection = m_Direction.GetInverse();
    }
}


template< unsigned int VImageDimension >
void
ImageBase< VImageDimension >
::ComputeIndexToPhysicalPointMatrices()
{
  DirectionType scale;

  for ( unsigned int i = 0; i < VImageDimension; i++ )
    {
    if ( this->m_Spacing[i] == 0.0 )
      {
      itkExceptionMacro("A spacing of 0 is not allowed: Spacing is " << this->m_Spacing);
      }
    scale[i][i] = this->m_Spacing[i];
    }

  if ( vnl_determinant( this->m_Direction.GetVnlMatrix() ) == 0.0 )
    {
    itkExceptionMacro(<< "Bad direction, determinant is 0. Direction is " << this->m_Direction);
    }

  this->m_IndexToPhysicalPoint = this->m_Direction * scale;
  this->m_PhysicalPointToIndex = m_IndexToPhysicalPoint.GetInverse();

  this->Modified();
}


template< unsigned int VImageDimension >
void
ImageBase< VImageDimension >
::ComputeOffsetTable()
{
  // vxl_uint_64 num=1;
  OffsetValueType  num = 1;
  const SizeType & bufferSize = this->GetBufferedRegion().GetSize();

  // m_OffsetTable[0] = (OffsetValueType)num;
  m_OffsetTable[0] = num;
  for ( unsigned int i = 0; i < VImageDimension; i++ )
    {
    num *= bufferSize[i];
    // m_OffsetTable[i+1] = (OffsetValueType)num;
    m_OffsetTable[i + 1] = num;
    }
  // if( num > NumericTraits<SizeValueType>::max() )
  //   {
  //   itkExceptionMacro(<< "Requested number of pixels (" << num
  //     << ") is greater than the largest possible number of pixels (" <<
  // NumericTraits<SizeValueType>::max() << ").");
  //   }
}


template< unsigned int VImageDimension >
void
ImageBase< VImageDimension >
::UpdateOutputInformation()
{
  if ( this->GetSource() )
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
    if ( this->GetBufferedRegion().GetNumberOfPixels() > 0 )
      {
      this->SetLargestPossibleRegion( this->GetBufferedRegion() );
      }
    }

  // Now we should know what our largest possible region is. If our
  // requested region was not set yet, (or has been set to something
  // invalid - with no data in it ) then set it to the largest possible
  // region.
  if ( this->GetRequestedRegion().GetNumberOfPixels() == 0 )
    {
    this->SetRequestedRegionToLargestPossibleRegion();
    }
}


template< unsigned int VImageDimension >
void
ImageBase< VImageDimension >
::UpdateOutputData()
{
  // If the requested region does not contain any pixels then there is
  // no reason to Update the output data. This is needed so that
  // filters don't need to update all inputs. This occours in
  // ImageBase as  oppose to DataObject, but cause this statement
  // requires the specific GetNumberOfPixels methods ( as oppose to a
  // generic Region::IsEmpty method ).
  //
  // Also note, the check of the largest possible region is needed so
  // that an exception will be thrown in the process object when no
  // input has been set. ( This part of the statement could be removed
  // if this check happened earlier in the pipeline )
  if ( this->GetRequestedRegion().GetNumberOfPixels() > 0
       || this->GetLargestPossibleRegion().GetNumberOfPixels() == 0 )
    {
    this->Superclass::UpdateOutputData();
    }
}


template< unsigned int VImageDimension >
void
ImageBase< VImageDimension >
::SetRequestedRegionToLargestPossibleRegion()
{
  this->SetRequestedRegion( this->GetLargestPossibleRegion() );
}


template< unsigned int VImageDimension >
void
ImageBase< VImageDimension >
::CopyInformation(const DataObject *data)
{
  // Standard call to the superclass' method
  Superclass::CopyInformation(data);

  if ( data )
    {
    // Attempt to cast data to an ImageBase
    const ImageBase< VImageDimension > * const imgData = dynamic_cast< const ImageBase< VImageDimension > * >( data );

    if ( imgData != ITK_NULLPTR )
      {
      // Copy the meta data for this data type
      this->SetLargestPossibleRegion( imgData->GetLargestPossibleRegion() );
      this->SetSpacing( imgData->GetSpacing() );
      this->SetOrigin( imgData->GetOrigin() );
      this->SetDirection( imgData->GetDirection() );
      this->SetNumberOfComponentsPerPixel(
        imgData->GetNumberOfComponentsPerPixel() );
      }
    else
      {
      // pointer could not be cast back down
      itkExceptionMacro( << "itk::ImageBase::CopyInformation() cannot cast "
                         << typeid( data ).name() << " to "
                         << typeid( const ImageBase * ).name() );
      }
    }
}


template< unsigned int VImageDimension >
void
ImageBase< VImageDimension >
::Graft(const Self *image)
{
  if ( !image )
    {
    return;
    }

  // Copy the meta-information
  this->CopyInformation(image);

  // Copy the remaining region information. Subclasses are
  // responsible for copying the pixel container.
  this->SetBufferedRegion( image->GetBufferedRegion() );
  this->SetRequestedRegion( image->GetRequestedRegion() );
}


template< unsigned int VImageDimension >
void
ImageBase< VImageDimension >
::Graft(const DataObject *data)
{
  typedef ImageBase< VImageDimension > ImageBaseType;

  const ImageBaseType *image = dynamic_cast< const ImageBaseType * >( data );

  if ( !image )
    {
    return;
    }

  // Call Graft() with image input to actually perform the graft operation
  this->Graft(image);
}


template< unsigned int VImageDimension >
bool
ImageBase< VImageDimension >
::RequestedRegionIsOutsideOfTheBufferedRegion()
{
  unsigned int      i;
  const IndexType & requestedRegionIndex = this->GetRequestedRegion().GetIndex();
  const IndexType & bufferedRegionIndex = this->GetBufferedRegion().GetIndex();

  const SizeType & requestedRegionSize = this->GetRequestedRegion().GetSize();
  const SizeType & bufferedRegionSize = this->GetBufferedRegion().GetSize();

  for ( i = 0; i < VImageDimension; i++ )
    {
    if ( ( requestedRegionIndex[i] < bufferedRegionIndex[i] )
         || ( ( requestedRegionIndex[i] + static_cast< OffsetValueType >( requestedRegionSize[i] ) )
              > ( bufferedRegionIndex[i] + static_cast< OffsetValueType >( bufferedRegionSize[i] ) ) ) )
      {
      return true;
      }
    }

  return false;
}


template< unsigned int VImageDimension >
bool
ImageBase< VImageDimension >
::VerifyRequestedRegion()
{
  bool         retval = true;
  unsigned int i;

  // Is the requested region within the LargestPossibleRegion?
  // Note that the test is indeed against the largest possible region
  // rather than the buffered region; see DataObject::VerifyRequestedRegion.
  const IndexType & requestedRegionIndex = this->GetRequestedRegion().GetIndex();
  const IndexType & largestPossibleRegionIndex =
    this->GetLargestPossibleRegion().GetIndex();

  const SizeType & requestedRegionSize = this->GetRequestedRegion().GetSize();
  const SizeType & largestPossibleRegionSize =
    this->GetLargestPossibleRegion().GetSize();

  for ( i = 0; i < VImageDimension; i++ )
    {
    if ( ( requestedRegionIndex[i] < largestPossibleRegionIndex[i] )
         || ( ( requestedRegionIndex[i] + static_cast< OffsetValueType >( requestedRegionSize[i] ) )
              > ( largestPossibleRegionIndex[i] + static_cast< OffsetValueType >( largestPossibleRegionSize[i] ) ) ) )
      {
      retval = false;
      }
    }

  return retval;
}


template< unsigned int VImageDimension >
void
ImageBase< VImageDimension >
::SetBufferedRegion(const RegionType & region)
{
  if ( m_BufferedRegion != region )
    {
    m_BufferedRegion = region;
    this->ComputeOffsetTable();
    this->Modified();
    }
}


template< unsigned int VImageDimension >
void
ImageBase< VImageDimension >
::InitializeBufferedRegion(void)
{
  //
  // We don't modify ourselves because the "ReleaseData" methods depend upon
  // no modification when initialized.
  //
  // Otherwise BUG: 8490 will reoccur

  m_BufferedRegion = RegionType();
  this->ComputeOffsetTable();
}


template< unsigned int VImageDimension >
void
ImageBase< VImageDimension >
::SetRequestedRegion(const RegionType & region)
{
  if ( m_RequestedRegion != region )
    {
    m_RequestedRegion = region;
    }
}


template< unsigned int VImageDimension >
void
ImageBase< VImageDimension >
::SetRequestedRegion( const DataObject *data )
{
  const ImageBase * const imgData = dynamic_cast< const ImageBase * >( data );

  if ( imgData != ITK_NULLPTR )
    {
    // only copy the RequestedRegion if the parameter object is an image
    this->SetRequestedRegion( imgData->GetRequestedRegion() );
    }
}


template< unsigned int VImageDimension >
void
ImageBase< VImageDimension >
::SetLargestPossibleRegion(const RegionType & region)
{
  if ( m_LargestPossibleRegion != region )
    {
    m_LargestPossibleRegion = region;
    this->Modified();
    }
}


template< unsigned int VImageDimension >
unsigned int
ImageBase< VImageDimension >
::GetNumberOfComponentsPerPixel() const
{
  // Returns the number of components in the image.
  // base implementation defaults to 1
  return 1;
}


template< unsigned int VImageDimension >
void
ImageBase< VImageDimension >
::SetNumberOfComponentsPerPixel(unsigned int)
{
  // does nothing (always 1)
}


template< unsigned int VImageDimension >
void
ImageBase< VImageDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "LargestPossibleRegion: " << std::endl;
  this->GetLargestPossibleRegion().PrintSelf( os, indent.GetNextIndent() );

  os << indent << "BufferedRegion: " << std::endl;
  this->GetBufferedRegion().PrintSelf( os, indent.GetNextIndent() );

  os << indent << "RequestedRegion: " << std::endl;
  this->GetRequestedRegion().PrintSelf( os, indent.GetNextIndent() );

  os << indent << "Spacing: " << this->GetSpacing() << std::endl;

  os << indent << "Origin: " << this->GetOrigin() << std::endl; \

  os << indent << "Direction: " << std::endl << this->GetDirection() << std::endl;

  os << indent << "IndexToPointMatrix: " << std::endl;
  os << this->m_IndexToPhysicalPoint << std::endl;

  os << indent << "PointToIndexMatrix: " << std::endl;
  os << this->m_PhysicalPointToIndex << std::endl;

  os << indent << "Inverse Direction: " << std::endl;
  os << this->GetInverseDirection() << std::endl;
}

} // end namespace itk

#endif
