/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageBase.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
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

  unsigned int i;
  for (i=0; i < VImageDimension; i++)
    {
    m_Spacing[i] = 1.0;
    m_Origin[i] = 0.0;
    }
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


//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
void 
ImageBase<VImageDimension>
::SetSpacing(const double spacing[VImageDimension] )
{
  unsigned int i; 
  for (i=0; i<VImageDimension; i++)
    {
    if ( spacing[i] != m_Spacing[i] )
      {
      break;
      }
    } 
  if ( i < VImageDimension ) 
    { 
    this->Modified(); 
    for (i=0; i<VImageDimension; i++)
      {
      m_Spacing[i] = spacing[i];
      }
    } 
}

//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
void 
ImageBase<VImageDimension>
::SetSpacing(const float spacing[VImageDimension] )
{
  unsigned int i; 
  for (i=0; i<VImageDimension; i++)
    {
    if ( (double)spacing[i] != m_Spacing[i] )
      {
      break;
      }
    } 
  if ( i < VImageDimension ) 
    { 
    this->Modified(); 
    for (i=0; i<VImageDimension; i++)
      {
      m_Spacing[i] = spacing[i];
      }
    } 
}



//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
const double *
ImageBase<VImageDimension>
::GetSpacing() const
{
  return m_Spacing;
}




//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
void 
ImageBase<VImageDimension>
::SetOrigin(const double origin[VImageDimension] )
{
  unsigned int i; 
  for (i=0; i<VImageDimension; i++)
    {
    if ( origin[i] != m_Origin[i] )
      {
      break;
      }
    } 
  if ( i < VImageDimension ) 
    { 
    this->Modified(); 
    for (i=0; i<VImageDimension; i++)
      {
      m_Origin[i] = origin[i];
      }
    } 
}


//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
void 
ImageBase<VImageDimension>
::SetOrigin(const float origin[VImageDimension] )
{
  unsigned int i; 
  for (i=0; i<VImageDimension; i++)
    {
    if ( (double)origin[i] != m_Origin[i] )
      {
      break;
      }
    } 
  if ( i < VImageDimension ) 
    { 
    this->Modified(); 
    for (i=0; i<VImageDimension; i++)
      {
      m_Origin[i] = origin[i];
      }
    } 
}


//----------------------------------------------------------------------------
template<unsigned int VImageDimension>
const double *
ImageBase<VImageDimension>
::GetOrigin() const
{
  return m_Origin;
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

//---------------------------------------------------------------------------
template<unsigned int VImageDimension>
ImageBase<VImageDimension>::AffineTransformType
ImageBase<VImageDimension>::
GetIndexToPhysicalTransform()
{
  AffineTransformType::MatrixType matrix;
  AffineTransformType::VectorType offset;
  for (unsigned int i = 0; i < VImageDimension; i++)
    {
    for (unsigned int j = 0; j < VImageDimension; j++)
      {
      matrix[i][j] = 0.0;
      }
    matrix[i][i] = m_Spacing[i];
    offset[i]    = m_Origin [i];
    }

  AffineTransformType result(matrix, offset);
  result.SetMatrix(matrix);
  result.SetOffset(offset);

  return result;
}


//---------------------------------------------------------------------------
template<unsigned int VImageDimension>
ImageBase<VImageDimension>::AffineTransformType
ImageBase<VImageDimension>::
GetPhysicalToIndexTransform()
{
  AffineTransformType::MatrixType matrix;
  AffineTransformType::VectorType offset;

  for (unsigned int i = 0; i < VImageDimension; i++)
    {
    for (unsigned int j = 0; j < VImageDimension; j++)
      {
      matrix[i][j] = 0.0;
      }
    matrix[i][i] = 1.0 / m_Spacing[i];
    offset[i]    = -m_Origin[i] / m_Spacing[i];
    }

  AffineTransformType result(matrix, offset);

  return result;
}



/**
 *
 */
template<unsigned int VImageDimension>
void 
ImageBase<VImageDimension>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);

  os << indent << "LargestPossibleRegion: " << m_LargestPossibleRegion
     << std::endl;
  os << indent << "BufferedRegion: " << m_BufferedRegion << std::endl;
  os << indent << "RequestedRegion: " << m_RequestedRegion << std::endl;
}

} // end namespace itk
