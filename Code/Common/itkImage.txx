/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImage.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkImage.h"
#include "itkProcessObject.h"

namespace itk
{

/**
 *
 */
template<class TPixel, unsigned int VImageDimension, class TPixelContainer>
Image<TPixel, VImageDimension, TPixelContainer>
::Image()
{
  m_Buffer = PixelContainer::New();

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
template<class TPixel, unsigned int VImageDimension, class TPixelContainer>
Image<TPixel, VImageDimension, TPixelContainer>
::~Image()
{
}


//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TPixelContainer>
void 
Image<TPixel, VImageDimension, TPixelContainer>
::Allocate()
{
  unsigned long num;

  this->ComputeOffsetTable();
  num = m_OffsetTable[VImageDimension];
  
  m_Buffer->Reserve(num);
}



//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TPixelContainer>
void
Image<TPixel, VImageDimension, TPixelContainer>
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



/**
 *
 */
template<class TPixel, unsigned int VImageDimension, class TPixelContainer>
void 
Image<TPixel, VImageDimension, TPixelContainer>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "Data: " << m_Buffer << std::endl;
  os << indent << "LargestPossibleRegion: " << m_LargestPossibleRegion
     << std::endl;
  os << indent << "BufferedRegion: " << m_BufferedRegion << std::endl;
  os << indent << "RequestedRegion: " << m_RequestedRegion << std::endl;
}

//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TPixelContainer>
void 
Image<TPixel, VImageDimension, TPixelContainer>
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
template<class TPixel, unsigned int VImageDimension, class TPixelContainer>
void 
Image<TPixel, VImageDimension, TPixelContainer>
::SetRequestedRegionToLargestPossibleRegion()
{
  m_RequestedRegion = m_LargestPossibleRegion;
}

//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TPixelContainer>
void 
Image<TPixel, VImageDimension, TPixelContainer>
::CopyInformation(DataObject *data)
{
  Image *imgData;
  
  try
    {
    imgData = dynamic_cast<Image*>(data);

    m_LargestPossibleRegion = imgData->GetLargestPossibleRegion();
    }
  catch (...)
    {
    return;
    }
}

//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TPixelContainer>
bool 
Image<TPixel, VImageDimension, TPixelContainer>
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
template<class TPixel, unsigned int VImageDimension, class TPixelContainer>
bool 
Image<TPixel, VImageDimension, TPixelContainer>
::VerifyRequestedRegion()
{
  bool retval = true;
  unsigned int i;

  // Is the region within the image?
  const IndexType &requestedRegionIndex = m_RequestedRegion.GetIndex();
  const IndexType &largestPossibleRegionIndex = m_LargestPossibleRegion.GetIndex();

  const SizeType& requestedRegionSize = m_RequestedRegion.GetSize();
  const SizeType& largestPossibleRegionSize = m_LargestPossibleRegion.GetSize();
  
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
template<class TPixel, unsigned int VImageDimension, class TPixelContainer>
void
Image<TPixel, VImageDimension, TPixelContainer>
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
template<class TPixel, unsigned int VImageDimension, class TPixelContainer>
void
Image<TPixel, VImageDimension, TPixelContainer>
::SetRequestedRegion(const RegionType &region)
{
  if (m_RequestedRegion != region)
    {
    m_RequestedRegion = region;
    }
}


//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TPixelContainer>
void
Image<TPixel, VImageDimension, TPixelContainer>
::SetLargestPossibleRegion(const RegionType &region)
{
  if (m_LargestPossibleRegion != region)
    {
    m_LargestPossibleRegion = region;
    this->Modified();
    }
}


//---------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TPixelContainer>
Image<TPixel, VImageDimension, TPixelContainer>::AffineTransformType
Image<TPixel, VImageDimension, TPixelContainer>::
GetIndexToPhysicalTransform()
{
    AffineTransformType::MatrixType matrix;
    AffineTransformType::VectorType offset;
    for (unsigned int i = 0; i < VImageDimension; i++) {
        for (unsigned int j = 0; j < VImageDimension; j++)
            matrix[i][j] = 0.0;
        matrix[i][i] = m_Spacing[i];
        offset[i]    = m_Origin [i];
    }

    AffineTransformType result(matrix, offset);
    result.SetMatrix(matrix);
    result.SetOffset(offset);

    return result;
}


//---------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TPixelContainer>
Image<TPixel, VImageDimension, TPixelContainer>::AffineTransformType
Image<TPixel, VImageDimension, TPixelContainer>::
GetPhysicalToIndexTransform()
{
    AffineTransformType::MatrixType matrix;
    AffineTransformType::VectorType offset;

    for (unsigned int i = 0; i < VImageDimension; i++) {
        for (unsigned int j = 0; j < VImageDimension; j++)
            matrix[i][j] = 0.0;
        matrix[i][i] = 1.0 / m_Spacing[i];
        offset[i]    = -m_Origin[i] / m_Spacing[i];
    }

    AffineTransformType result(matrix, offset);

    return result;
}


} // end namespace itk
