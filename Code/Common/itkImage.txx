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
template<class TPixel, unsigned int VImageDimension>
Image<TPixel, VImageDimension>
::Image()
{
  m_Buffer = 0;
  Index nullIndex = {0
  };
  
  m_BufferStartIndex = nullIndex;
  m_RegionStartIndex = nullIndex;
  
  memset( m_ImageSize, 0, VImageDimension*sizeof(unsigned long) );
  memset( m_BufferSize, 0, VImageDimension*sizeof(unsigned long) );
  memset( m_RegionSize, 0, VImageDimension*sizeof(unsigned long) );

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
template<class TPixel, unsigned int VImageDimension>
Image<TPixel, VImageDimension>
::~Image()
{
  if (m_Buffer != 0)
    {
    delete m_Buffer;
    m_Buffer = 0;
    }
}


//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension>
void 
Image<TPixel, VImageDimension>
::Allocate()
{
  unsigned long num=1;
  
  m_OffsetTable[0] = num;
  for (unsigned int i=0; i < VImageDimension; i++)
    {
    num *= m_BufferSize[i];
    m_OffsetTable[i+1] = num;
    }

  
  if (m_Buffer == 0)
    { 
    m_Buffer = new std::valarray<TPixel>(num);
    }
  else
    {
    m_Buffer->resize(num);
    }
}



/**
 *
 */
template<class TPixel, unsigned int VImageDimension>
void 
Image<TPixel, VImageDimension>
::PrintSelf(std::ostream& os, Indent indent)
{
  ImageBase::PrintSelf(os,indent);
  
  os << indent << "Data: " << m_Buffer << std::endl;
}

//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension>
void 
Image<TPixel, VImageDimension>
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
    m_ImageStartIndex = m_BufferStartIndex;
    memcpy( m_ImageSize, m_BufferSize, VImageDimension*sizeof(unsigned long) );
    }
  
  // Now we should know what our whole extent is. If our update extent
  // was not set yet, (or has been set to something invalid - with no 
  // data in it ) then set it to the whole extent.
  if ( ! m_UpdateExtentInitialized)
    {
    this->SetUpdateExtentToWholeExtent();
    m_UpdateExtentInitialized = true;
    }
  
  m_LastUpdateExtentWasOutsideOfTheExtent = 0;
}

//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension>
void 
Image<TPixel, VImageDimension>
::SetUpdateExtentToWholeExtent()
{
  m_RegionStartIndex = m_ImageStartIndex;
  memcpy( m_RegionSize, m_ImageSize, VImageDimension*sizeof(unsigned long) );
}

//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension>
void 
Image<TPixel, VImageDimension>
::CopyInformation(DataObject *data)
{
  Image *imgData;
  
  try
    {
    imgData = dynamic_cast<Image*>(data);

    m_ImageStartIndex = imgData->GetImageStartIndex();
    memcpy( m_ImageSize, imgData->GetImageSize(), VImageDimension*sizeof(unsigned long) );
    }
  catch (...)
    {
    return;
    }
}

//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension>
bool 
Image<TPixel, VImageDimension>
::UpdateExtentIsOutsideOfTheExtent()
{
  unsigned int i;

  for (i=0; i< VImageDimension; i++)
    {
    if ( (m_RegionStartIndex[i] < m_BufferStartIndex[i]) ||
         ((m_RegionStartIndex[i] + m_RegionSize[i]) > (m_BufferStartIndex[i] + m_BufferSize[i])) )
      {
      return true;
      }
    }

  return false;
}

//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension>
bool 
Image<TPixel, VImageDimension>
::VerifyUpdateRegion()
{
  bool retval = true;
  unsigned int i;

  // Is the region within the image?
  for (i=0; i< VImageDimension; i++)
    {
    if ( (m_RegionStartIndex[i] < m_ImageStartIndex[i]) ||
         ((m_RegionStartIndex[i] + m_RegionSize[i]) > (m_ImageStartIndex[i] + m_ImageSize[i])) )
      {
      itkErrorMacro( << "Region does not lie within the image" );
      retval = false;
      }
    }

  return retval;
}

} // end namespace itk
