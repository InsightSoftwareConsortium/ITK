/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNDimensionalSpatialObject.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __NDimensionalSpatialObject_txx
#define __NDimensionalSpatialObject_txx

#include "itkNDimensionalSpatialObject.h"

namespace itk
{

/** Constructor */
template <unsigned int SpaceDimension>
NDimensionalSpatialObject<SpaceDimension>
::NDimensionalSpatialObject()
{
  strcpy(m_TypeName,"NDimensionalSpatialObject");
  m_ParentId=-1; // by default
  m_Parent = NULL;
  m_Dimension = 3; // by default
  m_Id = 0;
}

/** Destructor */
template <unsigned int SpaceDimension>
NDimensionalSpatialObject<SpaceDimension>
::~NDimensionalSpatialObject()
{
}

/** Get the parent of the spatial object */
template <unsigned int SpaceDimension>
const NDimensionalSpatialObject<SpaceDimension> *
NDimensionalSpatialObject<SpaceDimension>
::GetParent( void ) const
{
  return m_Parent;
}

/** Set the parent of the spatial object */
template <unsigned int SpaceDimension>
void
NDimensionalSpatialObject<SpaceDimension>
::SetParent( const Self * parent )
{
  m_Parent = parent;
}

/** Return true if the spatial object has a parent */
template <unsigned int SpaceDimension>
bool
NDimensionalSpatialObject<SpaceDimension>
::HasParent( void ) const
{
  if( m_Parent )
  {
    return true;
  }
  else
  {
    return false;
  }
}

/** Set the largest possible region */
template <unsigned int SpaceDimension>
void
NDimensionalSpatialObject<SpaceDimension>
::SetLargestPossibleRegion(const RegionType &region)
{
  if (m_LargestPossibleRegion != region)
  {
    m_LargestPossibleRegion = region;
    this->Modified();
  }
}

/** Update the Output information */
template <unsigned int SpaceDimension>
void
NDimensionalSpatialObject<SpaceDimension>
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

template <unsigned int SpaceDimension>
void
NDimensionalSpatialObject<SpaceDimension>
::SetRequestedRegionToLargestPossibleRegion()
{
  m_RequestedRegion = m_LargestPossibleRegion;
  m_RequestedRegionInitialized = true;
}


template <unsigned int SpaceDimension>
void
NDimensionalSpatialObject<SpaceDimension>
::CopyInformation(const DataObject *data)
{
  // Standard call to the superclass' method
  Superclass::CopyInformation(data);

  // Attempt to cast data to an ImageBase
  const NDimensionalSpatialObject *imgData;
  
  imgData = dynamic_cast<const NDimensionalSpatialObject*>(data);

  if (imgData)
    {
    // Copy the meta data for this data type
    m_LargestPossibleRegion = imgData->GetLargestPossibleRegion();
    }
  else
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::NDimensionalSpatialObject::CopyInformation() cannot cast "
                   << typeid(data).name() << " to "
                   << typeid(NDimensionalSpatialObject*).name() );
    }
}


template <unsigned int SpaceDimension>
bool
NDimensionalSpatialObject<SpaceDimension>
::RequestedRegionIsOutsideOfTheBufferedRegion()
{
  unsigned int i;
  const IndexType &requestedRegionIndex = m_RequestedRegion.GetIndex();
  const IndexType &bufferedRegionIndex = m_BufferedRegion.GetIndex();

  const SizeType& requestedRegionSize = m_RequestedRegion.GetSize();
  const SizeType& bufferedRegionSize = m_BufferedRegion.GetSize();
  
  for (i=0; i< m_Dimension; i++)
  {
    if ( (requestedRegionIndex[i] < bufferedRegionIndex[i]) ||
         ((requestedRegionIndex[i] + static_cast<long>(requestedRegionSize[i]))
          > (bufferedRegionIndex[i] + static_cast<long>(bufferedRegionSize[i]))) )
    {
      return true;
    }
  }
  return false;
}


template <unsigned int SpaceDimension>
void
NDimensionalSpatialObject<SpaceDimension>
::SetBufferedRegion(const RegionType &region)
{
  if (m_BufferedRegion != region)
  {
    m_BufferedRegion = region;
    this->ComputeOffsetTable();
    this->Modified();
  }
}


template <unsigned int SpaceDimension>
bool
NDimensionalSpatialObject<SpaceDimension>
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
  
  for (i=0; i< m_Dimension; i++)
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

template <unsigned int SpaceDimension>
void
NDimensionalSpatialObject<SpaceDimension>
::SetRequestedRegion(const RegionType &region)
{
  if (m_RequestedRegion != region)
  {
    m_RequestedRegion = region;
    m_RequestedRegionInitialized = true;
    this->Modified();
  }
}


template <unsigned int SpaceDimension>
void
NDimensionalSpatialObject<SpaceDimension>
::SetRequestedRegion(DataObject *data)
{
  NDimensionalSpatialObject *imgData;
  
  imgData = dynamic_cast<NDimensionalSpatialObject*>(data);

  if (imgData)
  {
    m_RequestedRegion = imgData->GetRequestedRegion();
    m_RequestedRegionInitialized = true;
  }
  else
  {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::ImageBase::SetRequestedRegion(DataObject*) cannot cast " << typeid(data).name() << " to " << typeid(NDimensionalSpatialObject*).name() );
  }
}


template <unsigned int SpaceDimension>
void
NDimensionalSpatialObject<SpaceDimension>
::ComputeOffsetTable()
{
  double num=1;
  const SizeType& bufferSize = m_BufferedRegion.GetSize();
  
  m_OffsetTable[0] = num;
  for (unsigned int i=0; i < m_Dimension; i++)
  {
    num *= bufferSize[i];
    m_OffsetTable[i+1] = num;
  }
}


template <unsigned int SpaceDimension>
typename NDimensionalSpatialObject<SpaceDimension>::PropertyType * 
NDimensionalSpatialObject<SpaceDimension>
::GetProperty( void )
{ 
  return m_Property; 
}


template <unsigned int SpaceDimension>
void
NDimensionalSpatialObject<SpaceDimension>
::SetProperty( const PropertyType * property)
{ 
  m_Property = property; 
}

template <unsigned int SpaceDimension>
void
NDimensionalSpatialObject<SpaceDimension>
::Update(void)
{
  this->Modified();
}

} // end of namespace itk 


#endif // __SpatialNDimensionalObject_txx



