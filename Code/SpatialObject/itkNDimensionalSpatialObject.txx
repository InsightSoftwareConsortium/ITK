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
template <unsigned int PipelineDimension>
NDimensionalSpatialObject<PipelineDimension>
::NDimensionalSpatialObject()
{
  strcpy(m_TypeName,"NDimensionalSpatialObject");
  m_ParentId=-1; // by default
  m_Parent = NULL;
  m_Dimension = 3; // by default
  m_Id = 0;

  for(unsigned int i=0;i<6;i++)
  {
    m_Origin[i] = 0; // by default;
  }
}

/** Destructor */
template <unsigned int PipelineDimension>
NDimensionalSpatialObject<PipelineDimension>
::~NDimensionalSpatialObject()
{
}

/** Get the parent of the spatial object */
template <unsigned int PipelineDimension>
const NDimensionalSpatialObject<PipelineDimension> *
NDimensionalSpatialObject<PipelineDimension>
::GetParent( void ) const
{
  return m_Parent;
}

/** Set the parent of the spatial object */
template <unsigned int PipelineDimension>
void
NDimensionalSpatialObject<PipelineDimension>
::SetParent( const Self * parent )
{
  m_Parent = parent;
}

/** Return true if the spatial object has a parent */
template <unsigned int PipelineDimension>
bool
NDimensionalSpatialObject<PipelineDimension>
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
template <unsigned int PipelineDimension>
void
NDimensionalSpatialObject<PipelineDimension>
::SetLargestPossibleRegion(const RegionType &region)
{
  if (m_LargestPossibleRegion != region)
  {
    m_LargestPossibleRegion = region;
    this->Modified();
  }
}

/** Update the Output information */
template <unsigned int PipelineDimension>
void
NDimensionalSpatialObject<PipelineDimension>
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

template <unsigned int PipelineDimension>
void
NDimensionalSpatialObject<PipelineDimension>
::SetRequestedRegionToLargestPossibleRegion()
{
  m_RequestedRegion = m_LargestPossibleRegion;
  m_RequestedRegionInitialized = true;
}


template <unsigned int PipelineDimension>
void
NDimensionalSpatialObject<PipelineDimension>
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


template <unsigned int PipelineDimension>
bool
NDimensionalSpatialObject<PipelineDimension>
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


template <unsigned int PipelineDimension>
void
NDimensionalSpatialObject<PipelineDimension>
::SetBufferedRegion(const RegionType &region)
{
  if (m_BufferedRegion != region)
  {
    m_BufferedRegion = region;
    this->ComputeOffsetTable();
    this->Modified();
  }
}


template <unsigned int PipelineDimension>
bool
NDimensionalSpatialObject<PipelineDimension>
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

template <unsigned int PipelineDimension>
void
NDimensionalSpatialObject<PipelineDimension>
::SetRequestedRegion(const RegionType &region)
{
  if (m_RequestedRegion != region)
  {
    m_RequestedRegion = region;
    m_RequestedRegionInitialized = true;
    this->Modified();
  }
}


template <unsigned int PipelineDimension>
void
NDimensionalSpatialObject<PipelineDimension>
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


template <unsigned int PipelineDimension>
void
NDimensionalSpatialObject<PipelineDimension>
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


template <unsigned int PipelineDimension>
typename NDimensionalSpatialObject<PipelineDimension>::PropertyType * 
NDimensionalSpatialObject<PipelineDimension>
::GetProperty( void )
{ 
  return m_Property; 
}


template <unsigned int PipelineDimension>
void
NDimensionalSpatialObject<PipelineDimension>
::SetProperty( const PropertyType * property)
{ 
  m_Property = property; 
}

template <unsigned int PipelineDimension>
void
NDimensionalSpatialObject<PipelineDimension>
::SetOrigin(double* origin)
{
  for(unsigned int i=0;i<6;i++)
  {
    m_Origin[i]=origin[i];
  }
}


template <unsigned int PipelineDimension>
void
NDimensionalSpatialObject<PipelineDimension>
::SetOrigin(double x, double y)
{
  m_Origin[0]=x;
  m_Origin[1]=y;
}

template <unsigned int PipelineDimension>
void
NDimensionalSpatialObject<PipelineDimension>
::SetOrigin(double x, double y, double z)
{
  m_Origin[0]=x;
  m_Origin[1]=y;
  m_Origin[2]=z;
}


template <unsigned int PipelineDimension>
void
NDimensionalSpatialObject<PipelineDimension>
::SetOrigin(double x, double y, double z, double t)
{
  m_Origin[0]=x;
  m_Origin[1]=y;
  m_Origin[2]=z;
  m_Origin[3]=t;
}

template <unsigned int PipelineDimension>
void
NDimensionalSpatialObject<PipelineDimension>
::Update(void)
{
  this->Modified();
}

} // end of namespace itk 


#endif // __SpatialNDimensionalObject_txx



