/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointSet.txx
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
#ifndef _itkPointSet_txx
#define _itkPointSet_txx

#include "itkPointSet.h"
#include "itkProcessObject.h"
#include <algorithm>

namespace itk
{
  
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
PointSet<TPixelType, VDimension, TMeshTraits>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Number Of Points: " 
     << this->GetNumberOfPoints()  << std::endl;

  os << indent << "Requested Number Of Regions: " 
  << m_RequestedNumberOfRegions << std::endl;
  os << indent << "Requested Region: " << m_RequestedRegion << std::endl;
  os << indent << "Maximum Number Of Regions: " 
     << m_MaximumNumberOfRegions << std::endl;

}


/**
 * Access routine to set the points container.
 */
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
PointSet<TPixelType, VDimension, TMeshTraits>
::SetPoints(PointsContainer* points)
{
  itkDebugMacro("setting Points container to " << points);
  if(m_PointsContainer != points)
    {
    m_PointsContainer = points;
    this->Modified();
    }
}



/**
 * Access routine to get the points container.
 */
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
typename PointSet<TPixelType, VDimension, TMeshTraits>::PointsContainer *
PointSet<TPixelType, VDimension, TMeshTraits>
::GetPoints(void)
{
  itkDebugMacro("returning Points container of " << m_PointsContainer );
  if( !m_PointsContainer )
    {
    this->SetPoints(PointsContainer::New());
    }
  return m_PointsContainer;
}


/**
 * Access routine to get the points container.
 */
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
const typename PointSet<TPixelType, VDimension, TMeshTraits>::PointsContainer *
PointSet<TPixelType, VDimension, TMeshTraits>
::GetPoints(void) const
{
  itkDebugMacro("returning Points container of " << m_PointsContainer );
  return m_PointsContainer.GetPointer();
}


/**
 * Access routine to set the point data container.
 */
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
PointSet<TPixelType, VDimension, TMeshTraits>
::SetPointData(PointDataContainer* pointData)
{
  itkDebugMacro("setting PointData container to " << pointData);
  if(m_PointDataContainer != pointData)
    {
    m_PointDataContainer = pointData;
    this->Modified();
    }
}


/**
 * Access routine to get the point data container.
 */
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
typename PointSet<TPixelType, VDimension, TMeshTraits>::PointDataContainer *
PointSet<TPixelType, VDimension, TMeshTraits>
::GetPointData(void)
{
  itkDebugMacro("returning PointData container of "
                << m_PointDataContainer );
  return m_PointDataContainer;
}



/**
 * Access routine to get the point data container.
 */
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
const typename PointSet<TPixelType, VDimension, TMeshTraits>::PointDataContainer *
PointSet<TPixelType, VDimension, TMeshTraits>
::GetPointData(void) const
{
  itkDebugMacro("returning PointData container of "
                << m_PointDataContainer );
  return m_PointDataContainer.GetPointer();
}


/**
 * Assign a point to a point identifier.  If a spot for the point identifier
 * does not exist, it will be created automatically.
 */
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
PointSet<TPixelType, VDimension, TMeshTraits>
::SetPoint(PointIdentifier ptId, PointType point)
{
  /**
   * Make sure a points container exists.
   */
  if( !m_PointsContainer )
    {
    this->SetPoints(PointsContainer::New());
    }

  /**
   * Insert the point into the container with the given identifier.
   */
  m_PointsContainer->InsertElement(ptId, point);
}


/**
 * Check if a point exists for a given point identifier.  If a spot for
 * the point identifier exists, "point" is set, and true is returned.
 * Otherwise, false is returned, and "point" is not modified.
 * If "point" is NULL, then it is never set, but the existence of the point
 * is still returned.
 */
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
bool
PointSet<TPixelType, VDimension, TMeshTraits>
::GetPoint(PointIdentifier ptId, PointType* point) const
{
  /**
   * If the points container doesn't exist, then the point doesn't either.
   */
  if( !m_PointsContainer )
    {
    return false;
    }
  
  /**
   * Ask the container if the point identifier exists.
   */
  return m_PointsContainer->GetElementIfIndexExists(ptId, point);
}


/**
 * Assign data to a point identifier.  If a spot for the point identifier
 * does not exist, it will be created automatically.  There is no check if
 * a point with the same identifier exists.
 */
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
PointSet<TPixelType, VDimension, TMeshTraits>
::SetPointData(PointIdentifier ptId, PixelType data)
{
  /**
   * Make sure a point data container exists.
   */
  if( !m_PointDataContainer )
    {
    this->SetPointData(PointDataContainer::New());
    }

  /**
   * Insert the point data into the container with the given identifier.
   */
  m_PointDataContainer->InsertElement(ptId, data);
}


/**
 * Check if point data exists for a given point identifier.  If a spot for
 * the point identifier exists, "data" is set, and true is returned.
 * Otherwise, false is returned, and "data" is not modified.
 * If "data" is NULL, then it is never set, but the existence of the point
 * data is still returned.
 */
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
bool
PointSet<TPixelType, VDimension, TMeshTraits>
::GetPointData(PointIdentifier ptId, PixelType* data) const
{
  /**
   * If the point data container doesn't exist, then the point data doesn't
   * either.
   */
  if( !m_PointDataContainer )
    return false;
  
  /**
   * Ask the container if the point identifier exists.
   */
  return m_PointDataContainer->GetElementIfIndexExists(ptId, data);
}

/**
 * Copy the geometric and topological structure of the given input mesh.
 * The copying is done via reference counting.
 */
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
PointSet<TPixelType, VDimension, TMeshTraits>
::PassStructure(Self* in_mesh)
{
  // IMPLEMENT ME
}


/**
 * Get the number of points in the PointsContainer.
 */
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
unsigned long
PointSet<TPixelType, VDimension, TMeshTraits>
::GetNumberOfPoints(void) const
{  
  if( m_PointsContainer )
    {
    return m_PointsContainer->Size();
    }
  return 0;
}

/**
 * Get the bounding box of the entire mesh.
 */
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
const typename PointSet<TPixelType, VDimension, TMeshTraits>::BoundingBoxType * 
PointSet<TPixelType, VDimension, TMeshTraits>
::GetBoundingBox(void) const
{
  m_BoundingBox->SetPoints( this->GetPoints() );
  if( m_BoundingBox->GetMTime() > this->GetMTime() )
    {
    m_BoundingBox->ComputeBoundingBox();
    }
  return m_BoundingBox;
}


/**
 * Find the closest point in the mesh to the given point
 * (coords[PointDimension]).  Returns whether a closest point was found.  If
 * a point is found, its PointIdentifier is set through the "pointId" pointer
 * (if it isn't NULL).
 */
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
bool
PointSet<TPixelType, VDimension, TMeshTraits>
::FindClosestPoint(CoordRepType[PointDimension],
                   PointIdentifier*)
{
  m_BoundingBox->SetPoints(this->GetPoints());
  m_PointLocator->InitPointInsertion(m_PointsContainer, m_BoundingBox);

  return bool();
}

/**
 * Restore the PointSet to its initial state.  Useful for data pipeline updates
 * without memory re-allocation.
 */
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
PointSet<TPixelType, VDimension, TMeshTraits>
::Initialize(void)
{
  Superclass::Initialize();

  m_PointsContainer = 0;
  m_PointDataContainer = 0;
  m_PointLocator = 0;

}
  
/******************************************************************************
 * PROTECTED METHOD DEFINITIONS
 *****************************************************************************/

/**
 * A protected default constructor allows the New() routine to create an
 * instance of PointSet.  All the containers are initialized to non-existent.
 */
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
PointSet<TPixelType, VDimension, TMeshTraits>
::PointSet():
  m_PointsContainer(0),
  m_PointDataContainer(0)
{
  m_PointLocator = PointLocatorType::New();
  m_BoundingBox  = BoundingBoxType::New();
  
  // If we used unstructured regions instead of structured regions, then 
  // assume this object was created by the user and this is region 0 of 
  // 1 region.
  m_MaximumNumberOfRegions = 1;
  m_NumberOfRegions = 1;
  m_BufferedRegion  = -1;
  m_RequestedNumberOfRegions = 0;
  m_RequestedRegion = -1;
}


//----------------------------------------------------------------------------
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void 
PointSet<TPixelType, VDimension, TMeshTraits>
::UpdateOutputInformation()
{
  if (this->GetSource())
    {
    this->GetSource()->UpdateOutputInformation();
    }
  
  // Now we should know what our largest possible region is. If our 
  // requested region was not set yet, (or has been set to something 
  // invalid - with no data in it ) then set it to the largest 
  // possible region.
  if ( m_RequestedRegion == -1 && m_RequestedNumberOfRegions == 0 )
    {
    this->SetRequestedRegionToLargestPossibleRegion();
    }
  
  m_LastRequestedRegionWasOutsideOfTheBufferedRegion = 0;
}

//----------------------------------------------------------------------------
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void 
PointSet<TPixelType, VDimension, TMeshTraits>
::SetRequestedRegionToLargestPossibleRegion()
{
  m_RequestedNumberOfRegions     = 1;
  m_RequestedRegion           = 0;
}

//----------------------------------------------------------------------------
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void 
PointSet<TPixelType, VDimension, TMeshTraits>
::CopyInformation(const DataObject *data)
{
  const PointSet *mesh;
  
  mesh = dynamic_cast<const PointSet*>(data);

  if (mesh)
    {
    m_MaximumNumberOfRegions = mesh->GetMaximumNumberOfRegions();
    }
  else
    {
    // pointer could not be cast back down
    itkExceptionMacro(<< "itk::PointSet::CopyInformation() cannot cast "
                  << typeid(data).name() << " to "
                  << typeid(PointSet*).name() );
    }
}

//----------------------------------------------------------------------------
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void 
PointSet<TPixelType, VDimension, TMeshTraits>
::SetRequestedRegion(DataObject *data)
{
  PointSet *mesh;
  
  mesh = dynamic_cast<PointSet*>(data);

  if (mesh)
    {
    m_RequestedRegion = mesh->m_RequestedRegion;
    m_RequestedNumberOfRegions = mesh->m_RequestedNumberOfRegions;
    }
  else
    {
    // pointer could not be cast back down
    std::cerr << "itk::PointSet::SetRequestedRegion(DataObject*) cannot cast "
              << typeid(data).name() << " to "
              << typeid(PointSet*).name() << std::endl;
    }
}


//----------------------------------------------------------------------------
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
PointSet<TPixelType, VDimension, TMeshTraits>
::SetRequestedRegion(const RegionType &region)
{
  if (m_RequestedRegion != region)
    {
    m_RequestedRegion = region;
    m_RequestedRegionInitialized = true;
    }
}


//----------------------------------------------------------------------------
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
PointSet<TPixelType, VDimension, TMeshTraits>
::SetBufferedRegion(const RegionType &region)
{
  if (m_BufferedRegion != region)
    {
    m_BufferedRegion = region;
    this->Modified();
    }
}


//----------------------------------------------------------------------------
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
bool 
PointSet<TPixelType, VDimension, TMeshTraits>
::RequestedRegionIsOutsideOfTheBufferedRegion()
{
  if ( m_RequestedRegion != m_BufferedRegion ||
       m_RequestedNumberOfRegions != m_NumberOfRegions )
    {
    return true;
    }

  return false;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
bool 
PointSet<TPixelType, VDimension, TMeshTraits>
::VerifyRequestedRegion()
{
  bool retval = true;

  // Are we asking for more regions than we can get?
  if ( m_RequestedNumberOfRegions > m_MaximumNumberOfRegions )
    {
    itkExceptionMacro( << "Cannot break object into " <<
    m_RequestedNumberOfRegions << ". The limit is " <<
    m_MaximumNumberOfRegions );
    retval = false;
    }

  if ( m_RequestedRegion >= m_RequestedNumberOfRegions ||
       m_RequestedRegion < 0 )
    {
    itkExceptionMacro( << "Invalid update region " << m_RequestedRegion
                   << ". Must be between 0 and " 
                   << m_RequestedNumberOfRegions - 1);
    retval = false;
    }

  return retval;
}

} // end namespace itk

#endif
