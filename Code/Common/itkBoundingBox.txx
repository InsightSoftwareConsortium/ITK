/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBoundingBox.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkBoundingBox.h"

ITK_NAMESPACE_BEGIN

/**
 * Print out the bounding box.
 */
template <typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer>
void
BoundingBox<TPointIdentifier , VPointDimension, TCoordRep, TPointsContainer >
::PrintSelf(std::ostream& os, Indent indent)
{
  Object::PrintSelf(os, indent);

  if ( m_Bounds )
    {
    os << indent << "Bounding Box: ( " ;
    for (int i=0; i<PointDimension; i++)
      {
      os << m_Bounds[2*i] << "," << m_Bounds[2*i+1] << " ";
      }
    os << " )" << std::endl;
    }
  else
    {
    os << indent << "Bounding Box not defined" << std::endl;
    }
}

/**
 * Access routine to set the points container.
 */
template <typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer>
void
BoundingBox<TPointIdentifier , VPointDimension, TCoordRep, TPointsContainer >
::SetPoints(PointsContainer* points)
{
  itkDebugMacro(<< this->GetClassName() << " (" << this
                << "): setting Points container to " << points);
  if(m_PointsContainer != points)
    {
    m_PointsContainer = points;
    this->Modified();
    }
}

/**
 * Access routine to get the points container.
 */
template <typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer>
BoundingBox<TPointIdentifier , VPointDimension, TCoordRep, TPointsContainer >::PointsContainerPointer
BoundingBox<TPointIdentifier , VPointDimension, TCoordRep, TPointsContainer >
::GetPoints(void)
{
  itkDebugMacro(<< this->GetClassName() << " (" << this
                << "): returning Points container of " << m_PointsContainer );
  if(m_PointsContainer == 0)
    {
    this->SetPoints(PointsContainer::New());
    }
  return m_PointsContainer;
}

/******************************************************************************
 * PROTECTED METHOD DEFINITIONS
 *****************************************************************************/
template <typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer>
BoundingBox<TPointIdentifier , VPointDimension, TCoordRep, TPointsContainer >
::BoundingBox():
  m_PointsContainer(NULL),
  m_Bounds(NULL)
{
}

template <typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer>
BoundingBox<TPointIdentifier , VPointDimension, TCoordRep, TPointsContainer >
::~BoundingBox()
{
  if ( m_Bounds )
    {   
    delete [] m_Bounds;
    }
}


ITK_NAMESPACE_END
