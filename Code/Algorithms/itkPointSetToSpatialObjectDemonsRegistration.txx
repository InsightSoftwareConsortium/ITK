/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointSetToSpatialObjectDemonsRegistration.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPointSetToSpatialObjectDemonsRegistration_txx
#define __itkPointSetToSpatialObjectDemonsRegistration_txx

#include "itkPointSetToSpatialObjectDemonsRegistration.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TFixedPointSet, typename TMovingSpatialObject >
PointSetToSpatialObjectDemonsRegistration< TFixedPointSet, TMovingSpatialObject >
::PointSetToSpatialObjectDemonsRegistration()
{
  m_FixedPointSet           = 0; // has to be provided by the user.
  m_MovingSpatialObject     = 0; // has to be provided by the user.
}

/**
 * Starts the Registration Process
 */
template< typename TFixedPointSet, typename TMovingSpatialObject >
void
PointSetToSpatialObjectDemonsRegistration< TFixedPointSet, TMovingSpatialObject >
::StartRegistration(void)
{}

/**
 * PrintSelf
 */
template< typename TFixedPointSet, typename TMovingSpatialObject >
void
PointSetToSpatialObjectDemonsRegistration< TFixedPointSet, TMovingSpatialObject >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Fixed PointSet: " << m_FixedPointSet.GetPointer() << std::endl;
  os << indent << "Moving SpatialObject: " << m_MovingSpatialObject.GetPointer() << std::endl;
}
} // end namespace itk

#endif
