/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPath.txx
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
#ifndef _itkPath_txx
#define _itkPath_txx

#include "itkPath.h"
#include "itkProcessObject.h"

/* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** */
/* ** **  Much of the code in this file is based on itkPointSet.txx  ** ** */
/* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** */

namespace itk
{

/**
 * A protected default constructor allows the New() routine to create an
 * instance of PointSet.  All the containers are initialized to non-existent.
 */
template <class TInput, class TOutput, unsigned int VDimension>
Path<TInput, TOutput, VDimension>
::Path()
{
  m_ZeroOffset.Fill(0);
  m_ZeroIndex.Fill(0);
}


/**
 * Restore the Path to its initial state.  Useful for data pipeline updates
 * without memory re-allocation.
 */
template <class TInput, class TOutput, unsigned int VDimension>
void
Path<TInput, TOutput, VDimension>
::Initialize(void)
{
  Superclass::Initialize();

  // de-allocate any "useless" memory, etc.
}


//----------------------------------------------------------------------------
template <class TInput, class TOutput, unsigned int VDimension>
void
Path<TInput, TOutput, VDimension>
::UpdateOutputInformation()
{
  if (this->GetSource())
    {
    this->GetSource()->UpdateOutputInformation();
    }
  
  /* For the time being, always use the largest possible region
  
  // Now we should know what our largest possible region is. If our 
  // requested region was not set yet, (or has been set to something 
  // invalid - with no data in it ) then set it to the largest 
  // possible region.
  if ( m_RequestedRegion == -1 && m_RequestedNumberOfRegions == 0 )
    {
    this->SetRequestedRegionToLargestPossibleRegion();
    }
  */
  
  m_LastRequestedRegionWasOutsideOfTheBufferedRegion = 0;
}

//----------------------------------------------------------------------------
template <class TInput, class TOutput, unsigned int VDimension>
void
Path<TInput, TOutput, VDimension>
::SetRequestedRegionToLargestPossibleRegion()
{
  /* For the time being, always use the largest possible region
     Otherwise, additional code and member data would be necessary
  
  // The following are defined in itkPointSet.h:
  m_RequestedNumberOfRegions     = 1;
  m_RequestedRegion           = 0;
  */
}

//----------------------------------------------------------------------------
template <class TInput, class TOutput, unsigned int VDimension>
void
Path<TInput, TOutput, VDimension>
::CopyInformation(const DataObject *data)
{
  /* For the time being, always use the largest possible region

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
  */
}

//----------------------------------------------------------------------------
template <class TInput, class TOutput, unsigned int VDimension>
void
Path<TInput, TOutput, VDimension>
::SetRequestedRegion(DataObject *data)
{
  /* For the time being, always use the largest possible region
     THEREFORE, DO NOTHING.  THIS IS INCORRECT!!!

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
  */
}


//----------------------------------------------------------------------------
template <class TInput, class TOutput, unsigned int VDimension>
bool
Path<TInput, TOutput, VDimension>
::RequestedRegionIsOutsideOfTheBufferedRegion()
{
  /* For the time being, always use the largest possible region
     THEREFORE, ALWAYS RETURN TRUE
  
  if ( m_RequestedRegion != m_BufferedRegion ||
       m_RequestedNumberOfRegions != m_NumberOfRegions )
    {
    return true;
    }

  return false;
  */
  return true;
}

template <class TInput, class TOutput, unsigned int VDimension>
bool
Path<TInput, TOutput, VDimension>
::VerifyRequestedRegion()
{
  bool retval = true;

  /* For the time being, always use the largest possible region
     THEREFORE, ALWAYS RETURN TRUE
  
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
  */
  
  return retval;
}

} // end namespace itk

#endif
