/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConnectedRegionsMeshFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkConnectedRegionsMeshFilter_txx
#define _itkConnectedRegionsMeshFilter_txx

#include "itkConnectedRegionsMeshFilter.h"
#include "itkObjectFactory.h"

namespace itk
{

/**
 *
 */
template <class TInputMesh, class TOutputMesh>
ConnectedRegionsMeshFilter<TInputMesh,TOutputMesh>
::ConnectedRegionsMeshFilter()
{
  m_ExtractionMode = Self::LargestRegion;
}


/**
 *
 */
template <class TInputMesh, class TOutputMesh>
void 
ConnectedRegionsMeshFilter<TInputMesh,TOutputMesh>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Extraction Mode: ";
  if ( m_ExtractionMode == Self::PointSeededRegions )
    {
    os << "Point Seeded Regions" << std::endl;
    }
  else if ( m_ExtractionMode == Self::CellSeededRegions )
    {
    os << "Cell Seeded Regions" << std::endl;
    }
  else if ( m_ExtractionMode == Self::SpecifiedRegions )
    {
    os << "Specified Regions" << std::endl;
    }
  else if ( m_ExtractionMode == Self::LargestRegion )
    {
    os << "Largest Region" << std::endl;
    }
  else if ( m_ExtractionMode == Self::AllRegions )
    {
    os << "All Regions" << std::endl;
    }
  else if ( m_ExtractionMode == Self::ClosestPointRegion )
    {
    os << "Closest Point Region" << std::endl;
    }
}


/**
 *
 */
template <class TInputMesh, class TOutputMesh>
void 
ConnectedRegionsMeshFilter<TInputMesh,TOutputMesh>
::GenerateData()
{
  itkDebugMacro(<<"Actually executing");
}

} // end namespace itk

#endif
