/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConnectedRegionsMeshFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkConnectedRegionsMeshFilter_txx
#define _itkConnectedRegionsMeshFilter_txx

#include "itkConnectedRegionsMeshFilter.h"
#include "itkObjectFactory.h"

namespace itk
{

/**
 * ------------------------------------------------
 */
template <class TInputMesh, class TOutputMesh>
ConnectedRegionsMeshFilter<TInputMesh,TOutputMesh>
::ConnectedRegionsMeshFilter()
{
  m_ExtractionMode = Self::LargestRegion;
}

/**
 * ------------------------------------------------
 */
template <class TInputMesh, class TOutputMesh>
void
ConnectedRegionsMeshFilter<TInputMesh,TOutputMesh>
::DeleteSeed(unsigned long id)
{
  std::vector<unsigned long> tmpVector;
  std::vector<unsigned long>::iterator i;
  
  for ( i = m_SeedList.begin(); i != m_SeedList.end(); ++i)
    {
    if ( *i != id )
      {
      tmpVector.push_back(*i);
      }
    }
  m_SeedList.clear();
  for ( i = tmpVector.begin(); i != tmpVector.end(); ++i)
    {
    m_SeedList.push_back(*i);
    }
}

/**
 * ------------------------------------------------
 */
template <class TInputMesh, class TOutputMesh>
void
ConnectedRegionsMeshFilter<TInputMesh,TOutputMesh>
::DeleteSpecifiedRegion(unsigned long id)
{
  std::vector<unsigned long> tmpVector;
  std::vector<unsigned long>::iterator i;
  
  for ( i = m_RegionList.begin(); i != m_RegionList.end(); ++i)
    {
    if ( *i != id )
      {
      tmpVector.push_back(*i);
      }
    }
  m_RegionList.clear();
  for ( i = tmpVector.begin(); i != tmpVector.end(); ++i)
    {
    m_RegionList.push_back(*i);
    }
}


/**
 * ------------------------------------------------
 */
template <class TInputMesh, class TOutputMesh>
void 
ConnectedRegionsMeshFilter<TInputMesh,TOutputMesh>
::PrintSelf(std::ostream& os, Indent indent) const
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
  itkDebugMacro(<<"Executing connectivity");

}

} // end namespace itk

#endif
