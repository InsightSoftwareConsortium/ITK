/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConnectedRegionsMeshFilter.txx
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
#ifndef _itkConnectedRegionsMeshFilter_txx
#define _itkConnectedRegionsMeshFilter_txx

#include "itkConnectedRegionsMeshFilter.h"
#include "itkNumericTraits.h"
#include "itkObjectFactory.h"

#include <set>

namespace itk
{

/*
 * ------------------------------------------------
 */
template <class TInputMesh, class TOutputMesh>
ConnectedRegionsMeshFilter<TInputMesh,TOutputMesh>
::ConnectedRegionsMeshFilter()
{
  m_ExtractionMode = Self::LargestRegion;
  m_ClosestPoint.Fill(0);
}

/*
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

/*
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


/*
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


/*
 *
 */
template <class TInputMesh, class TOutputMesh>
void 
ConnectedRegionsMeshFilter<TInputMesh,TOutputMesh>
::GenerateData()
{
  InputMeshPointer input = this->GetInput();
  OutputMeshPointer output = this->GetOutput();
  InputMeshPointsContainerPointer inPts = input->GetPoints();
  InputMeshCellsContainerPointer inCells = input->GetCells();

  itkDebugMacro(<<"Executing connectivity");

  //  Check input/allocate storage
  unsigned long numCells = input->GetNumberOfCells();
  unsigned long numPts = input->GetNumberOfPoints();
  if ( numPts < 1 || numCells < 1 )
    {
    itkDebugMacro(<<"No data to connect!");
    return;
    }

  // Initialize.  Keep track of points and cells visited.
  input->BuildCellLinks(); //needed to get neighbors
  
  m_RegionSizes.clear();
  m_Visited.reserve(numCells);
  unsigned int i;
  for ( i=0; i < numCells; i++ )
    {
    m_Visited[i] = -1;
    }

  // Traverse all cells marking those visited.  Each new search
  // starts a new connected region. Connected region grows 
  // using a connected wave propagation.
  //
  m_Wave = new std::vector<unsigned long>;
  m_Wave2 = new std::vector<unsigned long>;
  m_Wave->reserve(numPts/4);
  m_Wave2->reserve(numPts/4);
  
  // variable used to keep track of propagation
  m_RegionNumber = 0;
  unsigned long maxCellsInRegion=0;
  unsigned long largestRegionId = 0;

  int tenth = numCells/10 + 1;
  int cellId=0;
  if ( m_ExtractionMode != PointSeededRegions && 
       m_ExtractionMode != CellSeededRegions &&
       m_ExtractionMode != ClosestPointRegion ) 
    { //visit all cells marking with region number
    for (CellsContainerConstIterator cell=inCells->Begin(); 
         cell != inCells->End(); ++cell, ++cellId)
      {
      if ( !(cellId % tenth) )
        {
        this->UpdateProgress ((float)cellId/numCells);
        }

      if ( m_Visited[cellId] < 0 )
        {
        m_NumberOfCellsInRegion = 0;
        m_Wave->push_back(cellId);
        this->PropagateConnectedWave();
        
        if ( m_NumberOfCellsInRegion > maxCellsInRegion )
          {
          maxCellsInRegion = m_NumberOfCellsInRegion;
          largestRegionId = m_RegionNumber;
          }

        m_RegionSizes[m_RegionNumber++] = m_NumberOfCellsInRegion;
        m_Wave->clear();
        m_Wave2->clear();
        }
      }
    }
  //Otherwise, seeds are used to indicate the region
  else if ( m_ExtractionMode == PointSeededRegions )
    {
    m_NumberOfCellsInRegion = 0;
    if ( m_ExtractionMode == PointSeededRegions )
      {
      InputMeshCellLinksContainerPointer cellLinks;
      cellLinks = input->GetCellLinks();
      InputMeshCellLinksContainer links;
      typename std::set<InputMeshCellIdentifier>::iterator citer;
      
      for (std::vector<unsigned long>::iterator i = m_SeedList.begin();
           i != m_SeedList.end(); ++i)
        {
        links = cellLinks->ElementAt(*i);
        for (citer = links.begin(); citer != links.end();
             ++citer)
          {
          m_Wave->push_back(*citer);
          }
        }
      }
    // use the seeds directly
    else if ( m_ExtractionMode == CellSeededRegions )
      {
      for (std::vector<unsigned long>::iterator i = m_SeedList.begin();
           i != m_SeedList.end(); ++i)
        {
        m_Wave->push_back(*i);
        }
      }
    // find the closest point and get the cells using it as the seeds
    else if ( m_ExtractionMode == ClosestPointRegion )
      {
      // find the closest point
      double minDist2=NumericTraits<double>::max(), dist2;
      InputMeshPointIdentifier minId = 0;
      InputMeshPointType x;
      for (PointsContainerConstIterator piter=inPts->Begin();
           piter != inPts->End(); ++piter)
        {
        x = piter->Value();
        dist2 = x.SquaredEuclideanDistanceTo(m_ClosestPoint);
        if ( dist2 < minDist2 )
          {
          minId = piter.Index();
          minDist2 = dist2;
          }
        }

      // get the cells using the closest point and use them as seeds
      InputMeshCellLinksContainerPointer cellLinks;
      cellLinks = input->GetCellLinks();
      InputMeshCellLinksContainer links;
      typename std::set<InputMeshCellIdentifier>::iterator citer;
      
      links = cellLinks->ElementAt(minId);
      for (citer = links.begin(); citer != links.end();
           ++citer)
        {
        m_Wave->push_back(*citer);
        }
      }//closest point

    // now propagate a wave
    this->PropagateConnectedWave();
    m_RegionSizes[m_RegionNumber] = m_NumberOfCellsInRegion;
    }
  
  delete m_Wave;
  delete m_Wave2;
  m_Wave = m_Wave2 = 0;
  
  itkDebugMacro (<<"Extracted " << m_RegionNumber << " region(s)");

  // Pass the point and point data through
  output->SetPoints(inPts);
  output->SetPointData(input->GetPointData());
  
  // Create output cells
  //
  InputMeshCellsContainerPointer 
    outCells(InputMeshCellsContainer::New());
  InputMeshCellDataContainerPointer 
    outCellData(InputMeshCellDataContainer::New());
  cellId = 0;
  CellsContainerConstIterator cell;
  CellDataContainerConstIterator cellData;

  if ( m_ExtractionMode == PointSeededRegions ||
       m_ExtractionMode == CellSeededRegions ||
       m_ExtractionMode == ClosestPointRegion ||
       m_ExtractionMode == AllRegions)
    { // extract any cell that's been visited
    for (cell=inCells->Begin(); cell != inCells->End(); ++cell, ++cellId)
      {
      if ( m_Visited[cellId] >= 0 )
        {
        outCells->InsertElement(cellId,cell->Value());
        outCellData->InsertElement(cellId,cellData->Value());
        }
      }
    }
  // if specified regions, add regions
  else if ( m_ExtractionMode == SpecifiedRegions )
    {
    std::vector<unsigned long>::iterator i;
    unsigned long regionId;
    bool inReg = false;
    for (cell=inCells->Begin(); cell != inCells->End(); ++cell, ++cellId)
      {
      if ( m_Visited[cellId] >= 0 )
        {
        regionId=static_cast<unsigned long>(m_Visited[cellId]);
        //see if cell is on region
        for ( i = m_RegionList.begin(); i != m_RegionList.end(); ++i)
          {
          if ( *i == regionId )
            {
            inReg = true;
            break;
            }
          }

        if ( inReg )
          {
          outCells->InsertElement(cellId,cell->Value());
          outCellData->InsertElement(cellId,cellData->Value());
          }
        }
      }
    }
  else //we are extracting the largest region
    {
    for (cell=inCells->Begin(); cell != inCells->End(); ++cell, ++cellId)
      {
      if ( m_Visited[cellId] == static_cast<long>(largestRegionId) )
        {
        outCells->InsertElement(cellId,cell->Value());
        outCellData->InsertElement(cellId,cellData->Value());
        }
      }
    }

  // Set the output and clean up
  output->SetCells(outCells);
  output->SetCellData(outCellData);
  m_Visited.clear();
  
  // Report some statistics
  if ( this->GetDebug() )
    {
    long count=0;
    for (std::vector<unsigned long>::const_iterator ii=m_RegionSizes.begin();
         ii != m_RegionSizes.end(); ++ii)
      {
      count += *ii;
      }
    itkDebugMacro (<< "Total # of cells accounted for: " << count);
    itkDebugMacro (<< "Extracted " << output->GetNumberOfCells() << " cells");
    }


  // This prevents unnecessary re-executions of the pipeline.
  output->SetBufferedRegion( output->GetRequestedRegion() );

  return;
}

template <class TInputMesh, class TOutputMesh>
void 
ConnectedRegionsMeshFilter<TInputMesh,TOutputMesh>
::PropagateConnectedWave()
{
  InputMeshPointer input = this->GetInput();
  unsigned long cellId;
  InputMeshCellPointer cellPtr;
  InputMeshPointIdConstIterator piter;
  InputMeshCellLinksContainerPointer cellLinks;
  cellLinks = input->GetCellLinks();
  InputMeshCellLinksContainer links;

  std::vector<unsigned long>::iterator i;
  std::vector<unsigned long> *tmpWave;
  typename std::set<InputMeshCellIdentifier>::iterator citer;

  while ( m_Wave->size() > 0 )
    {
    for (i=m_Wave->begin(); i != m_Wave->end(); ++i)
      {
      cellId = *i;
      if ( m_Visited[cellId] < 0 )
        {
        m_Visited[cellId] = m_RegionNumber;
        m_NumberOfCellsInRegion++;
        
        //now get the cell points, and then cells using these points
        input->GetCell(cellId, cellPtr);
        for ( piter=cellPtr->PointIdsBegin(); 
              piter != cellPtr->PointIdsEnd(); ++piter)
          {
          links = cellLinks->ElementAt(*piter);
          for (citer = links.begin(); citer != links.end(); ++citer)
            {
            if ( m_Visited[*citer] < 0 )
              {
              m_Wave2->push_back(*citer);
              }
            }
          }
        }//if visited
      }//for all cells in wave

    tmpWave = m_Wave;
    m_Wave = m_Wave2;
    m_Wave2 = tmpWave;
    tmpWave->clear();
    }//while wave is propagating
  
  return;
}



} // end namespace itk

#endif
