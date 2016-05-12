/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkConnectedRegionsMeshFilter_hxx
#define itkConnectedRegionsMeshFilter_hxx

#include "itkConnectedRegionsMeshFilter.h"
#include "itkNumericTraits.h"
#include "itkObjectFactory.h"

#include <set>

namespace itk
{
/**
 * ------------------------------------------------
 */
template< typename TInputMesh, typename TOutputMesh >
ConnectedRegionsMeshFilter< TInputMesh, TOutputMesh >
::ConnectedRegionsMeshFilter() :
  m_ExtractionMode(Self::LargestRegion),
  m_NumberOfCellsInRegion(NumericTraits< SizeValueType >::ZeroValue()),
  m_RegionNumber(NumericTraits< IdentifierType >::ZeroValue()),
  m_Wave(ITK_NULLPTR),
  m_Wave2(ITK_NULLPTR)
{
  m_ClosestPoint.Fill(0);
}

/**
 * ------------------------------------------------
 */
template< typename TInputMesh, typename TOutputMesh >
void
ConnectedRegionsMeshFilter< TInputMesh, TOutputMesh >
::DeleteSeed(IdentifierType id)
{
  std::vector< IdentifierType >           tmpVector;
  std::vector< IdentifierType >::iterator i;

  for ( i = m_SeedList.begin(); i != m_SeedList.end(); ++i )
    {
    if ( *i != id )
      {
      tmpVector.push_back(*i);
      }
    }
  m_SeedList.clear();
  for ( i = tmpVector.begin(); i != tmpVector.end(); ++i )
    {
    m_SeedList.push_back(*i);
    }
}

/**
 * ------------------------------------------------
 */
template< typename TInputMesh, typename TOutputMesh >
void
ConnectedRegionsMeshFilter< TInputMesh, TOutputMesh >
::DeleteSpecifiedRegion(IdentifierType id)
{
  std::vector< IdentifierType >           tmpVector;
  std::vector< IdentifierType >::iterator i;

  for ( i = m_RegionList.begin(); i != m_RegionList.end(); ++i )
    {
    if ( *i != id )
      {
      tmpVector.push_back(*i);
      }
    }
  m_RegionList.clear();
  for ( i = tmpVector.begin(); i != tmpVector.end(); ++i )
    {
    m_RegionList.push_back(*i);
    }
}

/**
 * ------------------------------------------------
 */
template< typename TInputMesh, typename TOutputMesh >
void
ConnectedRegionsMeshFilter< TInputMesh, TOutputMesh >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

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
template< typename TInputMesh, typename TOutputMesh >
void
ConnectedRegionsMeshFilter< TInputMesh, TOutputMesh >
::GenerateData()
{
  InputMeshConstPointer                  input = this->GetInput();
  OutputMeshPointer                      output = this->GetOutput();
  InputMeshPointsContainerConstPointer   inPts = input->GetPoints();
  InputMeshCellsContainerConstPointer    inCells = input->GetCells();
  InputMeshCellDataContainerConstPointer inCellData = input->GetCellData();

  itkDebugMacro(<< "Executing connectivity");

  //  Check input/allocate storage
  IdentifierType numCells = input->GetNumberOfCells();
  IdentifierType numPts = input->GetNumberOfPoints();
  if ( numPts < 1 || numCells < 1 )
    {
    itkDebugMacro(<< "No data to connect!");
    return;
    }

  // Initialize.  Keep track of points and cells visited.
  input->BuildCellLinks(); //needed to get neighbors

  m_RegionSizes.clear();
  m_Visited.resize(numCells);
  for ( unsigned int i = 0; i < numCells; i++ )
    {
    m_Visited[i] = -1;
    }

  // Traverse all cells marking those visited.  Each new search
  // starts a new connected region. Connected region grows
  // using a connected wave propagation.
  //
  m_Wave = new std::vector< IdentifierType >;
  m_Wave2 = new std::vector< IdentifierType >;
  m_Wave->reserve(numPts / 4);
  m_Wave2->reserve(numPts / 4);

  // variable used to keep track of propagation
  m_RegionNumber = 0;
  IdentifierType maxCellsInRegion = 0;
  IdentifierType largestRegionId = 0;

  int tenth = numCells / 10 + 1;
  int cellId = 0;
  if ( m_ExtractionMode != PointSeededRegions
       && m_ExtractionMode != CellSeededRegions
       && m_ExtractionMode != ClosestPointRegion )
    { //visit all cells marking with region number
    for ( CellsContainerConstIterator cell = inCells->Begin();
          cell != inCells->End(); ++cell, ++cellId )
      {
      if ( !( cellId % tenth ) )
        {
        this->UpdateProgress ( (float)cellId / numCells );
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

        m_RegionNumber++;
        m_RegionSizes.push_back(m_NumberOfCellsInRegion);
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
      InputMeshCellLinksContainerConstPointer cellLinks;
      cellLinks = input->GetCellLinks();
      InputMeshCellLinksContainer links;
      typename std::set< InputMeshCellIdentifier >::iterator citer;

      for ( std::vector< IdentifierType >::iterator i = m_SeedList.begin();
            i != m_SeedList.end(); ++i )
        {
        links = cellLinks->ElementAt(*i);
        for ( citer = links.begin(); citer != links.end();
              ++citer )
          {
          m_Wave->push_back(*citer);
          }
        }
      }
    // use the seeds directly
    else if ( m_ExtractionMode == CellSeededRegions )
      {
      for ( std::vector< IdentifierType >::iterator i = m_SeedList.begin();
            i != m_SeedList.end(); ++i )
        {
        m_Wave->push_back(*i);
        }
      }
    // find the closest point and get the cells using it as the seeds
    else if ( m_ExtractionMode == ClosestPointRegion )
      {
      // find the closest point
      double                   minDist2 = NumericTraits< double >::max(), dist2;
      InputMeshPointIdentifier minId = 0;
      InputMeshPointType       x;
      for ( PointsContainerConstIterator piter = inPts->Begin();
            piter != inPts->End(); ++piter )
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
      InputMeshCellLinksContainerConstPointer cellLinks;
      cellLinks = input->GetCellLinks();
      InputMeshCellLinksContainer links;
      typename std::set< InputMeshCellIdentifier >::iterator citer;

      links = cellLinks->ElementAt(minId);
      for ( citer = links.begin(); citer != links.end();
            ++citer )
        {
        m_Wave->push_back(*citer);
        }
      } //closest point

    // now propagate a wave
    this->PropagateConnectedWave();
    itkAssertOrThrowMacro( m_RegionNumber < m_RegionSizes.size(),
                           "Region number exceeds region sizes." );
    m_RegionSizes[m_RegionNumber] = m_NumberOfCellsInRegion;
    }

  delete m_Wave;
  delete m_Wave2;
  m_Wave = m_Wave2 = ITK_NULLPTR;

  itkDebugMacro (<< "Extracted " << m_RegionNumber << " region(s)");

  // Pass the point and point data through
  this->CopyInputMeshToOutputMeshPoints();
  this->CopyInputMeshToOutputMeshPointData();

  // Create output cells
  //
  InputMeshCellsContainerPointer
  outCells( InputMeshCellsContainer::New() );
  InputMeshCellDataContainerPointer
  outCellData( InputMeshCellDataContainer::New() );
  cellId = 0;
  CellsContainerConstIterator    cell;
  CellDataContainerConstIterator cellData;
  bool                           CellDataPresent = (   ITK_NULLPTR != inCellData
                                                    && 0 != inCellData->size() );
  InputMeshCellPointer           cellCopy; // need an autopointer to duplicate
                                           // a cell

  if ( m_ExtractionMode == PointSeededRegions
       || m_ExtractionMode == CellSeededRegions
       || m_ExtractionMode == ClosestPointRegion
       || m_ExtractionMode == AllRegions )
    { // extract any cell that's been visited
    if ( CellDataPresent )
      {
      cellData = inCellData->Begin();
      }
    for ( cell = inCells->Begin();
          cell != inCells->End();
          ++cell, ++cellId )
      {
      if ( m_Visited[cellId] >= 0 )
        {
        cell->Value()->MakeCopy(cellCopy);
        outCells->InsertElement( cellId, cellCopy.GetPointer() );
        cellCopy.ReleaseOwnership();  // Pass cell ownership to output mesh
        if ( CellDataPresent )
          {
          outCellData->InsertElement( cellId, cellData->Value() );
          ++cellData;
          }
        }
      }
    }
  // if specified regions, add regions
  else if ( m_ExtractionMode == SpecifiedRegions )
    {
    std::vector< IdentifierType >::iterator i;
    IdentifierType                          regionId;
    bool                                   inReg = false;
    if ( CellDataPresent )
      {
      cellData = inCellData->Begin();
      }
    for ( cell = inCells->Begin();
          cell != inCells->End();
          ++cell, ++cellId )
      {
      if ( m_Visited[cellId] >= 0 )
        {
        regionId = static_cast< IdentifierType >( m_Visited[cellId] );
        //see if cell is on region
        for ( i = m_RegionList.begin(); i != m_RegionList.end(); ++i )
          {
          if ( *i == regionId )
            {
            inReg = true;
            break;
            }
          }

        if ( inReg )
          {
          cell->Value()->MakeCopy(cellCopy);
          outCells->InsertElement( cellId, cellCopy.GetPointer() );
          cellCopy.ReleaseOwnership();  // Pass cell ownership to output mesh
          if ( CellDataPresent )
            {
            outCellData->InsertElement( cellId, cellData->Value() );
            ++cellData;
            }
          }
        }
      }
    }
  else //we are extracting the largest region
    {
    if ( CellDataPresent )
      {
      cellData = inCellData->Begin();
      }
    for ( cell = inCells->Begin();
          cell != inCells->End();
          ++cell, ++cellId )
      {
      if ( m_Visited[cellId] == static_cast< OffsetValueType >( largestRegionId ) )
        {
        cell->Value()->MakeCopy(cellCopy);
        outCells->InsertElement( cellId, cellCopy.GetPointer() );
        cellCopy.ReleaseOwnership(); // Pass cell ownership to output mesh
        if ( CellDataPresent )
          {
          outCellData->InsertElement( cellId, cellData->Value() );
          ++cellData;
          }
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
    SizeValueType count = 0;
    for ( std::vector< IdentifierType >::const_iterator ii = m_RegionSizes.begin();
          ii != m_RegionSizes.end(); ++ii )
      {
      count += *ii;
      }
    itkDebugMacro (<< "Total #of cells accounted for: " << count);
    itkDebugMacro (<< "Extracted " << output->GetNumberOfCells() << " cells");
    }

  // This prevents unnecessary re-executions of the pipeline.
  output->SetBufferedRegion( output->GetRequestedRegion() );
}

template< typename TInputMesh, typename TOutputMesh >
void
ConnectedRegionsMeshFilter< TInputMesh, TOutputMesh >
::PropagateConnectedWave()
{
  InputMeshConstPointer                   input = this->GetInput();
  IdentifierType                          cellId;
  InputMeshCellPointer                    cellPtr;
  InputMeshPointIdConstIterator           piter;
  InputMeshCellLinksContainerConstPointer cellLinks = input->GetCellLinks();
  InputMeshCellLinksContainer             links;

  std::vector< IdentifierType >::iterator i;
  std::vector< IdentifierType > *         tmpWave;
  typename std::set< InputMeshCellIdentifier >::iterator citer;

  while ( m_Wave->size() > 0 )
    {
    for ( i = m_Wave->begin(); i != m_Wave->end(); ++i )
      {
      cellId = *i;
      if ( m_Visited[cellId] < 0 )
        {
        m_Visited[cellId] = static_cast< OffsetValueType >( m_RegionNumber );
        m_NumberOfCellsInRegion++;

        //now get the cell points, and then cells using these points
        input->GetCell(cellId, cellPtr);
        for ( piter = cellPtr->PointIdsBegin();
              piter != cellPtr->PointIdsEnd(); ++piter )
          {
          links = cellLinks->ElementAt(*piter);
          for ( citer = links.begin(); citer != links.end(); ++citer )
            {
            if ( m_Visited[*citer] < 0 )
              {
              m_Wave2->push_back(*citer);
              }
            }
          }
        } //if visited
      }   //for all cells in wave

    tmpWave = m_Wave;
    m_Wave = m_Wave2;
    m_Wave2 = tmpWave;
    tmpWave->clear();
    } //while wave is propagating
}
} // end namespace itk

#endif
