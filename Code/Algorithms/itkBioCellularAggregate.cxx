/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBioCellularAggregate.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#pragma warning ( disable : 4503 )
#endif
#include <fstream>
#include "itkBioCellularAggregate.h"



namespace itk {


namespace bio {


CellularAggregate
::CellularAggregate()
{

    Cell::ColorType color;
    color.SetRed(1.0);
    color.SetGreen(1.0);
    color.SetBlue(1.0);
  Cell::SetDefaultColor( color );

  m_Mesh = MeshType::New();

  m_Mesh->SetCellsAllocationMethod( MeshType::CellsAllocatedDynamicallyCellByCell );
  m_Mesh->SetPoints( PointsContainer::New() );
  m_Mesh->SetPointData( PointDataContainer::New() );
  m_Mesh->SetCells( VoronoiRegionsContainer::New() );

  m_Iteration = 0;
  m_ClosestPointComputationInterval = 5;

  m_FrictionForce = 1.0f;

}




CellularAggregate
::~CellularAggregate()
{
  this->KillAll();
}



unsigned int
CellularAggregate
::GetNumberOfCells(void) const
{
  return m_Mesh->GetPointData()->Size();
}


void
CellularAggregate
::ExportXFIG(const char *) const
{

  /*
  std::ofstream output;
  output.open( filename );

  if(output.fail() )
    {
    return;
    }
  
  output << "#FIG 3.2"  << std::endl;
  output << "Landscape" << std::endl;
  output << "Center"    << std::endl;
  output << "Metric"    << std::endl;
  output << "A4"        << std::endl;      
  output << "100.00"    << std::endl;
  output << "Single"    << std::endl;
  output << "-2"        << std::endl;
  output << "1200 2"    << std::endl;

  PointType position;

  CellsIterator cellIt = m_Mesh->GetPointData()->Begin();
  CellsIterator end    = m_Mesh->GetPointData()->End();

  while( cellIt != end )
    {
    Cell * cell = cellIt.Value();
    const IdentifierType id = cell->GetSelfIdentifier();
    m_Mesh->GetPoint( id, &position );
    output << "1 3 0 1 0 7 50 0 -1 0.000 1 0.0000 ";  // properties
    const float scale = 100.0;
    unsigned int radius = static_cast<unsigned int>( cell->GetRadius()    * scale );
    unsigned int x      = static_cast<unsigned int>( 5000.0 + position[0] * scale );
    unsigned int y      = static_cast<unsigned int>( 5000.0 + position[1] * scale );
    unsigned int x2     = static_cast<unsigned int>( x + radius * 0.5000 );
    unsigned int y2     = static_cast<unsigned int>( y + radius * 0.8660 );
    output <<    x   << " " <<    y   << " ";
    output << radius << " " << radius << " ";
    output <<    x   << " " <<    y   << " ";
    output <<    x2  << " " <<    y2  << std::endl;
    cellIt++;
    }

  output.close();
  */
}





void
CellularAggregate
::ExportDrawing(const char *) const
{
  /*
  std::ofstream output;
  output.open( filename );

  if(output.fail() )
    {
    return;
    }
  
  PointType position;

  CellsIterator cellIt = m_Mesh->GetPointData()->Begin();
  CellsIterator end    = m_Mesh->GetPointData()->End();

  while( cellIt != end )
    {
    Cell * cell = cellIt.Value();
    const IdentifierType id = cell->GetSelfIdentifier();
    m_Mesh->GetPoint( id, &position );
    output << position << "   " << cell->GetRadius() << std::endl;
    cellIt++;
    }

  output.close();
  */
}





void
CellularAggregate
::SetGrowthRadiusLimit( double value ) 
{
  Cell::SetGrowthRadiusLimit( value );
}




void
CellularAggregate
::SetGrowthRadiusIncrement( double value ) 
{
  Cell::SetGrowthRadiusIncrement( value );
}






void
CellularAggregate
::PrintSelf(std::ostream& os, itk::Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << "Cellular aggregate " << m_Mesh << std::endl;

}



void
CellularAggregate
::Remove( Cell * cell )
{
  if( !cell )
  {
    itk::ExceptionObject exception;
    exception.SetDescription("Trying to remove a null pointer to cell");
    exception.SetLocation("CellularAggregate::Remove(Cell*)");
    throw exception;
  }
  
  IdentifierType id = cell->GetSelfIdentifier();

  MeshType::CellAutoPointer   region;
  bool regionExist = m_Mesh->GetCell( id, region );
  if( regionExist )
    {
    VoronoiRegionType * realRegion =
              dynamic_cast < VoronoiRegionType * >( region.GetPointer() );

    if( !realRegion )
      {
      std::cerr << "CellularAggregate::Remove() couldn't dynamic_cast region " << std::endl;
      }
    else
      {
      VoronoiRegionType::PointIdIterator neighbor = realRegion->PointIdsBegin();
      VoronoiRegionType::PointIdIterator end      = realRegion->PointIdsEnd();
      while( neighbor != end )
        {
        neighbor++;
        }
      }
    // update voronoi connections
    }
 
  m_Mesh->GetCells()->DeleteIndex( id );
  m_Mesh->GetPoints()->DeleteIndex( id );
  m_Mesh->GetPointData()->DeleteIndex( id );

 
  delete cell;

}




void
CellularAggregate
::GetVoronoi( IdentifierType cellId, VoronoiRegionAutoPointer & voronoiPointer ) const
{

  MeshType::CellAutoPointer cellPointer;

  bool voronoiExists = m_Mesh->GetCell( cellId, cellPointer );
  if( !voronoiExists ) 
    {
    itk::ExceptionObject exception;
    exception.SetDescription( "voronoi region does not exist in the container" );
    exception.SetLocation( "GetVoronoi( IdentifierType");
    throw exception;
    }

  VoronoiRegionType * region =
          dynamic_cast < VoronoiRegionType * >( cellPointer.GetPointer() );

  voronoiPointer.TakeNoOwnership( region );
  if( cellPointer.IsOwner() )
    {
    voronoiPointer.TakeOwnership();   
    cellPointer.ReleaseOwnership();  
    }

}



void
CellularAggregate
::SetEgg( Cell * cell, const PointType & position )
{
  VectorType perturbation = position.GetVectorFromOrigin();
  this->Add( cell, perturbation );
}




void
CellularAggregate
::Add( Cell * cell )
{
  VectorType perturbation;
  perturbation.Fill( 0.0 );
  this->Add( cell, perturbation );
}





void
CellularAggregate
::Add( Cell * cellA, Cell * cellB, const VectorType & perturbation )
{

  this->Add( cellA,  perturbation );
  this->Add( cellB, -perturbation );

  const IdentifierType cellAId = cellA->GetSelfIdentifier();
  const IdentifierType cellBId = cellB->GetSelfIdentifier();

  VoronoiRegionAutoPointer cellAptr;
  VoronoiRegionAutoPointer cellBptr;

  this->GetVoronoi( cellAId, cellAptr );
  this->GetVoronoi( cellBId, cellBptr );

  cellAptr->AddPointId( cellBId );
  cellBptr->AddPointId( cellAId );
}
 




void
CellularAggregate
::Add( Cell * cell, const VectorType & perturbation )
{

  IdentifierType  newcellId       = cell->GetSelfIdentifier();
  IdentifierType  newcellparentId = cell->GetParentIdentifier();
    
  PointType position;
  CellAutoPointer selfVoronoi;

  // If the cell does not have a parent 
  // from which receive a position
  if( !newcellparentId )
    {
    position.Fill( 0.0 );
    selfVoronoi.TakeOwnership( new VoronoiRegionType );
    }
  else
    {
    bool parentPositionExist = m_Mesh->GetPoint( newcellparentId, &position );
    if( !parentPositionExist ) 
      {
        itk::ExceptionObject exception;
        exception.SetDescription( "Parent cell does not exist in the container" );
        exception.SetLocation( "CellularAggregate::Add( cell * )" );
        throw exception;
      }

    VoronoiRegionAutoPointer parentVoronoi;
    this->GetVoronoi( newcellparentId, parentVoronoi );
    CellAutoPointer parentClone;
    parentVoronoi->MakeCopy( parentClone );
    itk::TransferAutoPointer( selfVoronoi, parentClone );
    }

  position += perturbation;

  m_Mesh->SetCell(      newcellId, selfVoronoi );
  m_Mesh->SetPoint(     newcellId, position    );
  m_Mesh->SetPointData( newcellId, cell        );
 
  cell->SetCellularAggregate( this );

  // Add this new cell as neighbor to cells in its neighborhood

  VoronoiRegionType::PointIdIterator neighbor = selfVoronoi->PointIdsBegin();
  VoronoiRegionType::PointIdIterator end      = selfVoronoi->PointIdsEnd();

  while( neighbor != end )
    {
    const IdentifierType neighborId = (*neighbor);
    MeshType::CellAutoPointer cellPointer;
    bool neighborVoronoiExist = m_Mesh->GetCell( neighborId, cellPointer );
    if( neighborVoronoiExist ) 
      {
      VoronoiRegionType * region =
         dynamic_cast < VoronoiRegionType * >( cellPointer.GetPointer() );

      if( !region )
        {
        std::cerr << "CellularAggregate::Add() Failed to find a region"  << std::endl;
        }
      else
        {
        region->AddPointId( newcellId );
        }
      }
    neighbor++;
    }

}



void
CellularAggregate
::AdvanceTimeStep(void)
{
  
  if( m_Iteration % m_ClosestPointComputationInterval == 0 ) 
  {
    this->ComputeClosestPoints();
  }

  this->ComputeForces();
  this->UpdatePositions();
  
  CellsIterator begin = m_Mesh->GetPointData()->Begin();
  CellsIterator end   = m_Mesh->GetPointData()->End();

  CellsIterator cell  = begin;

  while( cell != end )
    {
    Cell * theCell = cell.Value();
    theCell->AdvanceTimeStep();
    ++cell;
    if( theCell->MarkedForRemoval() )
      {
      this->Remove( theCell );      
      }
    }
  
  this->InvokeEvent( IterationEvent() );

  m_Iteration++;

}






void
CellularAggregate
::KillAll(void)
{
    
  if( !m_Mesh  )
    {
    return;
    }
  
  CellsIterator cell = m_Mesh->GetPointData()->Begin();
  CellsIterator end  = m_Mesh->GetPointData()->End();

  while( cell != end )
    {
    delete (cell.Value());
    ++cell;
    }

  Cell::ResetCounter();
}



void
CellularAggregate
::ClearForces(void)
{

  CellsIterator cell = m_Mesh->GetPointData()->Begin();
  CellsIterator end  = m_Mesh->GetPointData()->End();

  while( cell != end )
    {
    cell.Value()->ClearForce();
    ++cell;
    }
}





void
CellularAggregate
::UpdatePositions(void)
{
 
  CellsConstIterator cellIt = m_Mesh->GetPointData()->Begin();
  CellsConstIterator end    = m_Mesh->GetPointData()->End();

  PointType position;

  while( cellIt != end )
    {
    Cell * cell = cellIt.Value();
    IdentifierType cellId = cell->GetSelfIdentifier();
    m_Mesh->GetPoint( cellId, &position );
    const VectorType force = cell->GetForce(); 
    if( force.GetNorm() > m_FrictionForce )
      {
      position += force / 50.0;
      }
    m_Mesh->SetPoint( cellId, position );
    cellIt++;
    }

}
  


void
CellularAggregate
::ComputeForces(void)
{

  
  // Clear all the force accumulators
  this->ClearForces();
 

  CellsConstIterator   cell1It     = m_Mesh->GetPointData()->Begin();
  CellsConstIterator   end         = m_Mesh->GetPointData()->End();

  // compute forces 
  while( cell1It != end )
    {

    const IdentifierType cell1Id = cell1It.Index();

    Cell     * cell1      =  cell1It.Value();

    PointType  position1;
    m_Mesh->GetPoint( cell1Id, &position1 );

    const double rA          = cell1->GetRadius();
 
    VoronoiRegionAutoPointer voronoiRegion;
    this->GetVoronoi( cell1Id, voronoiRegion );
              
    VoronoiRegionType::PointIdIterator neighbor = voronoiRegion->PointIdsBegin();
    VoronoiRegionType::PointIdIterator end      = voronoiRegion->PointIdsEnd();

    while( neighbor != end )
      {

      const IdentifierType cell2Id = (*neighbor);  

      Cell     * cell2;
      PointType  position2;
      
      if( !m_Mesh->GetPoint(      cell2Id, &position2 ) )
        {
        neighbor++;  // if the neigbor has been removed, skip it
        continue;
        }
      m_Mesh->GetPointData(  cell2Id, &cell2     );
      
      const double rB      = cell2->GetRadius();

      Cell::VectorType relativePosition = position1 - position2;

      const double distance = relativePosition.GetNorm();

      if( distance < (rA + rB) / 2.0 )
        {
          const double factor = 2.0 * Cell::GetGrowthRadiusLimit() / distance;
        Cell::VectorType force = relativePosition * factor ;
        cell1->AddForce(  force );
        cell2->AddForce( -force );
        }
      else if( distance < rA + rB )
        {
        Cell::VectorType force = relativePosition;
        cell1->AddForce(  force );
        cell2->AddForce( -force );
        }
      
      neighbor++;
      }
    
    cell1It++;
    }

 }




void
CellularAggregate
::ComputeClosestPoints(void)
{

  PointsConstIterator   beginPoints   = m_Mesh->GetPoints()->Begin();
  PointsConstIterator   endPoints     = m_Mesh->GetPoints()->End();

  PointsConstIterator   point1It = beginPoints;

  while( point1It != endPoints )
    {

    PointType  position1 = point1It.Value();

    PointsConstIterator   point2It   = beginPoints;

    Cell  * cell1;

    IdentifierType cell1Id = point1It.Index();
    m_Mesh->GetPointData( cell1Id, &cell1 );
    const double radius  = cell1->GetRadius();
    const double limitDistance = radius * 4.0;
 
    VoronoiRegionAutoPointer voronoiRegion;
    this->GetVoronoi( cell1Id, voronoiRegion ); 

    voronoiRegion->ClearPoints();

    while( point2It != endPoints )
      {

      if( point2It == point1It )
        {
        point2It++;
        continue;
        }

      PointType  position2 = point2It.Value();

      Cell::VectorType relativePosition = position1 - position2;

      const double distance = relativePosition.GetNorm();
      if( distance < limitDistance )
        {
        voronoiRegion->AddPointId( point2It.Index() );
        }
      point2It++;
      }

    point1It++;
    }

 }





void
CellularAggregate
::DumpContent( std::ostream & os ) const
{
  CellsConstIterator   beginCell   = m_Mesh->GetPointData()->Begin();
  CellsConstIterator   endCell     = m_Mesh->GetPointData()->End();
  
  CellsConstIterator cell1It = beginCell;

  os << "Cell Identifiers " << std::endl;
  while( cell1It != endCell )
    {
    os << cell1It.Index() << " == ";
    os << cell1It.Value()->GetSelfIdentifier() << std::endl;
    cell1It++;
    }

  os << std::endl << "Points " << std::endl;
  PointsConstIterator  pointIt     = m_Mesh->GetPoints()->Begin();
  PointsConstIterator  endPoint    = m_Mesh->GetPoints()->End();

  while( pointIt != endPoint )
    {
    os << pointIt.Index() << "  ";
    os << pointIt.Value() << std::endl; 
    pointIt++;
    }


  os << std::endl << "Neighborhoods " << std::endl;
  cell1It = beginCell;
  while( cell1It != endCell )
    {
    VoronoiRegionAutoPointer voronoiRegion;
    this->GetVoronoi( cell1It.Index(), voronoiRegion );
    VoronoiRegionType::PointIdIterator neighbor = voronoiRegion->PointIdsBegin();
    VoronoiRegionType::PointIdIterator end      = voronoiRegion->PointIdsEnd();

    while( neighbor != end )
      {
      os << (*neighbor) << "   ";
      neighbor++;
      }
    os << std::endl;

    cell1It++;
    }
      
}



void
CellularAggregate
::AddSubstrate( SubstrateType * substrate )
{
  SubstratePointer smartPointer( substrate );
  m_Substrates.push_back( smartPointer );
}




CellularAggregate::SubstratesVector &
CellularAggregate
::GetSubstrates( void )
{
  return m_Substrates;
}




CellularAggregate::SubstrateValueType 
CellularAggregate
::GetSubstrateValue( Cell::IdentifierType cellId, unsigned int substrateId )
{

  PointType cellPosition;
  bool cellPositionExists = m_Mesh->GetPoint( cellId, &cellPosition );
  if( !cellPositionExists ) 
    {
    std::cerr << " Cell position doesn't exist for cell Id = ";
    std::cerr << cellId << std::endl;
    return itk::NumericTraits< SubstrateValueType >::Zero;
    }

  SubstratePointer    substrate = m_Substrates[ substrateId ];

  SubstrateType::IndexType index;
  typedef SubstrateType::IndexType::IndexValueType IndexValueType;

  SubstrateType::SizeType  substrateSize = 
                    substrate->GetBufferedRegion().GetSize();

  const double * spacing = substrate->GetSpacing().GetDataPointer();

  substrate->TransformPhysicalPointToIndex( cellPosition, index );

  SubstrateValueType  value = 0;
  if( substrate->GetBufferedRegion().IsInside( index ) )
    {
    value     = substrate->GetPixel( index );
    }

  return value;
}




} // end namespace bio

} // end namespace itk

