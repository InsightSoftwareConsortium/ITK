/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBioCell.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itk_Bio_Cell_Txx
#define __itk_Bio_Cell_Txx


#include "itkBioCell.h"
#include "vnl/vnl_math.h"
#include "vnl/vnl_sample.h"
#include <new>


namespace itk {

namespace bio {


  
/**
 *    Constructor Lonely Cell
 */ 
template<unsigned int NSpaceDimension>
Cell<NSpaceDimension>
::Cell()
{

  m_Force.Fill( 0.0f );

}




/**
 *    Destructor   
 */ 
template<unsigned int NSpaceDimension>
Cell<NSpaceDimension>
::~Cell()
{
}



/**
 *    Cell Division
 */ 

template<unsigned int NSpaceDimension>
void
Cell<NSpaceDimension>
::Mitosis(void) 
{

  Cell * siblingA = this->CreateNew();
  Cell * siblingB = this->CreateNew();

  siblingA->m_Radius   = m_Radius / sqrt( 2.0f );
  siblingB->m_Radius   = m_Radius / sqrt( 2.0f );

  siblingA->m_Generation = m_Generation + 1;
  siblingB->m_Generation = m_Generation + 1;

  // Pass the genome to each daughter cell
  siblingA->m_Genome = m_Genome;
  siblingB->m_Genome = m_GenomeCopy;

  const double perturbationLength = m_Radius * 0.75;

  CellularAggregateBase * aggregate = GetCellularAggregate();

  siblingA->m_ParentIdentifier = m_SelfIdentifier;
  siblingB->m_ParentIdentifier = m_SelfIdentifier;

  aggregate->Add( siblingA, siblingB, perturbationLength );

  this->MarkForRemoval();

}




/**
 *    Create a New cell
 *    this method behave like a factory, it is 
 *    intended to be overloaded in any class 
 *    deriving from Cell.
 */ 
template<unsigned int NSpaceDimension>
Cell<NSpaceDimension> *
Cell<NSpaceDimension>
::CreateNew(void) 
{
  Cell * cell = new Cell;
  cell->m_ParentIdentifier = m_SelfIdentifier;
  return cell;
}


/**
 *    Create a New Egg Cell
 *    this method behave like a factory, it is 
 *    intended to be overloaded in any class 
 *    deriving from Cell.
 */ 
template<unsigned int NSpaceDimension>
Cell<NSpaceDimension> *
Cell<NSpaceDimension>
::CreateEgg(void) 
{
  Superclass::Initialize();
  Cell * cell = new Cell;
  cell->m_ParentIdentifier = 0;
  cell->m_SelfIdentifier = 1;
  cell->m_Generation = 0;

  cell->m_Genome = new GenomeType;

  cell->ComputeGeneNetwork();
  cell->SecreteProducts();

  return cell;
}





/**
 *    Clear the cumulator for applied forces
 */ 
template<unsigned int NSpaceDimension>
void
Cell<NSpaceDimension>
::ClearForce(void) 
{
  m_Force.Fill( 0.0f );
  m_Pressure  = 0.0f;
}


/**
 *    Return the cumulated force
 */ 
template<unsigned int NSpaceDimension>
const typename Cell<NSpaceDimension>::VectorType &
Cell<NSpaceDimension>
::GetForce(void) const
{
  return m_Force;
}



/**
 *    Return a pointer to the Cellular Aggregate
 */ 
template<unsigned int NSpaceDimension>
CellularAggregateBase *
Cell<NSpaceDimension>
::GetCellularAggregate(void) 
{
  return m_Aggregate;
}



/**
 *    Return a const pointer to the Cellular Aggregate
 */ 
template<unsigned int NSpaceDimension>
const CellularAggregateBase *
Cell<NSpaceDimension>
::GetCellularAggregate(void) const
{
  return m_Aggregate;
}






/**
 *   Set Cellular Aggregate
 */ 
template<unsigned int NSpaceDimension>
void
Cell<NSpaceDimension>
::SetCellularAggregate( CellularAggregateBase * cells ) 
{
  m_Aggregate = cells;
}




/**
 *    Add a force to the cumulator
 */ 
template<unsigned int NSpaceDimension>
void
Cell<NSpaceDimension>
::AddForce( const VectorType & force )
{
  if( m_ChemoAttractantLevel > ChemoAttractantLowThreshold &&
      m_ChemoAttractantLevel < ChemoAttractantHighThreshold   )
    {
    double factor = 1.0 / pow( m_Radius, (double)(NSpaceDimension) );
    m_Force    += force;
    m_Pressure += force.GetNorm() * factor;
    }
  else
    {
    // no force so it is fixed in place....
    }
}


/**
 *    Programmed Cell Death 
 *    This is the cellular equivalent of suicide.
 */ 
template<unsigned int NSpaceDimension>
void
Cell<NSpaceDimension>
::Apoptosis(void) 
{
  this->Superclass::Apoptosis();

  CellularAggregateBase * aggregate = GetCellularAggregate();
  // "this" cell will be destroyed here
  aggregate->Remove( this ); 

}




/**
 *    Execute a time step in the life of the cell.
 *    This is one step in the cell cycle.
 *
 *    Nutrients are acquired
 *    Energy is acquired
 *    If conditions allow it, the cell will grow
 *    The position will be updated according to
 *    applied forces
 */ 
template<unsigned int NSpaceDimension>
void
Cell<NSpaceDimension>
::AdvanceTimeStep(void) 
{

  // get input from the environment
  this->ReceptorsReading(); 

  // update the level of expression of all the
  // genes in the gene network
  this->ComputeGeneNetwork();

  // this methods produce the effects of gene
  // activation and protein synthesis. It is
  // mostly used for secreting proteins already
  // synthetized in the ComputeGeneNetwork method.
  this->SecreteProducts();

  // If this happens, it is an
  // emergency situation: Do it first.
  if( this->CheckPointApoptosis() )
    {
    m_CycleState = Apop;
    }


  switch( m_CycleState )
  {
  case M: // Mitosis
    m_CycleState = Gap1;
    break;
  case Gap1: // Gap 1 : growing
    {
    if( this->CheckPointDNAReplication() )
      {
      m_CycleState = S;
      }

    break;
    }
  case S:
    m_CycleState = Gap2;
    break;
  case Gap2:
    if( this->CheckPointMitosis() )
      {
      m_CycleState = M;
      }
    break;
  case Gap0:
    // The cell is in cell cycle arrest
    m_CycleState = Gap0;
    break;
  case Apop:
    m_CycleState = Apop;
    break;
  }



  // Atomaton : Execute action
  switch( m_CycleState )
  {
  case M:  // Mitosis
    // This is a terminal action. The implementation of the cell 
    // is destroyed after division. Our abstraction assumes that 
    // the cell disapears and two new cell are created.
    this->Mitosis();
    break;
  case Gap1:
    // Eat and grow
    this->NutrientsIntake();
    this->EnergyIntake();
    this->Grow();
    break;
  case Gap0:
    this->NutrientsIntake();
    this->EnergyIntake();
    break;
  case S:
    this->DNAReplication();
    break; 
  case Gap2:
    break;
  case Apop:
    this->Apoptosis();
    break;
  }
}

/**
 *    Reading substrate using receptors
 */
template<unsigned int NSpaceDimension>
void
Cell<NSpaceDimension>
::ReceptorsReading(void) 
{
  m_Genome->SetExpressionLevel( Pressurin, m_Pressure );
  
  float substrate0 = m_Aggregate->GetSubstrateValue( m_SelfIdentifier, 0 );

  m_ChemoAttractantLevel = substrate0;

}




}  // end namespace bio

}  // end namespace itk

#endif

