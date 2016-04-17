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
#ifndef itkBioCell_hxx
#define itkBioCell_hxx

#include "itkBioCell.h"
#include "itkMath.h"
#include "vnl/vnl_sample.h"
#include <new>

namespace itk
{
namespace bio
{
/**
 *    Constructor Lonely Cell
 */
template< unsigned int NSpaceDimension >
Cell< NSpaceDimension >
::Cell() :
  m_Aggregate(ITK_NULLPTR)
{
  m_Force.Fill(0.0f);

  // Genome pointers are set to ITK_NULLPTR in the superclass.
}

/**
 *    Destructor
 */
template< unsigned int NSpaceDimension >
Cell< NSpaceDimension >
::~Cell()
{
  // Genomes are released in the destructor of the superclass.
}

/**
 *    Cell Division
 */

template< unsigned int NSpaceDimension >
void
Cell< NSpaceDimension >
::Mitosis(void)
{
  // Create the two daughters.
  Cell *siblingA = new Cell;
  Cell *siblingB = new Cell;

  // Broad compensation for Volume distribution among daugthers.
  // The type of root should depend on the Dimension...
  siblingA->m_Radius   = m_Radius / std::sqrt(2.0f);
  siblingB->m_Radius   = m_Radius / std::sqrt(2.0f);

  // Update Teleomeres
  siblingA->m_Generation = m_Generation + 1;
  siblingB->m_Generation = m_Generation + 1;

  // Prepare to separate them by a specific distance.
  // This helps to avoid infinite interaction forces
  // just after cellular division.
  const double perturbationLength = m_Radius * 0.75;

  // Register the parent
  siblingA->m_ParentIdentifier = m_SelfIdentifier;
  siblingB->m_ParentIdentifier = m_SelfIdentifier;

  // Pass the genome to each daughter cell
  siblingA->m_Genome = m_Genome;
  siblingB->m_Genome = m_GenomeCopy;

  // Mark that the genome pointer is not owned by this cell anymore.
  m_Genome     = ITK_NULLPTR;
  m_GenomeCopy = ITK_NULLPTR;

  // Register both daughter cells with the CellularAggregate.
  CellularAggregateBase *aggregate = this->GetCellularAggregate();
  aggregate->Add(siblingA, siblingB, perturbationLength);

  // Mark this cell for being removed from the Aggregate and deleted.
  this->MarkForRemoval();
}

/**
 *    Create a New Egg Cell
 *    This method behaves like a factory.  It is
 *    intended to be overloaded in any class
 *    deriving from Cell.
 */
template< unsigned int NSpaceDimension >
Cell< NSpaceDimension > *
Cell< NSpaceDimension >
::CreateEgg(void)
{
  Cell *cell = new Cell;

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
template< unsigned int NSpaceDimension >
void
Cell< NSpaceDimension >
::ClearForce(void)
{
  m_Force.Fill(0.0f);
  m_Pressure  = 0.0f;
}

/**
 *    Return the cumulated force
 */
template< unsigned int NSpaceDimension >
const typename Cell< NSpaceDimension >::VectorType &
Cell< NSpaceDimension >
::GetForce(void) const
{
  return m_Force;
}

/**
 *    Return a pointer to the Cellular Aggregate
 */
template< unsigned int NSpaceDimension >
CellularAggregateBase *
Cell< NSpaceDimension >
::GetCellularAggregate(void)
{
  return m_Aggregate;
}

/**
 *    Return a const pointer to the Cellular Aggregate
 */
template< unsigned int NSpaceDimension >
const CellularAggregateBase *
Cell< NSpaceDimension >
::GetCellularAggregate(void) const
{
  return m_Aggregate;
}

/**
 *   Set Cellular Aggregate
 */
template< unsigned int NSpaceDimension >
void
Cell< NSpaceDimension >
::SetCellularAggregate(CellularAggregateBase *cells)
{
  m_Aggregate = cells;
}

/**
 *    Add a force to the cumulator
 */
template< unsigned int NSpaceDimension >
void
Cell< NSpaceDimension >
::AddForce(const VectorType & force)
{
  if ( m_ChemoAttractantLevel > ChemoAttractantLowThreshold
       && m_ChemoAttractantLevel < ChemoAttractantHighThreshold   )
    {
    double factor = 1.0 / std::pow( m_Radius, (double)( NSpaceDimension ) );
    m_Force += force;
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
template< unsigned int NSpaceDimension >
void
Cell< NSpaceDimension >
::Apoptosis(void)
{
  // This call will release the Genomes
  this->Superclass::Apoptosis();

  CellularAggregateBase *aggregate = GetCellularAggregate();

  // "this" cell will be destroyed here
  aggregate->Remove(this);
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
template< unsigned int NSpaceDimension >
void
Cell< NSpaceDimension >
::AdvanceTimeStep(void)
{
  // get input from the environment
  this->ReceptorsReading();

  // update the level of expression of all the
  // genes in the gene network
  this->ComputeGeneNetwork();

  // this method produces the effects of gene
  // activation and protein synthesis. It is
  // mostly used for secreting proteins already
  // synthetized in the ComputeGeneNetwork method.
  this->SecreteProducts();

  // If this happens, it is an
  // emergency situation: Do it first.
  if ( this->CheckPointApoptosis() )
    {
    m_CycleState = Apop;
    }

  switch ( m_CycleState )
    {
    case M: // Mitosis
      m_CycleState = Gap1;
      break;
    case Gap1:
      {
      // Gap 1 : growing
      if ( this->CheckPointDNAReplication() )
        {
        m_CycleState = S;
        }
      break;
      }
    case S:
      m_CycleState = Gap2;
      break;
    case Gap2:
      if ( this->CheckPointMitosis() )
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
  switch ( m_CycleState )
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
template< unsigned int NSpaceDimension >
void
Cell< NSpaceDimension >
::ReceptorsReading(void)
{
  m_Genome->SetExpressionLevel(Pressurin, m_Pressure);

  float substrate0 = m_Aggregate->GetSubstrateValue(m_SelfIdentifier, 0);

  m_ChemoAttractantLevel = substrate0;
}
}  // end namespace bio
}  // end namespace itk

#endif
