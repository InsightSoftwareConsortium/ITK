/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBioCell.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/



#include "itkBioCell.h"
#include "itkBioCellularAggregate.h"
#include "vnl/vnl_math.h"
#include "vnl/vnl_sample.h"
#include <new>


namespace itk {

namespace bio {


  
Cell::ColorType    Cell::DefaultColor;

double             Cell::DefaultRadius         =        1.00; // microns


double             Cell::GrowthRadiusIncrement =        0.01; // microns
double             Cell::GrowthRadiusLimit     =        2.00; // microns
unsigned long      Cell::MaximumGenerationLimit =        30L; // 30th generation 

unsigned long      Cell::GrowthMaximumLatencyTime    =   50; 
unsigned long      Cell::DivisionMaximumLatencyTime  =   50; 

double             Cell::NutrientSelfRepairLevel  =       0; 
double             Cell::EnergySelfRepairLevel    =       0; 

double             Cell::DefaultEnergyIntake      =       1; 
double             Cell::DefaultNutrientsIntake   =       1; 

unsigned long      Cell::Counter = 0; // number of cells created

Cell::GeneIdType   Cell::RedGene   = "Red";
Cell::GeneIdType   Cell::GreenGene = "Green";
Cell::GeneIdType   Cell::BlueGene  = "Blue";
Cell::GeneIdType   Cell::Cdk2E     = "Cdk2E";
Cell::GeneIdType   Cell::Caspase   = "Caspase";
Cell::GeneIdType   Cell::Pressurin = "Pressurin";

double             Cell::ChemoAttractantLowThreshold  = 200.0f;
double             Cell::ChemoAttractantHighThreshold = 255.0f;

Cell::ColorType    Cell::WellNourishedColor;
Cell::ColorType    Cell::HopefullColor;
Cell::ColorType    Cell::StarvingColor;



/**
 *    Constructor Lonely Cell
 */ 
Cell
::Cell()
{

  m_Genome      = 0;
  m_GenomeCopy  = 0;
  
  m_Radius      = DefaultRadius;
  m_Color       = DefaultColor;
  
  m_Pressure  = 0.0f;
  m_Force.Fill( 0.0f );

  m_ParentIdentifier = 0;    // Parent cell has to write here

  // The first Cell is numbered as 1
  Counter++;
  m_SelfIdentifier = Counter;  

  m_Generation     = 0;
  m_CycleState     = Gap1;  // cells are created in Gap1 state

  // Start with minimum reserves
  m_NutrientsReserveLevel = NutrientSelfRepairLevel + DefaultNutrientsIntake;
  m_EnergyReserveLevel    = EnergySelfRepairLevel   + DefaultEnergyIntake;

  // delay before starting to grow after Mitosis
  m_GrowthLatencyTime   = static_cast<unsigned long>( 
       vnl_sample_uniform( 0UL, this->GetGrowthMaximumLatencyTime()) );

  // add a random time before starting to grow
  m_DivisionLatencyTime = static_cast<unsigned long>(
      vnl_sample_uniform( 0, this->GetDivisionMaximumLatencyTime() ) );

  m_ScheduleApoptosis    = false;
  m_ChemoAttractantLevel = 200.0f;

  // too young to die...
  m_MarkedForRemoval = false;

}




/**
 *    Destructor   
 */ 
Cell
::~Cell()
{
}



/**
 *    Cell Division
 */ 
void
Cell
::Mitosis(void) 
{

  Cell * siblingA = dynamic_cast<Cell*>( this->CreateNew() );
  Cell * siblingB = dynamic_cast<Cell*>( this->CreateNew() );

  siblingA->m_Radius   = m_Radius / sqrt( 2.0f );
  siblingB->m_Radius   = m_Radius / sqrt( 2.0f );

  siblingA->m_Generation = m_Generation + 1;
  siblingB->m_Generation = m_Generation + 1;

  // Pass the genome to each daughter cell
  siblingA->m_Genome = m_Genome;
  siblingB->m_Genome = m_GenomeCopy;

  // Create a perturbation for separating the daugther cells
  Cell::VectorType perturbationVector;
  for(unsigned int d=0; d<PointDimension; d++)
    {
    perturbationVector[d] = 
           vnl_sample_uniform( -1.0f, 1.0f ); 
    }

  const double perturbationLength = m_Radius * 0.75;

  const double norm = perturbationVector.GetNorm();
  if( vnl_math_abs( norm ) > 1e-10 ) 
    {
    perturbationVector *= perturbationLength / norm;
    }
  else
    {
    // this event should rarely happen... very rarely
    std::cout << "Cell:: unlikely event happend" << std::endl;
    perturbationVector[0] = perturbationLength;
    }

  CellularAggregate * aggregate = GetCellularAggregate();

  siblingA->m_ParentIdentifier = m_SelfIdentifier;
  siblingB->m_ParentIdentifier = m_SelfIdentifier;

  aggregate->Add( siblingA, siblingB, perturbationVector );

  this->MarkForRemoval();

}



/**
 *    DNA Replication 
 */ 
void
Cell
::DNAReplication(void) 
{
  m_GenomeCopy = new GenomeType;
  m_GenomeCopy->Copy( *m_Genome );
}




/**
 *    Programmed Cell Death 
 *    This is the cellular equivalent of suicide.
 */ 
void
Cell
::Apoptosis(void) 
{

  delete m_Genome;
  delete m_GenomeCopy;

  m_Genome     = 0;
  m_GenomeCopy = 0;

  CellularAggregate * aggregate = GetCellularAggregate();
  // "this" cell will be destroyed here
  aggregate->Remove( this ); 

}



/**
 *    Check point after division
 *    This check point will control
 *    the entrance in the growth stage.
 *    It returns true when conditions
 *    required for growth are satisfied.
 */ 
bool
Cell
::CheckPointGrowth(void) 
{
  return true;
}


/**
 *    Check point before initiating DNA replication.
 *    This check point controls the entrance in the 
 *    duplication of the genome by DNA synthesis, also
 *    known as the S phase of the Cell cycle.
 *    This method returns true when conditions required 
 *    for DNA replication are satisfied.
 */ 
bool
Cell
::CheckPointDNAReplication(void) 
{
  // radius & teleomerasa counting should be removed from here
  // and be related to Cdk expression by using proteins like P53
  // The radius should be estimated by a cytoskeleton-related protein.
  const bool fatality = (m_Generation < MaximumGenerationLimit );
  const bool radius   = (m_Radius >= GrowthRadiusLimit);

  bool isOkToReplicate = true;
  const double cdk2E = m_Genome->GetExpressionLevel( Cdk2E );
  if( cdk2E < 0.8 )
    {
    isOkToReplicate = false;
    }
  
  bool tooEarlyToReplicate = true;
  if( m_DivisionLatencyTime )
    {
    m_DivisionLatencyTime--;
    }
  else
    {
    tooEarlyToReplicate = false;
    }

  bool goodCellMatrix = false;
  if( !m_ScheduleApoptosis )
    {
    if( m_ChemoAttractantLevel > ChemoAttractantLowThreshold  &&
        m_ChemoAttractantLevel < ChemoAttractantHighThreshold    )
      {
      goodCellMatrix = true;
      }
    }


  return ( radius && fatality && isOkToReplicate && 
          !tooEarlyToReplicate && goodCellMatrix);
}



/**
 *    Check point before dividing the cell in two daughter cells
 *    at this point DNA replication has already been performed
 *    as well as DNA proofreading and error corrections. This 
 *    check point in principle shoult test if the resulting 
 *    genomes satisfy the quality standards of a living cell. 
 */
bool
Cell
::CheckPointMitosis(void) 
{
  const bool DNAProofRead = ( m_GenomeCopy && m_Genome );
  if( !DNAProofRead ) 
  {
    std::cerr << "PANIC: DNA failed ! " << std::endl;
  }
  return DNAProofRead;
}




/**
 *    Check point before apoptosis
 *    This check point will control
 *    the entrance in the apoptosis stage.
 *    It returns true when conditions
 *    required for apoptosis are satisfied.
 *    The cell will die in apoptosis.
 */ 
bool
Cell
::CheckPointApoptosis(void) 
{
  bool executeApoptosis = false;
  if(  m_Genome->GetExpressionLevel( Caspase ) > 0.8 )
    {
    executeApoptosis = true;
    }
  else
    {
    executeApoptosis = false;
    }
  return executeApoptosis;
}





/**
 *    Create a New cell
 *    this method behave like a factory, it is 
 *    intended to be overloaded in any class 
 *    deriving from Cell.
 */ 
Cell *
Cell 
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
Cell *
Cell 
::CreateEgg(void) 
{
  Cell::SetGrowthMaximumLatencyTime( 100 );
  Cell::SetDivisionMaximumLatencyTime( 100 );

  Cell::GrowthRadiusIncrement = 0.01;
  Cell::GrowthRadiusLimit     = 2.00;
 
  SetMaximumGenerationLimit( 40 ); // it should use Teleomeres for implementing this

  WellNourishedColor.Set(    0.0, 0.0, 1.0 );
  HopefullColor.Set(         0.0, 1.0, 0.0 );
  StarvingColor.Set(         1.0, 0.0, 0.0 );

  SetDefaultColor( HopefullColor );

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
 *  Mark this cell for removal
 *  The cellular aggregate with remove
 *  this cell from its list at the earliest occasion
 */ 
void
Cell 
::MarkForRemoval(void) 
{
  m_MarkedForRemoval = true;
}



/**
 *  Mark this cell for removal
 *  The cellular aggregate with remove
 *  this cell from its list at the earliest occasion
 */ 
bool
Cell 
::MarkedForRemoval(void) const
{
  return m_MarkedForRemoval;
}




/**
 *   Cell Growth
 *   Growth is conditioned to the availability of 
 *   nutrients and energy beyond the critical limit 
 *   of self-repair
 *   
 *   Growth is limited by a constraint in the size
 *   of cell's radius
 */ 
void
Cell
::Grow(void) 
{
  if( m_GrowthLatencyTime )
  {
    m_GrowthLatencyTime--;
    return;
  }

  if ( m_NutrientsReserveLevel > NutrientSelfRepairLevel &&
       m_EnergyReserveLevel    > EnergySelfRepairLevel       )
    {
    m_Radius += GrowthRadiusIncrement;
    if( m_Radius > GrowthRadiusLimit )
      {
      m_Radius = GrowthRadiusLimit;
      }
    }
}



/**
 *    Set Growth Latency Time
 */ 
void
Cell
::SetGrowthMaximumLatencyTime( unsigned long latency )
{
  Cell::GrowthMaximumLatencyTime = latency;
}




/**
 *    Get Growth Latency Time
 */ 
unsigned long 
Cell
::GetGrowthMaximumLatencyTime( void ) 
{
  return Cell::GrowthMaximumLatencyTime;
}





/**
 *    Clear the cumulator for applied forces
 */ 
void
Cell
::ClearForce(void) 
{
  m_Force.Fill( 0.0f );
  m_Pressure  = 0.0f;
}


/**
 *    Return the cumulated force
 */ 
const Cell::VectorType &
Cell
::GetForce(void) const
{
  return m_Force;
}



/**
 *    Return the ID  of this cell
 */ 
Cell::IdentifierType
Cell
::GetSelfIdentifier(void) const
{
  return m_SelfIdentifier;
}



/**
 *    Return the ID  of the parent cell
 */ 
Cell::IdentifierType
Cell
::GetParentIdentifier(void) const
{
  return m_ParentIdentifier;
}




/**
 *    Return a const pointer to the Cellular Aggregate
 */ 
const CellularAggregate *
Cell
::GetCellularAggregate(void) const
{
  return m_Aggregate;
}



/**
 *    Return a pointer to the Cellular Aggregate
 */ 
CellularAggregate *
Cell
::GetCellularAggregate(void) 
{
  return m_Aggregate;
}





/**
 *   Set Cellular Aggregate
 */ 
void
Cell
::SetCellularAggregate( CellularAggregate * cells ) 
{
  m_Aggregate = cells;
}



/**
 *    Return the radius 
 */ 
double
Cell
::GetRadius(void) const
{
  return m_Radius;
}


/**
 *    Return the Color 
 */ 
Cell::ColorType
Cell
::GetColor(void) const
{
  return m_Color;
}




/**
 *    Add a force to the cumulator
 */ 
void
Cell
::AddForce( const VectorType & force )
{
  if( m_ChemoAttractantLevel > ChemoAttractantLowThreshold &&
      m_ChemoAttractantLevel < ChemoAttractantHighThreshold   )
    {
    double factor = 1.0 / pow( m_Radius, Cell::Dimension );
    m_Force    += force;
    m_Pressure += force.GetNorm() * factor;
    }
  else
    {
    // no force so it is fixed in place....
    }
}




/**
 *    Set the value of the limiting cell radius
 *    this is a static value used for the whole
 *    cellular aggregate
 */ 
void
Cell
::SetGrowthRadiusLimit( double value ) 
{
  GrowthRadiusLimit = value;
}


/**
 *    Set the value of the limit of cell generation.
 *    After this generation cells will stop dividing
 *    A mechanism similar to the inhibition of Telomerase
 *    that impose a limit to the maximum number of times
 *    that the genome can be replicated.
 */ 
void
Cell
::SetMaximumGenerationLimit( unsigned long generationLimit )
{
  MaximumGenerationLimit = generationLimit;
}



/**
 *    Get the value of the limiting cell radius
 *    this is a static value used for the whole
 *    cellular aggregate
 */ 
double
Cell
::GetGrowthRadiusLimit( void ) 
{
  return GrowthRadiusLimit;
}


/**
 *    Set the value of the increment in cellular
 *    radius at each time step
 *    this is a static value used for the whole
 *    cellular aggregate
 */ 
void
Cell
::SetGrowthRadiusIncrement( double value ) 
{
  GrowthRadiusIncrement = value;
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
void
Cell
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
 *    Ingestion of nutrients
 */
void
Cell
::NutrientsIntake(void) 
{
  m_NutrientsReserveLevel += DefaultNutrientsIntake;
}



/**
 *    Acquisition of energy
 */
void
Cell
::EnergyIntake(void) 
{
  m_EnergyReserveLevel += DefaultEnergyIntake;
}



/**
 *    Reading substrate using receptors
 */
void
Cell
::ReceptorsReading(void) 
{
  m_Genome->SetExpressionLevel( Pressurin, m_Pressure );
  
  CellularAggregate::SubstrateType::PixelType substrate0 =
              m_Aggregate->GetSubstrateValue( m_SelfIdentifier, 0 );
  
  m_ChemoAttractantLevel = substrate0;


}



/**
 *   Compute the Gene Network
 *   This method update the level of expression of 
 *   all the genes in the cell's genome.
 *   see: http://www.ingeneue.org  for details
 */ 
void
Cell
::ComputeGeneNetwork(void) 
{
  // Default level of pigments
  m_Genome->SetExpressionLevel( Cell::RedGene,   1.0 );
  m_Genome->SetExpressionLevel( Cell::GreenGene, 1.0 );
  m_Genome->SetExpressionLevel( Cell::BlueGene,  1.0 );


  // Color the cell acording to pressure.
  // This is done by generating pigments under 
  // the influence of presure.
  const double pressurinLevel = m_Genome->GetExpressionLevel( Pressurin );

  const double red = GenomeType::Sigmoide( 5.0, 1.0, pressurinLevel );

  m_Genome->SetExpressionLevel( RedGene,       red );
  m_Genome->SetExpressionLevel( BlueGene,  1.0-red );
  m_Genome->SetExpressionLevel( GreenGene,     0.0 );

 
  // Color the Cell acording to the substrate.
  // This is done by generating pigments.
  // This color overrides the selection of the Presure...

  if( m_ChemoAttractantLevel > ChemoAttractantHighThreshold )
    {
    m_Genome->SetExpressionLevel( RedGene,   WellNourishedColor.GetRed() );
    m_Genome->SetExpressionLevel( GreenGene, WellNourishedColor.GetGreen() );
    m_Genome->SetExpressionLevel( BlueGene,  WellNourishedColor.GetBlue() );
    }
  else if( m_ChemoAttractantLevel > ChemoAttractantLowThreshold )
    {
    m_Genome->SetExpressionLevel( RedGene,   HopefullColor.GetRed() );
    m_Genome->SetExpressionLevel( GreenGene, HopefullColor.GetGreen() );
    m_Genome->SetExpressionLevel( BlueGene,  HopefullColor.GetBlue() );
    }
  else
    {
    m_Genome->SetExpressionLevel( RedGene,   StarvingColor.GetRed() );
    m_Genome->SetExpressionLevel( GreenGene, StarvingColor.GetGreen() );
    m_Genome->SetExpressionLevel( BlueGene,  StarvingColor.GetBlue() );
    }


  // Prevent cells from replicating if they are in a high pressure zone
  const double cdk2E = GenomeType::Sigmoide( 2.0, -0.5, pressurinLevel );
  m_Genome->SetExpressionLevel( Cdk2E, cdk2E );
  
  // If the pressure is really high, then commit suicide
  const double caspase = GenomeType::Sigmoide( 3.0, 90.0, pressurinLevel );
  m_Genome->SetExpressionLevel( Caspase, caspase );


}


/**
 *   Secrete synthetized products resulting from 
 *   the gene network update
 */ 
void
Cell
::SecreteProducts(void) 
{
  m_Color.SetRed(    m_Genome->GetExpressionLevel( RedGene   ) );
  m_Color.SetGreen(  m_Genome->GetExpressionLevel( GreenGene ) );
  m_Color.SetBlue(   m_Genome->GetExpressionLevel( BlueGene  ) );
}




/**
 *    Set default Color
 */
void
Cell
::SetDefaultColor( const ColorType & color )
{
  DefaultColor = color;
}



/**
 *    Reset the counter 
 */
void
Cell
::ResetCounter( void )
{
  Counter = 0;
}

 

/**
 *    Set Division Latency Time
 */ 
void
Cell
::SetDivisionMaximumLatencyTime( unsigned long latency )
{
  Cell::DivisionMaximumLatencyTime = latency;
}




/**
 *    Get Division Latency Time
 */ 
unsigned long 
Cell
::GetDivisionMaximumLatencyTime( void )
{
  return Cell::DivisionMaximumLatencyTime;
}




}  // end namespace bio

}  // end namespace itk

