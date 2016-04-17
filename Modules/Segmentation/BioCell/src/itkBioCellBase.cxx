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
#include "itkBioCellBase.h"

#include "vnl/vnl_sample.h"

namespace itk
{
namespace bio
{
CellBase::ColorType CellBase:: DefaultColor;

double CellBase:: DefaultRadius         =        1.00;            // microns

double CellBase:: GrowthRadiusIncrement =        0.01;            // microns
double CellBase:: GrowthRadiusLimit     =        2.00;            // microns

SizeValueType CellBase:: MaximumGenerationLimit =        30L;     // 30th
                                                                  // generation

SizeValueType CellBase:: GrowthMaximumLatencyTime    =   50;
SizeValueType CellBase:: DivisionMaximumLatencyTime  =   50;

double CellBase:: NutrientSelfRepairLevel  =       0;
double CellBase:: EnergySelfRepairLevel    =       0;

double CellBase:: DefaultEnergyIntake      =       1;
double CellBase:: DefaultNutrientsIntake   =       1;

SizeValueType CellBase:: Counter = 0;     // number of cells created

CellBase::GeneIdType CellBase:: RedGene   = "Red";
CellBase::GeneIdType CellBase:: GreenGene = "Green";
CellBase::GeneIdType CellBase:: BlueGene  = "Blue";
CellBase::GeneIdType CellBase:: Cdk2E     = "Cdk2E";
CellBase::GeneIdType CellBase:: Caspase   = "Caspase";
CellBase::GeneIdType CellBase:: Pressurin = "Pressurin";

double CellBase:: ChemoAttractantLowThreshold  = 200.0f;
double CellBase:: ChemoAttractantHighThreshold = 255.0f;

CellBase::ColorType CellBase:: WellNourishedColor;
CellBase::ColorType CellBase:: HopefullColor;
CellBase::ColorType CellBase:: StarvingColor;

/**
 *    Constructor Lonely Cell
 */
CellBase
::CellBase()
{
  m_Genome      = ITK_NULLPTR;
  m_GenomeCopy  = ITK_NULLPTR;

  m_Radius      = DefaultRadius;
  m_Color       = DefaultColor;

  m_Pressure  = 0.0f;

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
  m_GrowthLatencyTime   = static_cast< SizeValueType >(
    vnl_sample_uniform( 0, this->GetGrowthMaximumLatencyTime() ) );

  // add a random time before starting to grow
  m_DivisionLatencyTime = static_cast< SizeValueType >(
    vnl_sample_uniform( 0, this->GetDivisionMaximumLatencyTime() ) );

  m_ScheduleApoptosis    = false;
  m_ChemoAttractantLevel = 200.0f;

  // too young to die...
  m_MarkedForRemoval = false;
}

/**
 *    Destructor
 */
CellBase
::~CellBase()
{
  delete m_Genome;
  m_Genome = ITK_NULLPTR;
  delete m_GenomeCopy;
  m_GenomeCopy = ITK_NULLPTR;
}

/**
 *    DNA Replication
 */
void
CellBase
::DNAReplication(void)
{
  m_GenomeCopy = new GenomeType;
  m_GenomeCopy->Copy(*m_Genome);
}

/**
 *    Programmed Cell Death
 *    This is the cellular equivalent of suicide.
 */
void
CellBase
::Apoptosis(void)
{
  delete m_Genome;
  m_Genome = ITK_NULLPTR;
  delete m_GenomeCopy;
  m_GenomeCopy = ITK_NULLPTR;
}

/**
 *    Check point after division
 *    This check point will control
 *    the entrance in the growth stage.
 *    It returns true when conditions
 *    required for growth are satisfied.
 */
bool
CellBase
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
CellBase
::CheckPointDNAReplication(void)
{
  // radius & teleomerasa counting should be removed from here
  // and be related to Cdk expression by using proteins like P53
  // The radius should be estimated by a cytoskeleton-related protein.
  const bool fatality = ( m_Generation < MaximumGenerationLimit );
  const bool radius   = ( m_Radius >= GrowthRadiusLimit );

  bool         isOkToReplicate = true;
  const double cdk2E = m_Genome->GetExpressionLevel(Cdk2E);

  if ( cdk2E < 0.8 )
    {
    isOkToReplicate = false;
    }

  bool tooEarlyToReplicate = true;
  if ( m_DivisionLatencyTime )
    {
    m_DivisionLatencyTime--;
    }
  else
    {
    tooEarlyToReplicate = false;
    }

  bool goodCellMatrix = false;
  if ( !m_ScheduleApoptosis )
    {
    if ( m_ChemoAttractantLevel > ChemoAttractantLowThreshold
         && m_ChemoAttractantLevel < ChemoAttractantHighThreshold )
      {
      goodCellMatrix = true;
      }
    }

  return ( radius && fatality && isOkToReplicate
           && !tooEarlyToReplicate && goodCellMatrix );
}

/**
 *    Check point before dividing the cell in two daughter cells
 *    at this point DNA replication has already been performed
 *    as well as DNA proofreading and error corrections. This
 *    check point in principle should test if the resulting
 *    genomes satisfy the quality standards of a living cell.
 */
bool
CellBase
::CheckPointMitosis(void)
{
  const bool DNAProofRead = ( m_GenomeCopy && m_Genome );

  if ( !DNAProofRead )
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
CellBase
::CheckPointApoptosis(void)
{
  bool executeApoptosis;

  if (  m_Genome->GetExpressionLevel(Caspase) > 0.8 )
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
 *    Initialize common variables
 *    All this should go away once we setup Gene Networks for controlling the cell.
 */
void
CellBase
::Initialize(void)
{
  CellBase::SetGrowthMaximumLatencyTime(100);
  CellBase::SetDivisionMaximumLatencyTime(100);

  CellBase::SetDefaultRadius(1.0);
  CellBase::SetGrowthRadiusIncrement(0.01);
  CellBase::SetGrowthRadiusLimit(2.00);

  SetMaximumGenerationLimit(40);   // it should use Teleomeres for implementing
                                   // this

  WellNourishedColor.Set(0.0f, 0.0f, 1.0f);
  HopefullColor.Set(0.0f, 1.0f, 0.0f);
  StarvingColor.Set(1.0f, 0.0f, 0.0f);

  SetDefaultColor(HopefullColor);
}

/**
 *  Mark this cell for removal
 *  The cellular aggregate will remove
 *  this cell from its list at the earliest occasion
 */
void
CellBase
::MarkForRemoval(void)
{
  m_MarkedForRemoval = true;
}

/**
 *  Mark this cell for removal
 *  The cellular aggregate will remove
 *  this cell from its list at the earliest occasion
 */
bool
CellBase
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
CellBase
::Grow(void)
{
  if ( m_GrowthLatencyTime )
    {
    m_GrowthLatencyTime--;
    return;
    }

  if ( m_NutrientsReserveLevel > NutrientSelfRepairLevel
       && m_EnergyReserveLevel > EnergySelfRepairLevel )
    {
    m_Radius += GrowthRadiusIncrement;
    if ( m_Radius > GrowthRadiusLimit )
      {
      m_Radius = GrowthRadiusLimit;
      }
    }
}

/**
 *    Set Growth Latency Time
 */
void
CellBase
::SetGrowthMaximumLatencyTime(SizeValueType latency)
{
  CellBase::GrowthMaximumLatencyTime = latency;
}

/**
 *    Get Growth Latency Time
 */
SizeValueType
CellBase
::GetGrowthMaximumLatencyTime(void)
{
  return CellBase::GrowthMaximumLatencyTime;
}

/**
 *    Return the ID  of this cell
 */
CellBase::IdentifierType
CellBase
::GetSelfIdentifier(void) const
{
  return m_SelfIdentifier;
}

/**
 *    Return the ID  of the parent cell
 */
CellBase::IdentifierType
CellBase
::GetParentIdentifier(void) const
{
  return m_ParentIdentifier;
}

/**
 *    Return the radius
 */
double
CellBase
::GetRadius(void) const
{
  return m_Radius;
}

/**
 *    Return the Color
 */
CellBase::ColorType
CellBase
::GetColor(void) const
{
  return m_Color;
}

/**
 *    Set the value of the initial cell radius.
 */
void
CellBase
::SetDefaultRadius(double value)
{
  DefaultRadius = value;
}

/**
 *    Set the value of the limiting cell radius.
 *    This is a static value used for the whole
 *    cellular aggregate
 */
void
CellBase
::SetGrowthRadiusLimit(double value)
{
  GrowthRadiusLimit = value;
}

/**
 *    Set the amount of Energy that is needed for Self-repair.
 *    Cells that go below this level will degrade and enter
 *    termination stage.
 */
void
CellBase
::SetEnergySelfRepairLevel(double value)
{
  EnergySelfRepairLevel = value;
}

/**
 *    Set the amount of Nutrients that are needed for Self-repair.
 *    Cells that go below this level will degrade and enter
 *    termination stage.
 */
void
CellBase
::SetNutrientSelfRepairLevel(double value)
{
  NutrientSelfRepairLevel = value;
}

/**
 *    Set the value of the limit of cell generation.
 *    After this generation cells will stop dividing
 *    A mechanism similar to the inhibition of Telomerase
 *    that impose a limit to the maximum number of times
 *    that the genome can be replicated.
 */
void
CellBase
::SetMaximumGenerationLimit(SizeValueType generationLimit)
{
  MaximumGenerationLimit = generationLimit;
}

/**
 *    Get the value of the limiting cell radius
 *    this is a static value used for the whole
 *    cellular aggregate
 */
double
CellBase
::GetGrowthRadiusLimit(void)
{
  return GrowthRadiusLimit;
}

/**
 *    Set the value of the increment in cellular
 *    radius at each time step.
 *    This is a static value used for the whole
 *    cellular aggregate
 */
void
CellBase
::SetGrowthRadiusIncrement(double value)
{
  GrowthRadiusIncrement = value;
}

/**
 *    Ingestion of nutrients
 */
void
CellBase
::NutrientsIntake(void)
{
  m_NutrientsReserveLevel += DefaultNutrientsIntake;
}

/**
 *    Acquisition of energy
 */
void
CellBase
::EnergyIntake(void)
{
  m_EnergyReserveLevel += DefaultEnergyIntake;
}

/**
 *   Compute the Gene Network
 *   This method updates the level of expression of
 *   all the genes in the cell's genome.
 *   see: http://www.ingeneue.org  for details
 */
void
CellBase
::ComputeGeneNetwork(void)
{
  // Default level of pigments
  m_Genome->SetExpressionLevel(CellBase::RedGene,   1.0);
  m_Genome->SetExpressionLevel(CellBase::GreenGene, 1.0);
  m_Genome->SetExpressionLevel(CellBase::BlueGene,  1.0);

  // Color the cell according to pressure.
  // This is done by generating pigments under
  // the influence of pressure.
  const double pressurinLevel = m_Genome->GetExpressionLevel(Pressurin);

  const double red = GenomeType::Sigmoide(5.0, 1.0, pressurinLevel);

  m_Genome->SetExpressionLevel(RedGene,       red);
  m_Genome->SetExpressionLevel(BlueGene,  1.0 - red);
  m_Genome->SetExpressionLevel(GreenGene,     0.0);

  // Color the Cell according to the substrate.
  // This is done by generating pigments.
  // This color overrides the selection of the Pressure...

  if ( m_ChemoAttractantLevel > ChemoAttractantHighThreshold )
    {
    m_Genome->SetExpressionLevel( RedGene,   WellNourishedColor.GetRed() );
    m_Genome->SetExpressionLevel( GreenGene, WellNourishedColor.GetGreen() );
    m_Genome->SetExpressionLevel( BlueGene,  WellNourishedColor.GetBlue() );
    }
  else if ( m_ChemoAttractantLevel > ChemoAttractantLowThreshold )
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
  const double cdk2E = GenomeType::Sigmoide(2.0, -0.5, pressurinLevel);
  m_Genome->SetExpressionLevel(Cdk2E, cdk2E);

  // If the pressure is really high, then commit suicide
  const double caspase = GenomeType::Sigmoide(3.0, 90.0, pressurinLevel);
  m_Genome->SetExpressionLevel(Caspase, caspase);
}

/**
 *   Secrete synthetized products resulting from
 *   the gene network update
 */
void
CellBase
::SecreteProducts(void)
{
  typedef ColorType::ValueType ColorValueType;

  m_Color.SetRed( ColorValueType( m_Genome->GetExpressionLevel(RedGene) ) );
  m_Color.SetGreen( ColorValueType( m_Genome->GetExpressionLevel(GreenGene) ) );
  m_Color.SetBlue( ColorValueType( m_Genome->GetExpressionLevel(BlueGene) ) );
}

/**
 *    Set default Color
 */
void
CellBase
::SetDefaultColor(const ColorType & color)
{
  DefaultColor = color;
}

/**
 *    Reset the counter
 */
void
CellBase
::ResetCounter(void)
{
  Counter = 0;
}

/**
 *    Set Division Latency Time
 */
void
CellBase
::SetDivisionMaximumLatencyTime(SizeValueType latency)
{
  CellBase::DivisionMaximumLatencyTime = latency;
}

/**
 *    Get Division Latency Time
 */
SizeValueType
CellBase
::GetDivisionMaximumLatencyTime(void)
{
  return CellBase::DivisionMaximumLatencyTime;
}

/**
 *    Set ChemoAttractantLowThreshold
 */
void
CellBase
::SetChemoAttractantLowThreshold(double lowvalue)
{
  CellBase::ChemoAttractantLowThreshold = lowvalue;
}

/**
 *    Set ChemoAttractantHighThreshold
 */
void
CellBase
::SetChemoAttractantHighThreshold(double highvalue)
{
  CellBase::ChemoAttractantHighThreshold = highvalue;
}
}  // end namespace bio
}  // end namespace itk
