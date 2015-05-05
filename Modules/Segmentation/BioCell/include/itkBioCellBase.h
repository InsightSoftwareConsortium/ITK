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
#ifndef itkBioCellBase_h
#define itkBioCellBase_h

#include "itkRGBPixel.h"
#include "itkPoint.h"
#include "itkIntTypes.h"
#include "itkBioGenome.h"
#include "ITKBioCellExport.h"

namespace itk
{
namespace bio
{
/** \class CellBase
 * \brief Non-templated Base class from which the templated Cell classes will be derived.
 *
 * Derived classes are instantiated for a specific space dimension.
 * \ingroup ITKBioCell
 */
class ITKBioCell_EXPORT CellBase
{
public:
  typedef   itk::RGBPixel< float > ColorType;
  typedef   itk::IdentifierType    IdentifierType;
  typedef   itk::bio::Genome       GenomeType;
  typedef   GenomeType::GeneIdType GeneIdType;

  virtual ColorType GetColor() const;

  double GetRadius() const;

  IdentifierType GetSelfIdentifier() const;

  IdentifierType GetParentIdentifier() const;

  enum CellCycleState
  {
    M = 1UL,
    Gap1,
    S,
    Gap2,
    Gap0,
    Apop
  };

protected:
  CellBase();
  virtual ~CellBase();

  virtual void Grow();

  virtual void DNAReplication();

  virtual void Apoptosis();

  virtual void EnergyIntake();

  virtual void NutrientsIntake();

  virtual void ComputeGeneNetwork();

  virtual void SecreteProducts();

  virtual bool CheckPointGrowth();

  virtual bool CheckPointDNAReplication();

  virtual bool CheckPointMitosis();

  virtual bool CheckPointApoptosis();

  void MarkForRemoval();

  // Static Members
  static ColorType DefaultColor;

  static GeneIdType BlueGene;        // Pigment genes
  static GeneIdType RedGene;
  static GeneIdType GreenGene;
  static GeneIdType Cdk2E;           // cell cycle control  genes
  static GeneIdType Caspase;         // cleavage enzyme: apoptosis effector
  static GeneIdType Pressurin;       // signal from micro-tubules subject to
                                     // stress

  static SizeValueType MaximumGenerationLimit;
  static SizeValueType GrowthMaximumLatencyTime;
  static SizeValueType DivisionMaximumLatencyTime;

  static double EnergySelfRepairLevel;
  static double NutrientSelfRepairLevel;

  static double DefaultEnergyIntake;
  static double DefaultNutrientsIntake;

  static SizeValueType Counter;

  static ColorType WellNourishedColor;
  static ColorType HopefullColor;
  static ColorType StarvingColor;

  static double ChemoAttractantLowThreshold;
  static double ChemoAttractantHighThreshold;

  GenomeType *m_Genome;
  GenomeType *m_GenomeCopy;

  static double DefaultRadius;
  static double GrowthRadiusLimit;
  static double GrowthRadiusIncrement;

public:

  virtual bool MarkedForRemoval() const;

  static void SetDefaultRadius(double);

  static void SetGrowthRadiusLimit(double);

  static void SetGrowthRadiusIncrement(double);

  static void SetEnergySelfRepairLevel(double);

  static void SetNutrientSelfRepairLevel(double);

  static void SetDefaultColor(const ColorType & color);

  static void SetChemoAttractantHighThreshold(double);

  static void SetChemoAttractantLowThreshold(double);

  static void SetGrowthMaximumLatencyTime(SizeValueType latency);

  static SizeValueType GetGrowthMaximumLatencyTime();

  static double GetGrowthRadiusLimit();

  static void SetMaximumGenerationLimit(SizeValueType);

  static void SetDivisionMaximumLatencyTime(SizeValueType);

  static SizeValueType GetDivisionMaximumLatencyTime();

  static void ResetCounter();

  static void Initialize(); // define values in static variables.

protected:
  double m_Pressure;

  ColorType m_Color;

  double m_Radius;
  double m_EnergyReserveLevel;
  double m_NutrientsReserveLevel;

  SizeValueType m_GrowthLatencyTime;

  IdentifierType m_ParentIdentifier;
  IdentifierType m_SelfIdentifier;

  SizeValueType m_Generation;

  CellCycleState m_CycleState;

  bool          m_MarkedForRemoval;
  SizeValueType m_DivisionLatencyTime;

  bool   m_ScheduleApoptosis;
  double m_ChemoAttractantLevel;
};
} // end namespace bio
} // end namespace itk

#endif
