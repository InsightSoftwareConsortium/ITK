/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBioCellBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBioCellBase_h
#define __itkBioCellBase_h

#include "itkRGBPixel.h"
#include "itkVector.h"
#include "itkPoint.h"
#include "itkBioGenome.h"

namespace itk {

namespace bio {



/** \class CellBase
 * \brief non-templated Base class from which the templated Cell classes will be derived.
 * Derived classes are instantiated for a specific space dimension.
 */
class CellBase 
{
public:
  typedef   itk::RGBPixel<float>                ColorType;
  typedef   unsigned long int                   IdentifierType;
  typedef   itk::bio::Genome                    GenomeType;
  typedef   GenomeType::GeneIdType              GeneIdType;


  virtual ColorType GetColor(void) const;
  
  double GetRadius(void) const;
  
  IdentifierType GetSelfIdentifier(void) const;
  IdentifierType GetParentIdentifier(void) const;
 
  enum CellCycleState {
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

  virtual void Grow(void);
  virtual void DNAReplication(void);
  virtual void Apoptosis(void);
  
  virtual void EnergyIntake(void);
  virtual void NutrientsIntake(void);
  virtual void ComputeGeneNetwork(void);
  virtual void SecreteProducts(void);

  virtual bool CheckPointGrowth(void);
  virtual bool CheckPointDNAReplication(void);
  virtual bool CheckPointMitosis(void);
  virtual bool CheckPointApoptosis(void);

  void MarkForRemoval(void);

  // Static Members
  static     double      DefaultRadius;
  static     ColorType   DefaultColor;

  static     GeneIdType  BlueGene;   // Pigment genes
  static     GeneIdType  RedGene;
  static     GeneIdType  GreenGene;
  static     GeneIdType  Cdk2E;      // cell cycle control  genes
  static     GeneIdType  Caspase;    // cleavage enzyme: apoptosis effector
  static     GeneIdType  Pressurin;  // signal from micro-tubules subject to stress

  static     double      GrowthRadiusLimit;
  static     double      GrowthRadiusIncrement;

  static unsigned long   MaximumGenerationLimit;
  static unsigned long   GrowthMaximumLatencyTime;
  static unsigned long   DivisionMaximumLatencyTime;

  static     double      EnergySelfRepairLevel;
  static     double      NutrientSelfRepairLevel;

  static     double      DefaultEnergyIntake;
  static     double      DefaultNutrientsIntake;

  static     unsigned long  Counter;

  static ColorType       WellNourishedColor;
  static ColorType       HopefullColor;
  static ColorType       StarvingColor;

  static double          ChemoAttractantLowThreshold;
  static double          ChemoAttractantHighThreshold;

   GenomeType         * m_Genome;
   GenomeType         * m_GenomeCopy;

public:

  virtual bool MarkedForRemoval(void) const;

  static void SetGrowthRadiusLimit( double );
  static void SetGrowthRadiusIncrement( double );
  static void SetEnergySelfRepairLevel( double );
  static void SetNutrientSelfRepairLevel( double );
  static void SetDefaultColor( const ColorType & color );

  static void SetChemoAttractantHighThreshold( double );
  static void SetChemoAttractantLowThreshold( double );

  static void SetGrowthMaximumLatencyTime( unsigned long latency );
  static unsigned long GetGrowthMaximumLatencyTime( void );

  static double GetGrowthRadiusLimit( void );
  static void SetMaximumGenerationLimit( unsigned long );

  static void SetDivisionMaximumLatencyTime( unsigned long );
  static unsigned long GetDivisionMaximumLatencyTime(void);

  static void ResetCounter(void);
  static void Initialize(void); // define values in static variables.


protected:
   double               m_Pressure;

   ColorType            m_Color;
   
   double               m_Radius;
   double               m_EnergyReserveLevel;
   double               m_NutrientsReserveLevel;

   unsigned long        m_GrowthLatencyTime;

   IdentifierType       m_ParentIdentifier;
   IdentifierType       m_SelfIdentifier;

   unsigned long        m_Generation;

   CellCycleState       m_CycleState;
   
   bool                 m_MarkedForRemoval;
   unsigned long        m_DivisionLatencyTime;

   bool                 m_ScheduleApoptosis;
   double               m_ChemoAttractantLevel;

};

} // end namespace bio

} // end namespace itk


#endif

