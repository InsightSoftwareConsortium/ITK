/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExtensionVelocities.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef _itkExtensionVelocities_h
#define _itkExtensionVelocities_h

#include "itkLocateLevelSetExtension.h"
#include "itkFastMarchExtension.h"
#include "itkReinitializeLevelSet.h"

namespace itk
{
/** \class ExtensionVelocities
 *  \brief Extend velocities smoothly from a particular level set.
 *
 * ExtensionVelocities extends velocities smoothly from a particular
 * level set.
 *
 * This class is templated over the image type which represents
 * the level set, the type of the velocity and the
 * number of veclocities to be extended.
 *
 * This class supports narrowbanding. If the input narrowband is provided,
 * the algorithm will only locate the level set within the input narrowband.
 * For the output, the extended velocity is only valid for a distance
 * of OutputNarrowBandwidth / 2 of either side of the level set of interest.
 *
 * Implementation of this class is based on Chapter 11 of
 * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
 * Cambridge Press, Second edition, 1999.
 *
 */
template <
  class TLevelSet,
  class TAuxValue = float,
  unsigned int VAuxDimension = 1
>
class ITK_EXPORT ExtensionVelocities :
  public ReinitializeLevelSet<TLevelSet>
{
public:

  /**
   * Standard "Self" typedef
   */
  typedef ExtensionVelocities Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef ReinitializeLevelSet<TLevelSet> Superclass;

  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer<Self> Pointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ExtensionVelocities, ReinitializeLevelSet);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * SetDimension enumeration.
   * Although already defined in the superclass, needed here for gcc 2.95.2-5
   * to compile.
   */
  enum { SetDimension = LevelSetType::SetDimension};

  /**
   * AuxVarType typedef support.
   */
  typedef AuxVarTypeDefault<TAuxValue,VAuxDimension,SetDimension> AuxVarType;
  typedef typename AuxVarType::AuxValueType AuxValueType;
  typedef typename AuxVarType::AuxValueVectorType AuxValueVectorType;
  typedef typename AuxVarType::AuxValueContainer AuxValueContainer;
  typedef typename AuxVarType::AuxImageType AuxImageType;
  typedef typename AuxVarType::AuxImagePointer AuxImagePointer;

  /**
   * Get one of the extended velocity images.
   */
  AuxImagePointer GetVelocityImage( unsigned int idx = 0) const
  { if( idx >= VAuxDimension ) return NULL;
    return m_OutputAuxImage[idx]; }

  /**
   * Set one of the input velocity images to be extended.
   */
  void SetVelocityImage(AuxImageType * ptr, unsigned int idx = 0);

protected:
  ExtensionVelocities();
  ~ExtensionVelocities(){};
  ExtensionVelocities(const Self&){};
  void operator=(const Self&) {};
  void PrintSelf(std::ostream& os, Indent indent);

  virtual void GenerateDataFull();
  virtual void GenerateDataNarrowBand();
  virtual void AllocateOutput();

  virtual void GenerateInputRequestedRegion();
  virtual void EnlargeOutputRequestedRegion( DataObject * );

private:

  typedef LocateLevelSetExtension<TLevelSet,TAuxValue,VAuxDimension> 
    LocatorType;
  typedef FastMarchExtension<TLevelSet,TAuxValue,VAuxDimension> 
    FastMarchingType;

  typename LocatorType::Pointer         m_Locator;
  typename FastMarchingType::Pointer    m_Marcher;
  AuxImagePointer                       m_InputAuxImage[VAuxDimension];
  AuxImagePointer                       m_OutputAuxImage[VAuxDimension];

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkExtensionVelocities.txx"
#endif

#endif