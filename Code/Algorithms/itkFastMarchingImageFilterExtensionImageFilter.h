/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFastMarchingImageFilterExtensionImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef _itkFastMarchingImageFilterExtensionImageFilter_h
#define _itkFastMarchingImageFilterExtensionImageFilter_h

#include "itkFastMarchingImageFilter.h"
#include "itkImage.h"
#include "itkLevelSet.h"

namespace itk
{

/** \class FastMarchingImageFilterExtensionImageFilter
 * \brief Extend auxiliary variables smoothly using Fast Marching.
 *
 * Fast marching can be used to extend auxiliary variables smoothly
 * from the zero level set. Starting from an initial position on the
 * front, this class simultaneously calculate the signed distance and
 * extend a set of auxiliary values.
 *
 * This class is templated over the level set image type, the auxiliary
 * variable type and the number of auxiliary variables to extended. The initial
 * front is specified by two containers: one containing the known points
 * and one containing the trial points. The auxiliary variables on the front
 * are represented by two auxiliary variable containers: one containing
 * the value of the variables at the know points and on containing the
 * value of the variables at the trail points.
 *
 * Implemenation of this class is based on Chapter 11 of
 * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
 * Cambridge Press, Second edition, 1999.
 *
 * \sa FastMarchingImageFilter
 * \sa LevelSetTypeDefault
 * \sa AuxVarTypeDefault
 */
template <
  class TLevelSet, 
  class TAuxValue,
  unsigned int VAuxDimension = 1 
>
class ITK_EXPORT FastMarchingImageFilterExtensionImageFilter :
  public FastMarchingImageFilter<TLevelSet>
{
public:

  /** 
   * Standard "Self" typdedef
   */
  typedef FastMarchingImageFilterExtensionImageFilter Self;

  /**
   * Standard "Superclass" typedef
   */ 
  typedef FastMarchingImageFilter<TLevelSet> Superclass;

  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(FastMarchingImageFilterExtensionImageFilter, FastMarchingImageFilter);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * SetDimension enumeration.
   * Although already defined in the superclass, needed here for gcc 2.95.2-5
   * to compile.
   */
  typedef LevelSetTypeDefault<TLevelSet>  LevelSetType;
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
   * Index typedef support.
   */
  typedef Index<SetDimension> IndexType;

  /**
   * Get one of the extended auxiliary variable image.
   */
  AuxImagePointer GetAuxiliaryImage( unsigned int idx ) const
    { return m_AuxImage[idx]; }

  /**
   * Set the container auxiliary values at the initial alive points.
   */
  void SetAuxiliaryAliveValues( AuxValueContainer * values )
    { m_AuxAliveValues = values; }

  /**
   * Get the container of auxiliary values at the initial alive points.
   */
  typename AuxValueContainer::Pointer GetAuxiliaryAliveValues()
    { return m_AuxAliceValues; }

  /**
   * Set the container of auxiliary values at the initial trial points.
   */
  void SetAuxiliaryTrialValues( AuxValueContainer * values )
    { m_AuxTrialValues = values; }

  /**
   * Get the container of auxiliary values at the initial trial points.
   */
  typename AuxValueContainer::Pointer GetAuxiliaryTrialValues()
    { return m_AuxTrialValues; }


protected:
  FastMarchingImageFilterExtensionImageFilter();
  ~FastMarchingImageFilterExtensionImageFilter(){};
  FastMarchingImageFilterExtensionImageFilter( const Self& ) {};
  void operator= ( const Self& ) {};
  void PrintSelf( std::ostream& os, Indent indent );

  virtual void Initialize();
  virtual double UpdateValue( IndexType & index );
  virtual void GenerateOutputInformation();
  virtual void EnlargeOutputRequestedRegion( DataObject *output );

private:

  typename AuxValueContainer::Pointer    m_AuxAliveValues;
  typename AuxValueContainer::Pointer    m_AuxTrialValues;
  AuxImagePointer                        m_AuxImage[VAuxDimension] ;
  
  // temporary debugging flag
  bool m_DebugOn;

};

} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFastMarchingImageFilterExtensionImageFilter.txx"
#endif

#endif
