/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSetNeighborhoodExtractorExtension.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef _itkLevelSetNeighborhoodExtractorExtension_h
#define _itkLevelSetNeighborhoodExtractorExtension_h

#include "itkLevelSetNeighborhoodExtractor.h"
#include "itkLevelSet.h"
#include "itkIndex.h"

namespace itk
{

/** \class LevelSetNeighborhoodExtractorExtension
 * \brief Locate pixels of a particular level set.
 *
 * LevelSetNeighborhoodExtractor locates a particular level set in the input level
 * set. Specifically, the method Locate() fills 
 * two containers: one containing pixels immediately 
 * inside the contour defined by the level set and the other
 * containing pixels immediately outside.
 * For each located pixel, an estimated distance to the
 * particular level set is also calculated.
 *
 * This class also calculates the values of auxiliary variables
 * at located pixel.
 *
 * This class is templated over the image type representing
 * the level set, the type of the auxiliary variables and the
 * number of auxiliary variables.
 *
 * Implemenation of this class is based on Chapter 11 of
 * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
 * Cambridge Press, Second edition, 1999.
 *
 */
template <
  class TLevelSet,
  class TAuxValue,
  unsigned int VAuxDimension = 1
>
class ITK_EXPORT LevelSetNeighborhoodExtractorExtension :
  public LevelSetNeighborhoodExtractor<TLevelSet>
{
public:
  /** 
   * Standard "Self" typdedef
   */
  typedef LevelSetNeighborhoodExtractorExtension Self;

  /**
   * Standard "Superclass" typedef
   */ 
  typedef LevelSetNeighborhoodExtractor<TLevelSet> Superclass;

  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(LevelSetNeighborhoodExtractorExtension, LevelSetNeighborhoodExtractor);

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
   * Index typedef support
   */
  typedef Index<SetDimension> Index;

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
   * Set the auxiliary images
   */
  void SetAuxImage( AuxImageType * ptr, unsigned int idx = 0 )
  { 
    if( idx < VAuxDimension )
      {
      m_AuxImage[idx] = ptr;
      }
  }

  /**
   * Get the container of auxiliary values associated with the inside points
   */
  typename AuxValueContainer::Pointer GetAuxInsideValues()
    { return m_AuxInsideValues; }

  /**
   * Get the container of auxiliary values associate with the outside points
   */
  typename AuxValueContainer::Pointer GetAuxOutsideValues()
    { return m_AuxOutsideValues; }


protected:
  LevelSetNeighborhoodExtractorExtension();
  ~LevelSetNeighborhoodExtractorExtension(){};
  LevelSetNeighborhoodExtractorExtension( const Self& ) {};
  void operator= ( const Self& ) {};
  void PrintSelf( std::ostream& os, Indent indent );

  virtual void Initialize();
  virtual double CalculateDistance( Index & index );

  void GenerateDate();

private:

  typename AuxValueContainer::Pointer    m_AuxInsideValues;
  typename AuxValueContainer::Pointer    m_AuxOutsideValues;
  AuxImagePointer                        m_AuxImage[VAuxDimension];

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetNeighborhoodExtractorExtension.txx"
#endif

#endif


