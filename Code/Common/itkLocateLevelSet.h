/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLocateLevelSet.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef _itkLocateLevelSet_h
#define _itkLocateLevelSet_h

#include "itkLevelSet.h"
#include "itkIndex.h"

namespace itk
{

/** \class LocateLevelSet
 * \brief Locate pixels of a particular level set.
 *
 * LocateLevelSet locates a particular level set in the input level
 * set. Specifically, the method Locate() fills 
 * two containers: one containing pixels immediately 
 * inside the contour defined by the level set and the other
 * containing pixels immediately outside.
 * For each located pixel, an estimated distance to the
 * particular level set is also calculated.
 *
 * This class is templated over the image type representing
 * the level set.
 *
 * This class supports narrowbanding. If a input narrowband is
 * provided, the alogrithm will only serach of pixels within the
 * narrowband.
 *
 * Implemenation of this class is based on Chapter 11 of
 * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
 * Cambridge Press, Second edition, 1999.
 *
 */
template <class TLevelSet>
class ITK_EXPORT LocateLevelSet :
  public Object
{
public:
  /** 
   * Standard "Self" typdedef
   */
  typedef LocateLevelSet Self;

  /**
   * Standard "Superclass" typedef
   */ 
  typedef Object Superclass;

  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(LocateLevelSet, Object);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * LevelSetType typedef support.
   */
  typedef LevelSetTypeDefault<TLevelSet>  LevelSetType;
  typedef typename LevelSetType::LevelSetImageType  LevelSetImageType;
  typedef typename LevelSetType::LevelSetPointer  LevelSetPointer;
  typedef typename LevelSetType::PixelType  PixelType;
  typedef typename LevelSetType::NodeType NodeType;
  typedef typename LevelSetType::NodeContainer NodeContainer;
  typedef typename LevelSetType::NodeContainerPointer NodeContainerPointer;

  /**
   * SetDimension enumeration.
   */
  enum { SetDimension = LevelSetType::SetDimension};

  /**
   * Index typedef support.
   */
  typedef Index<SetDimension> IndexType;

  /**
   * Set the input level set.
   */
  void SetInput( TLevelSet * ptr );

  /**
   * Get the input level set.
   */
  LevelSetPointer GetInput();

  /**
   * Set the value of the level set to be located. Default is 0.
   */
  itkSetMacro( LevelSetValue, double );

  /**
   * Get the value of the level set to be located.
   */
  itkGetMacro( LevelSetValue, double );

  /**
   * Set the narrow band width. Default is 12.
   */
  itkSetClampMacro( NarrowBandwidth, double, 0.0, 
    NumericTraits<double>::max());

  /**
   * Get the narrow band width.
   */
  itkGetMacro( NarrowBandwidth, double );

  /**
   * Set the narrowbanding flag.
   */
  itkSetMacro( NarrowBanding, bool );

  /**
   * Get the narrowbanding flag.
   */
  itkGetMacro( NarrowBanding, bool );
  itkBooleanMacro( NarrowBanding );

  /**
   * Set the input narrowband. A narrowband is represented as
   * a VectorContainer of LevelSetNodes.
   */
  void SetInputNarrowBand( NodeContainer * ptr );

  /**
   * Get the container of inside points. The inside points are
   * stored in a VectorContainer of LevelSetNodes.
   */
  NodeContainerPointer GetInsidePoints()
    { return m_InsidePoints; }

  /**
   * Get the container of outside points. The outside points are
   * stored in a VectorContainer of LevelSetNodes.
   */
  NodeContainerPointer GetOutsidePoints()
    { return m_OutsidePoints; }

  /**
   * Locate the level set. This method evokes the level set
   * location algorithm.
   */
  void Locate();

  /**
   * Set the debugging mode
   */
  itkSetMacro( DebugOn, bool );
   
protected:
  LocateLevelSet();
  ~LocateLevelSet(){};
  LocateLevelSet( const Self& ) {};
  void operator= ( const Self& ) {};
  void PrintSelf( std::ostream& os, Indent indent );

  typename LevelSetImageType::ScalarValueType GetLargeValue() const
    { return m_LargeValue; }

  const NodeType& GetNodeUsedInCalculation(unsigned int idx) const
    { return m_NodesUsed[idx]; }

  virtual void Initialize();
  virtual double CalculateDistance( IndexType& index );

  void GenerateData();

private:

  void      GenerateDataFull();
  void      GenerateDataNarrowBand();

  double                                        m_LevelSetValue;

  NodeContainerPointer                          m_InsidePoints;
  NodeContainerPointer                          m_OutsidePoints;

  LevelSetPointer                               m_InputLevelSet;

  bool                                          m_NarrowBanding;
  double                                        m_NarrowBandwidth;
  NodeContainerPointer                          m_InputNarrowBand;

  typename LevelSetImageType::SizeType          m_ImageSize;
  typename LevelSetImageType::ScalarValueType   m_LargeValue;

  std::vector<NodeType>                         m_NodesUsed;

  bool                                          m_DebugOn;

};


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLocateLevelSet.txx"
#endif

#endif
