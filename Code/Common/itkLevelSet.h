/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSet.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkLevelSet_h
#define _itkLevelSet_h

#include "itkIndex.h"
#include "itkImage.h"
#include "itkVectorContainer.h"
#include "vnl/vnl_vector_fixed.h"

#include "vnl/vnl_vector_fixed.txx"
#include "vnl/vnl_vector.txx"
#include "vnl/vnl_c_vector.txx"

namespace itk
{

/** 
 * \class LevelSetNode
 * \brief Represent a node in a level set.
 *
 * LevelSetNode is a simple templated class that represents a node in a level set.
 * LevelSetNode is templated over the data type and dimension of the level set.
 *
 */
template<class TPixel, unsigned int VSetDimension = 2>
class ITK_EXPORT LevelSetNode
{
public:
  /**
   * Standard Self typedef
   */
  typedef LevelSetNode Self;

  /**
   * Operator >. A LevelSetNode is sorted by its value field.
   */
  bool operator> ( const Self& node ) const
    { return value > node.value; }

  /**
   * Operator <. A LevelSetNode is sorted by its value field.
   */
  bool operator< ( const Self& node ) const
    { return value < node.value; }

  /** 
   * Operator =. Two nodes are equal if both their value and index fields
   * are the same.
   */
  Self& operator= ( const Self& rhs )
  {
    if( this == &rhs ) return *this;
    value = rhs.value;
    index = rhs.index;
    return *this;
  }

  TPixel                     value;
  itk::Index<VSetDimension>  index;
  
};


/** 
 * \class LevelSetTypeDefault
 * \brief Level set type information.
 *
 * LevelSetTypeDefault is a simple class that holds type information
 * useful for level set algorithms. This class is templated over the
 * level set image type.
 */
template<class TLevelSet>
class ITK_EXPORT LevelSetTypeDefault
{
public:

  /**
   * LevelSetType typedef support.
   */
  typedef TLevelSet LevelSetImageType;

  /**
   * SetDimension enumeration
   */
  enum{ SetDimension = TLevelSet::ImageDimension };

  /**
   * LevelSetPointer typedef support.
   */
  typedef typename TLevelSet::Pointer LevelSetPointer;

  /**
   * PixelType typedef support.
   */
  typedef typename TLevelSet::PixelType PixelType;
  
  /**
   * ScalarValueType typedef support.
   */
  typedef typename TLevelSet::ScalarValueType ScalarValueType;

  /**
   * Node typdef support.
   */
  typedef itk::LevelSetNode<ScalarValueType, SetDimension> NodeType;

  /**
   * NodeContainer typedef support.
   */
  typedef itk::VectorContainer<int,NodeType> NodeContainer;

  /**
   * NodeContainerPointer typedef support.
   */
  typedef typename NodeContainer::Pointer NodeContainerPointer;

};


/**
 * \class AuxVarTypeDefault
 * \brief Level set auxiliary variables type information.
 *
 * \brief AuxVarTypeDefault is a simple class that holds type information
 * for auxiliary variables in some level set algorithms. This class is templated
 * over the auxiliary variable data type, the number of auxiliary variables
 * and the level set dimension.
 *
 */
template < 
class TPixel,
unsigned int VAuxDimension = 1,
unsigned int VSetDimension = 2
>
class ITK_EXPORT AuxVarTypeDefault
{
public:
  /**
   * PixelType typedef support
   */ 
  typedef TPixel AuxValueType;

  /**
   * Auxiliary variable dimension
   */
  enum { AuxDimension = VAuxDimension };

  /**
   * Level set dimension
   */
  enum { SetDimension = VSetDimension };

  /**
   * AuxVector typedef support
   */
  typedef vnl_vector_fixed<TPixel,VAuxDimension> AuxValueVectorType;

  /**
   * AuxContainer typdef support
   */
  typedef VectorContainer<int,AuxValueVectorType> AuxValueContainer;

  /**
   * AuxImage typdef support.
   */
  typedef Image<AuxValueType, VSetDimension> AuxImageType;

  /**
   * AuxImagePointer typedef support.
   */
  typedef typename AuxImageType::Pointer AuxImagePointer;

};


} // namespace itk

#endif
