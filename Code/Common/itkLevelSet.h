/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSet.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkLevelSet_h
#define _itkLevelSet_h

#include "itkIndex.h"
#include "itkImage.h"
#include "itkVectorContainer.h"
#include "itkVector.h"

namespace itk
{

/** 
 * \class LevelSetNode
 * \brief Represent a node in a level set.
 *
 * LevelSetNode is a simple templated class that represents a node 
 * or grid position of a level set. A group or collection of
 * LevelSetNode can then be used to represents a narrowband or
 * region of interest.  
 *
 * LevelSetNode is templated over the data type and dimension of the
 * level set.
 *
 * \ingroup LevelSetSegmentation 
 * 
 */
template<class TPixel, unsigned int VSetDimension = 2>
class ITK_EXPORT LevelSetNode
{
public:
  /** Standard class typedefs. */
  typedef LevelSetNode Self;

  /** Pixel typedef. */
  typedef TPixel PixelType;

  /** Level set dimension. */
  itkStaticConstMacro(SetDimension, unsigned int, VSetDimension);

  /** Index typedef. */
  typedef Index<VSetDimension> IndexType;

  /** Operator >. A LevelSetNode is sorted by its value field. */
  bool operator> ( const Self& node ) const
    { return m_Value > node.m_Value; }

  /** Operator <. A LevelSetNode is sorted by its value field. */
  bool operator< ( const Self& node ) const
    { return m_Value < node.m_Value; }

  /** Operator <=. A LevelSetNode is sorted by its value field. */
  bool operator<= ( const Self& node ) const
    { return m_Value <= node.m_Value; }

  /** Operator >=. A LevelSetNode is sorted by its value field. */
  bool operator>= ( const Self& node ) const
    { return m_Value >= node.m_Value; }

  /** Operator =. Two nodes are equal if both their value and index fields
   * are the same. */
  Self& operator= ( const Self& rhs )
    {
      if( this == &rhs ) {return *this;}
  
      m_Value = rhs.m_Value;
      m_Index = rhs.m_Index;
      return *this;
    }

  /** Get/Set level set value. */
  PixelType& GetValue()
    { return m_Value; };
  const PixelType& GetValue() const
    { return m_Value; };
  void SetValue( const PixelType& input )
    { m_Value = input; };

  /** Get/Set index. */
  IndexType& GetIndex()
    { return m_Index; }
  const IndexType& GetIndex() const
    { return m_Index; }
  void SetIndex( const IndexType& input )
    { m_Index = input; };

  /** Default constructor */
  LevelSetNode() : m_Value( NumericTraits<PixelType>::Zero ) {
    m_Index.Fill( 0 );
    };

  /** Copy constructor */
  LevelSetNode(const Self &node) : m_Value( node.m_Value ), m_Index( node.m_Index ) {};
  
private:
  PixelType       m_Value;
  IndexType       m_Index;
  
};

/**  \class LevelSetTypeDefault
 * \brief Level set type information.
 *
 * LevelSetTypeDefault is a simple class that holds type information
 * useful for level set algorithms. This class is templated over the
 * level set image type.
 *
 * A NodeContainer contains a group or collection of level set 
 * node or grid points useful for representing a narrowband or
 * region of interest.
 *
 * \ingroup LevelSetSegmentation 
 */
template<class TLevelSet>
class ITK_EXPORT LevelSetTypeDefault
{
 public:
  /** LevelSetType typedef support. */
  typedef LevelSetTypeDefault Self;
  typedef TLevelSet LevelSetImageType;
  
  
  /** SetDimension enumeration. */
  itkStaticConstMacro(SetDimension, unsigned int, TLevelSet::ImageDimension);

  /** LevelSetPointer typedef support. */
  typedef typename TLevelSet::Pointer LevelSetPointer;
  typedef typename TLevelSet::ConstPointer LevelSetConstPointer;

  /** PixelType typedef support. */
  typedef typename TLevelSet::PixelType PixelType;
  
  /** Node typdef support. */
  typedef
      LevelSetNode<PixelType, itkGetStaticConstMacro(SetDimension)> NodeType;

  /** NodeContainer typedef support. */
  typedef VectorContainer<unsigned int,NodeType> NodeContainer;

  /** NodeContainerPointer typedef support. */
  typedef typename NodeContainer::Pointer NodeContainerPointer;
};


/** \class AuxVarTypeDefault
 * \brief Level set auxiliary variables type information.
 *
 * \brief AuxVarTypeDefault is a simple class that holds type information
 * for auxiliary variables in some level set algorithms. This class is templated
 * over the auxiliary variable data type, the number of auxiliary variables
 * and the level set dimension.
 *
 * A AuxValueContainer contains a collection of auxiliary
 * values vectors. It is used in conjunction with 
 * LevelSetTypeDefault::NodeContainer to represent auxiliary variable values 
 * in a group or collection of level set nodes or grid positions.
 *
 * \ingroup LevelSetSegmentation 
 */
template < 
class TPixel,
unsigned int VAuxDimension = 1,
unsigned int VSetDimension = 2
>
class ITK_EXPORT AuxVarTypeDefault
{
 public:
  /** Standard typedefs */
  typedef AuxVarTypeDefault Self;
  
  /** PixelType typedef support. */ 
  typedef TPixel AuxValueType;

  /** Auxiliary variable dimension. */
  itkStaticConstMacro(AuxDimension, unsigned int, VAuxDimension);

  /** Level set dimension. */
  itkStaticConstMacro(SetDimension, unsigned int, VSetDimension);

  /** AuxVector typedef support. */
  typedef Vector<TPixel,VAuxDimension> AuxValueVectorType;

  /** AuxContainer typdef support. */
  typedef VectorContainer<unsigned int,AuxValueVectorType> AuxValueContainer;

  /** AuxImage typdef support. */
  typedef Image<AuxValueType, VSetDimension> AuxImageType;

  /** AuxImagePointer typedef support. */
  typedef typename AuxImageType::Pointer AuxImagePointer;
  typedef typename AuxImageType::ConstPointer AuxImageConstPointer;
};


} // namespace itk

#endif
