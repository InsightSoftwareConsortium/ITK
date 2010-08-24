/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSetNode.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLevelSetNode_h
#define __itkLevelSetNode_h

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
template< class TPixel, unsigned int VSetDimension = 2 >
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
  typedef Index< VSetDimension > IndexType;

  /** Operator >. A LevelSetNode is sorted by its value field. */
  bool operator>(const Self & node) const
  { return m_Value > node.m_Value; }

  /** Operator <. A LevelSetNode is sorted by its value field. */
  bool operator<(const Self & node) const
  { return m_Value < node.m_Value; }

  /** Operator <=. A LevelSetNode is sorted by its value field. */
  bool operator<=(const Self & node) const
  { return m_Value <= node.m_Value; }

  /** Operator >=. A LevelSetNode is sorted by its value field. */
  bool operator>=(const Self & node) const
  { return m_Value >= node.m_Value; }

  /** Operator =. Two nodes are equal if both their value and index fields
   * are the same. */
  Self & operator=(const Self & rhs)
  {
    if ( this == &rhs ) { return *this; }

    m_Value = rhs.m_Value;
    m_Index = rhs.m_Index;
    return *this;
  }

  /** Get/Set level set value. */
  PixelType & GetValue()
  { return m_Value; }
  const PixelType & GetValue() const
  { return m_Value; }
  void SetValue(const PixelType & input)
  { m_Value = input; }

  /** Get/Set index. */
  IndexType & GetIndex()
  { return m_Index; }
  const IndexType & GetIndex() const
  { return m_Index; }
  void SetIndex(const IndexType & input)
  { m_Index = input; }

  /** Default constructor */
  LevelSetNode():m_Value(NumericTraits< PixelType >::Zero)
  {
    m_Index.Fill(0);
  }

  /** Copy constructor */
  LevelSetNode(const Self & node):m_Value(node.m_Value), m_Index(node.m_Index) {}
private:
  PixelType m_Value;
  IndexType m_Index;
};
} // end namespace itk

#endif
