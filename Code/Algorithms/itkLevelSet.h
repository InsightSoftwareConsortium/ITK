/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSet.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
 * LevelSetNode is a simple templated class that represents a node in a level set.
 * LevelSetNode is templated over the data type and dimension of the level set.
 *
 * \ingroup LevelSetSegmentation 
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
  Index<VSetDimension>       index;
  
};


/** 
 * \class LevelSetTypeDefault
 * \brief Level set type information.
 *
 * LevelSetTypeDefault is a simple class that holds type information
 * useful for level set algorithms. This class is templated over the
 * level set image type.
 *
 * \ingroup LevelSetSegmentation 
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
   * Node typdef support.
   */
  typedef LevelSetNode<PixelType, SetDimension> NodeType;

  /**
   * NodeContainer typedef support.
   */
  typedef VectorContainer<unsigned int,NodeType> NodeContainer;

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
 * \ingroup LevelSetSegmentation 
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
  typedef Vector<TPixel,VAuxDimension> AuxValueVectorType;

  /**
   * AuxContainer typdef support
   */
  typedef VectorContainer<unsigned int,AuxValueVectorType> AuxValueContainer;

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
