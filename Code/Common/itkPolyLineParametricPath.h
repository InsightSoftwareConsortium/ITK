/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPolyLineParametricPath.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkPolyLineParametricPathPath_h
#define _itkPolyLineParametricPathPath_h

#include "itkParametricPath.h"
#include "itkVectorContainer.h"
#include "itkContinuousIndex.h"
#include "itkIndex.h"
#include "itkOffset.h"
#include "itkVector.h"

namespace itk
{


/** \class PolyLineParametricPath
 * \brief  Represent a path of line segments through ND Space
 *
 * This class is intended to represent parametric paths through an image, where
 * the paths are composed of line segments.  Each line segment traverses one
 * unit of input.  A classic application of this class is the representation of
 * contours in 2D images, especially when the contours only need to be
 * approximately correct.  Another use of a path is to guide the movement of an
 * iterator through an image.
 *
 * \sa EllipseParametricPath
 * \sa FourierSeriesPath
 * \sa OrthogonallyCorrectedParametricPath
 * \sa ParametricPath
 * \sa ChainCodePath
 * \sa Path
 * \sa ContinuousIndex
 * \sa Index
 * \sa Offset
 * \sa Vector
 *
 * \ingroup PathObjects
 */
template <unsigned int VDimension>
class ITK_EXPORT PolyLineParametricPath : public
ParametricPath< VDimension >
{
public:
  /** Standard class typedefs. */
  typedef PolyLineParametricPath      Self;
  typedef ParametricPath<VDimension>  Superclass;
  typedef SmartPointer<Self>          Pointer;
  typedef SmartPointer<const Self>    ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(PolyLineParametricPath, ParametricPath);
  
  /** Input type */
  typedef typename Superclass::InputType  InputType;
  
  /** Output type */
  typedef typename Superclass::OutputType OutputType;
  
  
  /** Basic data-structure types used */
  typedef ContinuousIndex<double,VDimension>    ContinuousIndexType;           
  typedef Index<  VDimension >                  IndexType;                     
  typedef Offset< VDimension >                  OffsetType;                    
  typedef Point<double,VDimension>              PointType;                    
  typedef Vector<double,VDimension>             VectorType;                    
  typedef ContinuousIndexType                   VertexType;                    
  typedef VectorContainer<unsigned, VertexType> VertexListType;
  typedef typename VertexListType::Pointer      VertexListPointer;


  /** Return the location of the parametric path at the specified location. */
  virtual OutputType Evaluate( const InputType & input ) const;
  
  ///** Evaluate the first derivative of the ND output with respect to the 1D
  //  * input.  This is an exact, algebraic function. */
  //virtual VectorType EvaluateDerivative(const InputType & input) const;
  
  /** Add a vertex (and a connecting line segment to the previous vertex).
   * Adding a vertex has the additional effect of extending the domain of the
   * PolyLineParametricPath by 1.0 (each pair of consecutive verticies is
   * seperated by one unit of input). */
  inline void AddVertex( const ContinuousIndexType & vertex )
    {
    m_VertexList->InsertElement( m_VertexList->Size(), vertex );
    this->Modified();
    }
  
  /** Where does the path end?  This value is necessary for IncrementInput() to
   * know how to go to the end of a path.  Since each line segment covers one
   * unit of input, this is the number of verticies - 1. */
  virtual inline InputType EndOfInput() const
    {
    return m_VertexList->Size() - 1;
    }
  
  /** New() method for dynamic construction */
  itkNewMacro( Self );
  
  
protected:
  PolyLineParametricPath();
  ~PolyLineParametricPath(){}
  void PrintSelf(std::ostream& os, Indent indent) const;
  
private:
  PolyLineParametricPath(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  VertexListPointer m_VertexList;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPolyLineParametricPath.txx"
#endif

#endif
