/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshEulerOperatorSplitFacetFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkQuadEdgeMeshEulerOperatorSplitFacetFunction_h
#define __itkQuadEdgeMeshEulerOperatorSplitFacetFunction_h

#include "itkQuadEdgeMeshFunctionBase.h"

namespace itk
{
/**
 * \class EulerOperatorSplitFacetFunction
 * \ingroup QuadEdgeMeshModifierFunctions
 *
 * \brief Given two edges h and g sharing the same Left() face,
 *        create a new edge joining h->Destination() to g->Destination(),
 *        thus splitting
 *        the original Left().
 */
template< class TMesh, class TQEType >
class ITK_EXPORT QuadEdgeMeshEulerOperatorSplitFacetFunction:
  public QuadEdgeMeshFunctionBase< TMesh, TQEType * >
{
public:
  /** Standard class typedefs. */
  typedef QuadEdgeMeshEulerOperatorSplitFacetFunction  Self;
  typedef QuadEdgeMeshFunctionBase< TMesh, TQEType * > Superclass;
  typedef SmartPointer< Self >                         Pointer;
  typedef SmartPointer< const Self >                   ConstPointer;

  itkNewMacro(Self);
  /** Run-time type information (and related methods). */
  itkTypeMacro(QuadEdgeMeshEulerOperatorSplitFacetFunction, QuadEdgeMeshFunctionBase);

  /** Type of QuadEdge with which to apply slicing. */
  typedef TQEType QEType;

  typedef typename Superclass::MeshType     MeshType;
  typedef typename Superclass::OutputType   OutputType;
  typedef typename Superclass::EdgeCellType EdgeCellType;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate(QEType *h, QEType *g);

protected:
  QuadEdgeMeshEulerOperatorSplitFacetFunction(){}
  ~QuadEdgeMeshEulerOperatorSplitFacetFunction(){}
private:
  QuadEdgeMeshEulerOperatorSplitFacetFunction(const Self &); //purposely not
                                                             // implemented
  void operator=(const Self &);                              //purposely not
                                                             // implemented
};
} // namespace itk

#include "itkQuadEdgeMeshEulerOperatorSplitFacetFunction.txx"

#endif

// eof - itkQuadEdgeMeshEulerOperatorSplitFacetFunction.h
