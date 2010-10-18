/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshEulerOperatorSplitVertexFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkQuadEdgeMeshEulerOperatorSplitVertexFunction_h
#define __itkQuadEdgeMeshEulerOperatorSplitVertexFunction_h

#include "itkQuadEdgeMeshFunctionBase.h"

namespace itk
{
/**
 * \class EulerOperatorSplitVertexFunction
 * \ingroup QuadEdgeMeshModifierFunctions
 *
 * \brief For two given edges e and f sharing the same dest(), disconnect the
 * two rings, create a new point to be set at f->dest(), and create
 * a new edge between e->Destination() and f->Destination().
 *
 * \sa \ref itk::QuadEdgeMeshEulerOperatorJoinVertexFunction
 */
template< class TMesh, class TQEType >
class ITK_EXPORT QuadEdgeMeshEulerOperatorSplitVertexFunction:
  public QuadEdgeMeshFunctionBase< TMesh, TQEType * >
{
public:
  /** Standard class typedefs. */
  typedef QuadEdgeMeshEulerOperatorSplitVertexFunction Self;
  typedef QuadEdgeMeshFunctionBase< TMesh, TQEType * > Superclass;
  typedef SmartPointer< Self >                         Pointer;
  typedef SmartPointer< const Self >                   ConstPointer;

  itkNewMacro(Self);
  /** Run-time type information (and related methods). */
  itkTypeMacro(QuadEdgeMeshEulerOperatorSplitVertexFunction, QuadEdgeMeshFunctionBase);

  /** Type of QuadEdge with which to apply slicing. */
  typedef TQEType QEType;

  typedef typename Superclass::MeshType      MeshType;
  typedef typename Superclass::OutputType    OutputType;
  typedef typename MeshType::VertexRefType   VertexRefType;
  typedef typename MeshType::EdgeCellType    EdgeCellType;
  typedef typename MeshType::PointIdentifier PointIdentifier;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate(QEType *e, QEType *f);

  PointIdentifier GetNewPointID()
  {
    return ( this->m_NewPoint );
  }

protected:
  QuadEdgeMeshEulerOperatorSplitVertexFunction()
  {
    m_NewPoint = (PointIdentifier)0;
  }

  ~QuadEdgeMeshEulerOperatorSplitVertexFunction(){}
private:
  QuadEdgeMeshEulerOperatorSplitVertexFunction(const Self &); //purposely not
                                                              // implemented
  void operator=(const Self &);                               //purposely not
                                                              // implemented

  PointIdentifier m_NewPoint; // stock newly created point ID for user.
};
} // namespace itk

#include "itkQuadEdgeMeshEulerOperatorSplitVertexFunction.txx"

#endif

// eof - itkQuadEdgeMeshEulerOperatorSplitVertexFunction.h
