/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction_h
#define __itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction_h

#include "itkQuadEdgeMeshFunctionBase.h"

namespace itk
{
/**
 * \class EulerOperatorDeleteCenterVertexFunction
 * \ingroup QEMeshModifierFunctions
 *
 * \brief Delete the vertex, connected edges and faces and create a new face
 *        In place of the previous vertex' one-ring.
 */
template< class TMesh, class TQEType >
class ITK_EXPORT QuadEdgeMeshEulerOperatorDeleteCenterVertexFunction:
  public QuadEdgeMeshFunctionBase< TMesh, TQEType * >
{
public:
  /** Standard class typedefs. */
  typedef QuadEdgeMeshEulerOperatorDeleteCenterVertexFunction Self;
  typedef QuadEdgeMeshFunctionBase< TMesh, TQEType * >        Superclass;
  typedef SmartPointer< Self >                                Pointer;
  typedef SmartPointer< const Self >                          ConstPointer;

  itkNewMacro(Self);
  /** Run-time type information (and related methods). */
  itkTypeMacro(QuadEdgeMeshEulerOperatorDeleteCenterVertexFunction, QuadEdgeMeshFunctionBase);

  /** Type of QuadEdge with which to apply slicing. */
  typedef TQEType QEType;

  typedef typename Superclass::MeshType   MeshType;
  typedef typename Superclass::OutputType OutputType;

  typedef typename MeshType::PointIdentifier PointIdentifier;
  typedef typename MeshType::FaceRefType     FaceRefType;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate(QEType *e);

  PointIdentifier GetOldPointID()
  {
    return ( this->m_OldPointID );
  }

protected:
  QuadEdgeMeshEulerOperatorDeleteCenterVertexFunction(){}
  ~QuadEdgeMeshEulerOperatorDeleteCenterVertexFunction(){}
private:
  QuadEdgeMeshEulerOperatorDeleteCenterVertexFunction(const Self &);
  //purposely not implemented
  void operator=(const Self &);

  //purposely not implemented
  PointIdentifier m_OldPointID;
};
} // namespace itk

#include "itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction.txx"

#endif

// eof - itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction.h
