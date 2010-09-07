/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction_h
#define __itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction_h

#include "itkQuadEdgeMeshFunctionBase.h"

namespace itk
{
/**
 * \class EulerOperatorCreateCenterVertexFunction
 * \ingroup QEMeshModifierFunctions
 *
 * \brief Create a vertex at the barycenter of the given face.
 *
 */
template< class TMesh, class TQEType >
class ITK_EXPORT QuadEdgeMeshEulerOperatorCreateCenterVertexFunction:
  public QuadEdgeMeshFunctionBase< TMesh, TQEType * >
{
public:
  /** Standard class typedefs. */
  typedef QuadEdgeMeshEulerOperatorCreateCenterVertexFunction Self;
  typedef QuadEdgeMeshFunctionBase< TMesh, TQEType * >        Superclass;
  typedef SmartPointer< Self >                                Pointer;
  typedef SmartPointer< const Self >                          ConstPointer;

  itkNewMacro(Self);
  /** Run-time type information (and related methods). */
  itkTypeMacro(QuadEdgeMeshEulerOperatorCreateCenterVertexFunction, QuadEdgeMeshFunctionBase);

  /** Type of QuadEdge with which to apply slicing. */
  typedef TQEType QEType;

  typedef typename Superclass::MeshType   MeshType;
  typedef typename Superclass::OutputType OutputType;

  typedef typename MeshType::PointIdentifier PointIdentifier;
  typedef typename MeshType::PointType       PointType;
  typedef typename MeshType::CoordRepType    CoordRepType;
  typedef typename MeshType::VectorType      VectorType;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate(QEType *e);

  PointIdentifier GetNewPointID()
  {
    return ( this->m_NewPointID );
  }

protected:
  QuadEdgeMeshEulerOperatorCreateCenterVertexFunction()
  {
    this->m_NewPointID = (PointIdentifier)0;
  }

  ~QuadEdgeMeshEulerOperatorCreateCenterVertexFunction() {}
private:
  QuadEdgeMeshEulerOperatorCreateCenterVertexFunction(const Self &);
  //purposely not implemented
  void operator=(const Self &);

  //purposely not implemented
  PointIdentifier m_NewPointID;
};
} // namespace itk

#include "itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction.txx"

#endif // __ITKQUADEDGEMESH__ITKQEEULEROPERATORCREATECENTERVERTEXFUNCTION__H__

// eof - itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction.h
