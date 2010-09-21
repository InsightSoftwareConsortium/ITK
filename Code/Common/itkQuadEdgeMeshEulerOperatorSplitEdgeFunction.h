/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshEulerOperatorSplitEdgeFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkQuadEdgeMeshEulerOperatorSplitEdgeFunction_h
#define __itkQuadEdgeMeshEulerOperatorSplitEdgeFunction_h

#include "itkQuadEdgeMeshFunctionBase.h"
#include "itkQuadEdgeMeshEulerOperatorSplitVertexFunction.h"

namespace itk
{
/**
 * \class EulerOperatorSplitEdgeFunction
 * \ingroup QEMeshModifierFunctions
 *
 * \brief Given Edge is splitted into two and associated faces see their degree
 *        increased by one (a triangle is transformed into a quad for example).
 */
template< class TMesh, class TQEType >
class ITK_EXPORT QuadEdgeMeshEulerOperatorSplitEdgeFunction:
  public QuadEdgeMeshFunctionBase< TMesh, TQEType * >
{
public:
  /** Standard class typedefs. */
  typedef QuadEdgeMeshEulerOperatorSplitEdgeFunction   Self;
  typedef QuadEdgeMeshFunctionBase< TMesh, TQEType * > Superclass;
  typedef SmartPointer< Self >                         Pointer;
  typedef SmartPointer< const Self >                   ConstPointer;

  itkNewMacro(Self);
  /** Run-time type information (and related methods). */
  itkTypeMacro(QuadEdgeMeshEulerOperatorSplitEdgeFunction, QuadEdgeMeshFunctionBase);

  /** Type of QuadEdge with which to apply slicing. */
  typedef TQEType QEType;

  typedef typename Superclass::MeshType      MeshType;
  typedef typename Superclass::OutputType    OutputType;
  typedef typename MeshType::PointIdentifier PointIdentifier;

  typedef QuadEdgeMeshEulerOperatorSplitVertexFunction< MeshType, QEType > SplitVertex;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate(QEType *e)
  {
    if ( !e )
      {
      itkDebugMacro("Input is not an edge.");
      return ( (QEType *)0 );
      }

    if ( !this->m_Mesh )
      {
      itkDebugMacro("No mesh present.");
      return ( (QEType *)0 );
      }

    m_SplitVertex->SetInput(this->m_Mesh);
    return ( m_SplitVertex->Evaluate( e->GetLprev(), e->GetSym() ) );
  }

  const PointIdentifier GetNewPointID()
  {
    return ( m_SplitVertex->GetNewPointID() );
  }

protected:
  QuadEdgeMeshEulerOperatorSplitEdgeFunction()
  {
    m_SplitVertex = SplitVertex::New();
  }

  ~QuadEdgeMeshEulerOperatorSplitEdgeFunction(){}
private:
  QuadEdgeMeshEulerOperatorSplitEdgeFunction(const Self &); //purposely not
                                                            // implemented
  void operator=(const Self &);                             //purposely not
                                                            // implemented

  typename SplitVertex::Pointer m_SplitVertex;
};
} // namespace itk

#endif

// eof - itkQuadEdgeMeshEulerOperatorSplitEdgeFunction.h
