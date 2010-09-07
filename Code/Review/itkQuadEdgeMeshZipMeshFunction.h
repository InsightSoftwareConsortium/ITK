/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshZipMeshFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkQuadEdgeMeshZipMeshFunction_h
#define __itkQuadEdgeMeshZipMeshFunction_h

#include "itkQuadEdgeMeshFunctionBase.h"

namespace itk
{
/**
 * \ingroup QEMeshModifierFunctions
 * \class MeshFunctionBase
 * \brief Fuse the incoming edge and it's Onext() follower (like a zipper does).
 * @return The OriginRefType of the point that will be removed during the
 *         zipping process.
 */
template< class TMesh, class TQEType >
class ITK_EXPORT QuadEdgeMeshZipMeshFunction:
  public QuadEdgeMeshFunctionBase< TMesh, typename TQEType::OriginRefType >
{
public:
  /** Standard class typedefs. */
  typedef QuadEdgeMeshZipMeshFunction Self;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;

  typedef QuadEdgeMeshFunctionBase< TMesh,
                                    typename TQEType::OriginRefType >  Superclass;

  itkNewMacro(Self);
  /** Run-time type information (and related methods). */
  itkTypeMacro(QuadEdgeMeshZipMeshFunction, QuadEdgeMeshFunctionBase);

  /** Type of QuadEdge with which to apply slicing. */
  typedef TQEType QEType;

  typedef typename Superclass::MeshType   MeshType;
  typedef typename Superclass::OutputType OutputType;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate(QEType *e);

protected:
  QuadEdgeMeshZipMeshFunction(){}
  ~QuadEdgeMeshZipMeshFunction(){}
private:
  QuadEdgeMeshZipMeshFunction(const Self &); //purposely not implemented
  void operator=(const Self &);              //purposely not implemented
};
} // namespace itk

#include "itkQuadEdgeMeshZipMeshFunction.txx"

#endif

// eof - itkQuadEdgeMeshZipMeshFunction.h
