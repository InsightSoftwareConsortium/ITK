// -------------------------------------------------------------------------
// itkQuadEdgeMeshEulerOperatorSplitFacetFunction.h
// $Revision: 1.1 $
// $Author: hanfei $
// $Name:  $
// $Date: 2007-07-26 06:30:26 $
// -------------------------------------------------------------------------
// This code is an implementation of the well known quad edge (QE) data
// structure in the ITK library. Although the original QE can handle non
// orientable 2-manifolds and its dual and its mirror, this implementation
// is specifically dedicated to handle orientable 2-manifolds along with
// their dual.
//
// Any comment, criticism and/or donation is welcome.
//
// Please contact any member of the team:
//
// - The frog master (Eric Boix)       eboix@ens-lyon.fr
// - The duck master (Alex Gouaillard) alexandre.gouaillard@sun.com
// - The cow  master (Leonardo Florez) florez@creatis.insa-lyon.fr
// -------------------------------------------------------------------------
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
template < class TMesh, class TQEType >
class ITK_EXPORT QuadEdgeMeshEulerOperatorSplitFacetFunction :
   public QuadEdgeMeshFunctionBase< TMesh, TQEType* >
{
public:
  /** Standard class typedefs. */
  typedef QuadEdgeMeshEulerOperatorSplitFacetFunction      Self;
  typedef QuadEdgeMeshFunctionBase< TMesh, TQEType* >      Superclass;
  typedef itk::SmartPointer< Self >                        Pointer;
  typedef itk::SmartPointer< const Self >                  ConstPointer;
  
  itkNewMacro( Self );
  /** Run-time type information (and related methods). */
  itkTypeMacro( QuadEdgeMeshEulerOperatorSplitFacetFunction, QuadEdgeMeshFunctionBase );

  /** Type of QuadEdge with which to apply slicing. */
  typedef TQEType QEType;

  typedef typename Superclass::MeshType   MeshType;
  typedef typename Superclass::OutputType OutputType;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate( QEType* h, QEType* g );

protected:
  QuadEdgeMeshEulerOperatorSplitFacetFunction(){};
  ~QuadEdgeMeshEulerOperatorSplitFacetFunction(){};

private:
  QuadEdgeMeshEulerOperatorSplitFacetFunction(const Self& ); //purposely not implemented
  void operator=(const Self& );        //purposely not implemented

};

} // namespace itk

#include "itkQuadEdgeMeshEulerOperatorSplitFacetFunction.txx"

#endif

// eof - itkQuadEdgeMeshEulerOperatorSplitFacetFunction.h
