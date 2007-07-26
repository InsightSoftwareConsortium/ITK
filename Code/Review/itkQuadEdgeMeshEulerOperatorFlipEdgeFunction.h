// -------------------------------------------------------------------------
// itkQuadEdgeMeshEulerOperatorFlipEdgeFunction.h
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
#ifndef __itkQuadEdgeMeshEulerOperatorFlipEdgeFunction_h
#define __itkQuadEdgeMeshEulerOperatorFlipEdgeFunction_h

#include "itkQuadEdgeMeshFunctionBase.h"

namespace itk
{
/**
 * \class EulerOperatorFlipEdgeFunction
 * \ingroup QEMeshModifierFunctions
 *
 * \brief Flip an edge.
 *
 * The original FlipEdge operator required both faces of the input edge
 * to be triangles (and to be set). This version does not have such requirement.
 * Either or both faces can be polygonal, the org and dest of the edge
 * is then "rotated" around the big polygon that would exist if the two faces 
 * of the edge e were joined.
 * \image html EulerOperatorFlipEdgeWidth600.png "Swapping the h edge"
 */
template < class TMesh, class TQEType >
class ITK_EXPORT QuadEdgeMeshEulerOperatorFlipEdgeFunction :
   public QuadEdgeMeshFunctionBase< TMesh, TQEType* >
{
public:
  /** Standard class typedefs. */
   typedef QuadEdgeMeshEulerOperatorFlipEdgeFunction       Self;
   typedef QuadEdgeMeshFunctionBase< TMesh, TQEType* >     Superclass;
   typedef itk::SmartPointer< Self >                       Pointer;
   typedef itk::SmartPointer< const Self >                 ConstPointer;
     
   itkNewMacro( Self );
   /** Run-time type information (and related methods). */
   itkTypeMacro( QuadEdgeMeshEulerOperatorFlipEdgeFunction, QuadEdgeMeshFunctionBase );

   /** Type of QuadEdge with which to apply slicing. */
   typedef TQEType QEType;

   typedef typename Superclass::MeshType   MeshType;
   typedef typename Superclass::OutputType OutputType;

   /** Evaluate at the specified input position */
   virtual OutputType Evaluate( QEType* h );

protected:
   QuadEdgeMeshEulerOperatorFlipEdgeFunction(){};
  ~QuadEdgeMeshEulerOperatorFlipEdgeFunction(){};

private:
   QuadEdgeMeshEulerOperatorFlipEdgeFunction(const Self& ); //purposely not implemented
   void operator=(const Self& );                //purposely not implemented

};

} // namespace itkQE

#include "itkQuadEdgeMeshEulerOperatorFlipEdgeFunction.txx"

#endif

// eof - itkQuadEdgeMeshEulerOperatorFlipEdgeFunction.h
