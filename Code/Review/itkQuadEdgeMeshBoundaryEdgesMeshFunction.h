// -------------------------------------------------------------------------
// itkQuadEdgeMeshBoundaryEdgesMeshFunction.h
// $Revision: 1.1 $
// $Author: ibanez $
// $Name:  $
// $Date: 2007-01-13 12:42:15 $
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
#ifndef __itkQuadEdgeMeshBoundaryEdgesMeshFunction_h
#define __itkQuadEdgeMeshBoundaryEdgesMeshFunction_h


#include<itkFunctionBase.h>

namespace itk
{

/**
 * \brief Build a list of references to edges (as \ref GeometricalQuadEdge::RawPointer)
 *        each one representing a different boundary component.
 * \note  Each resulting edge has the surface on its right and is hence
 *        ready for an walk on with the help of
 *        \ref QEPrimal::IteratorGeom::BeginGeomLnext().
 * \note  The size() of the resulting list is the number of boundary
 *        components.
 */
template< class TMesh >
class ITK_EXPORT QuadEdgeMeshBoundaryEdgesMeshFunction
   : public itk::FunctionBase< TMesh, typename TMesh::EdgeListPointerType >
{
public:
   // Standard types
   typedef QuadEdgeMeshBoundaryEdgesMeshFunction  Self;
   typedef itk::SmartPointer< Self >                Pointer;
   typedef itk::SmartPointer< const Self >          ConstPointer;
   typedef itk::FunctionBase< TMesh,
                              typename  TMesh::EdgeListPointerType > Superclass;

   // Types in superclass:
   typedef typename Superclass::InputType  InputType;
   typedef typename Superclass::OutputType OutputType;

   // Local aliases
   typedef InputType MeshType;
   typedef typename MeshType::QEPrimal     QEPrimal;
   typedef typename MeshType::EdgeListType EdgeListType;

   itkNewMacro( Self );
   itkTypeMacro( QuadEdgeMeshBoundaryEdgesMeshFunction, FunctionBase );

   virtual OutputType Evaluate( const InputType& mesh ) const;

 protected:
   QuadEdgeMeshBoundaryEdgesMeshFunction( ) { };

 private:
   QuadEdgeMeshBoundaryEdgesMeshFunction( const Self& ); //purposely not implemented 
   void operator=( const Self& );     //purposely not implemented 

};

} 

#if ITK_TEMPLATE_TXX
#include "itkQuadEdgeMeshBoundaryEdgesMeshFunction.txx"
#endif

#endif 



