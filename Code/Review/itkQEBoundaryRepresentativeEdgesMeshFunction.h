// -------------------------------------------------------------------------
// itkQEBoundaryRepresentativeEdgesMeshFunction.h
// $Revision: 1.1 $
// $Author: sylvain $
// $Name:  $
// $Date: 2007-01-09 00:58:17 $
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
#ifndef __ITKQUADEDGEMESH__ITKQEBOUNDARYREPRESENTATIVEEDGESMESHFUNCTION__H__
#define __ITKQUADEDGEMESH__ITKQEBOUNDARYREPRESENTATIVEEDGESMESHFUNCTION__H__

#include<itkFunctionBase.h>

namespace itkQE
{

/**
 * \brief Build a list of references to edges (as \ref QuadEdgeGeom::RawPointer)
 *        each one representing a different boundary component.
 * \note  Each resulting edge as the surface on it's right and is hence
 *        ready for an walk on with the help of
 *        \ref QEPrimal::IteratorGeom::BeginGeomLnext().
 * \note  The size() of the resulting list is the number of boundary
 *        components.
 */
template< class TMesh >
class ITK_EXPORT BoundaryRepresentativeEdgesMeshFunction
   : public itk::FunctionBase< TMesh, typename TMesh::EdgeListPointerType >
{
public:
   // Standard types
   typedef BoundaryRepresentativeEdgesMeshFunction  Self;
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

   public:
   itkNewMacro( Self );
   itkTypeMacro( BoundaryRepresentativeEdgesMeshFunction, FunctionBase );

   public:
   virtual OutputType Evaluate( const InputType& mesh ) const;

   private:
   BoundaryRepresentativeEdgesMeshFunction( ) { };
   BoundaryRepresentativeEdgesMeshFunction( const Self& );  // Not impl.
   void operator=( const Self& );                           // Not impl.

};

} // enamespace

#include "itkQEBoundaryRepresentativeEdgesMeshFunction.txx"

#endif // __ITKQUADEDGEMESH__ITKQEBOUNDARYREPRESENTATIVEEDGESMESHFUNCTION__H__

// eof - itkQEBoundaryRepresentativeEdgesMeshFunction.h
