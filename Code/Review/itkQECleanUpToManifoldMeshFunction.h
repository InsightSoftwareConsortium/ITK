// -------------------------------------------------------------------------
// itkQECleanUpToManifoldMeshFunction.h
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
#ifndef __ITKQUADEDGEMESH__ITKQECLEANUPTOMANIFOLDMESHFUNCTION__H__
#define __ITKQUADEDGEMESH__ITKQECLEANUPTOMANIFOLDMESHFUNCTION__H__

#include "itkQEMeshFunctionBase.h"

namespace itkQE
{

/**
 * \class CleanUpToManifoldMeshFunction
 * \brief Clean up the given mesh from isolated vertices or edges with
 *        non adjacent faces.
 *
 * A QEmesh is designed to be a placeholder for 2-manifolds but due to
 * it's compatibility with itk::Mesh it can also represent
 * non-manifold objects e.g. an isolated vertex or an edge with
 * no adjacent face. This method cleans up "this" Mesh from all such
 * non-manifold edges and vertices.
 * \warning The result is of course not guaranted to be singly
 * connected (see \ref MeshExtractComponentFilter on this matter) but
 * each component is 2-manifold.
 */
template < class TMesh, class TOutput >
class ITK_EXPORT CleanUpToManifoldMeshFunction :
   public MeshFunctionBase< TMesh, TOutput >
{
public:
  /** Standard class typedefs. */
  typedef CleanUpToManifoldMeshFunction       Self;
  typedef MeshFunctionBase< TMesh, TOutput >  Superclass;
  typedef itk::SmartPointer< Self >           Pointer;
  typedef itk::SmartPointer< const Self >     ConstPointer;
  
  itkNewMacro( Self );
  /** Run-time type information (and related methods). */
  itkTypeMacro( CleanUpToManifoldMeshFunction, MeshFunctionBase );

  typedef typename Superclass::MeshType   MeshType;
  typedef typename Superclass::OutputType OutputType;

  /** Type of QuadEdge with which to apply slicing. */
  typedef typename MeshType::QEPrimal QEPrimal;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate( );

protected:
  CleanUpToManifoldMeshFunction(){};
  ~CleanUpToManifoldMeshFunction(){};

private:
  CleanUpToManifoldMeshFunction(const Self& ); //purposely not implemented
  void operator=(const Self& );                //purposely not implemented

};

} // namespace itkQE

#include "itkQECleanUpToManifoldMeshFunction.txx"

#endif // __ITKQUADEDGEMESH__ITKQECLEANUPTOMANIFOLDMESHFUNCTION__H__

// eof - itkQECleanUpToManifoldMeshFunction.h

