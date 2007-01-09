// -------------------------------------------------------------------------
// itkQEDeleteIsolatedVerticesMeshFunction.h
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
#ifndef __ITKQUADEDGEMESH__ITKQEDELETEISOLATEDVERTICESMESHFUNCTION__H__
#define __ITKQUADEDGEMESH__ITKQEDELETEISOLATEDVERTICESMESHFUNCTION__H__

#include "itkQEMeshFunctionBase.h"

namespace itkQE
{

/**
 * \brief Delete all the isolated vertices.
 *
 * Isolated vertices are vertices which have no associated edge entry.
 * Remove them from the container.
 */
template < class TMesh >
class ITK_EXPORT DeleteIsolatedVerticesMeshFunction :
   public MeshFunctionBase< TMesh, void >
{
public:
  /** Standard class typedefs. */
  typedef DeleteIsolatedVerticesMeshFunction  Self;
  typedef MeshFunctionBase< TMesh, void >     Superclass;
  typedef itk::SmartPointer< Self >           Pointer;
  typedef itk::SmartPointer< const Self >     ConstPointer;
  
  itkNewMacro( Self );
  /** Run-time type information (and related methods). */
  itkTypeMacro( DeleteIsolatedVerticesMeshFunction, MeshFunctionBase );

  typedef typename Superclass::MeshType   MeshType;
  typedef typename Superclass::OutputType OutputType;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate( );

protected:
   DeleteIsolatedVerticesMeshFunction(){};
  ~DeleteIsolatedVerticesMeshFunction(){};

private:
  DeleteIsolatedVerticesMeshFunction(const Self& ); //purposely not implemented
  void operator=(const Self& );                     //purposely not implemented

};

} // namespace itkQE

#include "itkQEDeleteIsolatedVerticesMeshFunction.txx"

#endif // __ITKQUADEDGEMESH__ITKQEDELETEISOLATEDVERTICESMESHFUNCTION__H__

// eof - itkQEDeleteIsolatedVerticesMeshFunction.h

