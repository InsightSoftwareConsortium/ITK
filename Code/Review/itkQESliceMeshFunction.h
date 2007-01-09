// -------------------------------------------------------------------------
// itkQESliceMeshFunction.h
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
#ifndef __ITKQUADEDGEMESH__ITKQESLICEMESHFUNCTION__H__
#define __ITKQUADEDGEMESH__ITKQESLICEMESHFUNCTION__H__

#include "itkQEMeshFunctionBase.h"

namespace itkQE
{

/**
 * \brief SliceMeshFunction is the composition of \ref SliceOpenMeshFunction
 *    with \ref SliceAtPointOrgMeshFunction.
 *
 * We assert that the Org() of the Evaluate method is on the boundary.
 */
template < class TMesh, class TQEType, class TOutput >
class ITK_EXPORT SliceMeshFunction :
   public MeshFunctionBase< TMesh, TOutput >
{
public:
  /** Standard class typedefs. */
  typedef SliceMeshFunction        Self;
  typedef MeshFunctionBase         Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(SliceMeshFunction, MeshFunctionBase);

  /** Type of QuadEdge with which to apply slicing. */
  typedef TQEType QEType;

  typedef Superclass::MeshType   MeshType;
  typedef Superclass::OutputType OutputType;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate( QEType* e );

protected:
  SliceMeshFunction(){};
  ~SliceMeshFunction(){};

private:
  SliceMeshFunction(const Self& ); //purposely not implemented
  void operator=(const Self& );    //purposely not implemented

};

} // namespace itkQE

#include "itkQESliceMeshFunction.txx"

#endif // __ITKQUADEDGEMESH__ITKQESLICEMESHFUNCTION__H__

// eof - itkQESliceMeshFunction.h

