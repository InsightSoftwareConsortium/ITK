// -------------------------------------------------------------------------
// itkQEPlugHolesMeshFunction.h
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
#ifndef __ITKQUADEDGEMESH__ITKQEPLUGHOLESMESHFUNCTION__H__
#define __ITKQUADEDGEMESH__ITKQEPLUGHOLESMESHFUNCTION__H__

#include "itkQEMeshFunctionBase.h"

namespace itkQE
{

/**
 * \brief Convert a bounded surface to a surface with no boundaries.
 *
 * For each boundary (when they exist) of the surface add to the Mesh
 * a polygonal face whose adjacent vertices are the vertices of
 * that boundary.
 */
template < class TMesh >
class ITK_EXPORT PlugHolesMeshFunction :
   public MeshFunctionBase< TMesh, void >
{
public:
  /** Standard class typedefs. */
  typedef PlugHolesMeshFunction           Self;
  typedef itk::SmartPointer< Self >       Pointer;
  typedef itk::SmartPointer< const Self > ConstPointer;
  typedef MeshFunctionBase< TMesh, void > Superclass;
  
  itkNewMacro( Self );
  /** Run-time type information (and related methods). */
  itkTypeMacro( PlugHolesMeshFunction, MeshFunctionBase );

  typedef typename Superclass::MeshType   MeshType;
  typedef typename Superclass::OutputType OutputType;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate( );

protected:
   PlugHolesMeshFunction(){};
  ~PlugHolesMeshFunction(){};

private:
  PlugHolesMeshFunction(const Self& ); // Purposely not implemented
  void operator=(const Self& );        // Purposely not implemented

};

} // namespace itkQE

#include "itkQEPlugHolesMeshFunction.txx"

#endif // __ITKQUADEDGEMESH__ITKQEPLUGHOLESMESHFUNCTION__H__

// eof - itkQEPlugHolesMeshFunction.h
