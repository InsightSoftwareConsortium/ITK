// -------------------------------------------------------------------------
// itkQEZipMeshFunction.h
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
#ifndef __ITKQUADEDGEMESH__ITKQEZIPMESHFUNCTION__H__
#define __ITKQUADEDGEMESH__ITKQEZIPMESHFUNCTION__H__

#include "itkQEMeshFunctionBase.h"

namespace itkQE
{

/**
 * \brief Fuse the incoming edge and it's Onext() follower (like a zipper does).
 * @return The OrgRefType of the point that will be removed during the
 *         zipping process.
 */
template < class TMesh, class TQEType >
class ITK_EXPORT ZipMeshFunction :
   public MeshFunctionBase< TMesh, typename TQEType::OrgRefType >
{
public:
  /** Standard class typedefs. */
  typedef ZipMeshFunction                 Self;
  typedef itk::SmartPointer< Self >       Pointer;
  typedef itk::SmartPointer< const Self > ConstPointer;
  typedef MeshFunctionBase< TMesh, typename TQEType::OrgRefType >  Superclass;
  
  itkNewMacro( Self );
  /** Run-time type information (and related methods). */
  itkTypeMacro( ZipMeshFunction, MeshFunctionBase );

  /** Type of QuadEdge with which to apply slicing. */
  typedef TQEType QEType;

  typedef typename Superclass::MeshType   MeshType;
  typedef typename Superclass::OutputType OutputType;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate( QEType* e );

protected:
   ZipMeshFunction(){};
  ~ZipMeshFunction(){};

private:
  ZipMeshFunction(const Self& ); //purposely not implemented
  void operator=(const Self& );  //purposely not implemented

};

} // namespace itkQE

#include "itkQEZipMeshFunction.txx"

#endif // __ITKQUADEDGEMESH__ITKQEZIPMESHFUNCTION__H__

// eof - itkQEZipMeshFunction.h

