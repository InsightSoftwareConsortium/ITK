// -------------------------------------------------------------------------
// itkQESliceAtPointOrgMeshFunction.h
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
#ifndef __ITKQUADEDGEMESH__ITKQESLICEATPOINTORGMESHFUNCTION__H__
#define __ITKQUADEDGEMESH__ITKQESLICEATPOINTORGMESHFUNCTION__H__

#include "itkQEMeshFunctionBase.h"

namespace itkQE
{

/**
 * \brief When the Org() of the incoming argument is (at least) twice on
 * the border i.e. it is (at least) twice adjacent to noface, slice the
 * boundary at Org() to remove this boundary singularity.
 *
 * This process requires the creation of new point (newOrg). newOrg
 * has the same geometry as Org() (i.e. it has the same spatial coordinates
 * but it is different in the sense that it bears a different identifier
 * in the point container.
 *
 * \note After slicing newOrg will NOT be in the same surface component as
 *    the incoming argument entryEdge. Hence the Org() of the entryEdge
 *    will be unchanged.
 *
 * \note When the Org() of entryEdge is at least thrice adjacent to noface
 *    the surface component that gets isolated from entryEdge is guaranteed
 *    to be singly adjacent to noface.
 *    The above design will hence allow to call this method with the same
 *    incoming argument (which only has sense when Org of entryEdge is at
 *    least thrice on the boundary.
 */
template < class TMesh, class TQEType >
class ITK_EXPORT SliceAtPointOrgMeshFunction :
   public MeshFunctionBase< TMesh, typename TQEType::OrgRefType >
{
public:
  /** Standard class typedefs. */
  typedef SliceAtPointOrgMeshFunction         Self;
  typedef itk::SmartPointer< Self >           Pointer;
  typedef itk::SmartPointer< const Self >     ConstPointer;
  typedef MeshFunctionBase< TMesh, typename TQEType::OrgRefType >  Superclass;
  
  itkNewMacro( Self );
  /** Run-time type information (and related methods). */
  itkTypeMacro(SliceAtPointOrgMeshFunction, MeshFunctionBase);

  /** Type of QuadEdge with which to apply slicing. */
  typedef TQEType QEType;

  typedef typename Superclass::MeshType   MeshType;
  typedef typename Superclass::OutputType OutputType;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate( QEType* e );

protected:
  SliceAtPointOrgMeshFunction(){};
  ~SliceAtPointOrgMeshFunction(){};

private:
  SliceAtPointOrgMeshFunction(const Self& ); //purposely not implemented
  void operator=(const Self& );              //purposely not implemented

};

} // namespace itkQE

#include "itkQESliceAtPointOrgMeshFunction.txx"

#endif // __ITKQUADEDGEMESH__ITKQESLICEATPOINTORGMESHFUNCTION__H__

// eof - itkQESliceAtPointOrgMeshFunction.h

