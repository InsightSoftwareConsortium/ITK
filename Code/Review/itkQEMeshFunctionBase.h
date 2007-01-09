// -------------------------------------------------------------------------
// itkQEMeshFunctionBase.h
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
#ifndef __ITKQUADEDGEMESH__ITKQEMESHFUNCTIONBASE__H__
#define __ITKQUADEDGEMESH__ITKQEMESHFUNCTIONBASE__H__

#include <itkObject.h>
#include <itkObjectFactory.h>

namespace itkQE
{

/**
 * \class MeshFunctionBase
 * \brief Base class for mesh function object modifiers.
 *
 * MeshFunctionBase is the base class for itkQE function objects specialised
 * in Mesh "small" (reduced in range) modification.
 * Subclasses of itk::FunctionBase cannot modify their InputType since
 * the signature of their Evaluate( const InputType& ) method guarantees it.
 * Consider a method that modifies (the geometry, the connectivity or both)
 * a mesh.
 * For large modifications of this mesh we follow the classical itk Filter
 * schema, which implies duplicating the mesh which can be space consuming.
 * But for small modifications (think of the Euler operators) that an
 * algorithm needs to apply many times, this systematic duplication can
 * be daunting.
 * MeshFunctionBase thus offers a leightweight alternative to itk Filter.
 * Subclasses of MeshFunctionBase, which should override Evaluate(), are
 * function objects that apply reduced and localised modifications
 * (geometry, or connectivity) on the InputType mesh.
 *
 * This class is template over the mesh type (to be modified) and 
 * the output (usually a created/deleted vertex or face) type.
 *
 * \ingroup Functions
 * 
 */
template < class TMesh, class TOutput >
class ITK_EXPORT MeshFunctionBase : public itk::Object
{
public:
  /** Standard class typedefs. */
  typedef MeshFunctionBase              Self;
  typedef itk::Object                   Superclass;
  typedef itk::SmartPointer<Self>       Pointer;
  typedef itk::SmartPointer<const Self> ConstPointer;
  
  itkNewMacro( Self );
  /** Run-time type information (and related methods). */
  itkTypeMacro(MeshFunctionBase, itk::Object);

  /** Mesh type that must be modified */
  typedef TMesh MeshType;

  /** Output type */
  typedef TOutput OutputType;

  /** Set the mesh to be modified */
  virtual void SetInput( MeshType* input ) { m_Mesh = input; }

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate( ) { return ( OutputType ) ( 0 ); };

protected:
  MeshFunctionBase() { m_Mesh = (MeshType*) 0; };
  ~MeshFunctionBase(){};

private:
  MeshFunctionBase(const Self& ); //purposely not implemented
  void operator=(const Self& );   //purposely not implemented

protected:
  /** Mesh on which to apply the modification */
  MeshType* m_Mesh;

};

} // namespace itkQE

#endif // __ITKQUADEDGEMESH__ITKQEMESHFUNCTIONBASE__H__

// eof - itkQEMeshFunctionBase.h

