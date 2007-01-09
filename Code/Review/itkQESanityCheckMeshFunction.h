// -------------------------------------------------------------------------
// itkQESanityCheckMeshFunction.h
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
#ifndef __ITKQUADEDGEMESH__ITKQESANITYCHECKMESHFUNCTION__H__
#define __ITKQUADEDGEMESH__ITKQESANITYCHECKMESHFUNCTION__H__

#include<itkFunctionBase.h>

namespace itkQE
{

/** \brief Make some basic checks in order to assert that the considered
 *         mesh is not degenerated and correctly represents a surface
 *         with a potential boundary.
 *
 * We check that they are no isolated vertices, no isolated edges and
 * that the Euler formula is possible.
 */
template< class TMesh >
class ITK_EXPORT SanityCheckMeshFunction
   : public itk::FunctionBase< TMesh, bool >
{
public:
   // Standard types
   typedef SanityCheckMeshFunction          Self;
   typedef itk::FunctionBase< TMesh, bool > Superclass;
   typedef itk::SmartPointer< Self >        Pointer;
   typedef itk::SmartPointer< const Self >  ConstPointer;

   // Types in superclass:
   typedef typename Superclass::InputType  InputType;
   typedef typename Superclass::OutputType OutputType;
   typedef InputType                       MeshType;

   public:
   itkNewMacro( Self );
   itkTypeMacro( SanityCheckMeshFunction, FunctionBase );

   public:
   virtual OutputType Evaluate( const InputType& mesh ) const;

   private:
   SanityCheckMeshFunction( const Self& );  // Not impl.
   void operator=( const Self& );           // Not impl.
   SanityCheckMeshFunction( ) { };

};

} // enamespace

#include "itkQESanityCheckMeshFunction.txx"

#endif // __ITKQUADEDGEMESH__ITKQESANITYCHECKMESHFUNCTION__H__

// eof - itkQESanityCheckMeshFunction.h
