// -------------------------------------------------------------------------
// itkQEEulerOperatorSplitVertexFunction.txx
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
#ifndef __ITKQUADEDGEMESH__ITKQEEULEROPERATORSPLITVERTEXFUNCTION__TXX__
#define __ITKQUADEDGEMESH__ITKQEEULEROPERATORSPLITVERTEXFUNCTION__TXX__

#include "itkQEMesh.h"  // Just to mark the dependance towards this class.

namespace itkQE
{

template < class TMesh, class TQEType >
  typename EulerOperatorSplitVertexFunction< TMesh, TQEType >::OutputType
  EulerOperatorSplitVertexFunction< TMesh, TQEType >::
  Evaluate( QEType* e, QEType* f )
{
   return( e );
}

} // namespace itkQE

#endif // __ITKQUADEDGEMESH__ITKQEEULEROPERATORSPLITVERTEXFUNCTION__TXX__

// eof - itkQEEulerOperatorSplitVertexFunction.txx

