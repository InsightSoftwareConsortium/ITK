// -------------------------------------------------------------------------
// itkQuadEdgeMeshTopologyChecker.txx
// $Revision: 1.1 $
// $Author: ibanez $
// $Name:  $
// $Date: 2007-01-13 12:42:15 $
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
#ifndef __itkQuadEdgeMeshTopologyChecker_txx
#define __itkQuadEdgeMeshTopologyChecker_txx

#include "itkQuadEdgeMeshTopologyChecker.h"

namespace itk
{

template< class TMesh >
QuadEdgeMeshTopologyChecker< TMesh >
::QuadEdgeMeshTopologyChecker()
{
}


template< class TMesh >
bool
QuadEdgeMeshTopologyChecker< TMesh >
::ValidateEulerCharacteristic() const
{
  // FIXME move implementation here
  return true;
}


}


#endif 

