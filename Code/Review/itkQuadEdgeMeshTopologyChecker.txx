// -------------------------------------------------------------------------
// itkQuadEdgeMeshTopologyChecker.txx
// $Revision: 1.2 $
// $Author: sylvain $
// $Name:  $
// $Date: 2007-01-15 19:52:58 $
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

template< class TMesh >
void
QuadEdgeMeshTopologyChecker< TMesh >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "ExpectedNumberOfPoints: "
    << static_cast<long>(m_ExpectedNumberOfPoints) << std::endl;
  os << indent << "ExpectedNumberOfEdges: "
    << static_cast<long>(m_ExpectedNumberOfEdges) << std::endl;
  os << indent << "ExpectedNumberOfFaces: "
    << static_cast<long>(m_ExpectedNumberOfFaces) << std::endl;
  os << indent << "ExpectedNumberOfBoundaries: "
    << static_cast<long>(m_ExpectedNumberOfBoundaries) << std::endl;
  os << indent << "ExpectedGenus: "
    << static_cast<long>(m_ExpectedGenus) << std::endl;
}

}
#endif 

