#ifndef __itkBloxBoundaryProfileImage_txx
#define __itkBloxBoundaryProfileImage_txx

#include "itkBloxBoundaryProfileImage.h"

#include "itkSymmetricEllipsoidInteriorExteriorSpatialFunction.h"

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_math.h>
#include <itkArray.h>
#include <itkArray2D.h>

typedef vnl_matrix<double> MatrixType;
typedef vnl_vector<double> VectorType;

namespace itk
{

template <unsigned int VImageDimension>
BloxBoundaryProfileImage<VImageDimension>
::BloxBoundaryProfileImage()
{

}

template <unsigned int VImageDimension>
BloxBoundaryProfileImage<VImageDimension>
::~BloxBoundaryProfileImage()
{

}

template <unsigned int VImageDimension>
void
BloxBoundaryProfileImage<VImageDimension>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

} // end namespace itk

#endif
