#include "itkThinPlateSplineKernelTransform.h"

namespace itk
{

template <class TScalarType, int NDimensions>
ThinPlateSplineKernelTransform<TScalarType, NDimensions>::
ThinPlateSplineKernelTransform() : KernelTransform<TScalarType, NDimensions>()
{
}

template <class TScalarType, int NDimensions>
ThinPlateSplineKernelTransform<TScalarType, NDimensions>::
~ThinPlateSplineKernelTransform()
{
}

template <class TScalarType, int NDimensions>
ThinPlateSplineKernelTransform<TScalarType, NDimensions>::GMatrixType
ThinPlateSplineKernelTransform<TScalarType, NDimensions>::ComputeG(VectorType& x) const
{
  RowMatrixType xRV; // row vector rep. of x
  ColumnMatrixType xCV; // column vector rep. of x
  TScalarType r; // Euclidean norm of x

  xRV.set_row(0, x.Get_vnl_vector());
  xCV = xRV.transpose();
  r = (xCV.get_column(0)).magnitude();
  return r * m_I;
}

} // namespace itk
