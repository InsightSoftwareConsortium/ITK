#include "itkElasticBodySplineKernelTransform.h"

namespace itk
{

template <class TScalarType, int NDimensions>
ElasticBodySplineKernelTransform<TScalarType, NDimensions>::
ElasticBodySplineKernelTransform() : KernelTransform<TScalarType, NDimensions>()
{
}

template <class TScalarType, int NDimensions>
ElasticBodySplineKernelTransform<TScalarType, NDimensions>::
~ElasticBodySplineKernelTransform()
{
}

template <class TScalarType, int NDimensions>
ElasticBodySplineKernelTransform<TScalarType, NDimensions>::GMatrixType
ElasticBodySplineKernelTransform<TScalarType, NDimensions>::ComputeG(VectorType& x) const
{
  RowMatrixType xRV; // row vector rep. of x
  ColumnMatrixType xCV; // column vector rep. of x
  TScalarType r; // Euclidean norm of x

  xRV.set_row(0, x.Get_vnl_vector());
  xCV = xRV.transpose();
  r = (xCV.get_column(0)).magnitude();
  return ((m_Alpha * ((TScalarType) pow(r, 2)) * m_I) - (xCV*xRV*3)) * r;
}

} // namespace itk
