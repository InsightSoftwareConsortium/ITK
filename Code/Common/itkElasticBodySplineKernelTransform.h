#ifndef __itkElasticBodySplineKernelTransform_h
#define __itkElasticBodySplineKernelTransform_h

#include "itkKernelTransform.h"

namespace itk
{

/** \class ElasticBodySplineKernelTransform
 * This class defines the elastic body spline (EBS) transformation.
 * It is implemented in as straightforward a manner as possible from
 * the IEEE TMI paper by Davis, Khotanzad, Flamig, and Harms,
 * Vol. 16 No. 3 June 1997
 * Taken from the paper:
 * The EBS "is based on a physical model of a homogeneous, isotropic,
 * three-dimensional elastic body. The model can approximate the way
 * that some physical objects deform".
 */
template <class TScalarType,         // Data type for scalars (float or double)
          int NDimensions = 3>          // Number of dimensions
class ElasticBodySplineKernelTransform : public KernelTransform<TScalarType, NDimensions>
{
public:
  /**
   * Standard Self typedef
   */
  typedef ElasticBodySplineKernelTransform<TScalarType, NDimensions> Self;
  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  /**
   * Standard Superclass typedef
   */
  typedef KernelTransform<TScalarType, NDimensions> Superclass;
  /**
   * Run-time type information (and related methods).
   */
  itkTypeMacro(Self, Superclass);
  /**
   * Method for creation through the object factory
   */
  itkNewMacro(Self);
  /**
   * Set alpha
   */
  itkSetMacro(Alpha, TScalarType);
  /**
   * Get alpha
   */
  itkGetMacro(Alpha, TScalarType);

protected:
  /**
   * Compute G(x)
   * For the elastic body spline, this is:
   * G(x) = [alpha*r(x)^2*I - 3*x*x']*r(x)
   * where
   * alpha is a constant
   * r(x) = Euclidean norm = sqrt[x1^2 + x2^2 + x3^2]
   * I = identity matrix
   */
  GMatrixType ComputeG(VectorType& x) const;
  /**
   * alpha, Poisson's ratio
   */
  TScalarType m_Alpha;
  /**
   * Default constructor
   */
  ElasticBodySplineKernelTransform();
  /**
   * Destructor
   */
  virtual ~ElasticBodySplineKernelTransform();

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkElasticBodySplineKernelTransform.txx"
#endif

#endif // __itkElasticBodySplineKernelTransform_h
