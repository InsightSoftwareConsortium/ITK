#ifndef __itkThinPlateSplineKernelTransform_h
#define __itkThinPlateSplineKernelTransform_h

#include "itkKernelTransform.h"

namespace itk
{

/** \class ThinPlateSplineKernelTransform
 * This class defines the thin plate spline (TPS) transformation.
 * It is implemented in as straightforward a manner as possible from
 * the IEEE TMI paper by Davis, Khotanzad, Flamig, and Harms,
 * Vol. 16 No. 3 June 1997
 */

template <class TScalarType,         // Data type for scalars (float or double)
          int NDimensions = 3>          // Number of dimensions
class ThinPlateSplineKernelTransform : public KernelTransform<TScalarType, NDimensions>
{
public:
  /**
   * Standard Self typedef
   */
  typedef ThinPlateSplineKernelTransform<TScalarType, NDimensions> Self;
  /**
   * Standard Superclass typedef
   */
  typedef KernelTransform<TScalarType, NDimensions> Superclass;
	/**
	 * These (rather redundant) typedefs are needed because on SGI, typedefs
	 * are not inherited
	 */
	typedef typename Superclass::VectorType VectorType;
  /**
   * Default constructor
   */
  ThinPlateSplineKernelTransform();
  /**
   * Destructor
   */
  virtual ~ThinPlateSplineKernelTransform();

protected:
	/**
	 * These (rather redundant) typedefs are needed because on SGI, typedefs
	 * are not inherited
	 */
	typedef typename Superclass::GMatrixType GMatrixType;

  /**
   * Compute G(x)
   * For the thin plate spline, this is:
   * G(x) = r(x)*I
   * where
   * r(x) = Euclidean norm = sqrt[x1^2 + x2^2 + x3^2]
   * I = identity matrix
   */
  GMatrixType ComputeG(VectorType& x) const;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkThinPlateSplineKernelTransform.txx"
#endif

#endif // __itkThinPlateSplineKernelTransform_h
