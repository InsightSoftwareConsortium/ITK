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
   * Smart pointer typedef support
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  /**
   * Standard Superclass typedef
   */
  typedef Object Superclass;
  /**
   * Run-time type information (and related methods).
   */
  itkTypeMacro(Self, Superclass);
  /**
   * Method for creation through the object factory
   */
  itkNewMacro(Self);

protected:
  /**
   * Compute G(x)
   * For the thin plate spline, this is:
   * G(x) = r(x)*I
   * where
   * r(x) = Euclidean norm = sqrt[x1^2 + x2^2 + x3^2]
   * I = identity matrix
   */
  GMatrixType ComputeG(VectorType& x) const;
  /**
   * Default constructor
   */
  ThinPlateSplineKernelTransform();
  /**
   * Destructor
   */
  virtual ~ThinPlateSplineKernelTransform();

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkThinPlateSplineKernelTransform.txx"
#endif

#endif // __itkThinPlateSplineKernelTransform_h
