

#ifndef __itkPolarToCartesianTransform_h
#define __itkPolarToCartesianTransform_h


#include <iostream>
#include "itkTransform.h"
#include "itkExceptionObject.h"
#include "itkMatrix.h"


namespace itk
{

/** \brief Polar transformation of a vector space (e.g. space coordinates).
 *
 * Transforms first two coordinates form polar space <alpha,radius> to cartesian
 * coordinates. Other dimensions are left unchanges. In fact this is generalized
 * cylindric transform:
 * \f[			x_1 = r cos( \alpha ) \f]
 * \f[			x_2 = r sin( \alpha ) \f]
 * \f[			x_n = x_n, \mbox{ n>=2 } \f]
 *
 *
 * \par
 * Center of the polar transform is a center of coordinate system < 0, 0 >.
 *
 * Dimension must be at least 2 or an exception is thrown during transform.
 *
 * Extent of input in first dimension (alpha) should be only < 0, 2*pi ).
 *
 * \author Jakub Bican, Department of Image Processing, Institute of Information Theory and Automation, Academy of
 * Sciences of the Czech Republic.
 *
 * \ingroup Transforms
 */
template <class TScalarType = double,   // Data type for scalars (float or double)
          unsigned int NDimensions = 3> // Number of dimensions
class ITK_EXPORT PolarToCartesianTransform : public Transform<TScalarType, NDimensions, NDimensions>
{
public:
  /** Standard class typedefs. */
  typedef PolarToCartesianTransform                        Self;
  typedef Transform<TScalarType, NDimensions, NDimensions> Superclass;
  typedef SmartPointer<Self>                               Pointer;
  typedef SmartPointer<const Self>                         ConstPointer;

  /** New macro for creation of through the object factory.*/
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PolarToCartesianTransform, Transform);

  /** Dimension of the domain space. */
  itkStaticConstMacro(SpaceDimension, unsigned int, NDimensions);
  itkStaticConstMacro(ParametersDimension, unsigned int, 0);

  /** Standard scalar type for this class. */
  typedef typename Superclass::ScalarType ScalarType;

  /** Standard Jacobian container. */
  typedef typename Superclass::JacobianType JacobianType;

  /** Standard vector type for this class. */
  typedef Vector<TScalarType, itkGetStaticConstMacro(SpaceDimension)> InputVectorType;
  typedef Vector<TScalarType, itkGetStaticConstMacro(SpaceDimension)> OutputVectorType;

  /** Standard covariant vector type for this class. */
  typedef CovariantVector<TScalarType, itkGetStaticConstMacro(SpaceDimension)> InputCovariantVectorType;
  typedef CovariantVector<TScalarType, itkGetStaticConstMacro(SpaceDimension)> OutputCovariantVectorType;

  /** Standard vnl_vector type for this class. */
  typedef vnl_vector_fixed<TScalarType, itkGetStaticConstMacro(SpaceDimension)> InputVnlVectorType;
  typedef vnl_vector_fixed<TScalarType, itkGetStaticConstMacro(SpaceDimension)> OutputVnlVectorType;

  /** Standard coordinate point type for this class. */
  typedef Point<TScalarType, itkGetStaticConstMacro(SpaceDimension)> InputPointType;
  typedef Point<TScalarType, itkGetStaticConstMacro(SpaceDimension)> OutputPointType;

  /** Method to transform a point.
   * This method transforms first two dimensions of a point from polar
   * coordinates <alpha,radius> to cartesian coordinates.
   */
  OutputPointType
  TransformPoint(const InputPointType & point) const;

  /** Method to transform a vector - not applicable for this type of
      transform. */
  virtual OutputVectorType
  TransformVector(const InputVectorType &) const
  {
    itkExceptionMacro(<< "Method not applicable for polar transform.");
    return OutputVectorType();
  }

  /** Method to transform a vnl_vector - not applicable for this type of
      transform. */
  virtual OutputVnlVectorType
  TransformVector(const InputVnlVectorType &) const
  {
    itkExceptionMacro(<< "Method not applicable for polar transform. ");
    return OutputVnlVectorType();
  }

  /** Method to transform a CovariantVector - not applicable for this type of
      transform */
  virtual OutputCovariantVectorType
  TransformCovariantVector(const InputCovariantVectorType &) const
  {
    itkExceptionMacro(<< "Method not applicable for polar transfrom. ");
    return OutputCovariantVectorType();
  }

  /** Compute the Jacobian Matrix of the transformation at one point - not
      applicable for this type of transform */
  virtual const JacobianType &
  GetJacobian(const InputPointType & point) const
  {
    itkExceptionMacro(<< "Method not applicable for polar transform. ");
    return m_Jacobian;
  }

protected:
  PolarToCartesianTransform();
  ~PolarToCartesianTransform();

  /** Print contents of an PolarToCartesianTransform. */
  void
  PrintSelf(std::ostream & os, Indent indent) const;

private:
  PolarToCartesianTransform(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented

}; // class PolarToCartesianTransform


} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPolarToCartesianTransform.txx"
#endif

#endif /* __itkPolarToCartesianTransform_h */
