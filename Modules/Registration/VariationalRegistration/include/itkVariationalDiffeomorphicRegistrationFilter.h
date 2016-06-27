/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef __itkVariationalDiffeomorphicRegistrationFilter_h
#define __itkVariationalDiffeomorphicRegistrationFilter_h

#include "itkVariationalRegistrationFilter.h"
#include "itkExponentialDisplacementFieldImageFilter.h"


namespace itk
{

/** \class itk::VariationalDiffeomorphicRegistrationFilter
 *
 *  \brief Diffeomorphic deformable registration of two images using static velocity fields.
 *
 *  VariationalDiffeomorphicRegistrationFilter is derived from VariationalRegistrationFilter and aims
 *  to minimize the  functional
 *  \f[
 *    (1)\quad J( \phi ) = D[R, T\circ\phi] + \alpha S[\phi] \rightarrow \min
 *  \f]
 *  with \f$ \phi(x)=exp(v(x))\f$ and \f$ v(x)\f$ is a static velocity field.
 *  Let \f$ f \f$ denote the force term corresponding to the similarity measure \f$ D\f$ and \f$ A\f$ denote the  linear
 *  differential operator associated with the regularization term \f$ S \f$, VariationalDiffeomorphicRegistrationFilter
 *  implements the following iterative scheme to compute \f$ v\f$ (and \f$ \phi\f$):
 *    - initialize \f$ v^0 \f$ (default \f$ v^0=0 \f$) and \f$ \phi^0=exp(v)\f$ (default \f$ \phi^0=Id\f$)
 *    - <b>do</b>
 *      - compute the update field \f$ f^k \f$ using \f$ R(x) \f$ and the warped image \f$ T\circ\phi^k \f$
 *      - compute the next velocity field by \f$ v^{k+1} = (Id - \tau\alpha A)^{-1}(v^k + \tau f^k)\f$
 *      - compute the next transformation \f$ \phi^{k+1}=exp(v^{k+1})\f$
 *    - <b>until</b> \f$ StopCriterion\f$ is fulfilled or \f$ k>maxIter \f$
 *
 *  The force term \f$ f \f$ is implemented in a subclass of VariationalRegistrationFunction. The computation
 *  of the regularization with \f$ (Id - \tau\alpha A)^{-1}\f$ is implemented in a subclass of
 * VariationalRegistrationRegularizer. The exponentiation of the velocity field \f$ \phi(x)=exp(v(x))\f$ is done using
 * the ExponentialDisplacementFieldImageFilter.
 *
 *  You can set SmoothUpdateFieldOn() to smooth the velocity field before exponentiation.
 *
 *  \sa VariationalRegistrationFilter
 *  \sa VariationalRegistrationFunction
 *  \sa VariationalRegistrationRegularizer
 *  \sa ExponentialDisplacementFieldImageFilter
 *  \sa DenseFiniteDifferenceImageFilter
 *
 *  \ingroup VariationalRegistration
 *
 *  \note This class was developed with funding from the German Research
 *  Foundation (DFG: EH 224/3-1 and HA 235/9-1).
 *  \author Alexander Schmidt-Richberg
 *  \author Rene Werner
 *  \author Jan Ehrhardt
 *
 *  For details see:
 *    - Alexander Schmidt-Richberg, Rene Werner, Heinz Handels and Jan Ehrhardt:
 *      <i>A Flexible Variational Registration Framework</i>, Insight Journal, 2014
 *      http://hdl.handle.net/10380/3460
 *    - Rene Werner, Alexander Schmidt-Richberg, Heinz Handels and Jan Ehrhardt:
 *      <i>Estimation of lung motion fields in 4D CT data by variational non-linear
 *      intensity-based registration: A comparison and evaluation study</i>,
 *      Phys. Med. Biol., 2014
 *    - Jan Ehrhardt, Rene Werner, Alexander Schmidt-Richberg and Heinz Handels:
 *      <i>Statistical modeling of 4D respiratory lung motion using diffeomorphic
 *      image registration.</i> IEEE Trans. Med. Imaging, 30(2), 2011
 */
template <class TFixedImage, class TMovingImage, class TDisplacementField>
class ITK_EXPORT VariationalDiffeomorphicRegistrationFilter
  : public VariationalRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>
{
public:
  /** Standard class typedefs */
  typedef VariationalDiffeomorphicRegistrationFilter                                   Self;
  typedef VariationalRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField> Superclass;
  typedef SmartPointer<Self>                                                           Pointer;
  typedef SmartPointer<const Self>                                                     ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(itkVariationalDiffeomorphicRegistrationFilter, VariationalRegistrationFilter);

  /** Get image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** FixedImage image type. */
  typedef TFixedImage                           FixedImageType;
  typedef typename FixedImageType::Pointer      FixedImagePointer;
  typedef typename FixedImageType::ConstPointer FixedImageConstPointer;

  /** MovingImage image type. */
  typedef TMovingImage                           MovingImageType;
  typedef typename MovingImageType::Pointer      MovingImagePointer;
  typedef typename MovingImageType::ConstPointer MovingImageConstPointer;

  /** Deformation field type. */
  typedef TDisplacementField                      DisplacementFieldType;
  typedef typename DisplacementFieldType::Pointer DisplacementFieldPointer;

  /** Types inherited from the superclass */
  typedef typename Superclass::OutputImageType OutputImageType;

  /** VariationalRegistrationFunction type. */
  typedef typename Superclass::RegistrationFunctionType RegistrationFunctionType;

  /** Regularizer type. */
  typedef typename Superclass::RegularizerType RegularizerType;

  /** The value type of a time step.  Inherited from the superclass. */
  typedef typename Superclass::TimeStepType TimeStepType;

  /** Set the desired number of iterations for the exponentiator. */
  itkSetMacro(NumberOfExponentiatorIterations, unsigned int);

  /** Get the desired number of iterations for the exponentiator. */
  itkGetConstMacro(NumberOfExponentiatorIterations, unsigned int);

  /** Set initial deformation field. \warning This can't be used for diffeomorphic registration.*/
  virtual void
  SetInitialDisplacementField(DisplacementFieldType * ptr) ITK_OVERRIDE;

  /** Get output deformation field. Returns the displacement field of the current transformation.*/
  virtual DisplacementFieldType *
  GetDisplacementField() ITK_OVERRIDE
  {
    return m_DisplacementField;
  }

  /** Set initial deformation field. */
  virtual void
  SetInitialVelocityField(DisplacementFieldType * ptr)
  {
    this->SetInput(ptr);
  }

  /** Get output velocity field. */
  virtual DisplacementFieldType *
  GetVelocityField()
  {
    return this->GetOutput();
  }

protected:
  VariationalDiffeomorphicRegistrationFilter();
  ~VariationalDiffeomorphicRegistrationFilter() {}

  /** Print information about the filter. */
  virtual void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** This method is called before iterating the solution. */
  virtual void
  Initialize() ITK_OVERRIDE;

  /** Apply update. */
  virtual void
  ApplyUpdate(const TimeStepType & dt) ITK_OVERRIDE;

  /** Calculates the deformation field by calculating the exponential
   * of the velocity field. */
  virtual void
  CalcDeformationFromVelocityField(const DisplacementFieldType * velocityField);

  /** Exponential field calculator type. */
  typedef itk::ExponentialDisplacementFieldImageFilter<DisplacementFieldType, DisplacementFieldType>
    FieldExponentiatorType;

  /** Typename for the exponentiator. */
  typedef typename FieldExponentiatorType::Pointer FieldExponentiatorPointer;

  /** Get the exponentiator used to compute a displacement from a velocity field. */
  virtual FieldExponentiatorPointer
  GetExponentiator()
  {
    return m_Exponentiator;
  }

private:
  VariationalDiffeomorphicRegistrationFilter(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented

  /** The deformation field. */
  FieldExponentiatorPointer m_Exponentiator;
  DisplacementFieldPointer  m_DisplacementField;

  /** Number of iterations for exponentiation (self composing) of velocity field. */
  unsigned int m_NumberOfExponentiatorIterations;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVariationalDiffeomorphicRegistrationFilter.hxx"
#endif

#endif
