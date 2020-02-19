/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkVariationalSymmetricDiffeomorphicRegistrationFilter_h
#define itkVariationalSymmetricDiffeomorphicRegistrationFilter_h

#include "itkVariationalDiffeomorphicRegistrationFilter.h"

namespace itk
{

/** \class itk::VariationalSymmetricDiffeomorphicRegistrationFilter
 *
 *  \brief Symmetric diffeomorphic deformable registration of two images using
 *  static velocity fields.
 *
 *  VariationalSymmetricDiffeomorphicRegistrationFilter is derived from
 *  VariationalRegistrationFilter and aims to minimize the  functional
 *  \f[
 *    (1)\quad J( \phi ) = \frac{1}{2} (D[R, T\circ\phi] + D[R\circ\phi^{-1}, T])
 *    + \alpha S[\phi] \rightarrow \min
 *  \f]
 *  with \f$ \phi(x)=exp(v(x))\f$ and \f$ v(x)\f$ is a static velocity field.
 *  Let \f$ f \f$ denote the force term corresponding to the similarity measure
 *  \f$ D\f$ and \f$ A\f$ denote the  linear differential operator associated
 *  with the regularization term \f$ S \f$, VariationalSymmetricDiffeomorphicRegistrationFilter
 *  implements the following iterative scheme to compute \f$ v\f$ (and \f$ \phi\f$):
 *    - initialize \f$ v_0 \f$ (default \f$ v_0=0 \f$), \f$ \phi_0=exp(v_0)\f$
 *      and \f$\phi_0^{-1}=exp(-v_0)\f$
 *    - <b>do</b>
 *      - compute the forward update field \f$ f_k^{forw} \f$ using \f$ R(x) \f$
 *        and the warped image \f$ T\circ\phi_k \f$
 *      - compute the backward update field \f$ f_k^{back} \f$ using the warped
 *        image \f$ R(x)\circ\phi_k^{-1} \f$ and \f$ T \f$
 *      - compute the next velocity field by \f$ v_{k+1} = (Id - \tau\alpha A)^{-1}
 *        (v_k + \frac{\tau}{2}(f_k^{forw} + f_k^{back}))\f$
 *      - compute the next transformation \f$ \phi_{k+1}=exp(v_{k+1})\f$ and the
 *        inverse \f$ \phi_{k+1}^{-1}=exp(-v_{k+1})\f$
 *    - <b>until</b> \f$ StopCriterion\f$ is fulfilled or \f$ k>maxIter \f$
 *
 *  The force term \f$ f \f$ is implemented in a subclass of VariationalRegistrationFunction.
 *  The computation of the regularization with \f$ (Id - \tau\alpha A)^{-1}\f$
 *  is implemented in a subclass of VariationalRegistrationRegularizer. The
 *  exponentiation of the velocity field \f$ \phi(x)=exp(v(x))\f$ is done using
 *  the ExponentialDisplacementFieldImageFilter.
 *
 *  You can set SmoothUpdateFieldOn() to smooth the velocity field before exponentiation.
 *
 *  \sa VariationalRegistrationFilter
 *  \sa VariationalDiffeomorphicRegistrationFilter
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
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
class VariationalSymmetricDiffeomorphicRegistrationFilter
  : public VariationalDiffeomorphicRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(VariationalSymmetricDiffeomorphicRegistrationFilter);

  /** Standard class type alias */
  using Self = VariationalSymmetricDiffeomorphicRegistrationFilter;
  using Superclass = VariationalDiffeomorphicRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(VariationalSymmetricDiffeomorphicRegistrationFilter, VariationalDiffeomorphicRegistrationFilter);

  /** Get image dimension. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** FixedImage image type. */
  using FixedImageType = TFixedImage;
  using FixedImagePointer = typename FixedImageType::Pointer;
  using FixedImageConstPointer = typename FixedImageType::ConstPointer;

  /** MovingImage image type. */
  using MovingImageType = TMovingImage;
  using MovingImagePointer = typename MovingImageType::Pointer;
  using MovingImageConstPointer = typename MovingImageType::ConstPointer;

  /** Deformation field type. */
  using DisplacementFieldType = TDisplacementField;
  using DisplacementFieldPointer = typename DisplacementFieldType::Pointer;

  /** Types inherited from the superclass */
  using OutputImageType = typename Superclass::OutputImageType;
  using UpdateBufferType = OutputImageType;

  /** VariationalRegistrationFunction type. */
  using RegistrationFunctionType = typename Superclass::RegistrationFunctionType;

  /** Regularizer type. */
  using RegularizerType = typename Superclass::RegularizerType;

  /** The value type of a time step.  Inherited from the superclass. */
  using TimeStepType = typename Superclass::TimeStepType;

  /** Get output inverse deformation field. */
  itkGetModifiableObjectMacro(InverseDisplacementField, DisplacementFieldType);

protected:
  VariationalSymmetricDiffeomorphicRegistrationFilter();
  ~VariationalSymmetricDiffeomorphicRegistrationFilter() override = default;

  /** Print information about the filter. */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** This method is called before iterating the solution. */
  void
  Initialize() override;

  /** Initialize the backward update after forward update was completed */
  virtual void
  InitializeBackwardIteration();

  /** Apply update function that additionally computes the inverse displacement
   *  field for the next iteration. */
  void
  ApplyUpdate(const TimeStepType & dt) override;

  /** Calculate the update for each iteration by first performing the forward
   * and then the backward update step. */
  TimeStepType
  CalculateChange() override;

  /** Calculates the inverse deformation field by calculating the exponential
   * of the negative velocity field. */
  virtual void
  CalcInverseDeformationFromVelocityField(const DisplacementFieldType * velocityField);

  /** Method to allow subclasses to get direct access to the update
   * buffer */
  itkGetModifiableObjectMacro(BackwardUpdateBuffer, UpdateBufferType);

  /** The type of region used for multithreading */
  using ThreadRegionType = typename UpdateBufferType::RegionType;

  /** Threaded version of ApplyUpdate that adds forward and backward fields  */
  void
  ThreadedApplyUpdate(const TimeStepType &     dt,
                      const ThreadRegionType & regionToProcess,
                      unsigned int             threadId) override;

private:
  using FieldExponentiatorType = typename Superclass::FieldExponentiatorType;
  using FieldExponentiatorPointer = typename FieldExponentiatorType::Pointer;

  /** The deformation field. */
  FieldExponentiatorPointer          m_InverseExponentiator;
  DisplacementFieldPointer           m_InverseDisplacementField;
  typename UpdateBufferType::Pointer m_BackwardUpdateBuffer;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVariationalSymmetricDiffeomorphicRegistrationFilter.hxx"
#endif

#endif
