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
#ifndef itkVariationalRegistrationFilter_h
#define itkVariationalRegistrationFilter_h

#include "itkDenseFiniteDifferenceImageFilter.h"

#include "itkVariationalRegistrationFunction.h"
#include "itkVariationalRegistrationDemonsFunction.h"
#include "itkVariationalRegistrationRegularizer.h"
#include "itkVariationalRegistrationDiffusionRegularizer.h"

namespace itk
{

/** \class itk::VariationalRegistrationFilter
 *
 *  \brief Flexible framework for deformable registration of two images using PDE-based variational registration
 *
 *  VariationalRegistrationFilter aims to minimize the  functional
 *  \f[
 *    (1)\quad J( \phi ) = D[R, T\circ\phi] + \alpha S[\phi] \rightarrow \min
 *  \f]
 *  with \f$ \phi(x)=x+u(x) \f$ by solving the associated Euler-Lagrange equation (ELE)
 *  \f[
 *    f(x, u(x)) + \alpha A[u](x)=0,\quad x\in\Omega
 *  \f]
 *  \f$ f \f$ denotes the force term corresponding to the similarity measure \f$ D\f$ and \f$ A\f$ is a linear
 * differential operator associated with the regularization term \f$ S \f$. The ELE is solved using gradient descent
 * with a semi-implicite update scheme and step size \f$ \tau \f$: \f[ (2)\quad u^{k+1} = (Id - \tau\alpha A)^{-1}(u^k +
 * \tau f^k). \f] VariationalRegistrationFilter has two images as input (fixed image \f$ R \f$, and moving image \f$ T
 * \f$) and computes the displacement field \f$ u \f$ as output.
 *
 *  The force term \f$ f \f$ is implemented in a subclass of VariationalRegistrationFunction. The computation
 *  of the regularization with \f$ (Id - \tau A)^{-1}\f$ is implemented in a subclass of
 * VariationalRegistrationRegularizer. Different force terms and regularization methods can be combined by using the
 * methods SetDifferenceFunction() and SetRegularizer().
 *
 *  The implemented method can be summarized as follows:
 *    - initialize \f$ u \f$ (default \f$ u=0 \f$)
 *    - <b>do</b>
 *      - compute the update field \f$ f^k \f$ using \f$ R(x) \f$ and the warped image \f$ T(x+u^k(x)) \f$
 *      - compute the next displacement field by \f$ u^{k+1} = (Id - \tau A)^{-1}(u^k + \tau f^k)\f$
 *    - <b>until</b> \f$ StopCriterion\f$ is fulfilled or \f$ k>maxIter \f$
 *
 *  \f$ StopCriterion\f$ is implemented in the class VariationalRegistrationStopCriterion.
 *  Use AddObserver() to enable different types of stop criteria in the
 *  variational registration framework.
 *
 *  VariationalRegistrationFilter has the following parameters:
 *    - fixed  image \f$ R \f$
 *    - moving image \f$ T \f$
 *    - force term to compute the update field (see VariationalRegistrationFunction)
 *    - regularizer (see VariationalRegistrationRegularizer)
 *    - maximum number of iterations
 *    - Initial displacement \f$ u^0 \f$ (optional)
 *    - mask image to restrict force computation (optional)
 *
 *  This class make use of the finite difference solver hierarchy.
 *
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
 * For details see:
 *   - Alexander Schmidt-Richberg, Rene Werner, Heinz Handels and Jan Ehrhardt:
 *     <i>A Flexible Variational Registration Framework</i>, Insight Journal, 2014
 *     http://hdl.handle.net/10380/3460
 *   - Rene Werner, Alexander Schmidt-Richberg, Heinz Handels and Jan Ehrhardt:
 *     <i>Estimation of lung motion fields in 4D CT data by variational non-linear
 *     intensity-based registration: A comparison and evaluation study</i>,
 *     Phys. Med. Biol., 2014
 *   - Jan Ehrhardt, Rene Werner, Alexander Schmidt-Richberg and Heinz Handels:
 *     <i>Statistical modeling of 4D respiratory lung motion using diffeomorphic
 *     image registration.</i> IEEE Trans. Med. Imaging, 30(2), 2011
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
class VariationalRegistrationFilter : public DenseFiniteDifferenceImageFilter<TDisplacementField, TDisplacementField>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(VariationalRegistrationFilter);

  /** Standard class type alias */
  using Self = VariationalRegistrationFilter;
  using Superclass = DenseFiniteDifferenceImageFilter<TDisplacementField, TDisplacementField>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(VariationalRegistrationFilter, DenseFiniteDifferenceImageFilter);

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

  /** MovingImage image type. */
  using MaskImagePixelType = unsigned char;
  using MaskImageType = Image<MaskImagePixelType, ImageDimension>;
  using MaskImagePointer = typename MaskImageType::Pointer;
  using MaskImageConstPointer = typename MaskImageType::ConstPointer;

  /** Types inherited from the superclass */
  using OutputImageType = typename Superclass::OutputImageType;

  /** The value type of a time step.  Inherited from the superclass. */
  using TimeStepType = typename Superclass::TimeStepType;

  /** VariationalRegistrationFunction type. */
  using RegistrationFunctionType =
    VariationalRegistrationFunction<FixedImageType, MovingImageType, DisplacementFieldType>;
  using DefaultRegistrationFunctionType =
    VariationalRegistrationDemonsFunction<FixedImageType, MovingImageType, DisplacementFieldType>;

  /** Regularizer type. */
  using RegularizerType = VariationalRegistrationRegularizer<DisplacementFieldType>;
  using RegularizerPointer = typename RegularizerType::Pointer;
  using DefaultRegularizerType = VariationalRegistrationDiffusionRegularizer<DisplacementFieldType>;

  /** Set the regularizer. */
  itkSetObjectMacro(Regularizer, RegularizerType);

  /** Get the regularizer. */
  itkGetConstReferenceObjectMacro(Regularizer, RegularizerType);

  /** Set the fixed image. */
  virtual void
  SetFixedImage(const FixedImageType * ptr);

  /** Get the fixed image. */
  virtual const FixedImageType *
  GetFixedImage() const;

  /** Set the moving image. */
  virtual void
  SetMovingImage(const MovingImageType * ptr);

  /** Get the moving image. */
  virtual const MovingImageType *
  GetMovingImage() const;

  /** Set the mask image. */
  virtual void
  SetMaskImage(const MaskImageType * ptr);

  /** Get the mask image. */
  virtual const MaskImageType *
  GetMaskImage() const;

  /** Set initial deformation field. */
  virtual void
  SetInitialDisplacementField(DisplacementFieldType * ptr)
  {
    this->SetInput(ptr);
  }

  /** Get output deformation field. */
  virtual DisplacementFieldType *
  GetDisplacementField()
  {
    return this->GetOutput();
  }

  /** Get the number of valid inputs. For DenseRegistration,
   * this checks whether the fixed and moving images have been
   * set. While DenseRegistration can take a third input as an
   * initial deformation field, this input is not a required input. */
  std::vector<SmartPointer<DataObject>>::size_type
  GetNumberOfValidRequiredInputs() const override;

  /** Set that the deformation field is smoothed
   * (regularized). Smoothing the deformation yields a solution
   * elastic in nature. If SmoothDisplacementField is on, then the
   * deformation field is smoothed using the VariationalRegistrationRegularizer. */
  itkBooleanMacro(SmoothDisplacementField);

  /** Set whether the deformation field is smoothed
   * (regularized). Smoothing the deformation yields a solution
   * elastic in nature. If SmoothDisplacementField is on, then the
   * deformation field is smoothed using the VariationalRegistrationRegularizer. */
  itkSetMacro(SmoothDisplacementField, bool);

  /** Set that the update field is smoothed (regularized). Smoothing the
   * update field yields a solution viscous in nature. If SmoothUpdateField is
   * on, then the update field is smoothed using the VariationalRegistrationRegularizer. */
  itkGetConstMacro(SmoothDisplacementField, bool);

  /** Set whether the update field is smoothed (regularized). Smoothing the
   * update field yields a solution viscous in nature. If SmoothUpdateField is
   * on, then the update field is smoothed using the VariationalRegistrationRegularizer. */
  itkSetMacro(SmoothUpdateField, bool);

  /** Set/Get whether the update field is smoothed
   * (regularized). Smoothing the update field yields a solution
   * viscous in nature. If SmoothUpdateField is on, then the
   * update field is smoothed using the VariationalRegistrationRegularizer. */
  itkGetConstMacro(SmoothUpdateField, bool);

  /** Get whether the update field is smoothed (regularized). Smoothing the
   * update field yields a solution viscous in nature. If SmoothUpdateField is
   * on, then the update field is smoothed using the VariationalRegistrationRegularizer. */
  itkBooleanMacro(SmoothUpdateField);

  /** Get the metric value. The metric value is the mean square difference
   * in intensity between the fixed image and transforming moving image
   * computed over the the overlapping region between the two images.
   * This value is calculated for the current iteration */
  virtual double
  GetMetric() const;

  /** Stop the registration after the current iteration. */
  virtual void
  StopRegistration()
  {
    m_StopRegistrationFlag = true;
  }

protected:
  VariationalRegistrationFilter();
  ~VariationalRegistrationFilter() override = default;

  /** Print information about the filter. */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** It is difficult to compute in advance the input moving image region
   * required to compute the requested output region. Thus the safest
   * thing to do is to request for the whole moving image.
   *
   * For the fixed image and deformation field, the input requested region
   * set to be the same as that of the output requested region. */
  void
  GenerateInputRequestedRegion() override;

  /** By default the output deformation field has the same Spacing, Origin
   * and LargestPossibleRegion as the input/initial deformation field.  If
   * the initial deformation field is not set, the output information is
   * copied from the fixed image. */
  void
  GenerateOutputInformation() override;

  /** A simple method to copy the data from the input to the output.
   * If the input does not exist, a zero field is written to the output. */
  void
  CopyInputToOutput() override;

  /** This method is called before iterating the solution. */
  void
  Initialize() override;

  /** Initialize the state of filter and equation before each iteration.
   * Progress feedback is implemented as part of this method. */
  void
  InitializeIteration() override;

  /** Apply update. */
  void
  ApplyUpdate(const TimeStepType & dt) override;

  /** Override VerifyInputInformation() since this filter's inputs do
   * not need to occupy the same physical space.
   *
   * \sa ProcessObject::VerifyInputInformation
   */
  void
  VerifyInputInformation() const override
  {}

  /** This method returns true when the current iterative solution of the
   * equation has met the criteria to stop solving. */
  bool
  Halt() override
  {
    return (Superclass::Halt() || m_StopRegistrationFlag);
  }

  /** Downcast the DifferenceFunction using a dynamic_cast to ensure that it is
   * of the correct type. This method will throw an exception if the function
   * is not of the expected type. */
  RegistrationFunctionType *
  DownCastDifferenceFunctionType();
  const RegistrationFunctionType *
  DownCastDifferenceFunctionType() const;

private:
  /** Regularizer for the smoothing of the displacement field. */
  RegularizerPointer m_Regularizer;

  /** Flag to indicate user stop registration request. */
  bool m_StopRegistrationFlag;

  /** Modes to control smoothing of the update and deformation fields */
  bool m_SmoothDisplacementField;
  bool m_SmoothUpdateField;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVariationalRegistrationFilter.hxx"
#endif

#endif
