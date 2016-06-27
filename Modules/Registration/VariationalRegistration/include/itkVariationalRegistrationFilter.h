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
#ifndef __itkVariationalRegistrationFilter_h
#define __itkVariationalRegistrationFilter_h

#include "itkDenseFiniteDifferenceImageFilter.h"
#include "itkMultiThreader.h"

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
 * with a semi-implicite update scheme and step size \f$ \tau \f$:
 *  \f[
 *    (2)\quad u^{k+1} = (Id - \tau\alpha A)^{-1}(u^k + \tau f^k).
 *  \f]
 *  VariationalRegistrationFilter has two images as input (fixed image \f$ R \f$, and moving image \f$ T \f$)
 *  and computes the displacement field \f$ u \f$ as output.
 *
 *  The force term \f$ f \f$ is implemented in a subclass of VariationalRegistrationFunction. The computation
 *  of the regularization with \f$ (Id - \tau A)^{-1}\f$ is implemented in a subclass of
 * VariationalRegistrationRegularizer. Different force terms and regularization methods can be combined by using the
 * methods SetDifferenceFunction() and SetRegularizer().
 *
 *  The implemented method can be summariced as follows:
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
template <class TFixedImage, class TMovingImage, class TDisplacementField>
class ITK_EXPORT VariationalRegistrationFilter
  : public DenseFiniteDifferenceImageFilter<TDisplacementField, TDisplacementField>
{
public:
  /** Standard class typedefs */
  typedef VariationalRegistrationFilter                                            Self;
  typedef DenseFiniteDifferenceImageFilter<TDisplacementField, TDisplacementField> Superclass;
  typedef SmartPointer<Self>                                                       Pointer;
  typedef SmartPointer<const Self>                                                 ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(itkVariationalRegistrationFilter, DenseFiniteDifferenceImageFilter);

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

  /** MovingImage image type. */
  typedef unsigned char                             MaskImagePixelType;
  typedef Image<MaskImagePixelType, ImageDimension> MaskImageType;
  typedef typename MaskImageType::Pointer           MaskImagePointer;
  typedef typename MaskImageType::ConstPointer      MaskImageConstPointer;

  /** Types inherited from the superclass */
  typedef typename Superclass::OutputImageType OutputImageType;

  /** The value type of a time step.  Inherited from the superclass. */
  typedef typename Superclass::TimeStepType TimeStepType;

  /** VariationalRegistrationFunction type. */
  typedef VariationalRegistrationFunction<FixedImageType, MovingImageType, DisplacementFieldType>
    RegistrationFunctionType;
  typedef VariationalRegistrationDemonsFunction<FixedImageType, MovingImageType, DisplacementFieldType>
    DefaultRegistrationFunctionType;

  /** Regularizer type. */
  typedef VariationalRegistrationRegularizer<DisplacementFieldType>          RegularizerType;
  typedef typename RegularizerType::Pointer                                  RegularizerPointer;
  typedef VariationalRegistrationDiffusionRegularizer<DisplacementFieldType> DefaultRegularizerType;

  /** Set the regularizer. */
  itkSetObjectMacro(Regularizer, RegularizerType);

  /** Get the regularizer. */
  itkGetConstReferenceObjectMacro(Regularizer, RegularizerType);

  /** Set the fixed image. */
  virtual void
  SetFixedImage(const FixedImageType * ptr);

  /** Get the fixed image. */
  virtual const FixedImageType *
  GetFixedImage(void) const;

  /** Set the moving image. */
  virtual void
  SetMovingImage(const MovingImageType * ptr);

  /** Get the moving image. */
  virtual const MovingImageType *
  GetMovingImage(void) const;

  /** Set the mask image. */
  virtual void
  SetMaskImage(const MaskImageType * ptr);

  /** Get the mask image. */
  virtual const MaskImageType *
  GetMaskImage(void) const;

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
  virtual std::vector<SmartPointer<DataObject>>::size_type
  GetNumberOfValidRequiredInputs() const ITK_OVERRIDE;

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
  ~VariationalRegistrationFilter() {}

  /** Print information about the filter. */
  virtual void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** It is difficult to compute in advance the input moving image region
   * required to compute the requested output region. Thus the safest
   * thing to do is to request for the whole moving image.
   *
   * For the fixed image and deformation field, the input requested region
   * set to be the same as that of the output requested region. */
  virtual void
  GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** By default the output deformation field has the same Spacing, Origin
   * and LargestPossibleRegion as the input/initial deformation field.  If
   * the initial deformation field is not set, the output information is
   * copied from the fixed image. */
  virtual void
  GenerateOutputInformation() ITK_OVERRIDE;

  /** A simple method to copy the data from the input to the output.
   * If the input does not exist, a zero field is written to the output. */
  virtual void
  CopyInputToOutput() ITK_OVERRIDE;

  /** This method is called before iterating the solution. */
  virtual void
  Initialize() ITK_OVERRIDE;

  /** Initialize the state of filter and equation before each iteration.
   * Progress feedback is implemented as part of this method. */
  virtual void
  InitializeIteration() ITK_OVERRIDE;

  /** Apply update. */
  virtual void
  ApplyUpdate(const TimeStepType & dt) ITK_OVERRIDE;

  /** Override VerifyInputInformation() since this filter's inputs do
   * not need to occupy the same physical space.
   *
   * \sa ProcessObject::VerifyInputInformation
   */
  virtual void
  VerifyInputInformation() ITK_OVERRIDE
  {}

  /** This method returns true when the current iterative solution of the
   * equation has met the criteria to stop solving. */
  virtual bool
  Halt() ITK_OVERRIDE
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
  VariationalRegistrationFilter(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented

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
