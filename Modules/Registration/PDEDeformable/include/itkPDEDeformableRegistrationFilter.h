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
#ifndef itkPDEDeformableRegistrationFilter_h
#define itkPDEDeformableRegistrationFilter_h

#include "itkDenseFiniteDifferenceImageFilter.h"
#include "itkPDEDeformableRegistrationFunction.h"

namespace itk
{
/**
 * \class PDEDeformableRegistrationFilter
 * \brief Deformably register two images using a PDE algorithm.
 *
 * PDEDeformableRegistrationFilter is a base case for filter implementing
 * a PDE deformable algorithm that register two images by computing the
 * displacement field which will map a moving image onto a fixed image.
 *
 * A displacement field is represented as a image whose pixel type is some
 * vector type with at least N elements, where N is the dimension of
 * the fixed image. The vector type must support element access via operator
 * []. It is assumed that the vector elements behave like floating point
 * scalars.
 *
 * This class is templated over the fixed image type, moving image type
 * and the displacement Field type.
 *
 * The input fixed and moving images are set via methods SetFixedImage
 * and SetMovingImage respectively. An initial displacement field maybe set via
 * SetInitialDisplacementField or SetInput. If no initial field is set,
 * a zero field is used as the initial condition.
 *
 * The output displacement field can be obtained via methods GetOutput
 * or GetDisplacementField.
 *
 * The PDE algorithm is run for a user defined number of iterations.
 * Typically the PDE algorithm requires period Gaussin smoothing of the
 * displacement field to enforce an elastic-like condition. The amount
 * of smoothing is governed by a set of user defined standard deviations
 * (one for each dimension).
 *
 * In terms of memory, this filter keeps two internal buffers: one for storing
 * the intermediate updates to the field and one for double-buffering when
 * smoothing the displacement field. Both buffers are the same type and size as the
 * output displacement field.
 *
 * This class make use of the finite difference solver hierarchy. Update
 * for each iteration is computed using a PDEDeformableRegistrationFunction.
 *
 * \warning This filter assumes that the fixed image type, moving image type
 * and displacement field type all have the same number of dimensions.
 *
 * \sa PDEDeformableRegistrationFunction.
 * \ingroup DeformableImageRegistration
 * \ingroup ITKPDEDeformableRegistration
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
class ITK_TEMPLATE_EXPORT PDEDeformableRegistrationFilter:
  public DenseFiniteDifferenceImageFilter< TDisplacementField, TDisplacementField >
{
public:
  /** Standard class typedefs. */
  typedef PDEDeformableRegistrationFilter                                          Self;
  typedef DenseFiniteDifferenceImageFilter< TDisplacementField, TDisplacementField > Superclass;
  typedef SmartPointer< Self >                                                     Pointer;
  typedef SmartPointer< const Self >                                               ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(PDEDeformableRegistrationFilter,
               DenseFiniteDifferenceImageFilter);

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

#ifdef ITKV3_COMPATIBILITY
  typedef TDisplacementField                      DeformationFieldType;
  typedef typename DeformationFieldType::Pointer  DeformationFieldPointer;
#endif

  /** Types inherithed from the superclass */
  typedef typename Superclass::OutputImageType OutputImageType;

  /** FiniteDifferenceFunction type. */
  typedef typename Superclass::FiniteDifferenceFunctionType
  FiniteDifferenceFunctionType;

  /** PDEDeformableRegistrationFilterFunction type. */
  typedef PDEDeformableRegistrationFunction< FixedImageType, MovingImageType,
                                             DisplacementFieldType >  PDEDeformableRegistrationFunctionType;

  /** Inherit some enums and typedefs from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);

  /** Set the fixed image. */
  itkSetInputMacro(FixedImage, FixedImageType);

  /** Get the fixed image. */
  itkGetInputMacro(FixedImage, FixedImageType);

  /** Set the moving image. */
  itkSetInputMacro(MovingImage, MovingImageType);

  /** Get the moving image. */
  itkGetInputMacro(MovingImage, MovingImageType);

  /** Set initial displacement field. */
  itkSetInputMacro(InitialDisplacementField, DisplacementFieldType);

  /** Get initial displacement field. */
  itkGetInputMacro(InitialDisplacementField, DisplacementFieldType);

  /** Get output displacement field. */
  DisplacementFieldType * GetDisplacementField()
  { return this->GetOutput(); }

#ifdef ITKV3_COMPATIBILITY
  virtual void SetInitialDeformationField(DeformationFieldType *ptr)
  {
    this->SetInitialDisplacementField(ptr);
  }

  /** Get output deformation field. */
  DeformationFieldType * GetDeformationField(void)
  {
    return static_cast<DeformationFieldType *> (this->GetDisplacementField());
  }
#endif

  /** Get the number of valid inputs.  For PDEDeformableRegistration,
   * this checks whether the fixed and moving images have been
   * set. While PDEDeformableRegistration can take a third input as an
   * initial displacement field, this input is not a required input.
   */
  virtual std::vector< SmartPointer< DataObject > >::size_type GetNumberOfValidRequiredInputs() const ITK_OVERRIDE;

  /** Set/Get whether the displacement field is smoothed
   * (regularized). Smoothing the displacement yields a solution
   * elastic in nature. If SmoothDisplacementField is on, then the
   * displacement field is smoothed with a Gaussian whose standard
   * deviations are specified with SetStandardDeviations() */
  itkSetMacro(SmoothDisplacementField, bool);
  itkGetConstMacro(SmoothDisplacementField, bool);
  itkBooleanMacro(SmoothDisplacementField);

#ifdef ITKV3_COMPATIBILITY
  virtual void SetSmoothDeformationField(bool val)
  {
    SetSmoothDisplacementField(val);
  }
  virtual bool GetSmoothDeformationField()
  {
    return this->GetSmoothDisplacementField();
  }
  virtual void SmoothDeformationFieldOn()
  {
    this->SmoothDisplacementFieldOn();
  }
  virtual void SmoothDeformationFieldOff()
  {
    this->SmoothDisplacementFieldOff();
  }
#endif

  typedef FixedArray< double, ImageDimension > StandardDeviationsType;

  /** Set the Gaussian smoothing standard deviations for the
   * displacement field. The values are set with respect to pixel
   * coordinates. */
  itkSetMacro(StandardDeviations, StandardDeviationsType);
  virtual void SetStandardDeviations(double value);

  /** Get the Gaussian smoothing standard deviations use for smoothing
   * the displacement field. */
  itkGetConstReferenceMacro(StandardDeviations, StandardDeviationsType);

  /** Set/Get whether the update field is smoothed
   * (regularized). Smoothing the update field yields a solution
   * viscous in nature. If SmoothUpdateField is on, then the
   * update field is smoothed with a Gaussian whose standard
   * deviations are specified with SetUpdateFieldStandardDeviations() */
  itkSetMacro(SmoothUpdateField, bool);
  itkGetConstMacro(SmoothUpdateField, bool);
  itkBooleanMacro(SmoothUpdateField);

  /** Set the Gaussian smoothing standard deviations for the update
   * field. The values are set with respect to pixel coordinates. */
  itkSetMacro(UpdateFieldStandardDeviations, StandardDeviationsType);
  virtual void SetUpdateFieldStandardDeviations(double value);

  /** Get the Gaussian smoothing standard deviations used for
   * smoothing the update field. */
  itkGetConstReferenceMacro(UpdateFieldStandardDeviations, StandardDeviationsType);

  /** Stop the registration after the current iteration. */
  virtual void StopRegistration()
  { m_StopRegistrationFlag = true; }

  /** Set/Get the desired maximum error of the Gaussian kernel approximate.
   * \sa GaussianOperator. */
  itkSetMacro(MaximumError, double);
  itkGetConstMacro(MaximumError, double);

  /** Set/Get the desired limits of the Gaussian kernel width.
   * \sa GaussianOperator. */
  itkSetMacro(MaximumKernelWidth, unsigned int);
  itkGetConstMacro(MaximumKernelWidth, unsigned int);

protected:
  PDEDeformableRegistrationFilter();
  ~PDEDeformableRegistrationFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Supplies the halting criteria for this class of filters.  The
   * algorithm will stop after a user-specified number of iterations. */
  virtual bool Halt() ITK_OVERRIDE
  {
    if ( m_StopRegistrationFlag )
      {
      return true;
      }

    return this->Superclass::Halt();
  }

  /** A simple method to copy the data from the input to the output.
   * If the input does not exist, a zero field is written to the output. */
  virtual void CopyInputToOutput() ITK_OVERRIDE;

  /** Initialize the state of filter and equation before each iteration.
   * Progress feeback is implemented as part of this method. */
  virtual void InitializeIteration() ITK_OVERRIDE;

  /** Utility to smooth the displacement field (represented in the Output)
   * using a Gaussian operator. The amount of smoothing can be specified
   * by setting the StandardDeviations. */
  virtual void SmoothDisplacementField();
#ifdef ITKV3_COMPATIBILITY
  virtual void SmoothDeformationField()
  {
    this->SmoothDisplacementField();
  }
#endif
  /** Utility to smooth the UpdateBuffer using a Gaussian operator.
   * The amount of smoothing can be specified by setting the
   * UpdateFieldStandardDeviations. */
  virtual void SmoothUpdateField();

  /** This method is called after the solution has been generated. In this case,
   * the filter release the memory of the internal buffers. */
  virtual void PostProcessOutput() ITK_OVERRIDE;

  /** This method is called before iterating the solution. */
  virtual void Initialize() ITK_OVERRIDE;

  /** By default the output displacement field has the same Spacing, Origin
   * and LargestPossibleRegion as the input/initial displacement field.  If
   * the initial displacement field is not set, the output information is
   * copied from the fixed image. */
  virtual void GenerateOutputInformation() ITK_OVERRIDE;

  /** It is difficult to compute in advance the input moving image region
   * required to compute the requested output region. Thus the safest
   * thing to do is to request for the whole moving image.
   *
   * For the fixed image and displacement field, the input requested region
   * set to be the same as that of the output requested region. */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PDEDeformableRegistrationFilter);

  /** Standard deviation for Gaussian smoothing */
  StandardDeviationsType m_StandardDeviations;
  StandardDeviationsType m_UpdateFieldStandardDeviations;

  /** Modes to control smoothing of the update and displacement fields */
  bool m_SmoothDisplacementField;
  bool m_SmoothUpdateField;

  /** Temporary displacement field use for smoothing the
   * the displacement field. */
  DisplacementFieldPointer m_TempField;

private:
  /** Maximum error for Gaussian operator approximation. */
  double m_MaximumError;

  /** Limits of Gaussian kernel width. */
  unsigned int m_MaximumKernelWidth;

  /** Flag to indicate user stop registration request. */
  bool m_StopRegistrationFlag;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPDEDeformableRegistrationFilter.hxx"
#endif

#endif
