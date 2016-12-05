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
#ifndef itkMultiResolutionPDEDeformableRegistration_h
#define itkMultiResolutionPDEDeformableRegistration_h

#include "itkImage.h"
#include "itkDemonsRegistrationFilter.h"
#include "itkMultiResolutionPyramidImageFilter.h"
#include "itkVectorResampleImageFilter.h"
#include "itkArray.h"

namespace itk
{
/**
 * \class MultiResolutionPDEDeformableRegistration
 * \brief Framework for performing multi-resolution PDE
 * deformable registration.
 *
 * MultiResolutionPDEDeformableRegistration provides a generic framework
 * to peform multi-resolution deformable registration.
 *
 * At each resolution level a PDEDeformableRegistrationFilter is used
 * to register two images by computing the deformation field which will
 * map a moving image onto a fixed image.
 *
 * A deformation field is represented as an image whose pixel type is some
 * vector type with at least N elements, where N is the dimension of
 * the fixed image. The vector type must support element access via operator
 * []. It is assumed that the vector elements behave like floating point
 * scalars.
 *
 * The internal PDEDeformationRegistrationFilter can be set using
 * SetRegistrationFilter. By default a DemonsRegistrationFilter is used.
 *
 * The input fixed and moving images are set via methods SetFixedImage
 * and SetMovingImage respectively. An initial deformation field maybe set via
 * SetInitialDisplacementField if is matches the characteristics of the coarsest
 * pyramid level. If no such assumption can be made (e.g. the deformation field
 * has the same characteristics as the input images), an initial deformation
 * field can still be set via SetArbitraryInitialDisplacementField or
 * SetInput. The filter will then take care of mathching the coarsest level
 * characteristics. If no initial field is set a zero field is used as the
 * initial condition.
 *
 * MultiResolutionPyramidImageFilters are used to downsample the fixed
 * and moving images. A VectorExpandImageFilter is used to upsample
 * the deformation as we move from a coarse to fine solution.
 *
 * This class is templated over the fixed image type, the moving image type,
 * and the Deformation Field type.
 *
 * \warning This class assumes that the fixed, moving and deformation
 * field image types all have the same number of dimensions.
 *
 * \sa PDEDeformableRegistrationFilter
 * \sa DemonsRegistrationFilter
 * \sa MultiResolutionPyramidImageFilter
 * \sa VectorExpandImageFilter
 *
 * The current implementation of this class does not support streaming.
 *
 * \ingroup DeformableImageRegistration
 * \ingroup ITKPDEDeformableRegistration
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField, typename TRealType = float >
class ITK_TEMPLATE_EXPORT MultiResolutionPDEDeformableRegistration:
  public ImageToImageFilter< TDisplacementField, TDisplacementField >
{
public:
  /** Standard class typedefs */
  typedef MultiResolutionPDEDeformableRegistration Self;
  typedef ImageToImageFilter< TDisplacementField, TDisplacementField >
  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MultiResolutionPDEDeformableRegistration,
               ImageToImageFilter);

  /** Fixed image type. */
  typedef TFixedImage                           FixedImageType;
  typedef typename FixedImageType::Pointer      FixedImagePointer;
  typedef typename FixedImageType::ConstPointer FixedImageConstPointer;

  /** Moving image type. */
  typedef TMovingImage                           MovingImageType;
  typedef typename MovingImageType::Pointer      MovingImagePointer;
  typedef typename MovingImageType::ConstPointer MovingImageConstPointer;

  /** Deformation field image type. */
  typedef TDisplacementField                      DisplacementFieldType;
  typedef typename DisplacementFieldType::Pointer DisplacementFieldPointer;
#ifdef ITKV3_COMPATIBILITY
  typedef TDisplacementField                      DeformationFieldType;
  typedef typename DeformationFieldType::Pointer  DeformationFieldPointer;
#endif
  /** ImageDimension. */
  itkStaticConstMacro(ImageDimension, unsigned int, FixedImageType::ImageDimension);

  /** Internal float image type. */
  typedef Image< TRealType, itkGetStaticConstMacro(ImageDimension) > FloatImageType;

  /** The internal registration type. */
  typedef PDEDeformableRegistrationFilter< FloatImageType, FloatImageType, DisplacementFieldType > RegistrationType;
  typedef typename RegistrationType::Pointer                                                      RegistrationPointer;

  /** The default registration type. */
  typedef DemonsRegistrationFilter< FloatImageType, FloatImageType, DisplacementFieldType > DefaultRegistrationType;

  /** The fixed multi-resolution image pyramid type. */
  typedef MultiResolutionPyramidImageFilter< FixedImageType, FloatImageType > FixedImagePyramidType;
  typedef typename FixedImagePyramidType::Pointer                             FixedImagePyramidPointer;

  /** The moving multi-resolution image pyramid type. */
  typedef MultiResolutionPyramidImageFilter< MovingImageType, FloatImageType > MovingImagePyramidType;
  typedef typename MovingImagePyramidType::Pointer                             MovingImagePyramidPointer;

  /** The deformation field expander type. */
  typedef VectorResampleImageFilter< DisplacementFieldType, DisplacementFieldType > FieldExpanderType;
  typedef typename FieldExpanderType::Pointer                                     FieldExpanderPointer;

  typedef Array< unsigned int > NumberOfIterationsType;

  /** Set the fixed image. */
  virtual void SetFixedImage(const FixedImageType *ptr);

  /** Get the fixed image. */
  const FixedImageType * GetFixedImage() const;

  /** Set the moving image. */
  virtual void SetMovingImage(const MovingImageType *ptr);

  /** Get the moving image. */
  const MovingImageType * GetMovingImage() const;

  /** Set initial deformation field to be used as is (no smoothing, no
   *  subsampling at the coarsest level of the pyramid. */
  virtual void SetInitialDisplacementField(DisplacementFieldType *ptr)
  {
    this->m_InitialDisplacementField = ptr;
  }

  /** Set initial deformation field. No assumption is made on the
   *  input. It will therefore be smoothed and resampled to match the
   *  images characteristics at the coarsest level of the pyramid. */
  virtual void SetArbitraryInitialDisplacementField(DisplacementFieldType *ptr)
  {
    this->SetInput(ptr);
  }

  /** Get output deformation field. */
  const DisplacementFieldType * GetDisplacementField(void)
  { return this->GetOutput(); }

#ifdef ITKV3_COMPATIBILITY
  virtual void SetInitialDeformationField(DisplacementFieldType *ptr)
  {
    this->SetInitialDisplacementField(ptr);
  }

  virtual void SetArbitraryInitialDeformationField(DisplacementFieldType *ptr)
  {
    this->SetArbitraryInitialDisplacementField(ptr);
  }

  /** Get output deformation field. */
  const DeformationFieldType * GetDeformationField(void)
  {
    return itkDynamicCastInDebugMode<DeformationFieldType *> (this->GetDisplacementField());
  }
#endif

  /** Get the number of valid inputs.  For
   * MultiResolutionPDEDeformableRegistration, this checks whether the
   * fixed and moving images have been set. While
   * MultiResolutionPDEDeformableRegistration can take a third input
   * as an initial deformation field, this input is not a required input.
   */
  virtual std::vector< SmartPointer< DataObject > >::size_type GetNumberOfValidRequiredInputs() const ITK_OVERRIDE;

  /** Get/Set the internal registrator. */
  itkSetObjectMacro(RegistrationFilter, RegistrationType);
  itkGetModifiableObjectMacro(RegistrationFilter, RegistrationType);

  /** Get/Set the fixed image pyramid. */
  itkSetObjectMacro(FixedImagePyramid, FixedImagePyramidType);
  itkGetModifiableObjectMacro(FixedImagePyramid, FixedImagePyramidType);

  /** Get/Set the moving image pyramid. */
  itkSetObjectMacro(MovingImagePyramid, MovingImagePyramidType);
  itkGetModifiableObjectMacro(MovingImagePyramid, MovingImagePyramidType);

  /** Set number of multi-resolution levels. */
  virtual void SetNumberOfLevels(unsigned int num);

  /** Get number of multi-resolution levels. */
  itkGetConstReferenceMacro(NumberOfLevels, unsigned int);

  /** Get the current resolution level being processed. */
  itkGetConstReferenceMacro(CurrentLevel, unsigned int);

  /** Get/Set the moving image pyramid. */
  itkSetObjectMacro(FieldExpander, FieldExpanderType);
  itkGetModifiableObjectMacro(FieldExpander, FieldExpanderType);

  /** Set number of iterations per multi-resolution levels. */
  itkSetMacro(NumberOfIterations, NumberOfIterationsType);
  itkSetVectorMacro(NumberOfIterations, unsigned int, m_NumberOfLevels);

  /** Get number of iterations per multi-resolution levels. */
  itkGetConstReferenceMacro(NumberOfIterations, NumberOfIterationsType);

  /** Stop the registration after the current iteration. */
  virtual void StopRegistration();

protected:
  MultiResolutionPDEDeformableRegistration();
  // ~MultiResolutionPDEDeformableRegistration() {} default implementation ok

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Generate output data by performing the registration
   * at each resolution level. */
  virtual void GenerateData() ITK_OVERRIDE;

  /** The current implementation of this class does not support
   * streaming. As such it requires the largest possible region
   * for the moving, fixed and input deformation field. */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** By default, the output deformation field has the same
   * spacing, origin and LargestPossibleRegion as the input/initial
   * deformation field.
   *
   * If the initial deformation field is not set, the output
   * information is copied from the fixed image. */
  virtual void GenerateOutputInformation() ITK_OVERRIDE;

  /** The current implementation of this class does not supprot
   * streaming. As such it produces the output for the largest
   * possible region. */
  virtual void EnlargeOutputRequestedRegion(DataObject *ptr) ITK_OVERRIDE;

  /** This method returns true to indicate that the registration should
   * terminate at the current resolution level. */
  virtual bool Halt();

  /** Override VeriyInputInformation() since this filter's inputs do
   * not need to occoupy the same physical space.
   *
   * \sa ProcessObject::VerifyInputInformation
   */
  virtual void VerifyInputInformation() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MultiResolutionPDEDeformableRegistration);

  RegistrationPointer       m_RegistrationFilter;
  FixedImagePyramidPointer  m_FixedImagePyramid;
  MovingImagePyramidPointer m_MovingImagePyramid;
  FieldExpanderPointer      m_FieldExpander;
  DisplacementFieldPointer  m_InitialDisplacementField;

  unsigned int                m_NumberOfLevels;
  unsigned int                m_CurrentLevel;
  NumberOfIterationsType      m_NumberOfIterations;

  /** Flag to indicate user stop registration request. */
  bool m_StopRegistrationFlag;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMultiResolutionPDEDeformableRegistration.hxx"
#endif

#endif
