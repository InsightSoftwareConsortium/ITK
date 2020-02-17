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
#ifndef itkVariationalRegistrationMultiResolutionFilter_h
#define itkVariationalRegistrationMultiResolutionFilter_h

#include "itkImage.h"
#include "itkMultiResolutionPyramidImageFilter.h"
#include "itkResampleImageFilter.h"
#include "itkVariationalRegistrationFilter.h"
#include "itkArray.h"

namespace itk
{
/** \class itk::VariationalRegistrationMultiResolutionFilter
 *  \brief Framework for performing multi-resolution variational registration.
 *
 *  VariationalRegistrationMultiResolutionFilter provides a generic framework
 *  to perform multi-resolution variational registration.
 *
 *  At each resolution level a VariationalRegistrationFilter is used
 *  to register two images by computing the deformation field which will
 *  map a moving image onto a fixed image.
 *
 *  A displacement field is represented as an image whose pixel type is some
 *  vector type with at least N elements, where N is the dimension of
 *  the fixed image. The vector type must support element access via operator
 *  []. It is assumed that the vector elements behave like floating point
 *  scalars.
 *
 *  The internal VariationalRegistrationFilter can be set using
 *  SetRegistrationFilter. By default the standard VariationalRegistrationFilter
 *  is used.
 *
 *  The input fixed and moving images are set via methods SetFixedImage
 *  and SetMovingImage respectively.
 *
 *  The input and output of the filter can be set via SetInput() or
 *  SetInitialField() and is interpreted either as a displacement field
 *  (standard registration) or velocity field (diffeomorphic registration).
 *  The same is true for the outpur field returned by GetOutput() or
 *  GetOutputField(). However, GetDisplacementField() always returns the
 *  corresponding displacement field.
 *
 *  MultiResolutionPyramidImageFilter are used to downsample the fixed
 *  and moving images. A VectorExpandImageFilter is used to upsample
 *  the deformation as we move from a coarse to fine solution.
 *
 *  This class is templated over the fixed image type, the moving image type,
 *  and the Deformation Field type.
 *
 *  \warning This class assumes that the fixed, moving and deformation
 *  field image types all have the same number of dimensions.
 *
 *  \sa MultiResolutionPyramidImageFilter
 *  \sa VectorExpandImageFilter
 *
 *  \ingroup VariationalRegistration
 *
 *  \note This class was developed with funding from the German Research
 *  Foundation (DFG: EH 224/3-1 and HA 235/9-1).
 *  \author Alexander Schmidt-Richberg
 *  \author Rene Werner
 *  \author Jan Ehrhardt
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField, typename TRealType = float>
class VariationalRegistrationMultiResolutionFilter : public ImageToImageFilter<TDisplacementField, TDisplacementField>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(VariationalRegistrationMultiResolutionFilter);

  /** Standard class type alias */
  using Self = VariationalRegistrationMultiResolutionFilter;
  using Superclass = ImageToImageFilter<TDisplacementField, TDisplacementField>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VariationalRegistrationMultiResolutionFilter, ImageToImageFilter);

  /** Fixed image type. */
  using FixedImageType = TFixedImage;
  using FixedImagePointer = typename FixedImageType::Pointer;
  using FixedImageConstPointer = typename FixedImageType::ConstPointer;

  /** Moving image type. */
  using MovingImageType = TMovingImage;
  using MovingImagePointer = typename MovingImageType::Pointer;
  using MovingImageConstPointer = typename MovingImageType::ConstPointer;

  /** Deformation field image type. */
  using DisplacementFieldType = TDisplacementField;
  using DisplacementFieldPointer = typename DisplacementFieldType::Pointer;

  /** ImageDimension. */
  static constexpr unsigned int ImageDimension = FixedImageType::ImageDimension;

  /** MovingImage image type. */
  using MaskImagePixelType = unsigned char;
  using MaskImageType = Image<MaskImagePixelType, ImageDimension>;
  using MaskImagePointer = typename MaskImageType::Pointer;
  using MaskImageConstPointer = typename MaskImageType::ConstPointer;

  /** Internal float image type. */
  using FloatImageType = Image<TRealType, itkGetStaticConstMacro(ImageDimension)>;

  /** The internal registration type. */
  using RegistrationType = VariationalRegistrationFilter<FixedImageType, MovingImageType, DisplacementFieldType>;
  using RegistrationPointer = typename RegistrationType::Pointer;

  /** The default registration type. */
  using DefaultRegistrationType = VariationalRegistrationFilter<FixedImageType, MovingImageType, DisplacementFieldType>;

  /** The fixed multi-resolution image pyramid type. */
  using FixedImagePyramidType = MultiResolutionPyramidImageFilter<FixedImageType, FixedImageType>;
  using FixedImagePyramidPointer = typename FixedImagePyramidType::Pointer;

  /** The moving multi-resolution image pyramid type. */
  using MovingImagePyramidType = MultiResolutionPyramidImageFilter<MovingImageType, MovingImageType>;
  using MovingImagePyramidPointer = typename MovingImagePyramidType::Pointer;

  /** The mask multi-resolution image pyramid type. */
  using MaskImagePyramidType = MultiResolutionPyramidImageFilter<FloatImageType, FloatImageType>;
  using MaskImagePyramidPointer = typename MaskImagePyramidType::Pointer;

  /** The deformation field expander type. */
  using FieldExpanderType = ResampleImageFilter<DisplacementFieldType, DisplacementFieldType>;
  using FieldExpanderPointer = typename FieldExpanderType::Pointer;

  /** Array containing the number of iterations. */
  using NumberOfIterationsType = Array<unsigned int>;

  /** Set the fixed image. */
  virtual void
  SetFixedImage(const FixedImageType * ptr);

  /** Get the fixed image. */
  const FixedImageType *
  GetFixedImage(void) const;

  /** Set the moving image. */
  virtual void
  SetMovingImage(const MovingImageType * ptr);

  /** Get the moving image. */
  const MovingImageType *
  GetMovingImage(void) const;

  /** Set the mask image. */
  void
  SetMaskImage(const MaskImageType * ptr);

  /** Get the mask image. */
  const MaskImageType *
  GetMaskImage(void) const;

  /** Set initial field, which will be smoothed and scaled to the size of the
   *  coarsest level. The field is interpreted either as displacement field
   *  (standard registration) or velocity field  (diffeomorphic registration).
   */
  virtual void
  SetInitialField(DisplacementFieldType * ptr)
  {
    this->SetInput(ptr);
  }

  /** Get the initial field. The field is either a displacement field (standard
   *  registration) or velocity field (diffeomorphic registration). */
  const DisplacementFieldType *
  GetInitialField(void)
  {
    return this->GetInput();
  }

  /** Get the output field. The field is either a displacement field (standard
   *  registration) or velocity field (diffeomorphic registration). */
  const DisplacementFieldType *
  GetOutputField(void)
  {
    return this->GetOutput();
  }

  /** Get the output displacement field provided by the registration filter. */
  itkGetModifiableObjectMacro(DisplacementField, DisplacementFieldType);

  /** Get the number of valid inputs.  For
   *  VariationalRegistrationMultiResolutionFilter, this checks whether the
   *  fixed and moving images have been set. While
   *  VariationalRegistrationMultiResolutionFilter can take a third input
   *  as an initial deformation field, this input is not a required input. */
  std::vector<SmartPointer<DataObject>>::size_type
  GetNumberOfValidRequiredInputs() const override;

  /** Set the internal registration filter. */
  itkSetObjectMacro(RegistrationFilter, RegistrationType);

  /** Get the internal registration filter. */
  itkGetConstObjectMacro(RegistrationFilter, RegistrationType);

  /** Set the fixed image pyramid. */
  itkSetObjectMacro(FixedImagePyramid, FixedImagePyramidType);

  /** Get the fixed image pyramid. */
  itkGetConstObjectMacro(FixedImagePyramid, FixedImagePyramidType);

  /** Set the moving image pyramid. */
  itkSetObjectMacro(MovingImagePyramid, MovingImagePyramidType);

  /** Get the moving image pyramid. */
  itkGetConstObjectMacro(MovingImagePyramid, MovingImagePyramidType);

  /** Set the mask image pyramid. */
  itkSetObjectMacro(MaskImagePyramid, MaskImagePyramidType);

  /** Get the mask image pyramid. */
  itkGetConstObjectMacro(MaskImagePyramid, MaskImagePyramidType);

  /** Set number of multi-resolution levels. */
  virtual void
  SetNumberOfLevels(unsigned int num);

  /** Get number of multi-resolution levels. */
  itkGetConstReferenceMacro(NumberOfLevels, unsigned int);

  /** Get the number of elapsed resolution levels. */
  itkGetConstReferenceMacro(ElapsedLevels, unsigned int);

  /** Set number of iterations per multi-resolution levels. */
  itkSetMacro(NumberOfIterations, NumberOfIterationsType);

  /** Set number of iterations per multi-resolution levels to the same value in
   *  each dimension. */
  itkSetVectorMacro(NumberOfIterations, unsigned int, m_NumberOfLevels);

  /** Get number of iterations per multi-resolution levels. */
  itkGetConstReferenceMacro(NumberOfIterations, NumberOfIterationsType);

  /** Set the moving image pyramid. */
  itkSetObjectMacro(FieldExpander, FieldExpanderType);

  /** Get the moving image pyramid. */
  itkGetConstObjectMacro(FieldExpander, FieldExpanderType);

  /** Stop the registration after the current iteration. */
  virtual void
  StopRegistration();

protected:
  VariationalRegistrationMultiResolutionFilter();
  ~VariationalRegistrationMultiResolutionFilter() override = default;

  /** Print information about the filter. */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Generate output data by performing the registration
   *  at each resolution level. */
  void
  GenerateData() override;

  /** The current implementation of this class does not support
   *  streaming. As such it requires the largest possible region
   *  for the moving, fixed and input deformation field. */
  void
  GenerateInputRequestedRegion() override;

  /** By default, the output deformation field has the same
   *  spacing, origin and LargestPossibleRegion as the input/initial
   *  deformation field.
   *
   *  If the initial deformation field is not set, the output
   *  information is copied from the fixed image. */
  void
  GenerateOutputInformation() override;

  /** The current implementation of this class does not supprot
   *  streaming. As such it produces the output for the largest
   *  possible region. */
  void
  EnlargeOutputRequestedRegion(DataObject * ptr) override;

  /** This method returns true to indicate that the registration should
   *  terminate at the current resolution level. */
  virtual bool
  Halt();

private:
  RegistrationPointer       m_RegistrationFilter;
  FixedImagePyramidPointer  m_FixedImagePyramid;
  MovingImagePyramidPointer m_MovingImagePyramid;
  MaskImagePyramidPointer   m_MaskImagePyramid;
  FieldExpanderPointer      m_FieldExpander;
  DisplacementFieldPointer  m_DisplacementField;

  unsigned int           m_NumberOfLevels;
  unsigned int           m_ElapsedLevels;
  NumberOfIterationsType m_NumberOfIterations;

  /** Flag to indicate user stop registration request. */
  bool m_StopRegistrationFlag;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVariationalRegistrationMultiResolutionFilter.hxx"
#endif


#endif
