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
#ifndef itkGPUPDEDeformableRegistrationFilter_h
#define itkGPUPDEDeformableRegistrationFilter_h

#include "itkGPUDenseFiniteDifferenceImageFilter.h"
#include "itkGPUPDEDeformableRegistrationFunction.h"
#include "itkPDEDeformableRegistrationFilter.h"

namespace itk
{
/**
 * \class GPUPDEDeformableRegistrationFilter
 * \brief Deformably register two images using a PDE algorithm.
 *
 * GPUPDEDeformableRegistrationFilter is a base case for filter implementing
 * a PDE deformable algorithm that register two images by computing the
 * deformation field which will map a moving image onto a fixed image.
 *
 * A deformation field is represented as a image whose pixel type is some
 * vector type with at least N elements, where N is the dimension of
 * the fixed image. The vector type must support element access via operator
 * []. It is assumed that the vector elements behave like floating point
 * scalars.
 *
 * This class is templated over the fixed image type, moving image type
 * and the deformation Field type.
 *
 * The input fixed and moving images are set via methods SetFixedImage
 * and SetMovingImage respectively. An initial deformation field maybe set via
 * SetInitialDisplacementField or SetInput. If no initial field is set,
 * a zero field is used as the initial condition.
 *
 * The output deformation field can be obtained via methods GetOutput
 * or GetDisplacementField.
 *
 * The PDE algorithm is run for a user defined number of iterations.
 * Typically the PDE algorithm requires period Gaussin smoothing of the
 * deformation field to enforce an elastic-like condition. The amount
 * of smoothing is governed by a set of user defined standard deviations
 * (one for each dimension).
 *
 * In terms of memory, this filter keeps two internal buffers: one for storing
 * the intermediate updates to the field and one for double-buffering when
 * smoothing the deformation field. Both buffers are the same type and size as the
 * output deformation field.
 *
 * This class make use of the finite difference solver hierarchy. Update
 * for each iteration is computed using a PDEDeformableRegistrationFunction.
 *
 * \warning This filter assumes that the fixed image type, moving image type
 * and deformation field type all have the same number of dimensions.
 *
 * \sa PDEDeformableRegistrationFunction.
 * \ingroup DeformableImageRegistration
 * \ingroup ITKPDEDeformableRegistration
 * \ingroup ITKGPUPDEDeformableRegistration
 */

/** Create a helper GPU Kernel class for GPUPDEDeformableRegistrationFilter */
itkGPUKernelClassMacro(GPUPDEDeformableRegistrationFilterKernel);

template< typename TFixedImage, typename TMovingImage, typename TDisplacementField,
          typename TParentImageFilter = PDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
          >
class ITK_TEMPLATE_EXPORT GPUPDEDeformableRegistrationFilter :
  public GPUDenseFiniteDifferenceImageFilter< TDisplacementField, TDisplacementField, TParentImageFilter >
{
public:
  /** Standard class typedefs. */
  typedef GPUPDEDeformableRegistrationFilter                                                              Self;
  typedef GPUDenseFiniteDifferenceImageFilter< TDisplacementField, TDisplacementField, TParentImageFilter > GPUSuperclass;
  typedef TParentImageFilter                                                                              CPUSuperclass;
  typedef SmartPointer< Self >                                                                            Pointer;
  typedef SmartPointer< const Self >                                                                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(GPUPDEDeformableRegistrationFilter,
               GPUDenseFiniteDifferenceImageFilter);

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
  typedef typename TDisplacementField::PixelType  DeformationVectorType;
  typedef typename TDisplacementField::PixelType::ValueType
                                                 DeformationScalarType;

  /** Types inherithed from the GPUSuperclass */
  typedef typename GPUSuperclass::OutputImageType OutputImageType;

  /** FiniteDifferenceFunction type. */
  typedef typename GPUSuperclass::FiniteDifferenceFunctionType
  FiniteDifferenceFunctionType;

  /** PDEDeformableRegistrationFilterFunction type. */
  /** GPUPDEDeformableRegistrationFilterFunction type. */
  typedef GPUPDEDeformableRegistrationFunction< FixedImageType, MovingImageType,
                                                DisplacementFieldType >  GPUPDEDeformableRegistrationFunctionType;

  /** Inherit some enums and typedefs from the GPUSuperclass. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      GPUSuperclass::ImageDimension);

  /** Get OpenCL Kernel source as a string, creates a GetOpenCLSource method */
  itkGetOpenCLSourceFromKernelMacro(GPUPDEDeformableRegistrationFilterKernel);

  /** Get output deformation field. */
  DisplacementFieldType * GetDisplacementField()
  {
    return this->GetOutput();
  }

  typedef FixedArray< double, ImageDimension > StandardDeviationsType;

protected:
  GPUPDEDeformableRegistrationFilter();
  ~GPUPDEDeformableRegistrationFilter() ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** A simple method to copy the data from the input to the output.
   * If the input does not exist, a zero field is written to the output. */
  virtual void CopyInputToOutput() ITK_OVERRIDE;

  /** Initialize the state of filter and equation before each iteration.
   * Progress feeback is implemented as part of this method. */
  virtual void InitializeIteration() ITK_OVERRIDE;

  /** Utility to smooth the deformation field (represented in the Output)
   * using a Gaussian operator. The amount of smoothing can be specified
   * by setting the StandardDeviations. */
  virtual void SmoothDisplacementField() ITK_OVERRIDE;

  /** Smooth a vector field, which may be m_DisplacementField or
   * m_UpdateBuffer. */
  virtual void GPUSmoothVectorField(DisplacementFieldPointer field,
    typename GPUDataManager::Pointer GPUSmoothingKernels[],
    int GPUSmoothingKernelSizes[]);

  virtual void AllocateSmoothingBuffer();

  /** Utility to smooth the UpdateBuffer using a Gaussian operator.
   * The amount of smoothing can be specified by setting the
   * UpdateFieldStandardDeviations. */
  virtual void SmoothUpdateField() ITK_OVERRIDE;

  /** This method is called after the solution has been generated. In this case,
   * the filter release the memory of the internal buffers. */
  virtual void PostProcessOutput() ITK_OVERRIDE;

  /** This method is called before iterating the solution. */
  virtual void Initialize() ITK_OVERRIDE;

  /** By default the output deformation field has the same Spacing, Origin
   * and LargestPossibleRegion as the input/initial deformation field.  If
   * the initial deformation field is not set, the output information is
   * copied from the fixed image. */
  virtual void GenerateOutputInformation() ITK_OVERRIDE;

  /** It is difficult to compute in advance the input moving image region
   * required to compute the requested output region. Thus the safest
   * thing to do is to request for the whole moving image.
   *
   * For the fixed image and deformation field, the input requested region
   * set to be the same as that of the output requested region. */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GPUPDEDeformableRegistrationFilter);

  /** Temporary deformation field use for smoothing the
   * the deformation field. */
  DisplacementFieldPointer m_TempField;

private:
  /** Memory buffer for smoothing kernels of the displacement field. */
  int                              m_SmoothingKernelSizes[ImageDimension];
  DeformationScalarType*           m_SmoothingKernels[ImageDimension];
  typename GPUDataManager::Pointer m_GPUSmoothingKernels[ImageDimension];

  /** Memory buffer for smoothing kernels of the update field. */
  int                              m_UpdateFieldSmoothingKernelSizes[ImageDimension];
  DeformationScalarType*           m_UpdateFieldSmoothingKernels[ImageDimension];
  typename GPUDataManager::Pointer m_UpdateFieldGPUSmoothingKernels[ImageDimension];

  int*                             m_ImageSizes;
  typename GPUDataManager::Pointer m_GPUImageSizes;

  /* GPU kernel handle for GPUSmoothDisplacementField */
  int m_SmoothDisplacementFieldGPUKernelHandle;

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGPUPDEDeformableRegistrationFilter.hxx"
#endif

#endif
