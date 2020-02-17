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
#ifndef itkGPUNeighborhoodOperatorImageFilter_h
#define itkGPUNeighborhoodOperatorImageFilter_h

#include "itkGPUImage.h"
#include "itkGPUImageToImageFilter.h"
#include "itkImageToImageFilter.h"
#include "itkNeighborhoodOperator.h"
#include "itkImage.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkNeighborhoodOperatorImageFilter.h"

namespace itk
{
/** \class GPUNeighborhoodOperatorImageFilter
 * \brief Applies a single NeighborhoodOperator to an image region
 * using the GPU.
 *
 * This GPU filter calculates successive inner products between a single
 * NeighborhoodOperator and a NeighborhoodIterator, which is swept
 * across every pixel in an image region.
 *
 * \author Won-Ki Jeong (wkjeong\@seas.harvard.edu)
 * \ingroup ITKGPUImageFilterBase
 */

/** Create a helper GPU Kernel class for GPUNeighborhoodOperatorImageFilter */
itkGPUKernelClassMacro(GPUNeighborhoodOperatorImageFilterKernel);

template <typename TInputImage,
          typename TOutputImage,
          typename TOperatorValueType = typename TOutputImage::PixelType,
          typename TParentImageFilter = NeighborhoodOperatorImageFilter<TInputImage, TOutputImage, TOperatorValueType>>
class ITK_TEMPLATE_EXPORT GPUNeighborhoodOperatorImageFilter
  : public GPUImageToImageFilter<TInputImage, TOutputImage, TParentImageFilter>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(GPUNeighborhoodOperatorImageFilter);

  /** Standard "Self" & Superclass type alias. */
  using Self = GPUNeighborhoodOperatorImageFilter;
  using CPUSuperclass = TParentImageFilter;
  using GPUSuperclass = GPUImageToImageFilter<TInputImage, TOutputImage, TParentImageFilter>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GPUNeighborhoodOperatorImageFilter, GPUImageToImageFilter);

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  using OutputPixelType = typename TOutputImage::PixelType;
  using OutputInternalPixelType = typename TOutputImage::InternalPixelType;
  using InputPixelType = typename TInputImage::PixelType;
  using InputInternalPixelType = typename TInputImage::InternalPixelType;
  using OperatorValueType = TOperatorValueType;

  using InputPixelValueType = typename NumericTraits<InputPixelType>::ValueType;
  using ComputingPixelType = typename NumericTraits<OutputPixelType>::RealType;

  using NeighborhoodGPUBufferType = GPUImage<TOperatorValueType, Self::ImageDimension>;

  /** ImageDimension constants */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Image type alias support */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using InputImagePointer = typename InputImageType::Pointer;

  /** Typedef for generic boundary condition pointer. */
  using ImageBoundaryConditionPointerType = ImageBoundaryCondition<InputImageType> *;

  /** Typedef for the default boundary condition */
  using DefaultBoundaryCondition = ZeroFluxNeumannBoundaryCondition<InputImageType>;

  /** Superclass type alias. */
  using OutputImageRegionType = typename GPUSuperclass::OutputImageRegionType;

  /** Neighborhood types */
  using OutputNeighborhoodType = Neighborhood<OperatorValueType, Self::ImageDimension>;

  /** Get OpenCL Kernel source as a string, creates a GetOpenCLSource method */
  itkGetOpenCLSourceFromKernelMacro(GPUNeighborhoodOperatorImageFilterKernel);

  /** Sets the operator that is used to filter the image. Note
   * that the operator is stored as an internal COPY (it
   * is not part of the pipeline).*/
  void
  SetOperator(const OutputNeighborhoodType & p);

  /** Get the operator that is used to filter the image.
  const OutputNeighborhoodType & GetOperator() const
  { return m_Operator; }*/

  /** Allows a user to override the internal boundary condition. Care should be
   * be taken to ensure that the overriding boundary condition is a persistent
   * object during the time it is referenced.  The overriding condition
   * can be of a different type than the default type as long as it is
   * a subclass of ImageBoundaryCondition.
  void OverrideBoundaryCondition(const ImageBoundaryConditionPointerType i)
  { m_BoundsCondition = i; } */

  /** Get the boundary condition specified
  ImageBoundaryConditionPointerType GetBoundaryCondition()
  { return m_BoundsCondition; }*/

  /** NeighborhoodOperatorImageFilter needs a larger input requested
   * region than the output requested region.  As such,
   * NeighborhoodOperatorImageFilter needs to provide an implementation for
   * GenerateInputRequestedRegion() in order to inform the pipeline
   * execution model.
   *
   * \sa ProcessObject::GenerateInputRequestedRegion()
  virtual void GenerateInputRequestedRegion();*/

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  // itkConceptMacro( SameDimensionCheck,
  //                  ( Concept::SameDimension< InputImageDimension, ImageDimension > ) );
  // itkConceptMacro( OperatorConvertibleToOutputCheck,
  //                  ( Concept::Convertible< OperatorValueType, OutputPixelType > ) );
  // itkConceptMacro( InputConvertibleToOperatorCheck,
  //                  ( Concept::Convertible< InputPixelValueType, OperatorValueType > ) );
  // itkConceptMacro( OperatorMultiplyOperatorCheck,
  //                  ( Concept::MultiplyOperator< OperatorValueType > ) );
  // itkConceptMacro( OperatorAdditiveOperatorsCheck,
  //                  ( Concept::AdditiveOperators< OperatorValueType > ) );
  // End concept checking
#endif

protected:
  GPUNeighborhoodOperatorImageFilter();
  ~GPUNeighborhoodOperatorImageFilter() override = default;

  /** NeighborhoodOperatorImageFilter can be implemented as a
   * multithreaded filter.  Therefore, this implementation provides a
   * ThreadedGenerateData() routine which is called for each
   * processing thread. The output image data is allocated
   * automatically by the superclass prior to calling
   * ThreadedGenerateData().  ThreadedGenerateData can only write to
   * the portion of the output image specified by the parameter
   * "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()
  void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                            ThreadIdType threadId);*/

  void
  GPUGenerateData() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    GPUSuperclass::PrintSelf(os, indent);
  }

private:
  /** Internal operator used to filter the image.
  OutputNeighborhoodType m_Operator;*/

  /** Pointer to a persistent boundary condition object used
   * for the image iterator.
  ImageBoundaryConditionPointerType m_BoundsCondition;*/

  /** Default boundary condition
  DefaultBoundaryCondition m_DefaultBoundaryCondition;*/

  int m_NeighborhoodOperatorFilterGPUKernelHandle;

  typename NeighborhoodGPUBufferType::Pointer m_NeighborhoodGPUBuffer;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGPUNeighborhoodOperatorImageFilter.hxx"
#endif

#endif
