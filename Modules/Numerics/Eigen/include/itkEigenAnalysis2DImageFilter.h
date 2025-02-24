/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkEigenAnalysis2DImageFilter_h
#define itkEigenAnalysis2DImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/** \class EigenAnalysis2DImageFilter
 * \brief Computes pixel-wise the eigen values and eigen vectors
 *        of a 2D symmetrical matrix.
 *
 * The filter expects three inputs images { A, B, C } representing
 * the component of the matrix
 *
 *                    | A  B |
 *                    | B  c |
 *
 * The eigen values are stored in two output images, and the eigen
 * vector associated with the maximum eigenvalue is stored in an
 * image using vector as pixel type.
 *
 * \ingroup ShouldBeThreaded IntensityImageFilters
 * \ingroup ITKEigen
 */

template <typename TInputImage, typename TEigenValueImage, typename TEigenVectorImage>
class ITK_TEMPLATE_EXPORT EigenAnalysis2DImageFilter : public ImageToImageFilter<TInputImage, TEigenValueImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(EigenAnalysis2DImageFilter);

  /** Standard class type aliases. */
  using Self = EigenAnalysis2DImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TEigenValueImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(EigenAnalysis2DImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Typedef for the vector type representing the eigen vectors */
  using EigenVectorType = typename TEigenVectorImage::PixelType;
  using VectorComponentType = typename EigenVectorType::ValueType;

  /** Superclass type alias. */
  using typename Superclass::OutputImageRegionType;

  /** Some convenient type alias. */
  using EigenValueImageType = TEigenValueImage;
  using EigenValueImagePointer = typename EigenValueImageType::Pointer;
  using EigenValueImageRegionType = typename EigenValueImageType::RegionType;
  using EigenValueImagePixelType = typename EigenValueImageType::PixelType;
  using EigenVectorImageType = TEigenVectorImage;
  using EigenVectorImagePointer = typename EigenVectorImageType::Pointer;
  using EigenVectorImageRegionType = typename EigenVectorImageType::RegionType;
  using EigenVectorImagePixelType = typename EigenVectorImageType::PixelType;

  /** Image dimension. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Connect the image containing the elements [0,0]
   * of the input 2D matrix. */
  void
  SetInput1(TInputImage * image);

  /** Connect the image containing the elements [0,1]
   * of the input 2D matrix. This is the same [1,0]
   * element given that the input matrix is expected
   * to be symmetric. */
  void
  SetInput2(TInputImage * image);

  /** Connect the image containing the elements [1,1]
   * of the input 2D matrix. */
  void
  SetInput3(TInputImage * image);

  /** Get the output image with the largest eigenvalue.
   *
   * The sign is taken into account in the computation.
   */
  EigenValueImageType *
  GetMaxEigenValue();

  /** Get the output image with the smallest eigenvalue.
   *
   * The sign is taken into account in the computation.
   */
  EigenValueImageType *
  GetMinEigenValue();

  /** Get the output image with the eigenvector associated with
   * the greatest eigenvalue
   *
   * The sign is taken into account in the computation.
   */
  EigenVectorImageType *
  GetMaxEigenVector();

  /**  Create the output.
   *
   * \todo Verify that MakeOutput is creating the right type of objects
   * this could be the cause of the reinterpret_cast bug in this class.
   */
  using DataObjectPointerArraySizeType = ProcessObject::DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  DataObject::Pointer
  MakeOutput(DataObjectPointerArraySizeType idx) override;

  itkConceptMacro(VectorComponentHasNumericTraitsCheck, (Concept::HasNumericTraits<VectorComponentType>));

protected:
  EigenAnalysis2DImageFilter();
  ~EigenAnalysis2DImageFilter() override = default;

  void
  GenerateData() override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkEigenAnalysis2DImageFilter.hxx"
#endif

#endif
