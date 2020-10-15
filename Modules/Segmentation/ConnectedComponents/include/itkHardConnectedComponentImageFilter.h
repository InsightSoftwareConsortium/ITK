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
#ifndef itkHardConnectedComponentImageFilter_h
#define itkHardConnectedComponentImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"

namespace itk
{
/**
 * \class HardConnectedComponentImageFilter
 * The purpose of this program is to produce the connected components
 * for any input binary image of dimensionality n.
 *
 * The program does a forward pass line by line through the entire image.
 * Each cell in the foreground is assigned the same label value as cells
 * in its neighborhood. If there is no label among the cells in its neighborhood,
 * a new label value is assigned to the cell. This means that this cell belongs
 * to a different connected component. We set up an equivalence table for each
 * label to indicate the equivalence of the labels stored in the table. After
 * the forward pass goes through the entire image, we merge the different
 * connected components corresponding to the equivalence labels in the table.
 * We implement this strategy in the function GenerateData().
 *
 * There are two options in the program.
 * 1. Take an nD binary image as input, and produce an nD gray image, where intensity indicates label assigned to a
 * connected component.
 * 2. Take an nD binary image and a set of seed points as input, and output an nD binary image containing the cells
 * connected to the seeds. For option 2, users need to assign the member variable std::list<IndexType> m_Seeds before
 * calling function GenerateData(). \sa ImageToImageFilter \ingroup ITKConnectedComponents
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT HardConnectedComponentImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(HardConnectedComponentImageFilter);

  /**
   * Standard class typedef's
   */
  using Self = HardConnectedComponentImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /**
   * Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same.
   */
  using OutputPixelType = typename TOutputImage::PixelType;
  using OutputInternalPixelType = typename TOutputImage::InternalPixelType;
  using InputPixelType = typename TInputImage::PixelType;
  using InputInternalPixelType = typename TInputImage::InternalPixelType;
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  /**
   * Image type alias support
   */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using IndexType = typename TInputImage::IndexType;
  using SizeType = typename TInputImage::SizeType;
  using RegionType = typename TOutputImage::RegionType;
  using ListType = std::list<IndexType>;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(HardConnectedComponentImageFilter, ImageToImageFilter);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /** Setting the seed points for specified object. */
  void
  SetObjectSeed(const IndexType & seed)
  {
    m_Seeds.push_front(seed);
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<InputImageDimension, ImageDimension>));
  itkConceptMacro(IntConvertibleToOutputCheck, (Concept::Convertible<int, OutputPixelType>));
  itkConceptMacro(UnsignedShortConvertibleToOutputCheck, (Concept::Convertible<unsigned short, OutputPixelType>));
  itkConceptMacro(OutputEqualityComparableCheck, (Concept::EqualityComparable<OutputPixelType>));
  itkConceptMacro(UnsignedCharConvertibleToOutputCheck, (Concept::Convertible<unsigned char, OutputPixelType>));
  itkConceptMacro(OutputIncrementDecrementOperatorsCheck, (Concept::IncrementDecrementOperators<OutputPixelType>));
  // End concept checking
#endif

protected:
  HardConnectedComponentImageFilter() = default;
  ~HardConnectedComponentImageFilter() override = default;

  /**
   * Standard pipeline method.
   */
  void
  GenerateData() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);
  }

private:
  ListType m_Seeds;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkHardConnectedComponentImageFilter.hxx"
#endif

#endif
