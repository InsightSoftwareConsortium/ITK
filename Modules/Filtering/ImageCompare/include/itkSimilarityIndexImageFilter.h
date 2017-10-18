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
#ifndef itkSimilarityIndexImageFilter_h
#define itkSimilarityIndexImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNumericTraits.h"
#include "itkArray.h"

namespace itk
{
/** \class SimilarityIndexImageFilter
 * \brief Measures the similarity between the set of non-zero pixels of
 * two images.
 *
 * SimilarityIndexImageFilter measures the similarity between the set
 * non-zero pixels of two images using the following formula:
 * \f[ S = \frac{2 | A \cap B |}{|A| + |B|} \f]
 * where \f$A\f$ and \f$B\f$ are respectively the set of non-zero pixels
 * in the first and second input images. Operator \f$|\cdot|\f$ represents
 * the size of a set and \f$\cap\f$ represents the intersection of two sets.
 *
 * The measure is derived from a reliability measure known as the kappa
 * statistic. \f$S\f$ is sensitive to both differences in size and in
 * location and have been in the literature for comparing two segmentation masks.
 * For more information see:
 * "Morphometric Analysis of White Matter Lesions in MR Images: Method and
 * Validation", A. P. Zijdenbos, B. M. Dawant, R. A. Margolin and
 * A. C. Palmer, IEEE Trans. on Medical Imaging, 13(4) pp 716-724,1994
 *
 *
 * This filter requires the largest possible region of the first image
 * and the same corresponding region in the second image.
 * It behaves as filter with
 * two input and one output. Thus it can be inserted in a pipeline with
 * other filters. The filter passes the first input through unmodified.
 *
 * This filter is templated over the two input image type. It assume
 * both image have the same number of dimensions.
 *
 * \ingroup MultiThreaded
 * \ingroup ITKImageCompare
 */
template< typename TInputImage1, typename TInputImage2 >
class ITK_TEMPLATE_EXPORT SimilarityIndexImageFilter:
  public ImageToImageFilter< TInputImage1, TInputImage1 >
{
public:
  /** Standard Self typedef */
  typedef SimilarityIndexImageFilter                       Self;
  typedef ImageToImageFilter< TInputImage1, TInputImage1 > Superclass;
  typedef SmartPointer< Self >                             Pointer;
  typedef SmartPointer< const Self >                       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(SimilarityIndexImageFilter, ImageToImageFilter);

  /** Image related typedefs. */
  typedef TInputImage1                        InputImage1Type;
  typedef TInputImage2                        InputImage2Type;
  typedef typename TInputImage1::Pointer      InputImage1Pointer;
  typedef typename TInputImage2::Pointer      InputImage2Pointer;
  typedef typename TInputImage1::ConstPointer InputImage1ConstPointer;
  typedef typename TInputImage2::ConstPointer InputImage2ConstPointer;

  typedef typename TInputImage1::RegionType RegionType;
  typedef typename TInputImage1::SizeType   SizeType;
  typedef typename TInputImage1::IndexType  IndexType;

  typedef typename TInputImage1::PixelType InputImage1PixelType;
  typedef typename TInputImage2::PixelType InputImage2PixelType;

  /** Image related typedefs. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage1::ImageDimension);

  /** Type to use form computations. */
  typedef typename NumericTraits< InputImage1PixelType >::RealType RealType;

  /** Set the first input. */
  void SetInput1(const InputImage1Type *image)
  { this->SetInput(image); }

  /** Set the second input. */
  void SetInput2(const InputImage2Type *image);

  /** Get the first input. */
  const InputImage1Type * GetInput1(void)
  { return this->GetInput(); }

  /** Get the secong input. */
  const InputImage2Type * GetInput2();

  /** Return the computed similarity index. */
  itkGetConstMacro(SimilarityIndex, RealType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( Input1HasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< InputImage1PixelType > ) );
  itkConceptMacro( Input2HasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< InputImage2PixelType > ) );
  // End concept checking
#endif

protected:
  SimilarityIndexImageFilter();
  ~SimilarityIndexImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Pass the input through unmodified. Do this by Grafting in the
   * AllocateOutputs method. */
  void AllocateOutputs() ITK_OVERRIDE;

  /** Initialize some accumulators before the threads run. */
  void BeforeThreadedGenerateData() ITK_OVERRIDE;

  /** Do final mean and variance computation from data accumulated in threads.
    */
  void AfterThreadedGenerateData() ITK_OVERRIDE;

  /** Multi-thread version GenerateData. */
  void  ThreadedGenerateData(const RegionType &
                             outputRegionForThread,
                             ThreadIdType threadId) ITK_OVERRIDE;

  // Override since the filter needs all the data for the algorithm
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  // Override since the filter produces all of its output
  void EnlargeOutputRequestedRegion(DataObject *data) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SimilarityIndexImageFilter);

  RealType m_SimilarityIndex;

  Array< SizeValueType > m_CountOfImage1;
  Array< SizeValueType > m_CountOfImage2;
  Array< SizeValueType > m_CountOfIntersection;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSimilarityIndexImageFilter.hxx"
#endif

#endif
