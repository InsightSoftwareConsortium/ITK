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
#ifndef itkKLMRegionGrowImageFilter_h
#define itkKLMRegionGrowImageFilter_h

#include "itkImage.h"
#include "itkRegionGrowImageFilter.h"
#include "itkKLMSegmentationBorder.h"
#include "itkImageRegionIterator.h"
#include "itkConceptChecking.h"
#include <algorithm>
#include <functional>

namespace itk
{
/**
 *\class KLMRegionGrowImageFilter
 * \brief Base class for a region growing object that performs energy-based
 * region growing for multiband images.
 *
 * itkKLMRegionGrowImageFilter is the base class for the KLMRegionGrowImageFilter objects.
 * This object performs energy-based region growing for multiband images.
 * Since this is based on G. Koepfler,C. Lopez and J. M. Morel's work
 * described below, the acronym KLM is added at the end of the object name.
 *
 * The ApplyRegionGrowImageFilter() function implements the segmentation algorithm
 * that partitions the input image into non-overlapping regions
 * by minimizing an energy functional which trades off the similarity
 * of regions against the length of their shared boundary. The heart of the
 * process relies on the MergeRegion() method that calls a private function
 * to perform the merging of region based on the piecewise constant KLM
 * algorithm for region merging. For extensibility purposes, the MergeRegion()
 * function is made virtual. Extensions can be made possible using
 * function overloading or overriding the virtual function in a derived
 * class. It starts by breaking the image into many small regions and fitting
 * the regions to a polynomial model.  The algorithm iteratively merges into
 * one region the two adjoining regions which are most alike in terms
 * of the specified polynomial model given the length of the border
 * between the two regions.  Internally, the energy functional is
 * evaluated using a Lagrangian parameter called lambda which is also
 * called the scale parameter as it controls the coarseness of the
 * segmentation where a small value of lambda corresponds to a finer
 * segmentation with more regions and a large value corresponds to a
 * coarse segmentation with fewer regions.  Since the algorithm grows
 * regions by merging like regions, the internal value of lambda
 * increases as the number of regions decreases.
 *
 * The user can stop the merging of regions using the
 * SetMaximumNumberOfRegions() and SetMaximumLambda() functions.
 * The SetMaximumNumberOfRegions() function is publicly
 * inherited from its base class and internally sets the m_MaximumNumberOfRegions
 * parameter. The SetMaximumLambda() function sets the m_MaximumLambda
 * parameter. If the
 * number of regions in the image is equal to m_MaximumNumberOfRegions or if the
 * internal energy functional becomes greater than m_MaximumLambda, then the
 * merging iterations will stop.  Note that a larger energy function
 * value for m_MaximumLambda
 * will result in fewer boundaries and fewer regions, while a smaller value
 * for m_MaximumLambda will result in more boundaries and more regions. To have
 * m_MaximumNumberOfRegions control exactly the number of output regions, m_MaximumLamda
 * should be set to a very large number. To have m_MaximumLambda control exactly
 * the number of output regions, m_MaximumNumberOfRegions should be set to 2. As a
 * default value, m_MaximumLambda is set to 1000 and m_MaximumNumberOfRegions
 * is set to 2.
 *
 * Currently implementation puts equal weight to the multichannel values.
 * In future improvements we plan to allow the user to control the weights
 * associated with each individual channels.
 *
 * It is templated over the type of input and output image. This object
 * supports data handling of multiband images. The object accepts images
 * in vector format, where each pixel is a vector and each element of the
 * vector corresponds to an entry from 1 particular band of a multiband
 * dataset. We expect the user to provide the input to the routine in vector
 * format. A single band image is treated as a vector image with a single
 * element for every vector.
 *
 * This algorithm implementation takes a multiband image stored in vector
 * format as input and produces two outputs. Using the ImageToImageFilter,
 * the piecewise constant approximation image is the output calculated
 * using the process update mechanism. The second output, i.e., the
 * image with the region labels (segmentation image) is returned at
 * users request by calling GetLabelledImage() function. This function
 * returns a reference to the labelled image determined using the KLM
 * algorithm. The algorithm supports 2D and 3D data sets only. The input
 * image dimensions must be exact multiples of the user specified gridsizes.
 * Appropriate padding must be performed by the user if any image which
 * are not multiples of the gridsizes are used.
 *
 * For more information about the algorithm, see G. Koepfler, C. Lopez
 * and J. M. Morel, ``A Multiscale Algorithm for Image Segmentation by
 * Variational Method,'' {\em SIAM Journal of Numerical Analysis},
 * vol. 31, pp. 282-299, 1994.
 *
 * Algorithm details:
 *
 * This function segments a two-dimensional input image into
 * non-overlapping atomic regions \f$ O_i, i=1,2,\ldots,N \f$, where
 * \f$ N \f$ is the
 * total number of region, by minimizing the following energy functional
 * (also known as the simplified Mumford and Shah functional):
 * \f$
 * E(u,K)=\int_{\Omega-K}||u(r,c)-g(r,c)||^2{d{\Omega}}+\lambda\cdot{L(K)}
 * \f$,
 * where \f$ \Omega \f$ denotes the domain of an image, \f$ g(r,c) \f$
 * is the input image, and \f$ u(r,c) \f$ is an approximation of
 * \f$ g(r,c) \f$.  Furthermore, \f$ u(r,c) \f$ is defined to be
 * piecewise constant in regions \f$ O_i \f$.  If
 * \f$ \partial O_i \f$ represents the boundary of the region,
 * \f$ K=\bigcup_{i=1}^N\partial{O_i} \f$ denotes the set of all region
 * boundaries and \f$ L(K) \f$ is the total length of the boundaries.  The
 * parameter \f$ \lambda \f$ controls the coarseness of the segmentation
 * (i.e. a larger \f$ \lambda \f$ will result in fewer boundaries).
 *
 * Starting with small, piecewise-constant initial regions the algorithm
 * iteratively merges the two adjacent regions \f$ O_i \f$ and
 * \f$ O_j \f$ which  most
 * decrease the energy functional.  In other words, the merging criterion
 * is based on the difference between the current energy
 * \f$ E(u,K) \f$ and the
 * energy that would result after a merge,
 * \f$ E(\hat{u},K-\partial(O_i,O_j)) \f$,
 * where \f$ \hat{u} \f$ is the piecewise constant approximation of the
 * input image \f$ g \f$, and \f$ \partial(O_i,O_j) \f$ is the common boundary
 * between region \f$ O_i \f$ and \f$ O_j \f$.  It can be shown that
 * \f$ E(u,K)-E(\hat{u},K-\partial(O_i,O_j))=
 * \lambda\cdot{L(\partial(O_i,O_j))}-
 * {\frac{(|O_i| \cdot |O_j|)}{(|O_i|+|O_j|)}} \|c_i-c_j\|^2 \f$.
 *
 * Once two regions are merged the following update equations are used
 * to calculated the constant approximation of the new region:
 *
 * \f$ c_{i,j} = \frac{(c_i |O_i| + c_j |O_j|)}{(|O_i| + |O_j|)} \f$.
 *
 * Again, the merging of regions continues until the desired number of
 * regions has been reached or until the desired coarseness (specified
 * by the scale parameter \f$ \lambda \f$) has been reached.
 *
 * The two outputs are possible to derive from the object:
 * (1) u, the piecewise constant approximation (mean of the regions)
 *     to the input image set; This is currently generated by the
 *     process object pipeline and the
 * (2) the labelled regions in the input image set is generated by the
 *     GetLabelledImage() function.
 *
 * \ingroup RegionGrowingSegmentation
 * \ingroup ITKKLMRegionGrowing
 */

template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT KLMRegionGrowImageFilter : public RegionGrowImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(KLMRegionGrowImageFilter);

  /** Standard class type aliases. */
  using Self = KLMRegionGrowImageFilter;
  using Superclass = RegionGrowImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(KLMRegionGrowImageFilter, RegionGrowImageFilter);

  /** Type definition for the input image. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename TInputImage::Pointer;
  using InputImageConstPointer = typename TInputImage::ConstPointer;

  /** Type definition for the input image pixel type. */
  using InputImagePixelType = typename TInputImage::PixelType;

  /** Type definition for the input image pixel vector type. */
  using InputImageVectorType = typename TInputImage::PixelType::VectorType;

  /** InputImageVectorDimension enumeration. */
  static constexpr unsigned int InputImageVectorDimension = InputImagePixelType::Dimension;

  /** Type definition for the input image index type. */
  using InputImageIndexType = typename TInputImage::IndexType;

  /** Type definition for the image iterators to be used. */
  using InputImageIterator = ImageRegionIterator<TInputImage>;
  using InputImageConstIterator = ImageRegionConstIterator<TInputImage>;

  /** Type definition for the image region type. */
  using InputRegionType = typename TInputImage::RegionType;

  /** Type definition for the input grid size type used to create
   * initial atomic regions. */
  using GridSizeType = typename Superclass::GridSizeType;

  /** Type definition for the output image. */
  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename TOutputImage::Pointer;

  /** InputImageDimension enumeration. */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;

  /** OutputImageDimension enumeration. */
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Type definition for the output image pixel type. */
  using OutputImagePixelType = typename TOutputImage::PixelType;

  /** Type definition for the output image pixel vector type. */
  using OutputImageVectorType = typename TOutputImage::PixelType::VectorType;

  /** OutputImageVectorDimension enumeration. */
  static constexpr unsigned int OutputImageVectorDimension = OutputImagePixelType::Dimension;

  /** Type definition for the output image index type. */
  using OutputImageIndexType = typename TOutputImage::IndexType;

  /** Type definition for the output image iterators.  */
  using OutputImageIterator = ImageRegionIterator<TOutputImage>;

  /** type definition for the region label type. */
  using RegionLabelType = typename KLMSegmentationRegion::RegionLabelType;

  /** The dimension of the labelled image. */
  static constexpr RegionLabelType LabelImageDimension = InputImageDimension;

  /** Type definition for the labelled image pixel type. */
  using LabelImageType = Image<RegionLabelType, Self::LabelImageDimension>;

  /** Type definition for the labelled image pointer.  */
  using LabelImagePointer = typename LabelImageType::Pointer;

  /** Type definition for the labelled image pixel type. */
  using LabelImagePixelType = typename LabelImageType::PixelType;

  /** Type definition for the labelled image index type. */
  using LabelImageIndexType = typename LabelImageType::IndexType;

  /** Type definition for the labelled image iterators.  */
  using LabelImageIterator = ImageRegionIterator<LabelImageType>;

  /** Storage type for the mean region intensity. */
  using MeanRegionIntensityType = vnl_vector<double>;

  /** Type definition for the smart border type. */
  using BorderType = KLMSegmentationBorder;

  /** Type definition for the smart border pointers object. */
  using KLMSegmentationBorderArrayPtr = KLMDynamicBorderArray<BorderType>;

  /** Set/Get the desired threshold parameter for lambda. See
   * itkSegmentationBorder documentation for details regarding this
   * parameter.  */
  itkSetMacro(MaximumLambda, double);
  itkGetConstReferenceMacro(MaximumLambda, double);

  /** Set/Get the desired number of regions. */
  itkSetMacro(NumberOfRegions, unsigned int);
  itkGetConstReferenceMacro(NumberOfRegions, unsigned int);

  /** Generate labelled image. */
  LabelImagePointer
  GetLabelledImage();

  /** Function that prints all the region information.  */
  void
  PrintAlgorithmRegionStats();

  /** Function that prints all the border information.  */
  void
  PrintAlgorithmBorderStats();

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<typename InputImagePixelType::ValueType>));
  itkConceptMacro(SameDimension, (Concept::SameDimension<Self::InputImageDimension, Self::OutputImageDimension>));
#  if defined(THIS_CONCEPT_FAILS_ON_GCC)
  /** The input pixel type must be the same as that of the output image. */
  itkConceptMacro(SameVectorDimension,
                  (Concept::SameDimension<Self::InputImageVectorDimension, Self::OutputImageVectorDimension>));
#  endif
  // End concept checking
#endif

protected:
  KLMRegionGrowImageFilter();
  ~KLMRegionGrowImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /**
   * Standard pipeline method.
   */
  void
  GenerateData() override;

  /** KLMRegionGrowImageFilter needs the entire input. Therefore
   * it must provide an implementation GenerateInputRequestedRegion().
   * \sa ProcessObject::GenerateInputRequestedRegion(). */
  void
  GenerateInputRequestedRegion() override;

  /** KLMRegionGrowImageFilter will produce all of the output.
   * Therefore it must provide an implementation of
   * EnlargeOutputRequestedRegion().
   * \sa ProcessObject::EnlargeOutputRequestedRegion() */
  void
  EnlargeOutputRequestedRegion(DataObject *) override;

  /** This is the interface function that calls the specific algorithm
   * implementation of region growing. */
  void
  ApplyRegionGrowImageFilter() override;

  /** Function to merge two regions.
   * The smaller label is always assigned to the new region.  This is
   * consistent with the connected components algorithm. */
  void
  MergeRegions() override;

  /** Generate output approximated image. */
  virtual void
  GenerateOutputImage();

  /** Function that calls the KLM region growing algorithm. */
  void
  ApplyKLM();

  /** Initialize the RegionGrowImageFilter algorithm. */
  void
  InitializeKLM();

  /** Generate the labelled image. */
  LabelImagePointer
  GenerateLabelledImage(LabelImageType * labelImagePtr);

  /** Calculate the statistics representing the region. In this
   * case we compute the mean region intensity and the area of the
   * initial N-dimensional rectangular area. This is the function that
   * can be overriden in order to enable a different statistical
   * representation for region initialization. */
  virtual void
  InitializeRegionParameters(InputRegionType region);

  /** Function to resolve the region labels to be consecutively ordered.
   * Each initial atomic region is given a new label and the aggregrate
   * region area and mean intensity. */
  virtual void
  ResolveRegions();

private:
  using InputImageSizeType = typename TInputImage::SizeType;
  using KLMSegmentationRegionPtr = typename KLMSegmentationRegion::Pointer;
  using KLMSegmentationBorderPtr = typename KLMSegmentationBorder::Pointer;

  double       m_MaximumLambda{ 1000 };
  unsigned int m_NumberOfRegions{ 0 };

  /** Local variables. */

  double       m_InternalLambda{ 0 };
  unsigned int m_InitialNumberOfRegions{ 0 };
  double       m_TotalBorderLength{ 0.0 };

  std::vector<KLMSegmentationRegionPtr>      m_RegionsPointer;
  std::vector<KLMSegmentationBorderPtr>      m_BordersPointer;
  std::vector<KLMSegmentationBorderArrayPtr> m_BordersDynamicPointer;
  KLMSegmentationBorderArrayPtr *            m_BorderCandidate{ nullptr };

  MeanRegionIntensityType m_InitialRegionMean;
  double                  m_InitialRegionArea{ 0 };
}; // class KLMRegionGrowImageFilter
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkKLMRegionGrowImageFilter.hxx"
#endif

#endif
