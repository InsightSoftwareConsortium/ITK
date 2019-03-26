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
#ifndef itkBinaryThinningImageFilter3D_h
#define itkBinaryThinningImageFilter3D_h

#include <itkNeighborhoodIterator.h>
#include <itkImageToImageFilter.h>
#include <itkImageRegionIteratorWithIndex.h>
#include <itkConstantBoundaryCondition.h>

namespace itk
{
/** \class BinaryThinningImageFilter3D
 *
 * \brief This filter computes one-pixel-wide skeleton of a 3D input image.
 *
 * This class is parametrized over the type of the input image
 * and the type of the output image.
 *
 * The input is assumed to be a binary image. All non-zero valued voxels
 * are set to 1 internally to simplify the computation. The filter will
 * produce a skeleton of the object.  The output background values are 0,
 * and the foreground values are 1.
 *
 * A 26-neighbourhood configuration is used for the foreground and a
 * 6-neighbourhood configuration for the background. Thinning is performed
 * symmetrically in order to guarantee that the skeleton lies medial within
 * the object.
 *
 * This filter is a parallel thinning algorithm and is an implementation
 * of the algorithm described in:
 *
 * T.C. Lee, R.L. Kashyap, and C.N. Chu.
 * Building skeleton models via 3-D medial surface/axis thinning algorithms.
 * Computer Vision, Graphics, and Image Processing, 56(6):462--478, 1994.
 *
 * To do: Make use of multi-threading.
 *
 * \author Hanno Homann, Oxford University, Wolfson Medical Vision Lab, UK.
 *
 * \sa MorphologyImageFilter
 * \ingroup ImageEnhancement MathematicalMorphologyImageFilters Thickness3D
 */

template <class TInputImage, class TOutputImage>
class ITK_TEMPLATE_EXPORT BinaryThinningImageFilter3D : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryThinningImageFilter3D);

  /** Standard class typedefs. */
  using Self = BinaryThinningImageFilter3D;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BinaryThinningImageFilter3D, ImageToImageFilter);

  /** Type for input image. */
  using InputImageType = TInputImage;

  /** Type for output image: Skeleton of the object.  */
  using OutputImageType = TOutputImage;

  /** Type for the region of the input image. */
  using RegionType = typename InputImageType::RegionType;

  /** Type for the index of the input image. */
  using IndexType = typename RegionType::IndexType;

  /** Type for the pixel type of the input image. */
  using InputImagePixelType = typename InputImageType::PixelType;

  /** Type for the pixel type of the input image. */
  using OutputImagePixelType = typename OutputImageType::PixelType;

  /** Type for the size of the input image. */
  using SizeType = typename RegionType::SizeType;

  /** Pointer Type for input image. */
  using InputImagePointer = typename InputImageType::ConstPointer;

  /** Pointer Type for the output image. */
  using OutputImagePointer = typename OutputImageType::Pointer;

  /** Boundary condition type for the neighborhood iterator */
  using ConstBoundaryConditionType = ConstantBoundaryCondition<TInputImage>;

  /** Neighborhood iterator type */
  using NeighborhoodIteratorType = NeighborhoodIterator<TInputImage, ConstBoundaryConditionType>;

  /** Neighborhood type */
  using NeighborhoodType = typename NeighborhoodIteratorType::NeighborhoodType;

  /** Get Skeleton by thinning image. */
  OutputImageType *
  GetThinning(void);

  /** ImageDimension enumeration   */
  itkStaticConstMacro(InputImageDimension, unsigned int, TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int, TOutputImage::ImageDimension);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<InputImageDimension, 3>));
  itkConceptMacro(SameTypeCheck, (Concept::SameType<InputImagePixelType, OutputImagePixelType>));
  itkConceptMacro(InputAdditiveOperatorsCheck, (Concept::AdditiveOperators<InputImagePixelType>));
  itkConceptMacro(InputConvertibleToIntCheck, (Concept::Convertible<InputImagePixelType, int>));
  itkConceptMacro(IntConvertibleToInputCheck, (Concept::Convertible<int, InputImagePixelType>));
  itkConceptMacro(InputIntComparableCheck, (Concept::Comparable<InputImagePixelType, int>));
  /** End concept checking */
#endif

protected:
  BinaryThinningImageFilter3D();
  ~BinaryThinningImageFilter3D() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Compute thinning Image. */
  void
  GenerateData() override;

  /** Prepare data. */
  void
  PrepareData();

  /** Compute thinning image. */
  void
  ComputeThinImage();

  /** Check for Euler invariance (see [Lee94]). */
  bool
  IsEulerInvariant(NeighborhoodType neighbors, int * LUT);

  /** Fill the Euler look-up table (LUT) for later check of the Euler
   * invariance (see [Lee94]). */
  void
  FillEulerLUT(int * LUT);

  /** Check if the current point is a simple point.
   * This method is named 'N(v)_labeling' in [Lee94].
   * Outputs the number of connected objects in a neighborhood of a point
   * after this point would have been removed. */
  bool
  IsSimplePoint(NeighborhoodType neighbors);

  /** Recursive method that calculates the number of connected components in
   * the 3D neighbourhood after the center pixel would have been removed (see)
   * [Lee94]). */
  void
  OctreeLabeling(int octant, int label, int * cube);

}; // end of BinaryThinningImageFilter3D class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBinaryThinningImageFilter3D.hxx"
#endif

#endif // itkBinaryThinningImageFilter3D_h
