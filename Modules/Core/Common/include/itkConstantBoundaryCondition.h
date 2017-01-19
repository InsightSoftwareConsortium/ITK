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
#ifndef itkConstantBoundaryCondition_h
#define itkConstantBoundaryCondition_h

#include "itkNumericTraits.h"
#include "itkImageBoundaryCondition.h"

namespace itk
{
// Forward reference for VariableLengthVector
template <typename TValue> class VariableLengthVector;

/** \class ConstantBoundaryCondition
 * \brief This boundary condition returns a constant value for out-of-bounds
 * image pixels.
 *
 * For example, invoking this function object with a constant value of zero
 * (the default) on each out-of-bounds element of a 7x5 iterator that masks a
 * region at an image corner
 * (iterator is centered on the 2):
 *
 *               * * * * * * *
 *               * * * * * * *
 *               * * 1 2 3 4 5  (where * denotes pixels that lie
 *               * * 3 3 5 5 6          outside of the image boundary)
 *               * * 4 4 6 7 8
 *
 * would produce the following neighborhood of values:
 *
 *               0 0 0 0 0 0 0
 *               0 0 0 0 0 0 0
 *               0 0 1 2 3 4 5
 *               0 0 3 3 5 5 6
 *               0 0 4 4 6 7 8
 *
 * \note If you are using an image with Array as the pixel type, you will need
 * to set the constant explicitly with an array of the appropriate length. This
 * is also true if your image type is a VectorImage.
 *
 * \sa ImageBoundaryCondition
 *
 * \ingroup DataRepresentation
 * \ingroup ImageObjects
 * \ingroup ITKCommon
 *
 * \wiki
 * \wikiexample{Iterators/ConstantBoundaryCondition,Make out of bounds pixels return a constant value}
 * \endwiki
 */
template< typename TInputImage, typename TOutputImage = TInputImage >
class ITK_TEMPLATE_EXPORT ConstantBoundaryCondition:
    public ImageBoundaryCondition< TInputImage, TOutputImage >
{
public:
  /** Self & superclass typedefs */
  typedef ConstantBoundaryCondition                           Self;
  typedef ImageBoundaryCondition< TInputImage, TOutputImage > Superclass;

  /** Extract information from the image type */
  typedef typename Superclass::PixelType        PixelType;
  typedef typename Superclass::PixelPointerType PixelPointerType;
  typedef typename Superclass::OutputPixelType  OutputPixelType;
  typedef typename Superclass::RegionType       RegionType;
  typedef typename Superclass::IndexType        IndexType;
  typedef typename Superclass::SizeType         SizeType;
  typedef typename Superclass::OffsetType       OffsetType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;

  typedef typename Superclass::NeighborhoodAccessorFunctorType
  NeighborhoodAccessorFunctorType;

  /** Save the image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** Default constructor. */
  ConstantBoundaryCondition();

  /** Runtime information support. */
  virtual const char * GetNameOfClass() const
  {
    return "itkConstantBoundaryCondition";
  }

  /** Utility for printing the boundary condition. */
  virtual void Print( std::ostream & os, Indent i = 0 ) const;

  /** Special version of initialize for images with pixel type
   * VariableLengthVector. */
  template < typename TPixel >
  void Initialize( const VariableLengthVector< TPixel > * );

  /** Computes and returns appropriate out-of-bounds values from
   * neighborhood iterator data. */
  virtual OutputPixelType operator()(const OffsetType &,
                                     const OffsetType &,
                                     const NeighborhoodType *) const;

  /** Computes and returns the appropriate pixel value from
   * neighborhood iterator data, using the functor. */
  virtual OutputPixelType operator()(
    const OffsetType &,
    const OffsetType &,
    const NeighborhoodType *,
    const NeighborhoodAccessorFunctorType &) const;

  /** Set the value of the constant. */
  void SetConstant(const OutputPixelType & c);

  /** Get the value of the constant. */
  const OutputPixelType & GetConstant() const;

  /** Tell if the boundary condition can index to any location within
    * the associated iterator's neighborhood or if it has some limited
    * subset (such as none) that it relies upon. */
  bool RequiresCompleteNeighborhood() { return false; }

  /** Determines the necessary input region for the output region.
   * For this boundary condition, only the intersection of the largest
   * possible image region and the output requested region is needed.
   *
   * \param inputLargestPossibleRegion Largest possible region of the input image.
   * \param outputRequestedRegion The output requested region.
   * \return The necessary input region required to determine the
   * pixel values in the outputRequestedRegion.
   */
  virtual RegionType GetInputRequestedRegion( const RegionType & inputLargestPossibleRegion,
                                              const RegionType & outputRequestedRegion ) const;

  /** Returns a value for a given pixel at an index. If the index is inside the
   * bounds of the input image, then the pixel value is obtained from
   * the input image. Otherwise, the constant value is returned.
   *
   * \param index The index of the desired pixel.
   * \param image The image from which pixel values should be determined.
   */
  OutputPixelType GetPixel( const IndexType & index, const TInputImage * image ) const;

private:
  OutputPixelType m_Constant;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConstantBoundaryCondition.hxx"
#endif

#endif
