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
#ifndef itkCheckerBoardImageFilter_h
#define itkCheckerBoardImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/** \class CheckerBoardImageFilter
 * \brief Combines two images in a checkerboard pattern.
 *
 * CheckerBoardImageFilter takes two input images that must have the same
 * dimension, size, origin and spacing and produces an output image of the same
 * size by combinining the pixels from the two input images in a checkerboard
 * pattern. This filter is commonly used for visually comparing two images, in
 * particular for evaluating the results of an image registration process.
 *
 * This filter is implemented as a multithreaded filter. It provides a
 * ThreadedGenerateData() method for its implementation.
 *
 * \ingroup IntensityImageFilters  MultiThreaded
 * \ingroup ITKImageCompare
 *
 * \wiki
 * \wikiexample{Inspection/CheckerBoardImageFilter,Combine two images by alternating blocks of a checkerboard pattern}
 * \endwiki
 */
template< typename TImage >
class ITK_TEMPLATE_EXPORT CheckerBoardImageFilter:
  public ImageToImageFilter< TImage, TImage >
{
public:
  /** Standard class typedefs. */
  typedef CheckerBoardImageFilter              Self;
  typedef ImageToImageFilter< TImage, TImage > Superclass;
  typedef SmartPointer< Self >                 Pointer;
  typedef SmartPointer< const Self >           ConstPointer;

  typedef TImage                                InputImageType;
  typedef TImage                                OutputImageType;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;
  typedef typename OutputImageType::Pointer     OutputImagePointer;
  typedef typename OutputImageType::RegionType  ImageRegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CheckerBoardImageFilter, ImageToImageFilter);

  /** Number of dimensions. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TImage::ImageDimension);

  /** Type to hold the number of checker boxes per dimension. */
  typedef FixedArray< unsigned int,
                       TImage ::ImageDimension >  PatternArrayType;

  /** Set the first operand for checker board. */
  void SetInput1(const TImage *image1);

  /** Set the second operand for checker board. */
  void SetInput2(const TImage *image2);

  /** Set/Get the checker pattern array, i.e. the number of checker boxes
   * per image dimension. */
  itkSetMacro(CheckerPattern, PatternArrayType);
  itkGetConstReferenceMacro(CheckerPattern, PatternArrayType);

protected:
  CheckerBoardImageFilter();
  ~CheckerBoardImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** CheckerBoardImageFilter can be implemented as a multithreaded filter. Therefore,
   * this implementation provides a ThreadedGenerateData() routine which
   * is called for each processing thread. The output image data is allocated
   * automatically by the superclass prior to calling ThreadedGenerateData().
   * ThreadedGenerateData can only write to the portion of the output image
   * specified by the parameter "outputRegionForThread"
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void ThreadedGenerateData(const ImageRegionType & outputRegionForThread,
                            ThreadIdType threadId) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(CheckerBoardImageFilter);

  PatternArrayType m_CheckerPattern;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCheckerBoardImageFilter.hxx"
#endif

#endif
