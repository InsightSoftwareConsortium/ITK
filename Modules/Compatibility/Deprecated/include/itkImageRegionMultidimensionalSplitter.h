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
#ifndef itkImageRegionMultidimensionalSplitter_h
#define itkImageRegionMultidimensionalSplitter_h

#include "itkRegion.h"
#include "itkIndex.h"
#include "itkImageRegionSplitter.h"

namespace itk
{
/** \class ImageRegionMultidimensionalSplitter
 * \brief Divide a region into several pieces.
 *
 * ImageRegionMultidimensionalSplitter divides an ImageRegion into
 * smaller regions.  ImageRegionMultidimensionalSplitter is used by
 * the StreamingImageFilter to divide a requested output region into a
 * series of smaller requests of the pipeline.  This object has two
 * basic methods: GetNumberOfSplits() and GetSplit().
 *

 * GetNumberOfSplits() is used to determine how may subregions a given
 * region can be divided.  You call GetNumberOfSplits with an argument
 * that is the number of subregions you want.  If the image region can
 * support that number of subregions, that number is returned.
 * Otherwise, the maximum number of splits less then or equal to the
 * argumen  be returned.  For example, if a region splitter class only divides
 * a region into horizontal slabs, then the maximum number of splits
 * will be the number of rows in the region.
 *
 * GetSplit() returns the ith of N subregions (as an ImageRegion object).
 *
 * ImageRegionMultidimensionalSplitter class divides a region
 * into hypercubes (as opposed to the slabs used by the superclass
 * ImageRegionSplitter). In other words, it splits every dimension of the
 * region to form ND rectangular prisms.
 *
 * \deprecated The new class ImageRegionSplitterMultidimensional can be
 * used as a drop in replacement for the same functionality as it
 * implements the same algorithm.
 *
 * \ingroup ITKDeprecated
 */

template< unsigned int VImageDimension >
class ITK_TEMPLATE_EXPORT ImageRegionMultidimensionalSplitter:public ImageRegionSplitter< VImageDimension >
{
public:
  /** Standard class typedefs. */
  typedef ImageRegionMultidimensionalSplitter    Self;
  typedef ImageRegionSplitter< VImageDimension > Superclass;
  typedef SmartPointer< Self >                   Pointer;
  typedef SmartPointer< const Self >             ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageRegionMultidimensionalSplitter, ImageRegionSplitter);

  /** Dimension of the image available at compile time. */
  itkStaticConstMacro(ImageDimension, unsigned int, VImageDimension);

  /** Index typedef support. An index is used to access pixel values. */
  typedef Index< VImageDimension >           IndexType;

  /** Size typedef support. A size is used to define region bounds. */
  typedef Size< VImageDimension >          SizeType;

  /** Region typedef support.   */
  typedef ImageRegion< VImageDimension > RegionType;

  /** How many pieces can the specifed region be split? A given region
   * cannot always be divided into the requested number of pieces.  For
   * instance, if the numberOfPieces exceeds the number of pixels along
   * a certain dimensions, then some splits will not be possible. This
   * method returns a number less than or equal to the requested number
   * of pieces.  */
  virtual unsigned int GetNumberOfSplits(const RegionType & region,
                                         unsigned int requestedNumber) ITK_OVERRIDE;

  /** Get a region definition that represents the ith piece a specified region.
   * The "numberOfPieces" must be equal to what
   * GetNumberOfSplits() returns. */
  virtual RegionType GetSplit(unsigned int i, unsigned int numberOfPieces,
                              const RegionType & region) ITK_OVERRIDE;

protected:
  ImageRegionMultidimensionalSplitter() {}
  ~ImageRegionMultidimensionalSplitter() {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageRegionMultidimensionalSplitter);

  static unsigned int ComputeSplits(unsigned int numberOfPieces,
                                    const RegionType &region,
                                    unsigned int splits[]);

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageRegionMultidimensionalSplitter.hxx"
#endif

#endif
