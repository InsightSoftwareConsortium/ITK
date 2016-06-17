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
#ifndef itkImageRegionSplitterBase_h
#define itkImageRegionSplitterBase_h

#include "itkImageRegion.h"
#include "itkObjectFactory.h"
#include "itkImageIORegion.h"

namespace itk
{

/** \class ImageRegionSplitterBase
 * \brief Divide an image region into several pieces.
 *
 * ImageRegionSplitterBase is an abstract interface to divide an
 * ImageRegion into smaller regions. ImageRegionSplitterBase is used
 * by the ImageSource,  StreamingImageFilter, streaming ImageIO
 * classes to divide a region into a series of smaller subregions.
 *
 * This object has two basic methods: \c GetNumberOfSplits() and
 * \c GetSplit().
 *
 * \c GetNumberOfSplits() is used to determine how may subregions a given
 * region can be divided.  You call GetNumberOfSplits with an argument
 * that is the number of subregions you want.  If the image region can
 * support that number of subregions, that number is returned.
 * Otherwise, the maximum number of splits less then or equal to the
 * argumen  be returned.  For example, if a region splitter class only divides
 * a region into horizontal slabs, then the maximum number of splits
 * will be the number of rows in the region.
 *
 * \c GetSplit() returns the ith of N subregions (as an ImageRegion object).
 *
 * \sa ImageRegionSplitterDirection
 * \sa ImageRegionSplitterSlowDimension
 *
 * \ingroup ITKSystemObjects
 * \ingroup DataProcessing
 * \ingroup ITKCommon
 */

class ITKCommon_EXPORT ImageRegionSplitterBase
  :public Object
{
public:
  /** Standard class typedefs. */
  typedef ImageRegionSplitterBase    Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageRegionSplitterBase, Object);

  /** How many pieces can the specified region be split? A given region
   * cannot always be divided into the requested number of pieces. For
   * instance, if the \c numberOfPieces exceeds the number of pixels along
   * a certain dimensions, then some splits will not be possible. This
   * method returns a number less than or equal to the requested number
   * of pieces. */
  template <unsigned int VImageDimension>
    unsigned int GetNumberOfSplits(const ImageRegion<VImageDimension> & region,
                                   unsigned int requestedNumber) const
  {
    return this->GetNumberOfSplitsInternal( VImageDimension,
                                            region.GetIndex().m_Index,
                                            region.GetSize().m_Size,
                                            requestedNumber);
  }
  inline unsigned int GetNumberOfSplits(const ImageIORegion &region,
                                        unsigned int requestedNumber) const
  {
    return this->GetNumberOfSplitsInternal( region.GetImageDimension(),
                                            &region.GetIndex()[0],
                                            &region.GetSize()[0],
                                            requestedNumber);
  }


  /** \brief Get a region definition that represents the ith piece a
   * specified region.
   *
   * The \c numberOfPieces must be equal to what \c GetNumberOfSplits()
   * returns. The return value is the maximum number of splits
   * available. If \c i is greater than or equal to the return value
   * the value of the region is undefined.
   */
  template <unsigned int VImageDimension>
    unsigned int GetSplit( unsigned int i,
                           unsigned int numberOfPieces,
                           ImageRegion<VImageDimension> & region ) const
  {
    return this->GetSplitInternal( VImageDimension,
                                   i,
                                   numberOfPieces,
                                   region.GetModifiableIndex().m_Index,
                                   region.GetModifiableSize().m_Size );
  }
  unsigned int GetSplit( unsigned int i,
                         unsigned int numberOfPieces,
                         ImageIORegion & region ) const
  {
    return this->GetSplitInternal( region.GetImageDimension(),
                                   i,
                                   numberOfPieces,
                                   &region.GetModifiableIndex()[0],
                                   &region.GetModifiableSize()[0] );
  }

protected:
  ImageRegionSplitterBase();

  /** Templetless method to compute the number of possible splits for
   *  any number of dimensions. */
  virtual unsigned int GetNumberOfSplitsInternal( unsigned int dim,
                                                  const IndexValueType regionIndex[],
                                                  const SizeValueType regionSize[],
                                                  unsigned int requestedNumber ) const = 0;

  /** Templetless method to compute an actual split for any number of
   * dimensions. \c dim is the size of the \c regionIndex and \c
   * regionSize arrays.
   */
  virtual unsigned int GetSplitInternal( unsigned int dim,
                                         unsigned int i,
                                         unsigned int numberOfPieces,
                                         IndexValueType regionIndex[],
                                         SizeValueType regionSize[] ) const = 0;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageRegionSplitterBase);
};
} // end namespace itk

#endif
