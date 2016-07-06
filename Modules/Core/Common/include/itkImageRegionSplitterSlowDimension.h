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
#ifndef itkImageRegionSplitterSlowDimension_h
#define itkImageRegionSplitterSlowDimension_h

#include "itkImageRegionSplitterBase.h"

namespace itk
{

/** \class ImageRegionSplitterSlowDimension
 * \brief Divide an image region along the slowest dimension
 *
 * ImageRegionSplitterSlowDimension divides an ImageRegion into smaller regions.
 * ImageRegionSplitterSlowDimension is the default splitter for many situations.
 *
 * This ImageRegionSplitterSlowDimension class divides a region along the
 * outermost or slowest dimension. If the outermost dimension has size
 * 1 (i.e. a volume with a single slice), the ImageRegionSplitter will
 * divide the region along the next outermost dimension. If that
 * dimension has size 1, the process continues with the next outermost
 * dimension.
 *
 * \sa ImageRegionSplitterDirection
 *
 * \ingroup ITKSystemObjects
 * \ingroup DataProcessing
 * \ingroup ITKCommon
 */

class ITKCommon_EXPORT ImageRegionSplitterSlowDimension
  :public ImageRegionSplitterBase
{
public:
  /** Standard class typedefs. */
  typedef ImageRegionSplitterSlowDimension Self;
  typedef ImageRegionSplitterBase          Superclass;
  typedef SmartPointer< Self >             Pointer;
  typedef SmartPointer< const Self >       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageRegionSplitterSlowDimension, ImageRegionSplitterBase);


protected:
  ImageRegionSplitterSlowDimension();

  virtual unsigned int GetNumberOfSplitsInternal( unsigned int dim,
                                                  const IndexValueType regionIndex[],
                                                  const SizeValueType regionSize[],
                                                  unsigned int requestedNumber ) const ITK_OVERRIDE;

  virtual unsigned int GetSplitInternal( unsigned int dim,
                                         unsigned int i,
                                         unsigned int numberOfPieces,
                                         IndexValueType regionIndex[],
                                         SizeValueType regionSize[] ) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageRegionSplitterSlowDimension);
};
} // end namespace itk

#endif
