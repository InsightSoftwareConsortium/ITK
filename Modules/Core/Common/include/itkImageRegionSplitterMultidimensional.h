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
#ifndef itkImageRegionSplitterMultidimensional_h
#define itkImageRegionSplitterMultidimensional_h

#include "itkRegion.h"
#include "itkIndex.h"
#include "itkImageRegionSplitterBase.h"

namespace itk
{
/** \class ImageRegionSplitterMultidimensional
 * \brief Divide a region into several pieces.
 *
 * ImageRegionSplitterMultidimensional divides an ImageRegion into
 * smaller regions.  ImageRegionSplitterMultidimensional is used by
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
 * ImageRegionSplitterMultidimensional class divides a region
 * into hypercubes (as opposed to the slabs used by the superclass
 * ImageRegionSplitter). In other words, it splits every dimension of the
 * region to form ND rectangular prisms.
 *
 * \ingroup ITKSystemObjects
 * \ingroup DataProcessing
 * \ingroup ITKCommon
 */

class ITKCommon_EXPORT ImageRegionSplitterMultidimensional
  : public ImageRegionSplitterBase
{
public:
  /** Standard class typedefs. */
  typedef ImageRegionSplitterMultidimensional Self;
  typedef ImageRegionSplitterBase             Superclass;
  typedef SmartPointer< Self >                Pointer;
  typedef SmartPointer< const Self >          ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageRegionSplitterMultidimensional, ImageRegionSplitterBase);

protected:
  ImageRegionSplitterMultidimensional();


  virtual unsigned int GetNumberOfSplitsInternal(unsigned int dim,
                                                 const IndexValueType regionIndex[],
                                                 const SizeValueType regionSize[],
                                                 unsigned int requestedNumber) const ITK_OVERRIDE;

  virtual unsigned int GetSplitInternal(unsigned int dim,
                                        unsigned int i,
                                        unsigned int numberOfPieces,
                                        IndexValueType regionIndex[],
                                        SizeValueType regionSize[]) const ITK_OVERRIDE;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageRegionSplitterMultidimensional);

  static unsigned int ComputeSplits(unsigned int dim,
                                    unsigned int requestedNumber,
                                    const IndexValueType regionIndex[],
                                    const SizeValueType regionSize[],
                                    unsigned int splits[]);

};
} // end namespace itk

#endif
