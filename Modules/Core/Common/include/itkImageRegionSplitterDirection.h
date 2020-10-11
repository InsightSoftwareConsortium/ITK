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
#ifndef itkImageRegionSplitterDirection_h
#define itkImageRegionSplitterDirection_h

#include "itkImageRegionSplitterBase.h"

namespace itk
{
/** \class ImageRegionSplitterDirection
 *
 * Splits an Image region along the slowest axis not in the specified
 * direction. This splitter can be used with image algorithms which
 * operate in a specific direction, where the image should not be
 * split along that dimension.
 *
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT ImageRegionSplitterDirection : public ImageRegionSplitterBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ImageRegionSplitterDirection);

  /** Standard class type aliases. */
  using Self = ImageRegionSplitterDirection;
  using Superclass = ImageRegionSplitterBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageRegionSplitterDirection, ImageRegionSplitterBase);

  /** Get the direction in which not to split the image.*
   *
   * Defaults to 0.
   */
  itkGetConstMacro(Direction, unsigned int);
  itkSetMacro(Direction, unsigned int);


protected:
  ImageRegionSplitterDirection();


  unsigned int
  GetNumberOfSplitsInternal(unsigned int         dim,
                            const IndexValueType regionIndex[],
                            const SizeValueType  regionSize[],
                            unsigned int         requestedNumber) const override;

  unsigned int
  GetSplitInternal(unsigned int   dim,
                   unsigned int   i,
                   unsigned int   numberOfPieces,
                   IndexValueType regionIndex[],
                   SizeValueType  regionSize[]) const override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  unsigned int m_Direction;
};
} // end namespace itk

#endif
