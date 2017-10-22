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
#ifndef itkNearestNeighborExtrapolateImageFunction_h
#define itkNearestNeighborExtrapolateImageFunction_h

#include "itkExtrapolateImageFunction.h"

namespace itk
{
/** \class NearestNeighborExtrapolateImageFunction
 * \brief Nearest neighbor extrapolation of a scalar image.
 *
 * NearestNeighborExtrapolateImageFunction extrapolate image intensity at
 * a specified point, continuous index or index by copying the intensity
 * of the nearest neighbor within the image buffer.
 *
 * This class is templated
 * over the input image type and the coordinate representation type
 * (e.g. float or double).
 *
 * \ingroup ImageFunctions
 * \ingroup ITKImageFunction
 */
template< typename TInputImage, typename TCoordRep = float >
class NearestNeighborExtrapolateImageFunction:
  public ExtrapolateImageFunction< TInputImage, TCoordRep >
{
public:
  /** Standard class typedefs. */
  typedef NearestNeighborExtrapolateImageFunction            Self;
  typedef ExtrapolateImageFunction< TInputImage, TCoordRep > Superclass;
  typedef SmartPointer< Self >                               Pointer;
  typedef SmartPointer< const Self >                         ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(NearestNeighborExtrapolateImageFunction,
               InterpolateImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** OutputType typedef support. */
  typedef typename Superclass::OutputType OutputType;

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;

  /** Dimension underlying input image. */
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** Index typedef support. */
  typedef typename Superclass::IndexType      IndexType;
  typedef typename IndexType::IndexValueType  IndexValueType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** Evaluate the function at a ContinuousIndex position
   *
   * Returns the extrapolated image intensity at a
   * specified position by returning the intensity of the
   * nearest neighbor within the image buffer.
   *
   */
  virtual OutputType EvaluateAtContinuousIndex(
    const ContinuousIndexType & index) const ITK_OVERRIDE
  {
    IndexType nindex;

    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      nindex[j] = Math::RoundHalfIntegerUp< IndexValueType >(index[j]);
      if ( nindex[j] < this->GetStartIndex()[j] )
        {
        nindex[j] = this->GetStartIndex()[j];
        }
      else if ( nindex[j] > this->GetEndIndex()[j] )
        {
        nindex[j] = this->GetEndIndex()[j];
        }
      }
    return static_cast< OutputType >( this->GetInputImage()->GetPixel(nindex) );
  }

  /** Evaluate the function at a ContinuousIndex position
   *
   * Returns the extrapolated image intensity at a
   * specified position by returning the intensity of the
   * nearest neighbor within the image buffer.
   *
   */
  virtual OutputType EvaluateAtIndex(
    const IndexType & index) const ITK_OVERRIDE
  {
    IndexType nindex;

    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      if ( index[j] < this->GetStartIndex()[j] )
        {
        nindex[j] = this->GetStartIndex()[j];
        }
      else if ( index[j] > this->GetEndIndex()[j] )
        {
        nindex[j] = this->GetEndIndex()[j];
        }
      else
        {
        nindex[j] = index[j];
        }
      }
    return static_cast< OutputType >( this->GetInputImage()->GetPixel(nindex) );
  }

protected:
  NearestNeighborExtrapolateImageFunction(){}
  ~NearestNeighborExtrapolateImageFunction() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
  { Superclass::PrintSelf(os, indent); }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(NearestNeighborExtrapolateImageFunction);
};
} // end namespace itk

#endif
