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

#ifndef itkPhysicalPointImageSource_h
#define itkPhysicalPointImageSource_h

#include "itkGenerateImageSource.h"

namespace itk
{

/** \class PhysicalPointImageSource
 * \brief Generate an image of the physical locations of each pixel.
 *
 * This image source supports image which have a multi-component pixel
 * equal to the image dimension, and variable length VectorImages. It
 * is recommended that the component type be a real valued type.
 *
 * \ingroup DataSources
 * \ingroup ITKImageSources
 */
template< typename TOutputImage >
class ITK_TEMPLATE_EXPORT PhysicalPointImageSource
  : public GenerateImageSource< TOutputImage >
{
public:
  typedef PhysicalPointImageSource            Self;
  typedef GenerateImageSource< TOutputImage > Superclass;
  typedef SmartPointer< Self >                Pointer;
  typedef SmartPointer< const Self>           ConstPointer;

  /** Output image typedefs */
  typedef TOutputImage                            OutputImageType;
  typedef typename OutputImageType::PixelType     PixelType;
  typedef typename OutputImageType::RegionType    RegionType;
  typedef typename OutputImageType::SpacingType   SpacingType;
  typedef typename OutputImageType::PointType     PointType;
  typedef typename OutputImageType::DirectionType DirectionType;


  typedef typename RegionType::SizeType SizeType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(PhysicalPointImageSource, GenerateImageSource);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

protected:
  PhysicalPointImageSource( ) {};
  // virtual ~PhysicalPointImageSource() default implementation ok

  virtual void GenerateOutputInformation() ITK_OVERRIDE;

  virtual void ThreadedGenerateData (const RegionType &outputRegionForThread, ThreadIdType threadId) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PhysicalPointImageSource);
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPhysicalPointImageSource.hxx"
#endif

#endif
