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
#ifndef itkOtsuThresholdImageCalculator_h
#define itkOtsuThresholdImageCalculator_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkNumericTraits.h"
#include "itkIntTypes.h"

#ifndef ITKV3_COMPATIBILITY
#error "This file is only valid when ITKV3_COMPATIBILITY is turned on. Users are encouraged to convert to itk::OtsuThresholdCalculator in ITKv4"
#endif


namespace itk
{
/** \class OtsuThresholdImageCalculator
 * \brief Computes the Otsu's threshold for an image.
 *
 * This calculator computes the Otsu's threshold which separates an image
 * into foreground and background components. The method relies on a
 * histogram of image intensities. The basic idea is to maximize the
 * between-class variance.
 *
 * This class is templated over the input image type.
 *
 * \warning This method assumes that the input image consists of scalar pixel
 * types.
 *
 * \deprecated
 * \ingroup Operators
 * \ingroup ITKThresholding
 * \ingroup ITKV3Compatibility
 */
template< typename TInputImage >
class ITK_TEMPLATE_EXPORT OtsuThresholdImageCalculator:public Object
{
public:
  /** Standard class typedefs. */
  typedef OtsuThresholdImageCalculator Self;
  typedef Object                       Superclass;
  typedef SmartPointer< Self >         Pointer;
  typedef SmartPointer< const Self >   ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(OtsuThresholdImageCalculator, Object);

  /** Type definition for the input image. */
  typedef TInputImage ImageType;

  /** Pointer type for the image. */
  typedef typename TInputImage::Pointer ImagePointer;

  /** Const Pointer type for the image. */
  typedef typename TInputImage::ConstPointer ImageConstPointer;

  /** Type definition for the input image pixel type. */
  typedef typename TInputImage::PixelType PixelType;

  /** Type definition for the input image region type. */
  typedef typename TInputImage::RegionType RegionType;

  /** Set the input image. */
  itkSetConstObjectMacro(Image, ImageType);

  /** Compute the Otsu's threshold for the input image. */
  void Compute();

  /** Return the Otsu's threshold value. */
  itkGetConstMacro(Threshold, PixelType);

  /** Set/Get the number of histogram bins. Default is 128. */
  itkSetClampMacro( NumberOfHistogramBins, SizeValueType, 1,
                    NumericTraits< SizeValueType >::max() );
  itkGetConstMacro(NumberOfHistogramBins, SizeValueType);

  /** Set the region over which the values will be computed */
  void SetRegion(const RegionType & region);

protected:
  OtsuThresholdImageCalculator();
  virtual ~OtsuThresholdImageCalculator() {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(OtsuThresholdImageCalculator);

  PixelType         m_Threshold;
  SizeValueType     m_NumberOfHistogramBins;
  ImageConstPointer m_Image;
  RegionType        m_Region;
  bool              m_RegionSetByUser;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkOtsuThresholdImageCalculator.hxx"
#endif

#endif
