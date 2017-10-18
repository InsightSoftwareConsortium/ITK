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
#ifndef itkMinimumMaximumImageCalculator_h
#define itkMinimumMaximumImageCalculator_h

#include "itkObject.h"
#include "itkObjectFactory.h"

namespace itk
{
/** \class MinimumMaximumImageCalculator
 *  \brief Computes the minimum and the maximum intensity values of
 *         an image.
 *
 * This calculator computes the minimum and the maximum intensity values of
 * an image.  It is templated over input image type.  If only Maximum or
 * Minimum value is needed, just call ComputeMaximum() (ComputeMinimum())
 * otherwise Compute() will compute both.
 *
 * \ingroup Operators
 * \ingroup ITKCommon
 *
 * \wiki
 * \wikiexample{ImageProcessing/MinimumMaximumImageCalculator,Find the minimum and maximum value (and the position of the value) in an image}
 * \wikiexample{Developer/OilPaintingImageFilter,Multi-threaded oil painting image filter}
 * \endwiki
 */
template< typename TInputImage >
class ITK_TEMPLATE_EXPORT MinimumMaximumImageCalculator:public Object
{
public:
  /** Standard class typedefs. */
  typedef MinimumMaximumImageCalculator Self;
  typedef Object                        Superclass;
  typedef SmartPointer< Self >          Pointer;
  typedef SmartPointer< const Self >    ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MinimumMaximumImageCalculator, Object);

  /** Type definition for the input image. */
  typedef TInputImage ImageType;

  /** Pointer type for the image. */
  typedef typename TInputImage::Pointer ImagePointer;

  /** Const Pointer type for the image. */
  typedef typename TInputImage::ConstPointer ImageConstPointer;

  /** Type definition for the input image pixel type. */
  typedef typename TInputImage::PixelType PixelType;

  /** Type definition for the input image index type. */
  typedef typename TInputImage::IndexType IndexType;

  /** Type definition for the input image region type. */
  typedef typename TInputImage::RegionType RegionType;

  /** Set the input image. */
  itkSetConstObjectMacro(Image, ImageType);

  /** Compute the minimum value of intensity of the input image. */
  void ComputeMinimum();

  /** Compute the maximum value of intensity of the input image. */
  void ComputeMaximum();

  /** Compute the minimum and maximum values of intensity of the input image. */
  void Compute();

  /** Return the minimum intensity value. */
  itkGetConstMacro(Minimum, PixelType);

  /** Return the maximum intensity value. */
  itkGetConstMacro(Maximum, PixelType);

  /** Return the index of the minimum intensity value. */
  itkGetConstReferenceMacro(IndexOfMinimum, IndexType);

  /** Return the index of the maximum intensity value. */
  itkGetConstReferenceMacro(IndexOfMaximum, IndexType);

  /** Set the region over which the values will be computed */
  void SetRegion(const RegionType & region);

protected:
  MinimumMaximumImageCalculator();
  virtual ~MinimumMaximumImageCalculator() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MinimumMaximumImageCalculator);

  PixelType         m_Minimum;
  PixelType         m_Maximum;
  ImageConstPointer m_Image;

  IndexType m_IndexOfMinimum;
  IndexType m_IndexOfMaximum;

  RegionType m_Region;
  bool       m_RegionSetByUser;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMinimumMaximumImageCalculator.hxx"
#endif

#endif /* itkMinimumMaximumImageCalculator_h */
