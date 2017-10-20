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
#ifndef itkReflectImageFilter_h
#define itkReflectImageFilter_h
#if !defined( ITK_LEGACY_REMOVE )

#include "itkImageToImageFilter.h"

namespace itk
{
/** \class ReflectImageFilter
 * \brief Implements a Reflection of an image along a selected direction.
 *
 * This class is parameterized over the type of the input image and
 * the type of the output image.
 *
 * \deprecated
 * \ingroup ITKDeprecated
 * \ingroup IntensityImageFilters
 * \ingroup SingleThreaded
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT ReflectImageFilter:public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef ReflectImageFilter                              Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ReflectImageFilter, ImageToImageFilter);

  /** Some convenient typedefs. */
  typedef TInputImage                            InputImageType;
  typedef typename    InputImageType::Pointer    InputImagePointer;
  typedef typename    InputImageType::RegionType InputImageRegionType;
  typedef typename    InputImageType::PixelType  InputImagePixelType;

  typedef TOutputImage                             OutputImageType;
  typedef typename     OutputImageType::Pointer    OutputImagePointer;
  typedef typename     OutputImageType::RegionType OutputImageRegionType;
  typedef typename     OutputImageType::PixelType  OutputImagePixelType;

  /** Set the direction in which to reflect the data. */
  itkGetConstMacro(Direction, unsigned int);
  itkSetMacro(Direction, unsigned int);

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( SameDimensionCheck,
                   ( Concept::SameDimension< InputImageDimension, OutputImageDimension > ) );
  itkConceptMacro( InputConvertibleToOutputCheck,
                   ( Concept::Convertible< InputImagePixelType, OutputImagePixelType > ) );
  // End concept checking
#endif

protected:
  ReflectImageFilter();
  virtual ~ReflectImageFilter() {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** This method implements the actual reflection of the image.
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ReflectImageFilter);

  unsigned int m_Direction;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkReflectImageFilter.hxx"
#endif

#endif //#if !defined( ITK_LEGACY_REMOVE )
#endif
