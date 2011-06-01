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
#ifndef __itkGradientImageToBloxBoundaryPointImageFilter_h
#define __itkGradientImageToBloxBoundaryPointImageFilter_h

#include "itkBloxBoundaryPointImage.h"
#include "itkImageToImageFilter.h"
#include "itkSize.h"

namespace itk
{
/** \class GradientImageToBloxBoundaryPointImageFilter
 * \brief Converts a gradient image to an BloxImage of BloxBoundaryPoints
 *
 * Thresholds the magnitude of a gradient image to produce
 * a BloxBoundaryPointImage
 *
 * \ingroup ImageEnhancement
 * \ingroup ITK-Blox
 */
template< typename TInputImage >
class ITK_EXPORT GradientImageToBloxBoundaryPointImageFilter:
  public ImageToImageFilter< TInputImage,
                             BloxBoundaryPointImage< ::itk::GetImageDimension< TInputImage >::ImageDimension > >
{
public:
  /** Number of dimensions */
  itkStaticConstMacro(NDimensions, unsigned int, TInputImage::ImageDimension);

  /** Standard class typedefs. */
  typedef GradientImageToBloxBoundaryPointImageFilter Self;
  typedef ImageToImageFilter< TInputImage,
                              BloxBoundaryPointImage< itkGetStaticConstMacro(NDimensions) > > Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GradientImageToBloxBoundaryPointImageFilter, ImageToImageFilter);

  /** typedef for images */
  typedef TInputImage                                                   InputImageType;
  typedef BloxBoundaryPointImage< itkGetStaticConstMacro(NDimensions) > TOutputImage;
  typedef BloxBoundaryPointImage< itkGetStaticConstMacro(NDimensions) > OutputImageType;
  typedef typename OutputImageType::Pointer                             OutputImagePointer;
  typedef typename InputImageType::Pointer                              InputImagePointer;
  typedef typename InputImageType::ConstPointer                         InputImageConstPointer;

  /** Image size typedef */
  typedef Size< itkGetStaticConstMacro(NDimensions) > SizeType;

  /** Image index typedef */
  typedef typename TOutputImage::IndexType IndexType;

  /** Image pixel value typedef */
  typedef typename TOutputImage::PixelType PixelType;

  /** Typedef to describe the output image region type. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** The type of vector used to convert between physical and blox space */
  typedef Point< double, itkGetStaticConstMacro(NDimensions) > TPositionType;

  /** Get and set the number of times to repeat the filter. */
  itkSetMacro(Threshold, double);
  itkGetConstMacro(Threshold, double);

  typedef FixedArray< float, NDimensions > BloxResolutionType;

  /** Get and set the resolution of the blox
   *  This is the number of input pixels "contained" within
   *  each blox pixel
   */
  itkSetMacro(BloxResolution, BloxResolutionType);
  itkGetConstReferenceMacro(BloxResolution, BloxResolutionType);
  void SetBloxResolution(float bloxResolution);

  void GenerateInputRequestedRegion();

  virtual void GenerateOutputInformation();

protected:
  GradientImageToBloxBoundaryPointImageFilter();
  virtual ~GradientImageToBloxBoundaryPointImageFilter() {}
  void PrintSelf(std::ostream & os, Indent indent) const;

  /** Method for forming the BloxBoundaryPointImage. */
  void GenerateData();

private:
  GradientImageToBloxBoundaryPointImageFilter(const Self &); //purposely not
                                                             // implemented
  void operator=(const Self &);                              //purposely not

  // implemented

  /** The threshold used to decide whether or not a gradient indicates
    * a boundary point that should be included */
  double m_Threshold;

  /** The resolution of the blox in each dimension */
  BloxResolutionType m_BloxResolution;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGradientImageToBloxBoundaryPointImageFilter.txx"
#endif

#endif
