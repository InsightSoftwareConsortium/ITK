/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarToRGBColormapImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkScalarToRGBColormapImageFilter_h
#define __itkScalarToRGBColormapImageFilter_h

#include "itkImageToImageFilter.h"

#include "itkColormapFunctor.h"

namespace itk
{
/** \class ScalarToRGBColormapImageFilter
 * \brief Implements pixel-wise intensity->rgb mapping operation on one image.
 *
 * This class is parameterized over the type of the input image and
 * the type of the output image.
 *
 * ScalarToRGBColormapImageFilter
 *
 * \sa BinaryFunctorImageFilter TernaryFunctorImageFilter
 *
 * \ingroup   IntensityImageFilters     Multithreaded
 */
template< class TInputImage, class TOutputImage >
class ITK_EXPORT ScalarToRGBColormapImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef ScalarToRGBColormapImageFilter                  Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScalarToRGBColormapImageFilter, ImageToImageFilter);

  /** Some typedefs. */
  typedef TInputImage                           InputImageType;
  typedef typename InputImageType::ConstPointer InputImagePointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename InputImageType::PixelType    InputImagePixelType;
  typedef TOutputImage                          OutputImageType;
  typedef typename OutputImageType::Pointer     OutputImagePointer;
  typedef typename OutputImageType::RegionType  OutputImageRegionType;
  typedef typename OutputImageType::PixelType   OutputImagePixelType;

  typedef Functor::ColormapFunctor< InputImagePixelType,
                                    OutputImagePixelType >                                ColormapType;

  /**
   * Set/Get the colormap object.
   */
  typename ColormapType::Pointer GetColormap() { return m_Colormap; }

  void SetColormap(ColormapType *colormap)
  {
    if ( m_Colormap != colormap )
      {
      m_Colormap = colormap;
      this->Modified();
      }
  }

  /**
   * Enum type that provides for an easy interface to existing colormaps.
   */
  typedef enum { Red, Green, Blue, Grey, Hot, Cool, Spring, Summer,
                 Autumn, Winter, Copper, Jet, HSV, OverUnder } ColormapEnumType;

  void SetColormap(ColormapEnumType);

  /**
   * Set/Get UseInputImageExtremaForScaling.  If 'true', the colormap uses the
   * min and max values from the image to scale appropriately.  Otherwise,
   * these values can be set in the colormap manually.
   */
  itkSetMacro(UseInputImageExtremaForScaling, bool);
  itkGetConstMacro(UseInputImageExtremaForScaling, bool);
  itkBooleanMacro(UseInputImageExtremaForScaling);
protected:
  ScalarToRGBColormapImageFilter();
  virtual ~ScalarToRGBColormapImageFilter() {}

  void PrintSelf(std::ostream & os, Indent indent) const;

  /** ScalarToRGBColormapImageFilter
   * can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData() routine
   * which is called for each processing thread. The output image data is
   * allocated automatically by the superclass prior to calling
   * ThreadedGenerateData().  ThreadedGenerateData can only write to the
   * portion of the output image specified by the parameter
   * "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                            int threadId);

  /** Process to execute before entering the multithreaded section */
  void BeforeThreadedGenerateData();

private:
  ScalarToRGBColormapImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);                 //purposely not implemented

  typename ColormapType::Pointer m_Colormap;

  bool m_UseInputImageExtremaForScaling;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkScalarToRGBColormapImageFilter.txx"
#endif

#endif
