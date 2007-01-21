/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionalMinimaImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRegionalMinimaImageFilter_h
#define __itkRegionalMinimaImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk {

/** \class RegionalMinimaImageFilter
 * \brief Produce a binary image where foreground is the regional minima of the
 * input image
 *
 * Regional minima are flat zones surounded by pixels of greater value.
 *
 * If the input image is constant, the entire image can be considered as a
 * minima or not.  The SetFlatIsMinima() method let the user choose which
 * behavior to use.
 * 
 * This class was contribtued to the Insight Journal by
 * \author Gaëtan Lehmann. Biologie du Développement et de la Reproduction,
 * INRA de Jouy-en-Josas, France.
 *      http://hdl.handle.net/1926/153
 *
 *
 * \sa ValuedRegionalMinimaImageFilter, HConcaveImageFilter,
 * RegionalMaximaImageFilter \ingroup ImageEnhancement
 * MathematicalMorphologyImageFilters
 */
template<class TInputImage, class TOutputImage>
class ITK_EXPORT RegionalMinimaImageFilter : 
    public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef RegionalMinimaImageFilter                     Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                            Pointer;
  typedef SmartPointer<const Self>                      ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage                              InputImageType;
  typedef TOutputImage                             OutputImageType;
  typedef typename InputImageType::Pointer         InputImagePointer;
  typedef typename InputImageType::ConstPointer    InputImageConstPointer;
  typedef typename InputImageType::RegionType      InputImageRegionType;
  typedef typename InputImageType::PixelType       InputImagePixelType;
  typedef typename OutputImageType::Pointer        OutputImagePointer;
  typedef typename OutputImageType::ConstPointer   OutputImageConstPointer;
  typedef typename OutputImageType::RegionType     OutputImageRegionType;
  typedef typename OutputImageType::PixelType      OutputImagePixelType;
  
  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);  

  /** Runtime information support. */
  itkTypeMacro(RegionalMinimaImageFilter, 
               ImageToImageFilter);

  /**
   * Set/Get whether the connected components are defined strictly by
   * face connectivity or by face+edge+vertex connectivity.  Default is
   * FullyConnectedOff.  For objects that are 1 pixel wide, use
   * FullyConnectedOn.
   */
  itkSetMacro(FullyConnected, bool);
  itkGetConstReferenceMacro(FullyConnected, bool);
  itkBooleanMacro(FullyConnected);
  
  /**
   * Set/Get the value in the output image to consider as "foreground".
   * Defaults to maximum value of PixelType.
   */
  itkSetMacro(ForegroundValue, OutputImagePixelType);
  itkGetConstMacro(ForegroundValue, OutputImagePixelType);

  /**
   * Set/Get the value used as "background" in the output image.
   * Defaults to NumericTraits<PixelType>::NonpositiveMin().
   */
  itkSetMacro(BackgroundValue, OutputImagePixelType);
  itkGetConstMacro(BackgroundValue, OutputImagePixelType);

  /**
   * Set/Get wether a flat image must be considered as a minima or not.
   * Defaults to true.
   */
  itkSetMacro(FlatIsMinima, bool);
  itkGetConstMacro(FlatIsMinima, bool);
  itkBooleanMacro(FlatIsMinima);

protected:
  RegionalMinimaImageFilter();
  ~RegionalMinimaImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** RegionalMinimaImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion();

  /** RegionalMinimaImageFilter will produce the entire output. */
  void EnlargeOutputRequestedRegion(DataObject *itkNotUsed(output));
  
  /** Single-threaded version of GenerateData.  This filter delegates
   * to GrayscaleGeodesicErodeImageFilter. */
  void GenerateData();
  

private:
  RegionalMinimaImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  bool                                                m_FullyConnected;
  bool                                                m_FlatIsMinima;
  OutputImagePixelType                                m_ForegroundValue;
  OutputImagePixelType                                m_BackgroundValue;

}; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegionalMinimaImageFilter.txx"
#endif

#endif
