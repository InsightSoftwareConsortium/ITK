/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAdaptiveHistogramEqualizationImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPlaheImageFilter_h
#define __itkPlaheImageFilter_h


#include <itkImageToImageFilter.h>
#include <itkImage.h>

namespace itk
{
/** /class PlaheImageFilter
 * Power Law Adaptive Histogram Equalization (PLAHE) is one of adaptive
 * histogram equalization method.  For detail description, reference
 * "Adaptive Image Contrast Enhancement using Generalizations of Histogram 
 * Equalization."  J.Alex Stark. IEEE Transactions on Image Processing, 
 * May 2000.
 * 
 * \ingroup ImageEnhancement
 */
template <class TImageType>
class ITK_EXPORT PlaheImageFilter :
  public ImageToImageFilter< TImageType, TImageType >
{
public:
  /** Standard class typedefs.*/ 
  typedef PlaheImageFilter Self;
  typedef ImageToImageFilter< TImageType, TImageType > Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> constPointer;

  itkStaticConstMacro(ImageDimension, unsigned int,
                      TImageType::ImageDimension );

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PlaheImageFilter, ImageToImageFilter);

  /** Image type typedef support. */
  typedef TImageType ImageType;

  /** Standard Get/Set macros for filter parameters. */
  itkSetMacro(Alpha, float);
  itkGetMacro(Alpha, float);
  itkSetMacro(Beta, float);
  itkGetMacro(Beta, float);
  itkSetVectorMacro(Window, unsigned int, ImageDimension);
  itkGetVectorMacro(Window, const unsigned int, ImageDimension);

protected:
  PlaheImageFilter()
    {
    m_Alpha = .3;
    m_Beta = .3;
    for (unsigned int i = 0; i <ImageDimension; i++)
      {
      m_Window[i] = 0;
      }
    }
  virtual ~PlaheImageFilter(){}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Standard pipeline method.*/
  void GenerateData();

private:
  PlaheImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** The beta parameter of the Plahe. */
  float m_Alpha;

  /** The alpha parameter of the Plahe. */
  float m_Beta;

  /** The window size of the Plahe algorithm.
   * This parameter defines the size of neighborhood 
   * around the evaluated pixel. */
  unsigned int m_Window[ImageType::ImageDimension];

  /** A function which is used in GenerateData(). */
  float CumulativeFunction(float u, float v);
   
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPlaheImageFilter.txx"
#endif

#endif
