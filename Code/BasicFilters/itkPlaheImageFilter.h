/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPlaheImageFilter.h
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
 * /brief Power Law Adaptive Histogram Equalization
 *
 * Histogram equalization modifies the contrast in an image. The
 * PlaheImageFilter is a superset of many contrast enhancing
 * filters. By modifying its parameters (alpha, beta, and window), the
 * PlaheImageFilter can produce an adaptively equalized histogram or a
 * version of unsharp mask (local mean subtraction). Instead of
 * applying a strict histogram equalization in a window about a pixel,
 * this filter prescribes a mapping function (power law) controlled by
 * the parameters alpha and beta.
 *
 * The parameter alpha controls how much the filter acts like the
 * classical histogram equalization method (alpha=0) to how
 * much the filter acts like an unsharp mask (alpha=1).
 *
 * The parameter beta controls how much the filter acts like an
 * unsharp mask (beta=0) to much the filter acts like pass through
 * (beta=1, with alpha=1).
 *
 * The parameter window controls the size of the region over which
 * local statistics are calculated.
 *
 * By altering alpha, beta and window, a host of equalization and unsharp
 * masking filters is available.
 *
 * For detail description, reference "Adaptive Image Contrast
 * Enhancement using Generalizations of Histogram Equalization."
 * J.Alex Stark. IEEE Transactions on Image Processing, May 2000.
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
  typedef typename ImageType::SizeType ImageSizeType;

  /** Set/Get the value of alpha.  Alpha=0 produces the adaptive
   * histogram equalization (provided beta=0).  Alpha=1 produces an
   * unsharp mask. Default is 0.3. */
  itkSetMacro(Alpha, float);
  itkGetMacro(Alpha, float);

  /** Set/Get the value of beta.  If beta=1 (and alpha=1),
   * then the output image matches the input image.  As beta
   * approaches 0, the filter behaves as an unsharp mask. Default is
   * 0.3. */
  itkSetMacro(Beta, float);
  itkGetMacro(Beta, float);

  /** Set/Get the radius of the neighborhood used to compute local
   * statistics. Radius sizes of 10 and 20 are common.  Default is a
   * radius of 5. */
  itkSetMacro(Radius, ImageSizeType);
  itkGetConstReferenceMacro(Radius, ImageSizeType);

  /** Set/Get whether an optimized lookup table for the intensity
   * mapping function is used.  Default is off. */
  itkSetMacro(UseLookupTable, bool);
  itkGetMacro(UseLookupTable, bool);
  itkBooleanMacro(UseLookupTable);

protected:
  PlaheImageFilter()
    {
    m_Alpha = .3;
    m_Beta = .3;
    m_Radius.Fill( 5 );
    m_UseLookupTable = false;
    }
  virtual ~PlaheImageFilter(){}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Standard pipeline method.*/
  void GenerateData();

  /** Adaptive histogram equalization requires more input that it
   * outputs. It needs request an input region that is padded by the
   * radius.
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  void GenerateInputRequestedRegion();

private:
  PlaheImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** The beta parameter of the Plahe. */
  float m_Alpha;

  /** The alpha parameter of the Plahe. */
  float m_Beta;

  /** The window size of the adaptive algorithm.
   * This parameter defines the size of neighborhood 
   * around the evaluated pixel. */
  ImageSizeType m_Radius;

  /** Should we use a lookup table to optimize the use of the
   * intensity mapping function? */
  bool m_UseLookupTable;

  /** A function which is used in GenerateData(). */
  float CumulativeFunction(float u, float v);
   
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPlaheImageFilter.txx"
#endif

#endif
