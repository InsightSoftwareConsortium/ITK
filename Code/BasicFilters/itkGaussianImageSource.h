/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianImageSource.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGaussianImageSource_h
#define __itkGaussianImageSource_h

#include "itkImageSource.h"
#include "itkFixedArray.h"

namespace itk
{

/** \class GaussianImageSource
 * \brief Generate an n-dimensional image of a Gaussian.
 *
 * GaussianImageSource generates an image of a Gaussian.
 * m_Normalized determines whether or not the Gaussian is normalized
 * (whether or not the sum over infinite space is 1.0)
 * When creating an image, it is preferable to _not_ normalize the Gaussian
 * m_Scale scales the output of the Gaussian to span a range
 * larger than 0->1, and is typically set to the maximum value
 * of the output data type (for instance, 255 for uchars)
 *
 * The output image may be of any dimension. 
 *
 * \ingroup DataSources
 */
template <typename TOutputImage>
class ITK_EXPORT GaussianImageSource : public ImageSource<TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef GaussianImageSource   Self;
  typedef ImageSource<TOutputImage>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Typedef for the output image PixelType. */
  typedef typename TOutputImage::PixelType OutputImagePixelType;

  /** Typedef to describe the output image region type. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** Dimensionality of the output image */
  itkStaticConstMacro(NDimensions, unsigned int, TOutputImage::ImageDimension);

  /** Type used to store gaussian parameters. */
  typedef FixedArray<double, itkGetStaticConstMacro(NDimensions)> ArrayType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(GaussianImageSource,ImageSource);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Specify the size of the output image. */
  virtual void SetSize( const unsigned long values[NDimensions]);

  /** Get the size of the output image. */
  itkGetVectorMacro(Size,const unsigned long,NDimensions);
  
  /** Specify the spacing of the output image. */
  virtual void SetSpacing( const float values[NDimensions]);
  virtual void SetSpacing( const double values[NDimensions]);
  
  /** Get the spacing of the output image. */
  itkGetVectorMacro(Spacing,const double,NDimensions);

  /** Specify the origin of the output image. */
  virtual void SetOrigin( const float values[NDimensions]);
  virtual void SetOrigin( const double values[NDimensions]);

  /** Get the origin of the output image. */
  itkGetVectorMacro(Origin,const double,NDimensions);

  /** Gets and sets for gaussian parameters */
  itkSetMacro(Scale, double);
  itkGetMacro(Scale, double);
  itkSetMacro(Normalized, bool);
  itkGetMacro(Normalized, bool);
  itkSetMacro(Sigma, ArrayType);
  itkGetMacro(Sigma, ArrayType);
  itkSetMacro(Mean, ArrayType);
  itkGetMacro(Mean, ArrayType);
  

protected:
  GaussianImageSource();
  ~GaussianImageSource();
  void PrintSelf(std::ostream& os, Indent indent) const;
  void GenerateData();
  virtual void GenerateOutputInformation();

private:
  GaussianImageSource(const GaussianImageSource&); //purposely not implemented
  void operator=(const GaussianImageSource&); //purposely not implemented

  unsigned long  m_Size[NDimensions];    //size of the output image
  double         m_Spacing[NDimensions]; //spacing
  double         m_Origin[NDimensions];  //origin

  /** Parameters for the Gaussian. */
  
  /** The standard deviation in each direction. */
  ArrayType m_Sigma;

  /** The mean in each direction. */
  ArrayType m_Mean;

  /** A scale factor multiplied by the true value of the Gaussian. */
  double m_Scale;

  /** Whether or not to normalize the Gaussian. */
  bool m_Normalized;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGaussianImageSource.txx"
#endif

#endif
