/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianImageSource.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkGaussianImageSource_h
#define __itkGaussianImageSource_h

#include "itkImageSource.h"
#include "itkArray.h"

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
  enum { NDimensions = TOutputImage::ImageDimension };

  /** Type used to store gaussian parameters. */
  typedef Array<double, NDimensions> TArrayType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(GaussianImageSource,ImageSource);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Specify the size of the output image. */
  itkSetVectorMacro(Size,unsigned long,TOutputImage::ImageDimension);

  /** Get the size of the output image. */
  itkGetVectorMacro(Size,unsigned long,TOutputImage::ImageDimension);
  
  /** Specify the spacing of the output image. */
  itkSetVectorMacro(Spacing,float,TOutputImage::ImageDimension);

  /** Get the spacing of the output image. */
  itkGetVectorMacro(Spacing,float,TOutputImage::ImageDimension);

  /** Specify the origin of the output image. */
  itkSetVectorMacro(Origin,float,TOutputImage::ImageDimension);

  /** Get the origin of the output image. */
  itkGetVectorMacro(Origin,float,TOutputImage::ImageDimension);

    /** Gets and sets for gaussian parameters */
  itkSetMacro(Scale, double);
  itkGetMacro(Scale, double);
  itkSetMacro(Normalized, bool);
  itkGetMacro(Normalized, bool);
  itkSetMacro(Sigma, TArrayType);
  itkGetMacro(Sigma, TArrayType);
  itkSetMacro(Mean, TArrayType);
  itkGetMacro(Mean, TArrayType);
  

protected:
  GaussianImageSource();
  ~GaussianImageSource();
  void PrintSelf(std::ostream& os, Indent indent) const;
  void GenerateData();

private:
  GaussianImageSource(const GaussianImageSource&); //purposely not implemented
  void operator=(const GaussianImageSource&); //purposely not implemented

  unsigned long *m_Size;    //size of the output image
  float         *m_Spacing; //spacing
  float         *m_Origin;  //origin

  /** Parameters for the Gaussian. */
  
  /** The standard deviation in each direction. */
  TArrayType m_Sigma;

  /** The mean in each direction. */
  TArrayType m_Mean;

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
