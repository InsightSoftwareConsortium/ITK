/*=========================================================================
  
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPlaheImageFilter.h
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
#ifndef __itkPlaheImageFilter_h
#define __itkPlaheImageFilter_h


#include <itkImageToImageFilter.h>
#include <itkImage.h>

namespace itk
{
/**
 * /class PlaheImageFilter
 * Power Law Adaptive Histogram Equalization (PLAHE) is one of adaptive
 * histogram equalization method.  For detail description, reference
 * "Adaptive Image Contrast Enhancement using Generalizations of Histogram 
 * Equalization."  J.Alex Stark. IEEE Transactions on Image Processing, 
 * May 2000.
 */

template <class TPixel, unsigned int VImageDimension = 2>
class ITK_EXPORT PlaheImageFilter :
  public ImageToImageFilter< Image<TPixel, VImageDimension>,
                             Image<TPixel, VImageDimension> >
{
public:
 /**
  * Standard "Self" typedef.
  */ 
  typedef PlaheImageFilter Self;
  
 /**
  * Standard super class typedef support.
  */
  typedef ImageToImageFilter< Image<TPixel, VImageDimension>,
                              Image<TPixel, VImageDimension> > Superclass;

 /**
  * Smart pointer typedef support
  */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> constPointer;

 /**
  * Image type typedef support
  */
  typedef Image<TPixel, VImageDimension> ImageType;

 /**
  * Run-time type information (and related methods)
  */
  itkTypeMacro(PlaheImageFilter, ImageToImageFilter);

 /**
  * Method for creation through the object factory
  */
  itkNewMacro(Self);

 /**
  * A function which is used in GenerateData();
  */
  float CumulativeFunction(float u, float v);
   
 /**
  * Standard pipeline method.
  */
  void GenerateData();

 /**
  * Standard Get/Set macros for filter parameters
  */
  itkSetMacro(Alpha, float);
  itkGetMacro(Alpha, float);
  itkSetMacro(Beta, float);
  itkGetMacro(Beta, float);
  itkSetVectorMacro(Window, unsigned int, VImageDimension);
  itkGetVectorMacro(Window, const unsigned int, VImageDimension);

private:
 /**
  * The beta parameter of the Plahe
  */
  float m_Alpha;

 /**
  * The alpha parameter of the Plahe
  */
  float m_Beta;

 /**
  * The window size of the Plahe algorithm
  * This parameter defines the size of neighborhood around the evaluated pixel.
  */
  unsigned int m_Window[VImageDimension];

protected:
  PlaheImageFilter(){};
  virtual ~PlaheImageFilter(){};
  PlaheImageFilter(const Self&){};
  void operator=(const Self&){};
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPlaheImageFilter.txx"
#endif

#endif
