/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMinimumMaximumImageFilter.h
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
#ifndef __itkMinimumMaximumImageFilter_h
#define __itkMinimumMaximumImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/** \class MinimumMaximumImageFilter
 * \brief Computes the minimum and the maximum intensity values of
 * an image. 
 *
 * It is templated over input image type only.
 * This filter just copy the input image through this output to
 * be included within the pipeline.
 *
 * \ingroup Operators
 * \todo Use itkImageToValueFilter when available
 */
template <class TInputImage>
class ITK_EXPORT MinimumMaximumImageFilter :
    public ImageToImageFilter< TInputImage, TInputImage>
{
public:
  /** Extract dimension from input image. */
  enum {InputImageDimension = TInputImage::ImageDimension};
  enum {OutputImageDimension = TInputImage::ImageDimension};

  /** Standard class typedefs. */
  typedef MinimumMaximumImageFilter Self;
  typedef ImageToImageFilter< TInputImage, TInputImage> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MinimumMaximumImageFilter, ImageToImageFilter);
  
  /** Image typedef support. */
  typedef TInputImage InputImageType;
  typedef typename InputImageType::PixelType InputPixelType;

  /** Return the minimum intensity value. */
  itkGetMacro(Minimum,InputPixelType);
  
  /** Return the maximum intensity value. */
  itkGetMacro(Maximum,InputPixelType);

protected:
  MinimumMaximumImageFilter() {}
  virtual ~MinimumMaximumImageFilter() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  void GenerateData();

private:
  MinimumMaximumImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  InputPixelType    m_Minimum;
  InputPixelType    m_Maximum;
};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMinimumMaximumImageFilter.txx"
#endif

#endif
