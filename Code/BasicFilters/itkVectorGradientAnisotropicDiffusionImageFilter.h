/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkAcosImageAdaptor.h
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
#ifndef __itkVectorGradientAnisotropicDiffusionImageFilter_h_
#define __itkVectorGradientAnisotropicDiffusionImageFilter_h_

#include "itkExceptionObject.h"
#include "itkAnisotropicDiffusionImageFilter.h"
#include "itkVectorGradient2DAnisotropicDiffusionEquation.h"

namespace itk {

/**
 * \class VectorGradientAnisotropicDiffusionImageFilter
 *\todo Document.
 */
template <class TInputImage, class TOutputImage>
class VectorGradientAnisotropicDiffusionImageFilter
  : public AnisotropicDiffusionImageFilter<TInputImage, TOutputImage>
{
public:
  /**
   * Standard itk typedefs
   */
  typedef VectorGradientAnisotropicDiffusionImageFilter Self;
  typedef AnisotropicDiffusionImageFilter<TInputImage, TOutputImage>
   Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  itkNewMacro(Self);

  itkTypeMacro(VectorGradientAnisotropicDiffusionImageFilter,
               AnisotropicDiffusionImageFilter);
  
  typedef typename Superclass::UpdateBufferType UpdateBufferType;
  enum { ImageDimension = Superclass::ImageDimension };
  
protected:
  VectorGradientAnisotropicDiffusionImageFilter()
    {
      if ( ImageDimension == 2) 
        {
          VectorGradient2DAnisotropicDiffusionEquation<UpdateBufferType>::Pointer p        
            = VectorGradient2DAnisotropicDiffusionEquation<UpdateBufferType>::New();
          this->SetDifferenceEquation(p);
        }
      else
        {
          VectorGradient2DAnisotropicDiffusionEquation<UpdateBufferType>::Pointer p        
            = VectorGradient2DAnisotropicDiffusionEquation<UpdateBufferType>::New();
          this->SetDifferenceEquation(p);
        }
    }
  ~VectorGradientAnisotropicDiffusionImageFilter() {}
  VectorGradientAnisotropicDiffusionImageFilter(const Self&) {}
  
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent)
    {
      os << indent << "VectorGradientAnisotropicDiffusionImageFilter";
      Superclass::PrintSelf(os, indent.GetNextIndent());
    }
};

} // end namspace itk

#endif
