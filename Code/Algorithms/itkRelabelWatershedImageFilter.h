/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRelabelWatershedImageFilter.h
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
#ifndef __itkRelabelWatershedImageFilter_h
#define __itkRelabelWatershedImageFilter_h
#include "itkImageToImageFilter.h"
#include "itkWatershedImageFilter.h"
#include "itkImage.h"

namespace itk
{
/**
 * \class RelabelWatershedImageFilter
 * \brief Relabels a basic watershed segmentation labeled image at the flood
 * level percentage specified.
 *
 * This class uses the basic segmentation from a WatershedImageFilter
 * object to merge and relabel segments at a specified flood level.  Note that
 * the maximum Level (1.0) is equal to the Level parameter of the input
 * WatershedSegment object.
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT RelabelWatershedImageFilter :
    public ImageToImageFilter< TInputImage, TOutputImage > 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RelabelWatershedImageFilter Self;

  /**
   * Standard super class typedef support.
   */
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  
  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Compile time image dimensionality support.
   */
  enum {ImageDimension = TOutputImage::ImageDimension };
  
  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(RelabelWatershedImageFilter, ImageToImageFilter);
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Standard pipeline method.
   */
  void GenerateData();

  /**
   * Standard get set methods for filter parameter.
   */
  itkSetClampMacro(Level, float, 0.0f, 1.0f);
  itkGetMacro(Level, float);

  /**
   * Get and set input methods
   */
  typename WatershedSegmentBasicOutput<TInputImage, TOutputImage>::Pointer
  GetInput()
  {
    return static_cast<WatershedSegmentBasicOutput<TInputImage,
      TOutputImage>*>(this->ProcessObject::GetInput(0).GetPointer());
  }

  void SetInput(WatershedSegmentBasicOutput<TInputImage, TOutputImage>* input)
  {
    this->ProcessObject::SetNthInput(0, input);
  }

protected:
  RelabelWatershedImageFilter() {}
  virtual ~RelabelWatershedImageFilter() {}
  RelabelWatershedImageFilter(const Self&) {}
  void operator=(const Self&) {}

private:
  float m_Level;
  
};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRelabelWatershedImageFilter.txx"
#endif

#endif
