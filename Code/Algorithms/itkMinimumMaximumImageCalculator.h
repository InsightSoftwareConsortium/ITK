/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSFile: $
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
#ifndef __itkMinimumMaximumImageCalculator_h
#define __itkMinimumMaximumImageCalculator_h

#include "itkObject.h"


namespace itk
{

/** 
 * This calculator computes the minimum and the maximum intensity values
 * of an image.
 * It is templated over input image type.
 * If only Maximum or Minimum value is needed, just call
 * ComputeMaximum() (ComputeMinimum()) otherwise Compute() will compute both. 
 *
 */

template <class TInputImage>            
class ITK_EXPORT MinimumMaximumImageCalculator : public Object 
{
public:

  /** 
   * Standard self typedef
   */
  typedef MinimumMaximumImageCalculator      Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef Object  Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(Self, Object);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Type definition for the input image.
   */
  typedef TInputImage  ImageType;

  /**
   * Pointer type for the image
   */
  typedef typename TInputImage::Pointer  ImagePointer;
  
  /**
   * Const Pointer type for the image
   */
  typedef typename TInputImage::ConstPointer ImageConstPointer;

  /**
   * Type definition for the input image pixel type.
   */
  typedef typename TInputImage::PixelType PixelType;
  
  /**
   * Set the input image
   */
  itkSetConstObjectMacro(Image,ImageType);

  /**
   * Compute the Minimum value of Intensity of the Input Image
   */
  void ComputeMinimum(void);

  /**
   * Compute the Minimum value of Intensity of the Input Image
   */
  void ComputeMaximum(void);

  /**
   * Compute the Minimum value of Intensity of the Input Image
   */
  void Compute(void);

  /**
   * Return minimum of intensity
   */
  itkGetMacro(Minimum,PixelType);
  
  /**
   * Return maximum of intensity
   */
  itkGetMacro(Maximum,PixelType);

 
protected:
  
  MinimumMaximumImageCalculator();
  virtual ~MinimumMaximumImageCalculator() {};
  MinimumMaximumImageCalculator(const Self&) {}
  void operator=(const Self&) {}

private:
   PixelType            m_Minimum;
   PixelType            m_Maximum;
   ImageConstPointer    m_Image;

};

} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMinimumMaximumImageCalculator.txx"
#endif

#endif /* __itkMinimumMaximumImageCalculator_h */
