/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEigenAnalysis2DImageFilter.h
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
#ifndef __itkEigenAnalysis2DImageFilter_h
#define __itkEigenAnalysis2DImageFilter_h

#include "itkImageToImageFilter.h"


/** \class EigenAnalysis2DImageFilter
 * \brief Computes pixel-wise the eigen values and eigen vectors 
 *        of a 2D symmetrical matrix.
 *
 * The filter expects three inputs images { A, B, C } representing
 * the component of the matrix
 *
 *                    | A  B |
 *                    | B  c |
 *
 * The eigen values are stored in two output images, and the eigen
 * vector associated with the maximum eigenvalue is stored in an 
 * image using vector as pixel type.
 * 
 */

namespace itk {


template <class TInputImage, class TEigenValueImage, class TEigenVectorImage >
class ITK_EXPORT EigenAnalysis2DImageFilter:
  public ImageToImageFilter<TInputImage,TEigenValueImage>
{

public:

  /**
   * Standard "Self" typedef.
   */
  typedef EigenAnalysis2DImageFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageToImageFilter<TInputImage,TEigenValueImage> Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>                   Pointer;
  typedef SmartPointer<const Self>        ConstPointer;

  
  /** 
   * Typedef for the vector type representing the eigen vectors
   */
	typedef typename TEigenVectorImage::PixelType     EigenVectorType;
  typedef typename EigenVectorType::ValueType   VectorComponentType;


  /** 
   * Image Dimension
   */
  enum { ImageDimension = TInputImage::ImageDimension };

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Connect the image containting the elements [0,0]
   * of the input 2D matrix
   */
   void SetInput1( TInputImage * image1);

  /**
   * Connect the image containting the elements [0,1]
   * of the input 2D matrix. This is the same [1,0]
   * element given that the input matrix is expected
   * to be symmetric
   */
   void SetInput2( TInputImage * image2);

  /**
   * Connect the image containting the elements [1,1]
   * of the input 2D matrix
   */
   void SetInput3( TInputImage * image3);

  /**
   * Get the Output image with the greatest eigenvalue
   */
   typename TEigenValueImage::Pointer GetMaxEigenValue( void );

  /**
   * Get the Output image with the smallest eigenvalue
   */
   typename TEigenValueImage::Pointer GetMinEigenValue( void );

  /**
   * Get the Output image with the eigen vector associated with
   * the greatest eigen value
   */
   typename TEigenVectorImage::Pointer GetMaxEigenVector( void );


protected:
  EigenAnalysis2DImageFilter();
  virtual ~EigenAnalysis2DImageFilter() {};
  EigenAnalysis2DImageFilter(const Self&) {}
  void operator=(const Self&) {}
  
  /**
   * Generate Data supporting multithreading
   */
  void ThreadedGenerateData( 
                const OutputImageRegionType &outputRegionForThread,
                int threadId);


private:
  

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkEigenAnalysis2DImageFilter.txx"
#endif

#endif




