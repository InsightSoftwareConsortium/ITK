/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCentralDerivativeImageFunction.h
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
#ifndef _itkCentralDerivativeImageFunction_h
#define _itkCentralDerivativeImageFunction_h

#include "itkImageFunction.h"

namespace itk
{

/**
 * \class CentralDerivativeImageFunction
 * \brief Calculate the derivative by central differencing
 *
 * This class is templated over the input image type.
 *
 * Possible improvements:
 * - the use of Neighborhood operators may improve efficiency.
 */
template <class TInputImage >
class ITK_EXPORT CentralDerivativeImageFunction :
  public ImageFunction< TInputImage, double >
{
public:
  /**
   * Standard "Self" typedef
   */
  typedef CentralDerivativeImageFunction Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef ImageFunction<TInputImage, double > Superclass;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Dimension of the underlying image.
   */
  enum { ImageDimension = InputImageType::ImageDimension };

  /**
   * InputImageType typedef support.
   */
  typedef TInputImage InputImageType;

  /**
   * OutputType typdef support.
   */
  typedef typename Superclass::OutputType OutputType;

  /**
   * Index typedef support.
   */
  typedef typename Superclass::IndexType IndexType;

  /**
   * Set the input image.
   */
  virtual void SetInputImage( InputImageType * ptr );

  /**
   * Set the image spacing
   */
  void SetImageSpacing( const double * spacing );

  /**
   * Evalulate the function at specified index
   */
  virtual double Evaluate( const IndexType& index ) const
    { return ( this->Evaluate( index, 0 ) ); }
  virtual double Evaluate( const PointType& point ) const
    { return ( this->Superclass::Evaluate( point ) ); }

  virtual double Evaluate( const IndexType& index, unsigned int dim ) const;


protected:
  CentralDerivativeImageFunction();
  CentralDerivativeImageFunction( const Self& ){};

  ~CentralDerivativeImageFunction(){};

  void operator=( const Self& ){};
  void PrintSelf(std::ostream& os, Indent indent);

private:
  Size<ImageDimension>    m_ImageSize;
  IndexType               m_ImageStart;
  double                  m_ImageSpacing[ImageDimension];

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCentralDerivativeImageFunction.txx"
#endif

#endif

