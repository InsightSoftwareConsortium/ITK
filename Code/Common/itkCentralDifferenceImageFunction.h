/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCentralDifferenceImageFunction.h
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
#ifndef _itkCentralDifferenceImageFunction_h
#define _itkCentralDifferenceImageFunction_h

#include "itkImageFunction.h"

namespace itk
{

/**
 * \class CentralDifferenceImageFunction
 * \brief Calculate the derivative by central differencing.
 *
 * This class is templated over the input image type.
 *
 * Possible improvements:
 * - the use of Neighborhood operators may improve efficiency.
 *
 * \ingroup Functions
 */
template <class TInputImage >
class ITK_EXPORT CentralDifferenceImageFunction :
  public ImageFunction< TInputImage, double >
{
public:
  /** Standard class typedefs. */
  typedef CentralDifferenceImageFunction Self;
  typedef ImageFunction<TInputImage, double > Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(CentralDifferenceImageFunction, ImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** InputImageType typedef support. */
  typedef TInputImage InputImageType;

  /** OutputType typdef support. */
  typedef typename Superclass::OutputType OutputType;

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;
  
  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** Point typedef support. */
  typedef typename Superclass::PointType PointType;

  /** Dimension of the underlying image. */
  enum { ImageDimension = InputImageType::ImageDimension };

  /** Evalulate the function at specified index */
  virtual double EvaluateAtIndex( const IndexType& index ) const
    { return ( this->EvaluateAtIndex( index, 0 ) ); }
  virtual double EvaluateAtIndex( const IndexType& index, 
    unsigned int dim ) const;
  
  /** Evaluate the function at non-integer positions */
  virtual double Evaluate( const PointType& point ) const
    { 
      IndexType index;
      this->ConvertPointToNearestIndex( point, index );
      return this->EvaluateAtIndex( index, 0 ); 
    }
  virtual double EvaluateAtContinuousIndex( 
    const ContinuousIndexType& cindex ) const
    { 
      IndexType index;
      this->ConvertContinuousIndexToNearestIndex( cindex, index );
      return this->EvaluateAtIndex( index, 0 ) ; 
    }
  
protected:
  CentralDifferenceImageFunction();
  ~CentralDifferenceImageFunction(){};
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  CentralDifferenceImageFunction( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCentralDifferenceImageFunction.txx"
#endif

#endif

