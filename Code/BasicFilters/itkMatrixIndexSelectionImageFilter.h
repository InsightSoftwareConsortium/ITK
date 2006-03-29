/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMatrixIndexSelectionImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMatrixIndexSelectionImageFilter_h
#define __itkMatrixIndexSelectionImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{
  
namespace Functor {  
  
template< class TInput, class TOutput>
class MatrixIndexSelection
{
public:
  MatrixIndexSelection() {m_I = m_J = 0;}
  ~MatrixIndexSelection() {}

  void GetIndices(unsigned int& i, unsigned int& j) const {i= m_I; j=m_J;}
  void SetIndices(unsigned int i,unsigned int j) {m_I= i; m_J =j;}

  bool operator!=( const MatrixIndexSelection & other ) const
  {
    if( m_I != other.m_I ||
        m_J != other.m_J  )
        {
        return true;
        }
    return false;
   }
  bool operator==( const MatrixIndexSelection & other ) const
  {
    return !(*this != other);
  }

  inline TOutput operator()( const TInput & A )
  {
    return static_cast<TOutput>( A[m_I][m_J] );
  }
      
private:
  unsigned int m_I, m_J;   
}; 
}

 /** \class MatrixIndexSelectionImageFilter
 *
 * \brief Extracts the selected indices of a matrix image that is the input
 * pixel type
 *
 * This filter is templated over the input image type and 
 * output image type.
 * 
 * The filter expect the input image pixel type to be a matrix and 
 * the output image pixel type to be a scalar. The only requirement on
 * the type used for representing the vector is that it must provide an
 * operator(i,j).
 *
 * \ingroup IntensityImageFilters  Multithreaded
 */

template <class TInputImage, class TOutputImage>
class ITK_EXPORT MatrixIndexSelectionImageFilter :
    public
UnaryFunctorImageFilter<TInputImage,TOutputImage,Functor::MatrixIndexSelection< typename TInputImage::PixelType,typename TOutputImage::PixelType> >
{
public:
  /** Standard class typedefs. */
  typedef MatrixIndexSelectionImageFilter Self;
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage,Functor::MatrixIndexSelection<typename TInputImage::PixelType,typename TOutputImage::PixelType> > Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
    
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Get/Set methods for the index */
  void SetIndices(unsigned int i, unsigned int j)
    {
    this->GetFunctor().SetIndices(i,j);
    this->Modified();
    }

  void GetIndices(unsigned int& i, unsigned int& j) const
    { return this->GetFunctor().GetIndices(i,j); }

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputHasNumericTraitsCheck,
    (Concept::HasNumericTraits<typename TInputImage::PixelType::ValueType>));
  /** End concept checking */
#endif

protected:
  MatrixIndexSelectionImageFilter() {}
  virtual ~MatrixIndexSelectionImageFilter() {}
    
private:
  MatrixIndexSelectionImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};
 
} // end namespace itk


#endif
