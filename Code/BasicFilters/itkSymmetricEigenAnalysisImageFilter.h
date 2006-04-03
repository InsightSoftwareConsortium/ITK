/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSymmetricEigenAnalysisImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSymmetricEigenAnalysisImageFilter_h
#define __itkSymmetricEigenAnalysisImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkSymmetricEigenAnalysis.h"


namespace itk
{

// This functor class invokes the computation of Eigen Analysis for
// every pixel. The input pixel type must provide the API for the [][]
// operator, while the output pixel type must provide the API for the
// [] operator. Input pixel matrices should be symmetric.
// 
// The default operation is to order eigen values in ascending order.
// You may also use OrderEigenValuesBy( ) to order eigen values by
// magnitude as is common with use of tensors in vessel extraction.
namespace Functor {  
 
template< typename TInput, typename TOutput >
class SymmetricEigenAnalysisFunction
{
public:
  typedef typename TInput::RealValueType  RealValueType;
  SymmetricEigenAnalysisFunction() {}
  ~SymmetricEigenAnalysisFunction() {}
  typedef SymmetricEigenAnalysis< TInput, TOutput > CalculatorType;
  bool operator!=( const SymmetricEigenAnalysisFunction & ) const
  {
    return false;
  }
  bool operator==( const SymmetricEigenAnalysisFunction & other ) const
  {
    return !(*this != other);
  }

  inline TOutput operator()( const TInput & x )
    {
    TOutput eigenValues;
    m_Calculator.ComputeEigenValues( x, eigenValues );
    return eigenValues;
    }

  /** Method to explicitly set the dimension of the matrix */
  void SetDimension( unsigned int n )
    {
    m_Calculator.SetDimension(n);
    }

  /** Typdedefs to order eigen values. 
   * OrderByValue:      lambda_1 < lambda_2 < ....
   * OrderByMagnitude:  |lambda_1| < |lambda_2| < .....
   * DoNotOrder:        Default order of eigen values obtained after QL method
   */
  typedef enum {
    OrderByValue=1,
    OrderByMagnitude,
    DoNotOrder
  }EigenValueOrderType;
 
  /** Order eigen values. Default is to OrderByValue:  lambda_1 < lambda_2 < ....*/
  void OrderEigenValuesBy( EigenValueOrderType order )
    {
    if( order == OrderByMagnitude )
      {
      m_Calculator.SetOrderEigenMagnitudes( true );
      }
    else if( order == DoNotOrder )
      {
      m_Calculator.SetOrderEigenValues( false );
      }
    }

private:
  CalculatorType m_Calculator;
}; 

}  // end namespace functor


/** \class SymmetricEigenAnalysisImageFilter
 * \brief Computes the Fractional Anisotropy for every pixel of a input tensor image.
 *
 * SymmetricEigenAnalysisImageFilter applies pixel-wise the invokation for
 * computing the fractional anisotropy of every pixel. The pixel type of the
 * input image is expected to implement a method GetFractionalAnisotropy(), and
 * to specify its return type as  RealValueType.
 * 
 * The OrderEigenValuesBy( .. ) method can be used to order eigen values 
 * in ascending order by value or magnitude or no ordering.
 * OrderByValue:      lambda_1 < lambda_2 < ....
 * OrderByMagnitude:  |lambda_1| < |lambda_2| < .....
 * DoNotOrder:        Default order of eigen values obtained after QL method
 *
 * The user of this class is explicitly supposed to set the dimension of the 
 * 2D matrix using the SetDimension() method.
 *
 * \sa TensorRelativeAnisotropyImageFilter
 * \sa DiffusionTensor3D
 * 
 * \ingroup IntensityImageFilters  Multithreaded  TensorObjects
 *
 */
template <typename  TInputImage, typename  TOutputImage=TInputImage>
class ITK_EXPORT SymmetricEigenAnalysisImageFilter :
    public
UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                        Functor::SymmetricEigenAnalysisFunction< 
                                        typename TInputImage::PixelType,
                                        typename TOutputImage::PixelType > >
{
public:
  /** Standard class typedefs. */
  typedef SymmetricEigenAnalysisImageFilter  Self;
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                                  Functor::SymmetricEigenAnalysisFunction< 
                                        typename TInputImage::PixelType,
                                        typename TOutputImage::PixelType > >
                                                                      Superclass;

  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  typedef typename Superclass::OutputImageType    OutputImageType;
  typedef typename TOutputImage::PixelType        OutputPixelType;
  typedef typename TInputImage::PixelType         InputPixelType;
  typedef typename InputPixelType::ValueType      InputValueType;
  typedef typename Superclass::FunctorType        FunctorType; 

  /** Typdedefs to order eigen values. 
   * OrderByValue:      lambda_1 < lambda_2 < ....
   * OrderByMagnitude:  |lambda_1| < |lambda_2| < .....
   * DoNotOrder:        Default order of eigen values obtained after QL method
   */
  typedef typename FunctorType::EigenValueOrderType         EigenValueOrderType;
 
  /** Order eigen values. Default is to OrderByValue:  lambda_1 < lambda_2 < ....*/
  void OrderEigenValuesBy( EigenValueOrderType order )
    {
    this->GetFunctor().OrderEigenValuesBy( order );
    }

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Print internal ivars */
  void PrintSelf(std::ostream& os, Indent indent) const
    { this->Superclass::PrintSelf( os, indent ); }
  
  /** Set the dimension of the tensor. (For example the SymmetricSecondRankTensor
   * is a pxp matrix) */
  void SetDimension( unsigned int p )
    {
    this->GetFunctor().SetDimension(p);
    }

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputHasNumericTraitsCheck,
                  (Concept::HasNumericTraits<InputValueType>));
  /** End concept checking */
#endif

protected:
  SymmetricEigenAnalysisImageFilter() {};
  virtual ~SymmetricEigenAnalysisImageFilter() {};

private:
  SymmetricEigenAnalysisImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};


  
} // end namespace itk
  
#endif
