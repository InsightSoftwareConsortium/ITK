/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkModulusImageFilter_h
#define itkModulusImageFilter_h

#include "itkBinaryFunctorImageFilter.h"
#include "itkArithmeticOpsFunctors.h"


namespace itk
{

/** \class ModulusImageFilter
 * \brief Computes the modulus (x % dividend) pixel-wise
 *
 * The input pixel type must support the c++ modulus operator (%).
 *
 * If the dividend is zero, the maximum value will be returned.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \ingroup IntensityImageFilters  MultiThreaded
 *
 * \ingroup ITKImageIntensity
 */
template< typename  TInputImage1, typename TInputImage2 = TInputImage1, typename TOutputImage = TInputImage1 >
class ITK_TEMPLATE_EXPORT ModulusImageFilter:
  public
  BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage,
                            Functor::Modulus<
                              typename TInputImage1::PixelType,
                              typename TInputImage2::PixelType,
                              typename TOutputImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef ModulusImageFilter Self;
  typedef BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage,
                            Functor::Modulus<
                              typename TInputImage1::PixelType,
                              typename TInputImage2::PixelType,
                              typename TOutputImage::PixelType > > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  typedef typename TOutputImage::PixelType                    OutputPixelType;
  typedef typename TInputImage1::PixelType                    InputPixelType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ModulusImageFilter,
               UnaryFunctorImageFilter);

  /** Set/Get the dividend */
  virtual void SetDividend( InputPixelType _arg ) { this->SetConstant2(_arg); }
  virtual const InputPixelType &GetDividend () const { return this->GetConstant2(); }


#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< InputPixelType > ) );
  // End concept checking
#endif

protected:
  ModulusImageFilter();
  virtual ~ModulusImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ModulusImageFilter);

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkModulusImageFilter.hxx"
#endif

#endif
