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

#ifndef itkEigenToScalarImageFilter_h
#define itkEigenToScalarImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk {
/** \class EigenToScalarImageFilter
 * \brief Abstract class for converting eigenvalue image to scalar image.
 *
 * This is an abstract class that converts an eigenvalue image to a scalar image based using a mathematical function.
 * Other classes should inherit from this class so they can be used in the BoneEnhancement framework. This abstract
 * class guarantees that the variable EigenValueOrder has a valid type and enforces some concept checking
 * on the class template parameters: TInputImage and TOutputImage .
 * 
 * \sa MultiScaleHessianEnhancementImageFilter.
 * 
 * \author: Bryce Besler
 * \ingroup BoneEnhancement
 */
template< typename TInputImage, typename TOutputImage = TInputImage >
class ITK_TEMPLATE_EXPORT EigenToScalarImageFilter:
public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard Self typedef */
  typedef EigenToScalarImageFilter                        Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(EigenToScalarImageFilter, ImageToImageFilter);

  /** Template the EigenValueOrderType. Methods that inherit from this class can override this function
   * to produce a different eigenvalue ordering. Ideally, the enum EigenValueOrderType should come from
   * itkSymmetricEigenAnalysisImageFilter.h or itkSymmetricEigenAnalysis.h. That turns out to be non-trivial
   * because the enumeration is hidden within the templated class. Therefore, you would need the hessian type
   * and eigenvalue type to do such an operation. We do not necessarily have the hessian type information.
   */
  typedef enum {
    OrderByValue = 1,
    OrderByMagnitude,
    DoNotOrder
  } EigenValueOrderType;
  itkStaticConstMacro(EigenValueOrder, EigenValueOrderType, OrderByMagnitude);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputOutputHaveSamePixelDimensionCheck,
                   ( Concept::SameDimension< TInputImage::ImageDimension, TOutputImage::ImageDimension >) );
  // End concept checking
#endif
protected:
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE {
    Superclass::PrintSelf(os, indent);
    os << indent << "EigenValueOrder: " << this->GetEigenValueOrder() << std::endl;
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(EigenToScalarImageFilter);
}; //end class
} // end namespace 

#endif // itkEigenToScalarImageFilter_h
