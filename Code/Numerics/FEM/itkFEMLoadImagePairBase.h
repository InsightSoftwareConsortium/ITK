/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkFEMLoadImagePairBase.h Language:  C++
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
#ifndef _itkFEMLoadImagePairBase_h_
#define _itkFEMLoadImagePairBase_h_


#include "itkImage.h"
#include "itkTranslationTransform.h"

#include "itkImageRegionIteratorWithIndex.h"
#include "itkNeighborhoodIterator.h"
#include "itkSmartNeighborhoodIterator.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkDerivativeOperator.h"
#include "itkForwardDifferenceOperator.h"

#include "vnl/vnl_math.h"
#include "itkFEMLoadGrav.h"



namespace itk 
{
namespace fem
{

/**
 * \class LoadImagePairBase
 * \brief general image pair load
 *
 * LoadImagePairBase is a base class for implementing FEM gravity loads derived 
 * from image information.
 */ 
template<class TReference,class TTarget> 
class LoadImagePairBase : public LoadGrav
{
FEM_CLASS_SP(LoadImagePairBase,LoadGrav)
public:
//  typedef LoadImagePairBase                Self;                   
//  typedef LoadGrav                Superclass;         
//  typedef SmartPointer<Self>            Pointer;   
//  typedef SmartPointer<const Self> ConstPointer;  
//  itkTypeMacro(LoadImagePairBase,LoadGrav);
//  itkNewMacro(Self);   // need light object

  typedef LoadGrav::Float Float;

  typedef TReference ReferenceType;
  typedef ReferenceType*  ReferencePointer;
  typedef TTarget       TargetType;
  typedef TargetType*  TargetPointer;
  /** Inherit some enums and typedefs from template */
  enum{ ImageDimension = TReference::ImageDimension };
  typedef ImageRegionIteratorWithIndex<ReferenceType> RefRegionIteratorType; 
  typedef ImageRegionIteratorWithIndex<TargetType>    TarRegionIteratorType; 
  
  typedef SmartNeighborhoodIterator<ReferenceType> 
                                     ReferenceNeighborhoodIteratorType; 
  typedef typename ReferenceNeighborhoodIteratorType::IndexType  
                                     ReferenceNeighborhoodIndexType;
  typedef typename ReferenceNeighborhoodIteratorType::RadiusType 
                                     ReferenceRadiusType;
  typedef SmartNeighborhoodIterator<TargetType> 
                                     TargetNeighborhoodIteratorType; 
  typedef typename TargetNeighborhoodIteratorType::IndexType  
                                     TargetNeighborhoodIndexType;
  typedef typename TargetNeighborhoodIteratorType::RadiusType 
                                     TargetRadiusType;

  typedef   Array<Float>  ParametersType;
  

// IMAGE DATA
  typedef   typename  ReferenceType::PixelType RefPixelType;
  typedef   typename  TargetType::PixelType    TarPixelType;
  typedef   Float PixelType;
  typedef   Float ComputationType;
  typedef   CovariantVector< PixelType, ImageDimension >  CovariantVectorType;
  typedef   Image< CovariantVectorType, ImageDimension >  CovariantVectorImageType;
  typedef   Image< RefPixelType, ImageDimension >       RefImageType;
  typedef   Image< TarPixelType, ImageDimension >       TarImageType;
  typedef   Image< PixelType, ImageDimension >            ImageType;
  typedef   vnl_vector<Float>                                   VectorType;
  
// FUNCTIONS
/**
 * Compute the image based gravity load - implemented in the derived class.
 */
   VectorType Fg(VectorType) { return vnl_vector<Float>(ImageDimension,0.0); }
   
  LoadImagePairBase(); 
  
  void SetReferenceImage(ReferenceType*); /** Define the reference (moving) image. */
  void SetTargetImage(TargetType*);       /** Define the target (fixed) image. */ 
  ReferencePointer GetReferenceImage() { return m_RefImage; };
  TargetPointer GetTargetImage() { return m_TarImage; };

  void SetReferenceRadius(ReferenceRadiusType R) {m_RefRadius  = R; }; 
  /** Define the reference (moving) image region size. */
  void SetTargetRadius(TargetRadiusType T) {m_TarRadius  = T; };       
  /** Define the target (fixed) image region size. */ 


  virtual Baseclass::Pointer Clone() const 
      { return new Self(*this); }  //FIXME?


protected:
 
  typedef typename CovariantVectorImageType::Pointer  m_DerivativeImage;
  
  ReferencePointer                                    m_RefImage;
  TargetPointer                                       m_TarImage;
  ReferenceRadiusType                                 m_RefRadius; /** used by the neighborhood iterator */
  TargetRadiusType                                    m_TarRadius; /** used by the metric to set region size for fixed image*/ 
  typename ReferenceType::SizeType                    m_RefSize;
  typename TargetType::SizeType                       m_TarSize;

private:

//  LoadImagePairBase(const Self&);   FIXME NEED COPY CONSTRUCTOR   //purposely not implemented
  void operator=(const Self&); //purposely not implemented  

};


}} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFEMLoadImagePairBase.txx"
#endif

#endif
