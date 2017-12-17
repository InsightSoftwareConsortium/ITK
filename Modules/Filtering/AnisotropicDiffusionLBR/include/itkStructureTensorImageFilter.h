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
//
//  Created by Jean-Marie Mirebeau on 05/03/2014.
//
//

#ifndef itkStructureTensorImageFilter_h
#define itkStructureTensorImageFilter_h

#include "itkCastImageFilter.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkAddImageFilter.h"
#include "itkVectorIndexSelectionCastImageFilter.h"
#include "itkGradientImageFilter.h"
#include "itkSymmetricSecondRankTensor.h"

namespace itk
{
/**
 * \class StructureTensorImageFilter
 *
 * \brief Computes the structure tensor.
 *
 * Implementation of the structure tensor, defined by
 *
 * \f[K_\rho (\nabla u_\sigma \otimes \nabla u_\sigma),\f]
 *
 * where \f$K_\rho\f$ denotes the gaussian kernel of standard deviation \f$\rho\f$,
 * and \f$u_\sigma := K_\sigma * u\f$.
 *
 * \ingroup AnisotropicDiffusionLBR
 */
template< typename TImage,
          typename TTensorImage =
            Image< SymmetricSecondRankTensor< typename TImage::PixelType,TImage::ImageDimension >, TImage::ImageDimension > >
class StructureTensorImageFilter:
  public ImageToImageFilter< TImage, TTensorImage >
{
public:
  typedef StructureTensorImageFilter          Self;
  typedef ImageToImageFilter< TImage, TImage> Superclass;
  typedef SmartPointer<Self>                  Pointer;
  typedef SmartPointer<const Self>            ConstPointer;

  /// Method for creation through the object factory.
  itkNewMacro(Self);
  /// Run-time type information (and related methods).
  itkTypeMacro(StructureTensorImageFilter, Superclass);

  typedef TImage                              ImageType;
  typedef typename ImageType::PixelType       PixelType;
  static const unsigned int Dimension =       ImageType::ImageDimension;
  typedef TTensorImage                        TensorImageType;
  typedef typename TensorImageType::PixelType TensorType;
  typedef typename TensorType::ComponentType  ScalarType;
  typedef Image<ScalarType, Dimension>        ScalarImageType;

  ///Parameter \f$\sigma\f$ of the structure tensor definition.
  itkSetMacro(NoiseScale, ScalarType);
  ///Parameter \f$\rho\f$ of the structure tensor definition.
  itkSetMacro(FeatureScale, ScalarType);
  ///Rescales all structure tensors by a common factor, so that the maximum trace is 1.
  itkSetMacro(RescaleForUnitMaximumTrace, bool);

  itkGetConstMacro(NoiseScale, ScalarType);
  itkGetConstMacro(FeatureScale, ScalarType);
  itkGetConstMacro(RescaleForUnitMaximumTrace, bool);
  itkGetConstMacro(PostRescaling, ScalarType); /// Global rescaling constant used.

protected:
  void GenerateData() override;

  ScalarType m_FeatureScale;
  ScalarType m_NoiseScale;
  bool       m_RescaleForUnitMaximumTrace;
  ScalarType m_PostRescaling;
  bool       m_UseGradientRecursiveGaussianImageFilter;

  struct DispatchBase {};
  template< bool >
  struct Dispatch: public DispatchBase {};

  void IntermediateFilter( const Dispatch< true > & );
  void IntermediateFilter( const Dispatch< false > & );
  typename TensorImageType::Pointer m_IntermediateResult;

  typedef CovariantVector<ScalarType,Dimension> CovariantVectorType;
  typedef Image<CovariantVectorType,Dimension>  CovariantImageType;

  struct OuterFunctor
  {
    TensorType operator()(const CovariantVectorType & u) const
      {
      TensorType m;
      for( unsigned int i = 0; i < Dimension; ++i )
        {
        for( unsigned int j = i; j < Dimension; ++j)
          {
          m(i,j) = u[i]*u[j];
          }
        }
        return m;
      }
  };
  struct TraceFunctor
  {
    ScalarType operator()(const TensorType & t) const {
        return t.GetTrace();
    }
  };
  struct ScaleFunctor
  {
    ScalarType scaling;
    TensorType operator()(const TensorType & t) const {
        return t*scaling;
    }
  };

  StructureTensorImageFilter();
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStructureTensorImageFilter.hxx"
#endif

#endif
