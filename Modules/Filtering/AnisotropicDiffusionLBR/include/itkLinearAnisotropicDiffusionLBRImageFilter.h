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
//  Created by Jean-Marie Mirebeau on 28/02/2014.
//
//

#ifndef itkLinearAnisotropicDiffusionLBRImageFilter_h
#define itkLinearAnisotropicDiffusionLBRImageFilter_h


#include "itkImageToImageFilter.h"
#include "itkSymmetricSecondRankTensor.h"

namespace itk
{
/**
 * \class LinearAnisotropicDiffusionLBRImageFilter
 *
 * \brief Anisotropic diffusion using lattice basis reduction.
 *
 * \f[\partial_t u = {\rm div} (D \nabla u),\f]
 *
 * with Neumann boundary conditions. The numerical scheme is stable and
 * satisfies the maximum principle, even for strongly anisotropic tensors,
 * thanks to an adaptive discretization using arithmetic techniques
 * (Lattice Basis Reduction, LBR).
 *
 * \ingroup AnisotropicDiffusionLBR
 */
template< typename TImage, typename TScalar = typename NumericTraits< typename TImage::PixelType >::RealType >
class LinearAnisotropicDiffusionLBRImageFilter:
  public ImageToImageFilter< TImage, TImage >
{
public:
  /** Standard class typedefs. */
  typedef LinearAnisotropicDiffusionLBRImageFilter Self;
  typedef ImageToImageFilter< TImage, TImage >     Superclass;
  typedef SmartPointer< Self >                     Pointer;
  typedef SmartPointer< const Self >               ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LinearAnisotropicDiffusionLBRImageFilter, ImageToImageFilter);

  typedef TImage                        ImageType;
  typedef typename ImageType::PixelType PixelType;

  static const int Dimension = ImageType::ImageDimension;

  typedef TScalar                                            ScalarType;
  typedef SymmetricSecondRankTensor< ScalarType, Dimension > TensorType;
  typedef Image< TensorType, Dimension >                     TensorImageType;
  typedef ImageRegion< Dimension >                           RegionType;

  void SetInputImage(const ImageType* image);
  void SetInputTensor(const TensorImageType* tensorImage);

  void SetMaxDiffusionTime(ScalarType time);
  itkGetConstMacro(DiffusionTime, ScalarType);

  void SetMaxNumberOfTimeSteps(int n);
  itkGetConstMacro(MaxNumberOfTimeSteps, int);

  void SetRatioToMaxStableTimeStep(ScalarType ratio);
  itkGetConstMacro(RatioToMaxStableTimeStep, ScalarType);

  itkGetConstMacro(EffectiveDiffusionTime, ScalarType);
  itkGetConstMacro(EffectiveNumberOfTimeSteps, int);

protected:
  LinearAnisotropicDiffusionLBRImageFilter();
  ~LinearAnisotropicDiffusionLBRImageFilter() override{}

  typename ImageType::ConstPointer GetInputImage();
  typename TensorImageType::ConstPointer GetInputTensor();

  typedef Index<Dimension> IndexType;

  // ******* Containers for the stencils used in the discretization
  static const unsigned int HalfStencilSize = (Dimension == 2) ? 3 : 6;
  static const unsigned int StencilSize = 2 * HalfStencilSize;

  typedef Vector<ScalarType,HalfStencilSize>  StencilCoefficientsType;
  typedef Offset<Dimension>                   OffsetType;
  typedef Vector<OffsetType, HalfStencilSize> StencilOffsetsType;

  typedef int                               InternalSizeT;
  typedef Vector<InternalSizeT,StencilSize> StencilBufferIndicesType;


  // *************** Computation *****************
  void GenerateData() override;
  virtual void GenerateStencils(); /// Automatically called by GenerateData
  virtual void ImageUpdateLoop(); /// Automatically called by GenerateData

  typedef std::pair< StencilBufferIndicesType, StencilCoefficientsType > StencilType;
  typedef Image< StencilType, Dimension >                                StencilImageType;
  typename StencilImageType::Pointer m_StencilImage;

  typedef Image<ScalarType,Dimension> ScalarImageType;
  typename ScalarImageType::Pointer m_DiagonalCoefficients;

  virtual ScalarType MaxStableTimeStep();

  ScalarType m_DiffusionTime;
  ScalarType m_RatioToMaxStableTimeStep;
  int        m_MaxNumberOfTimeSteps;

  ScalarType m_EffectiveDiffusionTime;
  int        m_EffectiveNumberOfTimeSteps;

  virtual void ImageUpdate(ScalarType delta);
  typename ImageType::Pointer m_PreviousImage;
  typename ImageType::Pointer m_NextImage;

  virtual RegionType GetRequestedRegion(){return GetInputImage()->GetRequestedRegion();}

  InternalSizeT OutsideBufferIndex() const {return NumericTraits<InternalSizeT>::max();}

  struct StencilFunctor;
  struct FunctorType;

  typedef Vector<ScalarType,Dimension> VectorType;
  static ScalarType ScalarProduct(const TensorType &, const VectorType &, const VectorType &);

private:
  LinearAnisotropicDiffusionLBRImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);  //purposely not implemented
};
} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLinearAnisotropicDiffusionLBRImageFilter.hxx"
#endif

#endif
