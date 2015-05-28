//
//  StructureTensorImageFilter.h
//  itkDiffusion
//
//  Created by Jean-Marie Mirebeau on 05/03/2014.
//
//

#ifndef itkDiffusion_StructureTensorImageFilter_h
#define itkDiffusion_StructureTensorImageFilter_h

#include "itkCastImageFilter.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkAddImageFilter.h"
#include "itkVectorIndexSelectionCastImageFilter.h"
#include "itkGradientImageFilter.h"

namespace itk {
    /**
     Implementation of the structure tensor, defined by 
     \f[K_\rho (\nabla u_\sigma \otimes \nabla u_\sigma),\f] where \f$K_\rho\f$ denotes the gaussian kernel of standard deviation \f$\rho\f$, and \f$u_\sigma := K_\sigma * u\f$.
     */
    template<
    typename TImage,
    typename TTensorImage = Image<
        SymmetricSecondRankTensor<typename TImage::PixelType,TImage::ImageDimension >,
        TImage::ImageDimension>
    >
    class StructureTensorImageFilter : public ImageToImageFilter<TImage, TTensorImage> {
    public:
        typedef StructureTensorImageFilter Self;
        typedef ImageToImageFilter< TImage, TImage> Superclass;
        typedef SmartPointer<Self> Pointer;
        typedef SmartPointer<const Self> ConstPointer;
        
        /// Method for creation through the object factory.
        itkNewMacro(Self);
        /// Run-time type information (and related methods).
        itkTypeMacro(StructureTensorImageFilter, Superclass);
        
        typedef TImage ImageType;
        typedef typename ImageType::PixelType PixelType;
        static const unsigned int Dimension = ImageType::ImageDimension;
        typedef TTensorImage TensorImageType;
        typedef typename TensorImageType::PixelType TensorType;
        typedef typename TensorType::ComponentType ScalarType;
        typedef Image<ScalarType, Dimension> ScalarImageType;
        
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
        
        bool m_UseGradientRecursiveGaussianImageFilter;
    protected:
        virtual void GenerateData();
        
        ScalarType m_FeatureScale, m_NoiseScale;
        bool m_RescaleForUnitMaximumTrace;
        ScalarType m_PostRescaling;
        
        template<typename Dummy=void, bool b=std::numeric_limits<PixelType>::is_specialized>
        struct IntermediateFilter;
        typename TensorImageType::Pointer intermediateResult;
        
        typedef CovariantVector<ScalarType,Dimension> CovariantVectorType;
        typedef Image<CovariantVectorType,Dimension> CovariantImageType;
        
        struct OuterFunctor;
        struct TraceFunctor;
        struct ScaleFunctor;
        
        StructureTensorImageFilter(){
            m_FeatureScale=2;
            m_NoiseScale=1;
            m_RescaleForUnitMaximumTrace=false;
            m_UseGradientRecursiveGaussianImageFilter=true;
        }
    };
}

#include "StructureTensorImageFilter.hxx"

#endif
