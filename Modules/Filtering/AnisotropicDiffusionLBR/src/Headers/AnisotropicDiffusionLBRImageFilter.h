//
//  AnisotropicDiffusionLBRImageFilter.h
//  itkDiffusion
//
//  Created by Jean-Marie Mirebeau on 28/02/2014.
//
//

#ifndef itkDiffusion_AnisotropicDiffusionLBRImageFilter_h
#define itkDiffusion_AnisotropicDiffusionLBRImageFilter_h

#include "LinearAnisotropicDiffusionLBRImageFilter.h"
#include "StructureTensorImageFilter.h"
//#include "itkVectorMagnitudeImageFilter.h"
//#include "itkRescaleIntensityImageFilter.h"

namespace itk
{
    /** Implementation of Non-linear Anisotropic Diffusion.
     This class repeatedly calls the LinearAnisotropicDiffusionLBRImageFilter, with non-linear diffusion tensors built on the fly. These tensors are obtained by computing the image structure tensors, and appropriately modifying their eigenvalues with the method EigenValuesTransform. The latter method is not implemented, and needs to be provided in a subclass, such as CoherenceEnhancingDiffusionFilter.h
    */
    template<typename TImage, typename TScalar = typename TImage::PixelType>
    class AnisotropicDiffusionLBRImageFilter : public ImageToImageFilter< TImage, TImage>
    {
    public:
        typedef  AnisotropicDiffusionLBRImageFilter Self;
        typedef ImageToImageFilter< TImage, TImage> Superclass;
        typedef SmartPointer<Self> Pointer;
        typedef SmartPointer<const Self> ConstPointer;
        
        /// Method for creation through the object factory.
        itkNewMacro(Self);
        /// Run-time type information (and related methods).
        itkTypeMacro(AnisotropicDiffusionLBRImageFilter, Superclass);
        
        typedef TImage ImageType;
        static const unsigned int Dimension = ImageType::ImageDimension;
        
        typedef typename ImageType::PixelType PixelType;
        typedef TScalar ScalarType;
        
        typedef SymmetricSecondRankTensor<ScalarType,Dimension> TensorType;
        typedef Image<TensorType,Dimension> TensorImageType;
        
        typedef StructureTensorImageFilter<ImageType, TensorImageType> StructureTensorFilterType;
        typedef LinearAnisotropicDiffusionLBRImageFilter<ImageType, ScalarType> LinearDiffusionFilterType;
        
        
        itkSetMacro(NoiseScale, ScalarType); /// Passed to a StructureTensorImageFilter.
        itkGetConstMacro(NoiseScale, ScalarType);
        itkSetMacro(FeatureScale, ScalarType); /// Passed to a StructureTensorImageFilter.
        itkGetConstMacro(FeatureScale, ScalarType);
    
        itkSetMacro(RatioToMaxStableTimeStep, ScalarType); /// Passed to a LinearAnisotropicDiffusion Filter.
        itkGetConstMacro(RatioToMaxStableTimeStep, ScalarType);
        itkSetMacro(MaxTimeStepsBetweenTensorUpdates, int);
        itkGetConstMacro(MaxTimeStepsBetweenTensorUpdates, int);
        
        itkSetMacro(DiffusionTime, ScalarType);
        itkGetConstMacro(DiffusionTime, ScalarType);
        
        ///If true, uses unit pixel spacing, and rescales structure tensors for uni maximum trace..
        itkSetMacro(Adimensionize, bool);
        itkGetConstMacro(Adimensionize, bool);

        
        typedef typename TensorType::EigenValuesArrayType EigenValuesArrayType;
        /// Transformation of the Structure tensor eigenvalues into the diffusion tensor eigenvalues. Needs to be overloaded in a subclass. (Structure tensor eigenvalues are sorted by increasing order for convenience).
        virtual EigenValuesArrayType EigenValuesTransform(const EigenValuesArrayType &) const
        {itkExceptionMacro("Undefined tensor eigenvalues transform");};
        
        virtual typename TensorImageType::Pointer GetLastTensorImage(){return tensorImage;}
        typedef std::vector< std::pair<ScalarType, int> > EffectiveTimesAndIterationsType;
        itkGetConstReferenceMacro(LinearFilterEffectiveTimesAndIterations, EffectiveTimesAndIterationsType);
        
    protected:
        ScalarType m_NoiseScale, m_FeatureScale;
        
        ScalarType m_RatioToMaxStableTimeStep;
        int m_MaxTimeStepsBetweenTensorUpdates;
        
        AnisotropicDiffusionLBRImageFilter(){
            m_DiffusionTime=1;
            m_Adimensionize=true;
            
            m_NoiseScale=0.5;
            m_FeatureScale=2;
            
            m_RatioToMaxStableTimeStep=0.7;
            m_MaxTimeStepsBetweenTensorUpdates=5;
        };
        ~AnisotropicDiffusionLBRImageFilter(){};
        
        typename TensorImageType::Pointer tensorImage;
        virtual void ComputeDiffusionTensors(ImageType*);

        
        ScalarType m_DiffusionTime;
        bool m_Adimensionize;
        
        virtual void GenerateData();
        
        EffectiveTimesAndIterationsType m_LinearFilterEffectiveTimesAndIterations;
        
        struct DiffusionTensorFunctor;
    };
}

#include "AnisotropicDiffusionLBRImageFilter.hxx"
#endif
