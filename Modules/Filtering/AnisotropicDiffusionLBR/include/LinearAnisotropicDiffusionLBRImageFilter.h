//
//  LinearAnisotropicDiffusionLBRImageFilter.h
//  itkDiffusion
//
//  Created by Jean-Marie Mirebeau on 28/02/2014.
//
//

#ifndef itkDiffusion_LinearAnisotropicDiffusionLBRImageFilter_h
#define itkDiffusion_LinearAnisotropicDiffusionLBRImageFilter_h


#include "itkImageToImageFilter.h"

namespace itk
{
    /**
     Implementation of Anisotropic Diffusion
     \f[\partial_t u = {\rm div} (D \nabla u),\f]
     with Neumann boundary conditions. The numerical scheme is stable and satisfies the maximum principle, even for strongly anisotropic tensors, thanks to an adaptive discretization using arithmetic techniques (Lattice Basis Reduction, LBR).
     */
    template<
    typename TImage,
    typename TScalar = typename TImage::PixelType
    >
    class LinearAnisotropicDiffusionLBRImageFilter : public ImageToImageFilter< TImage, TImage >
    {
    public:
        /** Standard class typedefs. */
        typedef LinearAnisotropicDiffusionLBRImageFilter    Self;
        typedef ImageToImageFilter< TImage, TImage >        Superclass;
        typedef SmartPointer< Self >                        Pointer;
        
        /** Method for creation through the object factory. */
        itkNewMacro(Self);
        
        /** Run-time type information (and related methods). */
        itkTypeMacro(LinearAnisotropicDiffusionLBRImageFilter, ImageToImageFilter);
        
        typedef TImage ImageType;
        static const int Dimension = ImageType::ImageDimension;
        typedef typename ImageType::PixelType PixelType;
        
        typedef TScalar ScalarType;
        typedef SymmetricSecondRankTensor<ScalarType,Dimension> TensorType;
        typedef Image<TensorType,Dimension> TensorImageType;
        typedef ImageRegion<Dimension> RegionType;
        
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
        
        itkGetConstMacro(SparseMatrixAssemblyTimeCost, ScalarType);
        itkGetConstMacro(IterationsTimeCost, ScalarType);
    protected:
        LinearAnisotropicDiffusionLBRImageFilter();
        ~LinearAnisotropicDiffusionLBRImageFilter(){}
        
        typename ImageType::ConstPointer GetInputImage();
        typename TensorImageType::ConstPointer GetInputTensor();
        
        typedef Index<Dimension> IndexType;

        // ******* Containers for the stencils used in the discretization
        static const unsigned int HalfStencilSize = (Dimension == 2) ? 3 : 6;
        static const unsigned int StencilSize = 2*HalfStencilSize;
        
        typedef Vector<ScalarType,HalfStencilSize> StencilCoefficientsType;
        typedef Offset<Dimension> OffsetType;
        typedef Vector<OffsetType, HalfStencilSize> StencilOffsetsType;
        
        typedef int InternalSizeT;
        typedef Vector<InternalSizeT,StencilSize> StencilBufferIndicesType;
        
        // ****************** Stencil support ******************
        
        template<unsigned int NDimension=Dimension, typename Dummy=void> struct GetDiffusion;
        
        template<typename Dummy> struct GetDiffusion<2,Dummy>{
            static void Stencil(const TensorType &, StencilOffsetsType &, StencilCoefficientsType &);
        };
        
        template<typename Dummy> struct GetDiffusion<3,Dummy>{
            static void Stencil(const TensorType &, StencilOffsetsType &, StencilCoefficientsType &);
        };
                
        // *************** Computation *****************
        virtual void GenerateData();
        virtual void GenerateStencils(); /// Automatically called by GenerateData
        virtual void ImageUpdateLoop(); /// Automatically called by GenerateData
        
        typedef std::pair<StencilBufferIndicesType, StencilCoefficientsType> StencilType;
        typedef Image<StencilType,Dimension> StencilImageType;
        typename StencilImageType::Pointer stencilImage;
        
        typedef Image<ScalarType,Dimension> ScalarImageType;
        typename ScalarImageType::Pointer diagonalCoefficients;
        
        virtual ScalarType MaxStableTimeStep();
        
        ScalarType m_DiffusionTime;
        ScalarType m_RatioToMaxStableTimeStep;
        int m_MaxNumberOfTimeSteps;
        
        ScalarType m_EffectiveDiffusionTime;
        int m_EffectiveNumberOfTimeSteps;

        virtual void ImageUpdate(ScalarType delta);
        typename ImageType::Pointer previousImage, nextImage;
        
        virtual RegionType GetRequestedRegion(){return GetInputImage()->GetRequestedRegion();}
        
        InternalSizeT OutsideBufferIndex() const {return NumericTraits<InternalSizeT>::max();}
        
        ScalarType m_SparseMatrixAssemblyTimeCost, m_IterationsTimeCost;
        
        
        struct StencilFunctor;
        struct FunctorType;
        
        typedef Vector<ScalarType,Dimension> VectorType;
        static ScalarType ScalarProduct(const TensorType &, const VectorType &, const VectorType &);
    private:
        LinearAnisotropicDiffusionLBRImageFilter(const Self &); //purposely not implemented
        void operator=(const Self &);  //purposely not implemented
    };
} //namespace ITK


#ifndef ITK_MANUAL_INSTANTIATION
#include "LinearAnisotropicDiffusionLBRImageFilter.hxx"
#endif

#endif
