//
//  LinearAnisotropicDiffusionLBRImageFilter.hxx
//  itkDiffusion
//
//  Created by Jean-Marie Mirebeau on 28/02/2014.
//
//

#ifndef itkDiffusion_LinearAnisotropicDiffusionLBRImageFilter_hxx
#define itkDiffusion_LinearAnisotropicDiffusionLBRImageFilter_hxx

#include "itkUnaryFunctorImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkCastImageFilter.h"
#include "itkExtractImageFilter.h"
#include "sys/time.h"


#include "UnaryFunctorWithIndexImageFilter.h"
#include "itkTernaryFunctorImageFilter.h"

namespace itk
{
    
    template<typename TI, typename TS>
    LinearAnisotropicDiffusionLBRImageFilter<TI, TS>::LinearAnisotropicDiffusionLBRImageFilter()
    {
        this->SetNumberOfRequiredInputs(2);
        m_DiffusionTime = 1;
        m_RatioToMaxStableTimeStep = 0.7;
        m_MaxNumberOfTimeSteps = 10;
        
        m_EffectiveDiffusionTime=0;
        m_EffectiveNumberOfTimeSteps=0;
    }
    
    template<typename TI, typename TS>
    void LinearAnisotropicDiffusionLBRImageFilter<TI, TS>::SetInputImage(const ImageType* image)
    {
        this->SetNthInput(0, const_cast<ImageType*>(image));
    }
    
    template<typename TI, typename TS>
    void LinearAnisotropicDiffusionLBRImageFilter<TI, TS>::SetInputTensor(const TensorImageType* tensorImage)
    {
        this->SetNthInput(1, const_cast<TensorImageType*>(tensorImage));
    }
    
    template<typename TI, typename TS>
    typename TI::ConstPointer LinearAnisotropicDiffusionLBRImageFilter<TI, TS>::GetInputImage()
    {
        return static_cast< const ImageType * >
        ( this->ProcessObject::GetInput(0) );
    }
    
    template<typename TI, typename TS>
    typename LinearAnisotropicDiffusionLBRImageFilter<TI, TS>::TensorImageType::ConstPointer
    LinearAnisotropicDiffusionLBRImageFilter<TI, TS>::GetInputTensor()
    {
        return static_cast< const TensorImageType * >
        ( this->ProcessObject::GetInput(1) );
    }
    
    template<typename TI, typename TS>
    void LinearAnisotropicDiffusionLBRImageFilter<TI, TS>::GenerateData()
    {
        timeval start, end;
        gettimeofday(&start, NULL); // nullptr is not accepted by older compilers
        
        GenerateStencils();
        this->UpdateProgress(0.5);
        
        gettimeofday(&end, NULL);
        m_SparseMatrixAssemblyTimeCost = (end.tv_sec-start.tv_sec)+(end.tv_usec-start.tv_usec)/1000000.;
        start=end;
        
        
        ImageUpdateLoop();
        
        gettimeofday(&end, NULL);
        m_IterationsTimeCost = (end.tv_sec-start.tv_sec)+(end.tv_usec-start.tv_usec)/1000000.;
    }
    
// **************************** Computation ***********************
    template<typename TI, typename TS>
    struct LinearAnisotropicDiffusionLBRImageFilter<TI, TS>::StencilFunctor {
        typedef typename TensorImageType::SpacingType SpacingType;
        void Initialize(RegionType region_, SpacingType spacing){
            region = region_;
            prod[0]=1;
            for(int i=1; i<Dimension; ++i)
                prod[i]=prod[i-1]*region.GetSize()[i-1];
            for(int i=0; i<Dimension; ++i)
                invSpacing[i] = ScalarType(1)/spacing[i];
        }
        
        InternalSizeT BufferIndex(const IndexType & x) const {
            IndexValueType ans=0;
            for(int i=0; i<Dimension; ++i)
                ans+=this->prod[i]*(x[i]-this->region.GetIndex()[i]);
            return ans;
        }
        
        StencilType operator()(const TensorType & tensor, const IndexType & x) const {
            StencilType stencil;
            StencilOffsetsType offsets;
            
            // Diffusion tensors are homogeneous to the inverse of norms, and are thus rescaled with an inverse spacing.

            TensorType D;
            for(int i=0; i<Dimension; ++i)
                for(int j=i; j<Dimension; ++j)
                    D(i,j)=tensor(i,j)*this->invSpacing[i]*this->invSpacing[j];
            GetDiffusion<>::Stencil(D,offsets,stencil.second);
            
            InternalSizeT * yIndex = &stencil.first[0];
            
            //Compute buffer offsets from geometrical offsets
            for(int i=0; i<(int)HalfStencilSize; ++i){
                for(int orientation = 0; orientation<2; ++orientation, ++yIndex){
                    const IndexType y = orientation ? x-offsets[i] : x+offsets[i];
                    if(this->region.IsInside(y)){
                        *yIndex = this->BufferIndex(y);
                    } else {
                        // Neumann boundary conditions.
                        *yIndex = this->OutsideBufferIndex();
                    } // if y
                } // for eps
            } // for i
            return stencil;
        }
    protected:
        RegionType region;
        IndexType prod;
        SpacingType invSpacing;
        InternalSizeT OutsideBufferIndex() const {return NumericTraits<InternalSizeT>::max();}
    };
    
    template<typename TI, typename TS>
    void LinearAnisotropicDiffusionLBRImageFilter<TI, TS>::GenerateStencils(){
        
        // Stencil type is a pair type because itk::UnaryFunctorImage filter
        // only produces one output
//        typedef typename TensorImageType::SpacingType SpacingType;
        const RegionType region = GetRequestedRegion();
        
        typedef UnaryFunctorWithIndexImageFilter<TensorImageType, StencilImageType, StencilFunctor > FunctorFilterType;
        typename FunctorFilterType::Pointer filter = FunctorFilterType::New();
        filter->SetInput(GetInputTensor());
        filter->GetFunctor().Initialize(region, GetInputTensor()->GetSpacing());
        filter->Update();
        stencilImage = filter->GetOutput();
        
        
        //setup diagonal coefficients. Cannot be parallelized due to non-local modifications of diagBuffer.
        
        diagonalCoefficients = ScalarImageType::New();
        diagonalCoefficients->CopyInformation(GetInputTensor());
        diagonalCoefficients->SetRegions(GetRequestedRegion());
        diagonalCoefficients->Allocate();
        diagonalCoefficients->FillBuffer(ScalarType(0));

        ImageRegionConstIterator<StencilImageType> stencilIt(stencilImage,region);
        ImageRegionIterator<ScalarImageType> diagIt(diagonalCoefficients, region);
        ScalarType * diagBuffer = diagonalCoefficients->GetBufferPointer();

        for(stencilIt.GoToBegin(), diagIt.GoToBegin();
            !stencilIt.IsAtEnd();
            ++stencilIt, ++diagIt)
            for(int i=0; i<(int)StencilSize; ++i) {
                const InternalSizeT yIndex = stencilIt.Value().first[i];
                if(yIndex!=OutsideBufferIndex()){
                    const ScalarType coefficient = stencilIt.Value().second[i/2];
                    diagIt.Value()      += coefficient;
                    diagBuffer[yIndex]  += coefficient;
                } // if y
            } // for i
        // for stencilIt, diagIt
    }
    
    template<typename TI, typename TS>
    typename LinearAnisotropicDiffusionLBRImageFilter<TI, TS>::ScalarType
    LinearAnisotropicDiffusionLBRImageFilter<TI, TS>::MaxStableTimeStep(){
        typedef MinimumMaximumImageCalculator<ScalarImageType> MaxCalculatorType;
        typename MaxCalculatorType::Pointer maximumCalculator = MaxCalculatorType::New();
        maximumCalculator->SetImage(diagonalCoefficients);
        maximumCalculator->SetRegion(GetRequestedRegion());
        maximumCalculator->ComputeMaximum();
        return 1./maximumCalculator->GetMaximum();
    }
    
    template<typename TI, typename TS>
    void LinearAnisotropicDiffusionLBRImageFilter<TI, TS>::SetMaxDiffusionTime(ScalarType time){
        if(time<0)
            itkExceptionMacro("diffusion time must be finite and positive");
        m_DiffusionTime = time;
    }
    
    template<typename TI, typename TS>
    void LinearAnisotropicDiffusionLBRImageFilter<TI, TS>::SetRatioToMaxStableTimeStep(ScalarType ratio){
        if(ratio<=0 || ratio>1)
            itkExceptionMacro("Ratio to max time step " << ratio << "should be within ]0,1]");
        m_RatioToMaxStableTimeStep=ratio;
    }

    template<typename TI, typename TS>
    void LinearAnisotropicDiffusionLBRImageFilter<TI, TS>::SetMaxNumberOfTimeSteps(int n){
        if(n<=0)
            itkExceptionMacro("Max number of time steps must be positive");
        m_MaxNumberOfTimeSteps=n;
    }

    template<typename TI, typename TS>
    void LinearAnisotropicDiffusionLBRImageFilter<TI, TS>::ImageUpdateLoop(){
        ScalarType delta = MaxStableTimeStep() * m_RatioToMaxStableTimeStep;
        int n = ceil(m_DiffusionTime / delta);
        if(n>m_MaxNumberOfTimeSteps) {
            n=m_MaxNumberOfTimeSteps;
            m_EffectiveDiffusionTime = n*delta;
        } else {
            delta = m_DiffusionTime/n;
            m_EffectiveDiffusionTime = m_DiffusionTime;
        }
        m_EffectiveNumberOfTimeSteps=n;
        
        // Extraction of the region of interest is required for image buffer access.
        typedef ExtractImageFilter<ImageType, ImageType> InputCasterType;
        typename InputCasterType::Pointer inputCaster = InputCasterType::New();
        inputCaster->SetInput(GetInputImage());
        inputCaster->SetExtractionRegion(GetRequestedRegion());
        inputCaster->SetDirectionCollapseToIdentity();
        inputCaster->Update();
        previousImage = inputCaster->GetOutput();
        
        nextImage = ImageType::New();
        nextImage->CopyInformation(previousImage);
        nextImage->SetRegions(previousImage->GetBufferedRegion());
        nextImage->Allocate();
        
        for(int k=0; k<n; ++k){
            ImageUpdate(delta);
            std::swap(previousImage, nextImage);
            this->UpdateProgress(0.5+0.5*k/float(n));
        }
        this->GraftOutput(previousImage);
    }
    
    template<typename TI, typename TS>
    struct LinearAnisotropicDiffusionLBRImageFilter<TI, TS>::FunctorType {
        ScalarType delta;
        PixelType operator()(PixelType output, PixelType input, ScalarType diag){
            return output*this->delta + input*(ScalarType(1)-this->delta*diag);
        }
    };
    
    template<typename TI, typename TS>
    void LinearAnisotropicDiffusionLBRImageFilter<TI, TS>::ImageUpdate(ScalarType delta){
    
    //Setting up iterators
    
    ImageRegion<Dimension> region = GetRequestedRegion();
    
    ImageRegionConstIterator<ImageType>   inputIt(previousImage,region);
    ImageRegionIterator<ImageType>        outputIt(nextImage,region);
    
    const PixelType * inputBuffer =  previousImage->GetBufferPointer();
    PixelType       * outputBuffer = nextImage->GetBufferPointer();
        
    ImageRegionConstIterator<ScalarImageType>           diagIt(diagonalCoefficients,    region);
    ImageRegionConstIterator<StencilImageType>          stencilIt(stencilImage,         region);
    
    // Rest of function is a hand-made (sparse matrix)*vector product.
    nextImage->FillBuffer(0.);
    
    // Taking care of Off-Diagonal matrix elements. Cannot be parallelized due to non-local modifications of outputBuffer
    for(inputIt.GoToBegin(), outputIt.GoToBegin(), stencilIt.GoToBegin();
        !inputIt.IsAtEnd();
        ++inputIt, ++outputIt, ++stencilIt){
        for(int i=0; i<(int)StencilSize; ++i){
        const InternalSizeT   yIndex = stencilIt.Value().first[i];
            if(yIndex!=OutsideBufferIndex()){
                const ScalarType coefficient = stencilIt.Value().second[i/2];
                outputIt.Value()        +=  coefficient * (inputBuffer[yIndex]);
                outputBuffer[yIndex]   += coefficient * inputIt.Value();
            }
        }
    }
      
    typedef TernaryFunctorImageFilter<ImageType, ImageType, ScalarImageType, ImageType, FunctorType> ImageFunctorType;
    typename ImageFunctorType::Pointer imageFunctor = ImageFunctorType::New();
    imageFunctor->SetInput1(nextImage);
    imageFunctor->SetInput2(previousImage);
    imageFunctor->SetInput3(diagonalCoefficients);
    imageFunctor->GetFunctor().delta = delta;
    
    assert(imageFunctor->CanRunInPlace());
    imageFunctor->InPlaceOn();
    imageFunctor->Update();
    nextImage=imageFunctor->GetOutput();
      
    /*
    // Old Serial version for diagonal elements
    for(inputIt.GoToBegin(), outputIt.GoToBegin(), diagIt.GoToBegin();
        !inputIt.IsAtEnd();
        ++inputIt, ++outputIt, ++diagIt)
        outputIt.Value() = delta*outputIt.Value() + (1-delta*diagIt.Value())*inputIt.Value();
    */
    }
    
// **************************** subclass SSRT_Traits call method **************************

template<typename TI, typename TS>
TS
LinearAnisotropicDiffusionLBRImageFilter<TI,TS>::ScalarProduct(const TensorType & m,
                                                               const VectorType & u,
                                                               const VectorType & v){
    ScalarType result(0);
    for(int i=0; i<Dimension; ++i)
        result+=m(i,i)*u[i]*v[i];
    for(int i=0; i<Dimension; ++i)
        for(int j=i+1; j<Dimension; ++j)
            result+=m(i,j)*(u[i]*v[j]+u[j]*v[i]);
    return result;
}
    
    
    // 2D stencil construction.
template<typename TI,typename TS> template<typename Dummy>
void LinearAnisotropicDiffusionLBRImageFilter<TI,TS>::GetDiffusion<2,Dummy>::
Stencil(const TensorType &D, StencilOffsetsType &Offsets, StencilCoefficientsType &Coefficients){
    
    // Construct a superbase, and make it obtuse with Selling's algorithm
    VectorType sb[Dimension+1]; //SuperBase
    for(int i=0; i<Dimension; ++i)
        for(int j=0; j<Dimension; ++j)
            sb[i][j]=(i==j);
    
    sb[Dimension] = -(sb[0]+sb[1]);
    const int maxIter=200;
    int iter=0;
    for(; iter<maxIter; ++iter){
        bool same=true;
        for(int i=1; i<=Dimension && same; ++i)
            for(int j=0; j<i && same; ++j)
                if( ScalarProduct(D,sb[i],sb[j]) > 0 ){
                    const VectorType u=sb[i], v=sb[j];
                    sb[0]=v-u;
                    sb[1]=u;
                    sb[2]=-v;
                    same=false;
                }
        if(same) break;
    }
    if(iter==maxIter)
        std::cerr << "Warning: Selling's algorithm not stabilized.\n";

    for(int i=0; i<3; ++i){
        Coefficients[i] = (-0.5)*ScalarProduct(D,sb[(i+1)%3],sb[(i+2)%3]);
        assert(Coefficients[i]>=0);
        
        Offsets[i][0] = static_cast<OffsetValueType>(-sb[i][1]);
        Offsets[i][1] = static_cast<OffsetValueType>( sb[i][0]);
    }
    
} // 2D stencil construction
    
    
    // 3D stencil construction
template<typename TI,typename TS> template<typename Dummy>
void LinearAnisotropicDiffusionLBRImageFilter<TI,TS>::GetDiffusion<3,Dummy>::
Stencil(const TensorType &D, StencilOffsetsType &Offsets, StencilCoefficientsType &Coefficients){
    
    // Construct a superbase, and make it obtuse with Selling's algorithm
    typedef Vector<ScalarType,Dimension> VectorType;
    VectorType sb[Dimension+1];
    for(int i=0; i<Dimension; ++i)
        for(int j=0; j<Dimension; ++j)
            sb[i][j] = (i==j);
    sb[Dimension]=-(sb[0]+sb[1]+sb[2]);
    
    const int maxIter=200;
    int iter=0;
    for(; iter<maxIter; ++iter){
        bool same=true;
        for(int i=1; i<=Dimension && same; ++i)
            for(int j=0; j<i && same; ++j)
                if( ScalarProduct(D,sb[i],sb[j]) > 0 ){
                    const VectorType u=sb[i], v=sb[j];
                    for(int k=0,l=0; k<=Dimension; ++k)
                        if(k!=i && k!=j)
                            sb[l++]=sb[k]+u;
                    sb[2]=-u;
                    sb[3]=v;
                    same=false;
                }
        if(same) break;
    }
    if(iter==maxIter)
        std::cerr << "Warning: Selling's algorithm not stabilized.\n";
    
    // Computation of the weights
    itk::SymmetricSecondRankTensor<ScalarType,Dimension+1> Weights;
    for(int i=1; i<Dimension+1; ++i)
        for(int j=0; j<i; ++j)
            Weights(i,j) = (-0.5)*ScalarProduct(D,sb[i],sb[j]);
    
    // Now that the obtuse superbasis has been created, generate the stencil.
    // First get the dual basis. Obtained by computing the comatrix of Basis[1..Dimension].
    
    for(int i=0; i<Dimension; ++i)
        for(int j=0; j<Dimension; ++j)
            Offsets[i][j] = sb[(i+1)%Dimension][(j+1)%Dimension]*sb[(i+2)%Dimension][(j+2)%Dimension]
            - sb[(i+2)%Dimension][(j+1)%Dimension]*sb[(i+1)%Dimension][(j+2)%Dimension];
    
    Offsets[Dimension  ] = Offsets[0]-Offsets[1];
    Offsets[Dimension+1] = Offsets[0]-Offsets[2];
    Offsets[Dimension+2] = Offsets[1]-Offsets[2];
    
    // The corresponding coefficients are given by the scalar products.
    for(int i=0; i<Dimension; ++i)
        Coefficients[i]=Weights(i,3);
    
    Coefficients[Dimension]   = Weights(0,1);
    Coefficients[Dimension+1] = Weights(0,2);
    Coefficients[Dimension+2] = Weights(1,2);
} // 3D stencil construction
    
}// end namespace


#endif
