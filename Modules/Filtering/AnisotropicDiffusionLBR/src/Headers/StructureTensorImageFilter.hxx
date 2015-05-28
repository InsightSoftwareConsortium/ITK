//
//  StructureTensorImageFilter.hxx
//  itkDiffusion
//
//  Created by Jean-Marie Mirebeau on 21/11/2014.
//
//

#ifndef itkDiffusion_StructureTensorImageFilter_hxx
#define itkDiffusion_StructureTensorImageFilter_hxx


namespace itk {
    template<typename TI, typename TTI>
    struct StructureTensorImageFilter<TI,TTI>::OuterFunctor {
        TensorType operator()(const CovariantVectorType & u) const {
            TensorType m;
            for(int i=0; i<(int)Dimension; ++i)
                for(int j=i; j<(int)Dimension; ++j)
                    m(i,j) = u[i]*u[j];
            return m;
        }
    };
    
    
    template<typename TI,typename TTI>
    template<typename Dummy>
    struct StructureTensorImageFilter<TI,TTI>::IntermediateFilter<Dummy,true> {
        void operator()(Self *self){
            typedef GradientRecursiveGaussianImageFilter<ImageType,CovariantImageType> GradientFilterType;
            typename GradientFilterType::Pointer gradientFilter = GradientFilterType::New();
            gradientFilter->SetInput(self->GetInput());
            gradientFilter->SetSigma(self->m_NoiseScale);
            
            typedef UnaryFunctorImageFilter<CovariantImageType, TensorImageType,
            OuterFunctor> OuterFilterType;
            typename OuterFilterType::Pointer outerFilter = OuterFilterType::New();
            outerFilter->SetInput(gradientFilter->GetOutput());
            
            outerFilter->Update();
            self->intermediateResult = outerFilter->GetOutput();
        }
    };
    
    template<typename TI,typename TTI>
    template<typename Dummy>
    struct StructureTensorImageFilter<TI,TTI>::IntermediateFilter<Dummy,false> {
        void operator()(Self *self){
            
            typename ImageType::ConstPointer input = self->GetInput();
            typename TensorImageType::Pointer output = TensorImageType::New();
            output->CopyInformation(input);
            output->SetRegions(input->GetRequestedRegion());
            output->Allocate();
            output->FillBuffer(TensorType(0.));
            
            for(int index=0; index<PixelType::Dimension; ++index){
                typedef VectorIndexSelectionCastImageFilter<ImageType, ScalarImageType> SelectionFilterType;
                typename SelectionFilterType::Pointer selectionFilter = SelectionFilterType::New();
                selectionFilter->SetIndex(index);
                selectionFilter->SetInput(input);
                
                typedef RecursiveGaussianImageFilter<ScalarImageType> GaussianFilterType;
                typedef GradientImageFilter<ScalarImageType,ScalarType,ScalarType,CovariantImageType> GradientFilterType;
                typedef GradientRecursiveGaussianImageFilter<ScalarImageType,CovariantImageType> GradientGaussianFilterType;
                
                typename GaussianFilterType::Pointer gaussianFilter = GaussianFilterType::New();
                typename GradientFilterType::Pointer gradientFilter = GradientFilterType::New();
                typename GradientGaussianFilterType::Pointer gradientGaussianFilter = GradientGaussianFilterType::New();
                
                gaussianFilter->SetSigma(self->m_NoiseScale);
                gradientGaussianFilter->SetSigma(self->m_NoiseScale);
                
                typedef UnaryFunctorImageFilter<CovariantImageType, TensorImageType,
                OuterFunctor> OuterFilterType;
                typename OuterFilterType::Pointer outerFilter = OuterFilterType::New();
                
                if(self->m_UseGradientRecursiveGaussianImageFilter){
                    gradientGaussianFilter->SetInput(selectionFilter->GetOutput());
                    outerFilter->SetInput(gradientGaussianFilter->GetOutput());
                } else {
                    gaussianFilter->SetInput(selectionFilter->GetOutput());
                    gradientFilter->SetInput(gaussianFilter->GetOutput());
                    outerFilter->SetInput(gradientFilter->GetOutput());
                }
                
                typedef AddImageFilter<TensorImageType> AddFilterType;
                typename AddFilterType::Pointer addFilter = AddFilterType::New();
                addFilter->InPlaceOn();
                addFilter->SetInput1(output);
                addFilter->SetInput2(outerFilter->GetOutput());
                addFilter->Update();
                output = addFilter->GetOutput();
                
                self->UpdateProgress(index/float(PixelType::Dimension+1));
            }
            self->intermediateResult = output;
        }
    };
    
    
    template<typename TI, typename TTI>
    struct StructureTensorImageFilter<TI,TTI>::TraceFunctor {
        ScalarType operator()(const TensorType & t) const {
            return t.GetTrace();
        }
    };
    
    template<typename TI, typename TTI>
    struct StructureTensorImageFilter<TI,TTI>::ScaleFunctor {
        ScalarType scaling;
        TensorType operator()(const TensorType & t) const {
            return t*scaling;
        }
    };
    
    template<typename TI, typename TTI>
    void StructureTensorImageFilter<TI,TTI>::GenerateData(){
        IntermediateFilter<>()(this);
        
        typedef RecursiveGaussianImageFilter<TensorImageType> GaussianFilterType;
        typename GaussianFilterType::Pointer gaussianFilter = GaussianFilterType::New();
        gaussianFilter->SetInput(intermediateResult);
        gaussianFilter->SetSigma(m_FeatureScale);
        
        if(!m_RescaleForUnitMaximumTrace){
            m_PostRescaling=1.;
            gaussianFilter->Update();
            this->GraftOutput(gaussianFilter->GetOutput());
            return;
        }
        
        // *** Rescaling for normalization of largest trace ***
        
        typedef UnaryFunctorImageFilter<TensorImageType, ScalarImageType, TraceFunctor> TraceFilterType;
        typename TraceFilterType::Pointer traceFilter = TraceFilterType::New();
        traceFilter->SetInput(gaussianFilter->GetOutput());
        
        typedef MinimumMaximumImageCalculator<ScalarImageType> MaximumCalculatorType;
        typename MaximumCalculatorType::Pointer maximumCalculator = MaximumCalculatorType::New();
        maximumCalculator->SetImage(traceFilter->GetOutput());
        
        typedef UnaryFunctorImageFilter<TensorImageType, TensorImageType, ScaleFunctor> ScaleFilterType;
        typename ScaleFilterType::Pointer scaleFilter = ScaleFilterType::New();
        scaleFilter->SetInput(gaussianFilter->GetOutput());
        
        traceFilter->Update();
        maximumCalculator->ComputeMaximum();
        m_PostRescaling = 1./maximumCalculator->GetMaximum();
        scaleFilter->GetFunctor().scaling = m_PostRescaling;
        scaleFilter->Update();
        this->GraftOutput(scaleFilter->GetOutput());
    }
}


#endif
