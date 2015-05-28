//
//  UnaryFunctorWithIndexImageFilter.h
//  itkDiffusion
//
//  Created by Jean-Marie Mirebeau on 07/03/2014.
//
//

#ifndef itkDiffusion_UnaryFunctorWithIndexImageFilter_h
#define itkDiffusion_UnaryFunctorWithIndexImageFilter_h

#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageScanlineIterator.h"
#include "Macro.h"
/** A (simplification of) UnaryFunctorImageFilter, which provides the pixel index to the functor.
 */

namespace itk {
    template<typename TInputImage, typename TOutputImage, typename TFunctor>
    class UnaryFunctorWithIndexImageFilter : public ImageToImageFilter<TInputImage, TOutputImage> {
    public:
        typedef UnaryFunctorWithIndexImageFilter Self;
        typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
        typedef SmartPointer<Self> Pointer;
        typedef SmartPointer<const Self> ConstPointer;
        
        /// Method for creation through the object factory.
        itkNewMacro(Self);
        /// Run-time type information (and related methods).
        itkTypeMacro(UnaryFunctorWithIndexImageFilter, Superclass);
        
        typedef TInputImage     InputImageType;
        typedef TOutputImage    OutputImageType;
        typedef TFunctor FunctorType;
        
        GetSetFunctorMacro(Functor, FunctorType);
        
    protected:
        UnaryFunctorWithIndexImageFilter(){};

        FunctorType m_Functor;
        
        typedef typename InputImageType::RegionType     InputRegionType;
        typedef typename OutputImageType::RegionType    OutputRegionType;

        virtual void ThreadedGenerateData(const OutputRegionType & region, ThreadIdType){
            if(region.GetSize()[0]==0) return;
            
            ImageRegionConstIteratorWithIndex<InputImageType>   inputIt(    this->GetInput(),   region);
            ImageScanlineIterator< OutputImageType >            outputIt(   this->GetOutput(),  region);

            for(inputIt.GoToBegin(), outputIt.GoToBegin();
                !outputIt.IsAtEnd();
                ){
                while(!outputIt.IsAtEndOfLine()){
                    outputIt.Set(m_Functor(inputIt.Value(), inputIt.GetIndex()));
                    ++inputIt;
                    ++outputIt;
                }
                outputIt.NextLine();
            }
        }
    };
}

#endif
