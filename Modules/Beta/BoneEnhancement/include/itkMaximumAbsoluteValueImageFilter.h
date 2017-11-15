#ifndef __MaximumAbsoluteValueImageFilter_h_
#define __MaximumAbsoluteValueImageFilter_h_

#include "itkBinaryFunctorImageFilter.h"

namespace itk {
    namespace Functor {
        template<typename TInputPixel1, typename TInputPixel2, typename TOutputPixel>
        class MaximumAbsoluteValue {
        public:
            MaximumAbsoluteValue() {
            }

            ~MaximumAbsoluteValue() {
            }

            inline TOutputPixel operator()(const TInputPixel1 A, const TInputPixel2 B) {
                return static_cast<TOutputPixel>(vnl_math_abs(A) >= vnl_math_abs(B) ? A : B);
            }
        };
    } // namespace functor

    template<typename TInputImage1, typename TInputImage2, typename TOutputImage>
    class MaximumAbsoluteValueImageFilter :
            public BinaryFunctorImageFilter<TInputImage1, TInputImage2, TOutputImage,
                    Functor::MaximumAbsoluteValue<typename TInputImage1::PixelType, typename TInputImage2::PixelType,
                            typename TOutputImage::PixelType> > {
    public:
        // itk requirements
        typedef MaximumAbsoluteValueImageFilter Self;
        typedef BinaryFunctorImageFilter<TInputImage1, TInputImage2, TOutputImage,
                Functor::MaximumAbsoluteValue<typename TInputImage1::PixelType, typename TInputImage2::PixelType,
                        typename TOutputImage::PixelType> > Superclass;
        typedef SmartPointer<Self> Pointer;
        typedef SmartPointer<const Self> ConstPointer;

        itkNewMacro(Self); // create the smart pointers and register with ITKs object factory
        itkTypeMacro(MaximumAbsoluteValueImageFilter, BinaryFunctorImageFilter); // type information for runtime evaluation

    protected:
        MaximumAbsoluteValueImageFilter() {
        };

        virtual ~MaximumAbsoluteValueImageFilter() {
        };

    private:
        MaximumAbsoluteValueImageFilter(const Self &); //purposely not implemented
        void operator=(const Self &);   //purposely not implemented
    };
}

#endif //__MaximumAbsoluteValueImageFilter_h_