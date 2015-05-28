//
//  CoherenceEnhancingDiffusionFilter.h
//  itkDiffusion
//
//  Created by Jean-Marie Mirebeau on 06/03/2014.
//
//

#ifndef itkDiffusion_CoherenceEnhancingDiffusionFilter_h
#define itkDiffusion_CoherenceEnhancingDiffusionFilter_h

#include "AnisotropicDiffusionLBRImageFilter.h"


namespace itk {
/**
 Implementation of Coherence Enhancing Diffusion (CED), and Edge Enhancing Diffusion (EED), as described by Weickert. CED heuristically smoothes everywhere except accross image contours, while EED smoothes nowhere but tangentially to image contours.
 
 The non-linear diffusion tensor is defined in terms of the structure tensor.
 Denote by \f$\mu_i\f$ the structure tensor eigenvalues, at a given point \f$x\f$, with \f$0\leq i < d\f$. Let also \f$\mu_{\rm min}\f$ and \f$\mu_{\rm max}\f$, be the smallest and largest eigenvalues respectively. The diffusion tensor is defined by the same eigenvectors, but with modified with eigenvalues \f$\lambda_i\f$.
 
 Coherence Enhancing Diffusion : \f$\lambda_i := g(\mu_i - \mu_{\rm min})\f$, where \f$g(s) = 1 - (1-\alpha)*exp(-(\lambda/s)^m)\f$. Note the limit values \f$g(0) = 1\f$, \f$g(\infty) = \alpha\f$.\br

 Edge enhancing diffusion \f$\lambda_i := g(\mu_{\rm max} - \mu_i)\f$, where  \f$g(s) = \alpha + (1-\alpha)*exp(-(\lambda/s)^m)\f$. Note the limit values \f$g(0) = \alpha\f$, \f$g(\infty) = 1\f$.
 */
    template<typename TImage,typename TScalar=typename TImage::PixelType>
    class CoherenceEnhancingDiffusionFilter : public AnisotropicDiffusionLBRImageFilter<TImage,TScalar> {
    public:
        typedef CoherenceEnhancingDiffusionFilter Self;
        typedef AnisotropicDiffusionLBRImageFilter<TImage,TScalar> Superclass;
        typedef SmartPointer<Self> Pointer;
        typedef SmartPointer<const Self> ConstPointer;
        
        /// Method for creation through the object factory.
        itkNewMacro(Self);
        /// Run-time type information (and related methods).
        itkTypeMacro(CoherenceEnhancingDiffusionFilter, Superclass);
        
        typedef typename Superclass::EigenValuesArrayType EigenValuesArrayType;
        virtual EigenValuesArrayType EigenValuesTransform(const EigenValuesArrayType &) const;
        
        typedef typename Superclass::ScalarType ScalarType;
        /// Exponent m involved in the function g defining eigenvalues.
        itkSetMacro(Exponent, ScalarType);
        itkSetMacro(Lambda, ScalarType);
        itkSetMacro(Alpha, ScalarType);
        
        itkGetMacro(Exponent, ScalarType);
        itkGetMacro(Lambda, ScalarType);
        itkGetMacro(Alpha, ScalarType);
        
        enum EnhancementType {CED, cCED, EED, cEED, Isotropic};
        /// Switch between CED, EED, and variants.
        itkSetEnumMacro(Enhancement, EnhancementType);
        itkGetEnumMacro(Enhancement, EnhancementType);
        
    protected:
        ScalarType m_Lambda;
        ScalarType m_Exponent;
        ScalarType m_Alpha;
        EnhancementType m_Enhancement;
        
        ScalarType g_CED(ScalarType s) const {return s<=0 ? m_Alpha : m_Alpha + (1-m_Alpha)*exp(-pow(m_Lambda/s,m_Exponent));}
        ScalarType g_EED(ScalarType s) const {return s<=0 ? 1 : 1 - (1-m_Alpha)*exp(-pow(m_Lambda/s,m_Exponent));}
        
        CoherenceEnhancingDiffusionFilter(){
            m_Lambda = 0.05;
            m_Exponent = 2;
            m_Alpha = 0.01;
            m_Enhancement = CED;
        }
    };
    
    template<typename TI,typename TS>
    typename CoherenceEnhancingDiffusionFilter<TI,TS>::EigenValuesArrayType
    CoherenceEnhancingDiffusionFilter<TI,TS>::EigenValuesTransform(const EigenValuesArrayType & ev0) const {
        static const int Dimension = Superclass::Dimension;
        const ScalarType evMin = ev0[0], evMax = ev0[Dimension-1];
        EigenValuesArrayType ev;
        switch(m_Enhancement){
                
                // Weickert's filter.
            case CED:
                for(int i=0; i<Dimension; ++i)
                    ev[i] = g_CED(evMax-ev0[i]);
                break;
                
                // A variance, requiring stronger coherence.
            case cCED:
                for(int i=0; i<Dimension; ++i)
                    ev[i] = g_CED( (evMax-ev0[i])/(1.+ev0[i]/m_Lambda) );
                break;

                // Weickert's filter.
            case EED:
                for(int i=0; i<Dimension; ++i)
                    ev[i] = g_EED(ev0[i]-evMin);
                break;
                
                // A variant, promoting diffusion in at least one direction at each point.
            case cEED:
                for(int i=0; i<Dimension; ++i)
                    ev[i] = g_EED(ev0[i]);
                break;

                // Isotropic tensors, closely related to Perona-Malik's approach.
            case Isotropic:
                for (int i=0; i<Dimension; ++i)
                    ev[i] = g_EED(evMax);
                break;

            default:
                itkExceptionMacro("Unsupported diffusion type");
        }
        return ev;
    }

    
}
#endif
