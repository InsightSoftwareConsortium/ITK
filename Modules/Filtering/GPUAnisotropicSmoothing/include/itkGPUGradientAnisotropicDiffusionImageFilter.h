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
#ifndef itkGPUGradientAnisotropicDiffusionImageFilter_h
#define itkGPUGradientAnisotropicDiffusionImageFilter_h

#include "itkOpenCLUtil.h"
#include "itkGradientAnisotropicDiffusionImageFilter.h"
#include "itkGPUAnisotropicDiffusionImageFilter.h"
#include "itkGPUGradientNDAnisotropicDiffusionFunction.h"

namespace itk
{
/**
 * \class GPUGradientAnisotropicDiffusionImageFilter
 * This filter performs anisotropic diffusion on a scalar itk::Image using the
 * classic Perona-Malik, gradient magnitude based equation implemented in
 * itkGradientNDAnisotropicDiffusionFunction.  For detailed information on
 * anisotropic diffusion, see itkAnisotropicDiffusionFunction and
 * itkGradientNDAnisotropicDiffusionFunction.
 *
 * \par Inputs and Outputs
 * The input to this filter should be a scalar itk::Image of any
 * dimensionality.  The output image will be a diffused copy of the input.

 * \par Parameters
 * Please see the description of parameters given in
 * itkAnisotropicDiffusionImageFilter.
 *
 * \ingroup ITKGPUAnisotropicSmoothing
 */
template< typename TInputImage, typename TOutputImage, typename TParentImageFilter =
            GradientAnisotropicDiffusionImageFilter< TInputImage, TOutputImage > >
class GPUGradientAnisotropicDiffusionImageFilter :
  public GPUAnisotropicDiffusionImageFilter< TInputImage, TOutputImage, TParentImageFilter >
{
public:
  /** Standard class typedefs. */
  typedef GPUGradientAnisotropicDiffusionImageFilter                                          Self;
  typedef GPUAnisotropicDiffusionImageFilter< TInputImage, TOutputImage, TParentImageFilter > GPUSuperclass;
  typedef SmartPointer< Self >                                                                Pointer;
  typedef SmartPointer< const Self >                                                          ConstPointer;

  /** Standard method for creation through object factory. */
  itkNewMacro(Self);

  /** Run-time class information. */
  itkTypeMacro(GPUGradientAnisotropicDiffusionImageFilter,
               GPUAnisotropicDiffusionImageFilter);

  /** Extract information from the superclass. */
  typedef typename GPUSuperclass::UpdateBufferType UpdateBufferType;

  /** Extract information from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int, GPUSuperclass::ImageDimension);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( UpdateBufferHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< typename UpdateBufferType::PixelType > ) );
  // End concept checking
#endif

protected:
  GPUGradientAnisotropicDiffusionImageFilter()
  {
    // Set DiffusionFunction
    typename GPUGradientNDAnisotropicDiffusionFunction< UpdateBufferType >::Pointer p =
      GPUGradientNDAnisotropicDiffusionFunction< UpdateBufferType >::New();
    this->SetDifferenceFunction(p);
  }

  ~GPUGradientAnisotropicDiffusionImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GPUGradientAnisotropicDiffusionImageFilter);
};

/**
 * \class GPUGradientAnisotropicDiffusionImageFilterFactory
 * Object Factory implemenatation for GPUGradientAnisotropicDiffusionImageFilter
 *
 * \ingroup ITKGPUAnisotropicSmoothing
 */
class GPUGradientAnisotropicDiffusionImageFilterFactory : public ObjectFactoryBase
{
public:
  typedef GPUGradientAnisotropicDiffusionImageFilterFactory Self;
  typedef ObjectFactoryBase                                 Superclass;
  typedef SmartPointer<Self>                                Pointer;
  typedef SmartPointer<const Self>                          ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char* GetITKSourceVersion() const ITK_OVERRIDE {
    return ITK_SOURCE_VERSION;
  }
  const char* GetDescription() const ITK_OVERRIDE {
    return "A Factory for GPUGradientAnisotropicDiffusionImageFilter";
  }

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GPUGradientAnisotropicDiffusionImageFilterFactory, itk::ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    GPUGradientAnisotropicDiffusionImageFilterFactory::Pointer factory =
      GPUGradientAnisotropicDiffusionImageFilterFactory::New();

    itk::ObjectFactoryBase::RegisterFactory(factory);
  }

private:
  GPUGradientAnisotropicDiffusionImageFilterFactory(const Self&); //purposely
                                                                  // not implemented
  void operator=(const Self&);                                    //purposely
                                                                  // not implemented

#define GradientAnisotropicDiffusionImageFilterTypeMacro(ipt,opt,dm) \
    { \
    typedef itk::Image<ipt,dm> InputImageType; \
    typedef itk::Image<opt,dm> OutputImageType; \
    this->RegisterOverride( \
      typeid(itk::GradientAnisotropicDiffusionImageFilter<InputImageType,OutputImageType>).name(), \
      typeid(itk::GPUGradientAnisotropicDiffusionImageFilter<InputImageType,OutputImageType>).name(), \
      "GPU GradientAnisotropicDiffusionImageFilter Override", \
      true, \
      itk::CreateObjectFunction<GPUGradientAnisotropicDiffusionImageFilter<InputImageType,OutputImageType> >::New() ); \
    }

  GPUGradientAnisotropicDiffusionImageFilterFactory()
  {
    if( IsGPUAvailable() )
      {
      GradientAnisotropicDiffusionImageFilterTypeMacro(unsigned char, unsigned char, 1);
      GradientAnisotropicDiffusionImageFilterTypeMacro(char, char, 1);
      GradientAnisotropicDiffusionImageFilterTypeMacro(float,float,1);
      GradientAnisotropicDiffusionImageFilterTypeMacro(int,int,1);
      GradientAnisotropicDiffusionImageFilterTypeMacro(unsigned int,unsigned int,1);
      GradientAnisotropicDiffusionImageFilterTypeMacro(double,double,1);

      GradientAnisotropicDiffusionImageFilterTypeMacro(unsigned char, unsigned char, 2);
      GradientAnisotropicDiffusionImageFilterTypeMacro(char, char, 2);
      GradientAnisotropicDiffusionImageFilterTypeMacro(float,float,2);
      GradientAnisotropicDiffusionImageFilterTypeMacro(int,int,2);
      GradientAnisotropicDiffusionImageFilterTypeMacro(unsigned int,unsigned int,2);
      GradientAnisotropicDiffusionImageFilterTypeMacro(double,double,2);

      GradientAnisotropicDiffusionImageFilterTypeMacro(unsigned char, unsigned char, 3);
      GradientAnisotropicDiffusionImageFilterTypeMacro(char, char, 3);
      GradientAnisotropicDiffusionImageFilterTypeMacro(float,float,3);
      GradientAnisotropicDiffusionImageFilterTypeMacro(int,int,3);
      GradientAnisotropicDiffusionImageFilterTypeMacro(unsigned int,unsigned int,3);
      GradientAnisotropicDiffusionImageFilterTypeMacro(double,double,3);
      }
  }

};

} // end namspace itk

#endif
