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
#ifndef itkGPUMeanImageFilter_h
#define itkGPUMeanImageFilter_h

#include "itkMeanImageFilter.h"
#include "itkGPUBoxImageFilter.h"
#include "itkVersion.h"
#include "itkObjectFactoryBase.h"
#include "itkOpenCLUtil.h"

namespace itk
{
/** \class GPUMeanImageFilter
 *
 * \brief GPU-enabled implementation of the MeanImageFilter.
 *
 * Current GPU mean filter reads in neighborhood pixels from global memory.
 *
 * \ingroup ITKGPUSmoothing
 */

/** Create a helper GPU Kernel class for GPUMeanImageFilter */
itkGPUKernelClassMacro(GPUMeanImageFilterKernel);

template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT GPUMeanImageFilter : //public GPUImageToImageFilter<
                                      // TInputImage, TOutputImage,
                                      // MeanImageFilter< TInputImage,
                                      // TOutputImage > >
  public GPUBoxImageFilter< TInputImage, TOutputImage, MeanImageFilter< TInputImage, TOutputImage > >
{
public:
  /** Standard class typedefs. */
  typedef GPUMeanImageFilter                                                                           Self;
  typedef GPUBoxImageFilter< TInputImage, TOutputImage, MeanImageFilter< TInputImage, TOutputImage > > Superclass;
  typedef SmartPointer< Self >                                                                         Pointer;
  typedef SmartPointer< const Self >                                                                   ConstPointer;

  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GPUMeanImageFilter, GPUBoxImageFilter);

  /** Superclass typedefs. */
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;
  typedef typename Superclass::OutputImagePixelType  OutputImagePixelType;

  /** Some convenient typedefs. */
  typedef TInputImage                           InputImageType;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename InputImageType::PixelType    InputImagePixelType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Get OpenCL Kernel source as a string, creates a GetOpenCLSource method */
  itkGetOpenCLSourceFromKernelMacro(GPUMeanImageFilterKernel);

protected:
  GPUMeanImageFilter();
  ~GPUMeanImageFilter() ITK_OVERRIDE;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void GPUGenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GPUMeanImageFilter);

  int m_MeanFilterGPUKernelHandle;
};

/** \class GPUMeanImageFilterFactory
 *
 * \brief Object Factory implemenatation for GPUMeanImageFilter
 * \ingroup ITKGPUSmoothing
 */
class GPUMeanImageFilterFactory : public ObjectFactoryBase
{
public:
  typedef GPUMeanImageFilterFactory Self;
  typedef ObjectFactoryBase         Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char* GetITKSourceVersion() const ITK_OVERRIDE
    {
    return ITK_SOURCE_VERSION;
    }
  const char* GetDescription() const ITK_OVERRIDE
    {
    return "A Factory for GPUMeanImageFilter";
    }

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GPUMeanImageFilterFactory, itk::ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    GPUMeanImageFilterFactory::Pointer factory = GPUMeanImageFilterFactory::New();

    ObjectFactoryBase::RegisterFactory(factory);
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GPUMeanImageFilterFactory);

#define OverrideMeanFilterTypeMacro(ipt,opt,dm) \
    { \
    typedef Image<ipt,dm> InputImageType; \
    typedef Image<opt,dm> OutputImageType; \
    this->RegisterOverride( \
      typeid(MeanImageFilter<InputImageType,OutputImageType>).name(), \
      typeid(GPUMeanImageFilter<InputImageType,OutputImageType>).name(), \
      "GPU Mean Image Filter Override", \
      true, \
      CreateObjectFunction<GPUMeanImageFilter<InputImageType,OutputImageType> >::New() ); \
    }

  GPUMeanImageFilterFactory()
  {
    if( IsGPUAvailable() )
      {
      OverrideMeanFilterTypeMacro(unsigned char, unsigned char, 1);
      OverrideMeanFilterTypeMacro(char, char, 1);
      OverrideMeanFilterTypeMacro(float,float,1);
      OverrideMeanFilterTypeMacro(int,int,1);
      OverrideMeanFilterTypeMacro(unsigned int,unsigned int,1);
      OverrideMeanFilterTypeMacro(double,double,1);

      OverrideMeanFilterTypeMacro(unsigned char, unsigned char, 2);
      OverrideMeanFilterTypeMacro(char, char, 2);
      OverrideMeanFilterTypeMacro(float,float,2);
      OverrideMeanFilterTypeMacro(int,int,2);
      OverrideMeanFilterTypeMacro(unsigned int,unsigned int,2);
      OverrideMeanFilterTypeMacro(double,double,2);

      OverrideMeanFilterTypeMacro(unsigned char, unsigned char, 3);
      OverrideMeanFilterTypeMacro(char, char, 3);
      OverrideMeanFilterTypeMacro(float,float,3);
      OverrideMeanFilterTypeMacro(int,int,3);
      OverrideMeanFilterTypeMacro(unsigned int,unsigned int,3);
      OverrideMeanFilterTypeMacro(double,double,3);
      }
  }

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGPUMeanImageFilter.hxx"
#endif

#endif
