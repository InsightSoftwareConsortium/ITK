#ifndef __itkGPUMeanImageFilter_h
#define __itkGPUMeanImageFilter_h

#include "pathToOpenCLSourceCode.h"
#include "itkMeanImageFilter.h"
#include "itkGPUImageToImageFilter.h"
#include "itkVersion.h"
#include "itkObjectFactoryBase.h"

namespace itk
{

/** \class GPUMeanImageFilter
 *
 * \brief GPU enabled implementation of the MeanImageFilter.
 *
 * FIXME   Won-Ki to write more documentation here...
 *
 * \ingroup ITKGPUCommon
 */
template< class TInputImage, class TOutputImage >
class ITK_EXPORT GPUMeanImageFilter: public GPUImageToImageFilter< TInputImage, TOutputImage, MeanImageFilter< TInputImage, TOutputImage > >
{
public:
  /** Standard class typedefs. */
  typedef GPUMeanImageFilter       Self;
  typedef GPUImageToImageFilter< TInputImage, TOutputImage, MeanImageFilter< TInputImage, TOutputImage > > Superclass;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;

  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GPUMeanImageFilter, Superclass);

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

protected:
  GPUMeanImageFilter();
  ~GPUMeanImageFilter();

  virtual void PrintSelf(std::ostream & os, Indent indent) const;

  virtual void GPUGenerateData();

private:
  GPUMeanImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);     //purposely not implemented

  int m_KernelHandle;
};


class GPUMeanImageFilterFactory : public itk::ObjectFactoryBase
{
public:
  typedef GPUMeanImageFilterFactory     Self;
  typedef ObjectFactoryBase             Superclass;
  typedef SmartPointer<Self>            Pointer;
  typedef SmartPointer<const Self>      ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char* GetITKSourceVersion() const { return ITK_SOURCE_VERSION; }
  const char* GetDescription() const { return "A Factory for GPUMeanImageFilter"; }

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GPUMeanImageFilterFactory, itk::ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    GPUMeanImageFilterFactory::Pointer factory = GPUMeanImageFilterFactory::New();
    itk::ObjectFactoryBase::RegisterFactory(factory);
  }

private:
  GPUMeanImageFilterFactory(const Self&);    //purposely not implemented
  void operator=(const Self&); //purposely not implemented

#define OverrideMedianFilterTypeMacro(ipt,opt,dm)\
  {\
  typedef itk::Image<ipt,dm> InputImageType;\
  typedef itk::Image<opt,dm> OutputImageType;\
  this->RegisterOverride(\
  typeid(itk::MeanImageFilter<InputImageType,OutputImageType>).name(),\
        typeid(itk::GPUMeanImageFilter<InputImageType,OutputImageType>).name(),\
        "GPU Mean Image Filter Override",\
        true,\
        itk::CreateObjectFunction<GPUMeanImageFilter<InputImageType,OutputImageType> >::New());\
  }


  GPUMeanImageFilterFactory()
    {
      //this->IfGPUISAvailable()
      //{
      OverrideMedianFilterTypeMacro(unsigned char, unsigned char, 1);
      OverrideMedianFilterTypeMacro(signed char, signed char, 1);
      OverrideMedianFilterTypeMacro(float,float,1);
      OverrideMedianFilterTypeMacro(int,int,1);
      OverrideMedianFilterTypeMacro(unsigned int,unsigned int,1);
      OverrideMedianFilterTypeMacro(double,double,1);

      OverrideMedianFilterTypeMacro(unsigned char, unsigned char, 2);
      OverrideMedianFilterTypeMacro(signed char, signed char, 2);
      OverrideMedianFilterTypeMacro(float,float,2);
      OverrideMedianFilterTypeMacro(int,int,2);
      OverrideMedianFilterTypeMacro(unsigned int,unsigned int,2);
      OverrideMedianFilterTypeMacro(double,double,2);

      OverrideMedianFilterTypeMacro(unsigned char, unsigned char, 3);
      OverrideMedianFilterTypeMacro(signed char, signed char, 3);
      OverrideMedianFilterTypeMacro(float,float,3);
      OverrideMedianFilterTypeMacro(int,int,3);
      OverrideMedianFilterTypeMacro(unsigned int,unsigned int,3);
      OverrideMedianFilterTypeMacro(double,double,3);
      //}
    }
};


} // end namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_GPUMeanImageFilter(_, EXPORT, TypeX, TypeY)                  \
  namespace itk                                                                   \
  {                                                                               \
  _( 2 ( class EXPORT GPUMeanImageFilter< ITK_TEMPLATE_2 TypeX > ) )              \
  namespace Templates                                                             \
  {                                                                               \
  typedef GPUMeanImageFilter< ITK_TEMPLATE_2 TypeX > GPUMeanImageFilter##TypeY; \
  }                                                                               \
  }

#if ITK_TEMPLATE_EXPLICIT
#include "Templates/itkGPUMeanImageFilter+-.h"
#endif

#if ITK_TEMPLATE_TXX
#include "itkGPUMeanImageFilter.hxx"
#endif

#endif
