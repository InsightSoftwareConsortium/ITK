#ifndef __itkGradientVectorFlowImageFilter_h
#define __itkGradientVectorFlowImageFilter_h

#include "itkImageToImageFilter.h"
#include "vnl/vnl_matrix_fixed.h"
#include "vnl/vnl_math.h"
#include "itkImage.h"
#include "itkVector.h"
#include "itkLaplacianImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
//#include "itkImageRegionConstIterator.h"

namespace itk
{

/** \class GradientVectorFlowImageFilter
 * \brief 
 * This class computes a diffusion of the gradient vectors for graylevel or binary 
 * edge map derive from the image. It enlarges the capture range of the gradient
 * force and make external force derived from the gradient work effectively in the
 * framework of deformable model.
 *
 * \ingroup ImageFilters
 * \ingroup ImageSegmentation */
template <class TInputImage, class TOutputImage>
class GradientVectorFlowImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard "Self" typedef. */
  typedef GradientVectorFlowImageFilter  Self;

  /** Standard "Superclass" typedef. */
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;

  /** Smart pointer typedef support */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method of creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(GradientVectorFlowImageFilter, ImageToImageFilter);

  /** Some typedefs. */
  typedef TInputImage InputImageType;
  typedef TOutputImage OutputImageType;

  typedef typename TInputImage::IndexType IndexType;
  typedef typename TInputImage::SizeType SizeType;
  typedef typename TInputImage::PixelType PixelType;
  typedef typename OutputImageType::Pointer OutputImagePointer;
  typedef typename OutputImageType::RegionType RegionType;

  /** Image and Image iterator definition. */
//  typedef ImageRegionConstIterator<InputImageType> InputImageConstIterator;
  typedef ImageRegionIterator<InputImageType>      InputImageIterator;
  typedef ImageRegionConstIteratorWithIndex<InputImageType>  InputImageConstIterator;
  typedef ImageRegionIterator<OutputImageType>     OutputImageIterator;

  /** Image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  typedef itk::Image<double, itkGetStaticConstMacro(ImageDimension)>  InternalImageType;
  typedef typename InternalImageType::Pointer InternalImagePointer;
  typedef ImageRegionIterator<InternalImageType> InternalImageIterator;

  typedef LaplacianImageFilter<InternalImageType, InternalImageType> LaplacianFilterType;
  typedef typename LaplacianFilterType::Pointer LaplacianFilterPointer;

  /** Routines. */

  itkSetMacro(LaplacianFilter, LaplacianFilterPointer);

  itkSetMacro(TimeStep, double);
  itkGetMacro(TimeStep, double);

  itkSetMacro(NoiseLevel, double);
  itkGetMacro(NoiseLevel, double);

  itkSetMacro(IterationNum, int);
  itkGetMacro(IterationNum, int);

protected:
  GradientVectorFlowImageFilter();
  ~GradientVectorFlowImageFilter() {}
  GradientVectorFlowImageFilter(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const; 

  virtual void GenerateData();

  void InitInterImage();
  void UpdateInterImage();
  void UpdatePixels();

private:
  // parameters;
  double m_TimeStep; //the timestep of each iteration
  double m_Steps[Superclass::InputImageDimension]; // set to be 1 in all directions in most cases
  double m_NoiseLevel; // the noise level of the image
  int m_IterationNum;  // the iteration number

  LaplacianFilterPointer m_LaplacianFilter;
  typename Superclass::InputImagePointer m_IntermediateImage;
  InternalImagePointer m_InternalImages[Superclass::InputImageDimension];
  InternalImagePointer m_BImage;  // store the "b" value for every pixel
  typename Superclass::InputImagePointer m_CImage; // store the $c_i$ value for every pixel
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGradientVectorFlowImageFilter.txx"
#endif

#endif
