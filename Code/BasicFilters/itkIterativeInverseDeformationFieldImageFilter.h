#ifndef __itkIterativeInverseDeformationFieldImageFilter_h
#define __itkIterativeInverseDeformationFieldImageFilter_h


#include "itkImageToImageFilter.h"

#include "itkWarpVectorImageFilter.h"
#include "itkVectorLinearInterpolateImageFunction.h"
#include "itkImageRegionIterator.h"
#include "itkTimeProbe.h"


namespace itk{
  
  template < class TInputImage, class TOutputImage >
  class ITK_EXPORT IterativeInverseDeformationFieldImageFilter :
  public ImageToImageFilter<TInputImage,TOutputImage> 
  {
  public:
    /** Standard class typedefs. */
    typedef IterativeInverseDeformationFieldImageFilter  Self;
    typedef ImageToImageFilter<TInputImage,TOutputImage>   Superclass;
    typedef SmartPointer<Self>        Pointer;
    typedef SmartPointer<const Self>  ConstPointer;

    /** Method for creation through the object factory. */
    itkNewMacro(Self);

    /** Run-time type information (and related methods). */
    itkTypeMacro(IterativeInverseDeformationFieldImageFilter, ImageToImageFilter);

    /** Some typedefs. */
    typedef TInputImage InputImageType;
    typedef typename InputImageType::ConstPointer    InputImageConstPointer;
    typedef typename InputImageType::Pointer         InputImagePointer;
    typedef typename InputImageType::PointType       InputImagePointType; 
    typedef typename InputImageType::RegionType      InputImageRegionType; 
    typedef typename InputImageType::SpacingType     InputImageSpacingType; 
    typedef TOutputImage OutputImageType;
    typedef typename OutputImageType::Pointer        OutputImagePointer;
    typedef typename OutputImageType::PixelType      OutputImagePixelType;
    typedef typename OutputImageType::PointType      OutputImagePointType;
    typedef typename OutputImageType::IndexType      OutputImageIndexType;
    typedef typename OutputImagePixelType::ValueType OutputImageValueType;

    typedef TimeProbe TimeType;

    typedef ImageRegionConstIterator<InputImageType> InputConstIterator;
    typedef ImageRegionIterator<InputImageType> InputIterator;
    typedef ImageRegionIterator<OutputImageType> OutputIterator;

    typedef WarpVectorImageFilter<TOutputImage,TInputImage,TOutputImage> VectorWarperType;

    typedef VectorLinearInterpolateImageFunction<TInputImage,double> FieldInterpolatorType;
    typedef typename FieldInterpolatorType::Pointer     FieldInterpolatorPointer;
    typedef typename FieldInterpolatorType::OutputType  FieldInterpolatorOutputType;

    /** Functions. */
    void SetInput(const InputImageType *input);

    itkSetMacro(NumberOfIterations, unsigned int);
    itkGetMacro(NumberOfIterations, unsigned int);

    // If the error (in mm) between forward and backward mapping is smaller than the StopValue,
    // the algorithm stops.
    // This value can be used to speed up the calculation.
    itkSetMacro(StopValue, double);
    itkGetMacro(StopValue, double);

    char* GetReport() {return m_Report;}

  protected:
    IterativeInverseDeformationFieldImageFilter();
    ~IterativeInverseDeformationFieldImageFilter() {}

    void PrintSelf(std::ostream& os, Indent indent) const;
    void MakeReport();

    void GenerateData( );

    unsigned int m_NumberOfIterations;
    double m_StopValue;
    double m_Time;

  private:
    IterativeInverseDeformationFieldImageFilter(const Self&); //purposely not implemented
    void operator=(const Self&); //purposely not implemented
  };

} // end namespace itk

#include "itkIterativeInverseDeformationFieldImageFilter.txx"

#endif
