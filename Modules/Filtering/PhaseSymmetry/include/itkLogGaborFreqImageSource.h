/*=========================================================================


=========================================================================*/
#ifndef __itkLogGaborFreqImageSource_h
#define __itkLogGaborFreqImageSource_h

#include "itkImageSource.h"
#include "itkFixedArray.h"
#include "itkSize.h"
#include "itkArray2D.h"

#include <vector>
#include <complex>


namespace itk
{

/** \class LogGaborFreqImageSource
 *
 * \ingroup PhaseSymmetry
 */
template <typename TOutputImage>
class LogGaborFreqImageSource : public ImageSource<TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef LogGaborFreqImageSource   Self;
  typedef ImageSource<TOutputImage> Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Typedef for the output image PixelType. */
  typedef typename TOutputImage::PixelType OutputImagePixelType;

  /** Typedef to describe the output image region type. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** Spacing typedef support.  Spacing holds the size of a pixel.  The
   * spacing is the geometric distance between image samples. */
  typedef typename TOutputImage::SpacingType SpacingType;

  /** Origin typedef support.  The origin is the geometric coordinates
   * of the index (0,0). */
  typedef typename TOutputImage::PointType PointType;

  /** Direction typedef support.  The direction is the direction
   * cosines of the image. */
  typedef typename TOutputImage::DirectionType DirectionType;

  typedef std::vector<std::vector<double>> RangeType;

  /** Dimensionality of the output image */
  itkStaticConstMacro(NDimensions, unsigned int, TOutputImage::ImageDimension);

  /** Type used to store gaussian parameters. */


  // Type used to store the range for each axis

  /** Size type matches that used for images */
  typedef typename TOutputImage::SizeType      SizeType;
  typedef typename TOutputImage::SizeValueType SizeValueType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(LogGaborFreqImageSource, ImageSource);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Specify the size of the output image. */
  virtual void
  SetSize(const SizeValueType * values);

  /** Specify the size of the output image. */
  virtual void
  SetSize(const SizeType values);

  /** Get the size of the output image. */
  itkGetVectorMacro(Size, const SizeValueType, NDimensions);

  /** Specify the spacing of the output image. */
  itkSetMacro(Spacing, SpacingType);
  virtual void
  SetSpacing(const float * values);
  virtual void
  SetSpacing(const double * values);

  /** Get the spacing of the output image. */
  itkGetConstReferenceMacro(Spacing, SpacingType);

  /** Specify the origin of the output image. */
  itkSetMacro(Origin, PointType);
  virtual void
  SetOrigin(const float * values);
  virtual void
  SetOrigin(const double * values);

  /** Get the origin of the output image. */
  itkGetConstReferenceMacro(Origin, PointType);

  /** Specify the direction of the output image. */
  itkSetMacro(Direction, DirectionType);

  itkGetConstReferenceMacro(Direction, DirectionType);

  // itkSetMacro(Ranges, RangeType);

  typedef FixedArray<double, itkGetStaticConstMacro(NDimensions)> DoubleArrayType;


  /** Gets and sets for Log Gabor parameters */
  itkSetMacro(Sigma, double);
  itkGetConstReferenceMacro(Sigma, double);

  itkSetMacro(Wavelengths, DoubleArrayType);
  itkGetConstReferenceMacro(Wavelengths, DoubleArrayType);

protected:
  LogGaborFreqImageSource();
  ~LogGaborFreqImageSource();
  void
  PrintSelf(std::ostream & os, Indent indent) const;
  void
  ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType tid);
  virtual void
  GenerateOutputInformation();

private:
  LogGaborFreqImageSource(const LogGaborFreqImageSource &); // purposely not implemented
  void
  operator=(const LogGaborFreqImageSource &); // purposely not implemented

  SizeValueType m_Size[NDimensions]; // size of the output image
  SpacingType   m_Spacing;           // spacing
  PointType     m_Origin;            // origin
  DirectionType m_Direction;         // direction
  RangeType     m_Ranges;

  // Ratio of k/wo in each direction
  double m_Sigma;

  // The wavelengths in each direction
  DoubleArrayType m_Wavelengths;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLogGaborFreqImageSource.hxx"
#endif

#endif
