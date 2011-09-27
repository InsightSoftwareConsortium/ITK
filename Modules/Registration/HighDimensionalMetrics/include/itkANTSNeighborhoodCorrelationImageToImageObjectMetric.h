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
#ifndef __itkANTSNeighborhoodCorrelationImageToImageObjectMetric_h
#define __itkANTSNeighborhoodCorrelationImageToImageObjectMetric_h

#include "itkImageToImageObjectMetric.h"
#include "itkConstNeighborhoodIterator.h"

#include <deque>

namespace itk {

/** \class ANTSNeighborhoodCorrelationImageToImageObjectMetric
 *
 * \brief Computes normalized cross correlation using a small neighborhood
 * for each voxel between two images, with speed optimizations for dense
 * registration.
 *
 * Please cite this reference for more details:
 *
 * Brian B. Avants, Nicholas J. Tustison, Gang Song, Philip A. Cook,
 * Arno Klein, James C. Gee, A reproducible evaluation of ANTs similarity metric
 * performance in brain image registration, NeuroImage, Volume 54, Issue 3,
 * 1 February 2011, Pages 2033-2044, ISSN 1053-8119,
 * DOI: 10.1016/j.neuroimage.2010.09.025.
 *
 * Around each voxel, the neighborhood is defined as a N-Dimensional
 * rectangle centered at the voxel. The size of the rectangle is 2*radius+1.
 * The normalized correlation between neighborhoods of fixed image and moving
 * image are averaged over the whole image as the final metric.
 *
 * This class uses a specific fast implementation that is described in the
 * above paper. There are two particular speed-ups:
 *
 * 1) It is assumed that the derivative is only affected by changes in the
 * transform at the center of the window. This is obviously not true but speeds
 * the evaluation up considerably and works well in practice. This assumption
 * is the main differentiattion of this approach from a more generic one.
 *
 * 2) The evaluation uses on-the-fly queues with multi-threading and a sliding
 * neighborhood window. This is described in the above paper and specifically
 * optimized for dense registration.
 *
 *  Example of usage:
 *
 *  typedef itk::ANTSNeighborhoodCorrelationImageToImageObjectMetric
 *    <ImageType, ImageType> MetricType;
 *  typedef MetricType::Pointer MetricTypePointer;
 *  MetricTypePointer metric = MetricType::New();
 *
 *  // set all parameters
 *  Size<Dimension> neighborhoodRadius;
 *  neighborhoodRadius.Fill(2);
 *  metric->SetRadius(neighborhood_radius);
 *  metric->SetFixedImage(fixedImage);
 *  metric->SetMovingImage(movingImage);
 *  metric->SetFixedTransform(transformFix);
 *  metric->SetMovingTransform(transformMov);
 *
 *  // initialization after parameters are set.
 *  metric->Initialize();
 *
 *  // getting derivative and metric value
 *  metric->GetValueAndDerivative(valueReturn, derivativeReturn);
 *
 *
 * This Class is templated over the type of the two input objects.
 * This is the base class for a hierarchy of similarity metrics that may, in
 * derived classes, operate on meshes, images, etc.  This class computes a
 * value that measures the similarity between the two objects.
 *
 * \ingroup ITKHighDimensionalMetrics
 */
template<class TFixedImage,
         class TMovingImage,
         class TVirtualImage = TFixedImage>
class ITK_EXPORT ANTSNeighborhoodCorrelationImageToImageObjectMetric :
  public ImageToImageObjectMetric< TFixedImage, TMovingImage, TVirtualImage>
{
public:

  /** Standard class typedefs. */
  typedef ANTSNeighborhoodCorrelationImageToImageObjectMetric Self;
  typedef ImageToImageObjectMetric<TFixedImage, TMovingImage, TVirtualImage>
                                                              Superclass;
  typedef SmartPointer<Self>                                  Pointer;
  typedef SmartPointer<const Self>                            ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Self, Superclass);

  /** superclass types */
  typedef typename Superclass::MeasureType                    MeasureType;
  typedef typename Superclass::DerivativeType                 DerivativeType;
  typedef typename Superclass::VirtualPointType               VirtualPointType;
  typedef typename Superclass::FixedImagePointType            FixedImagePointType;
  typedef typename Superclass::FixedImagePixelType            FixedImagePixelType;
  typedef typename Superclass::FixedTransformType             FixedTransformType;
  typedef typename Superclass::FixedImageGradientType         FixedImageGradientType;
  typedef typename FixedTransformType::JacobianType           FixedImageJacobianType;

  typedef typename Superclass::MovingImagePointType           MovingImagePointType;
  typedef typename Superclass::MovingImagePixelType           MovingImagePixelType;
  typedef typename Superclass::MovingImageGradientType        MovingImageGradientType;
  typedef typename Superclass::MovingTransformType            MovingTransformType;
  typedef typename MovingTransformType::JacobianType          MovingImageJacobianType;
  typedef typename Superclass::JacobianType                   JacobianType;

  typedef typename Superclass::VirtualImageGradientType       VirtualImageGradientType;

  typedef typename Superclass::FixedImageType                 FixedImageType;
  typedef typename Superclass::MovingImageType                MovingImageType;
  typedef typename Superclass::VirtualImageType               VirtualImageType;
  typedef typename Superclass::DenseThreaderDomainType        DenseThreaderDomainType;
  typedef typename Superclass::FixedOutputPointType           FixedOutputPointType;
  typedef typename Superclass::MovingOutputPointType          MovingOutputPointType;

  typedef double InternalComputationValueType;

  typedef typename Superclass::FixedTransformType::JacobianType
                            FixedTransformJacobianType;
  typedef typename Superclass::MovingTransformType::JacobianType
                            MovingTransformJacobianType;

  typedef typename Superclass::NumberOfParametersType         NumberOfParametersType;
  typedef typename Superclass::ImageDimensionType             ImageDimensionType;

  typedef typename VirtualImageType::RegionType               ImageRegionType;
  typedef typename VirtualImageType::SizeType                 RadiusType;
  typedef typename VirtualImageType::IndexType                IndexType;

  /* Image dimension accessors */
  itkStaticConstMacro(FixedImageDimension, ImageDimensionType,
      ::itk::GetImageDimension<FixedImageType>::ImageDimension);

  itkStaticConstMacro(MovingImageDimension, ImageDimensionType,
        ::itk::GetImageDimension<MovingImageType>::ImageDimension);

  itkStaticConstMacro(VirtualImageDimension, ImageDimensionType,
        ::itk::GetImageDimension<VirtualImageType>::ImageDimension);

  /** Initialize. Must be called before first call to GetValue or
   *  GetValueAndDerivative, after metric settings are changed. */
  virtual void Initialize(void) throw (itk::ExceptionObject);

  /** Evaluate and return the value and derivative */
  using Superclass::GetValueAndDerivative;
  void GetValueAndDerivative(MeasureType & value,
      DerivativeType & derivative) const;

  /** Evaluate and return the metric value */
  MeasureType GetValue() const {
    itkExceptionMacro("GetValue not yet implemented.");
  }

  // Set the radius of the neighborhood window centered at each pixel
  itkSetMacro(Radius, RadiusType);

  // Get the Radius of the neighborhood window centered at each pixel
  itkGetMacro(Radius, RadiusType);
  itkGetConstMacro(Radius, RadiusType);

protected:

  // interested values here updated during scanning
  typedef InternalComputationValueType                 QueueRealType;
  typedef std::deque<QueueRealType>                    SumQueueType;
  typedef ConstNeighborhoodIterator<VirtualImageType>  ScanningIteratorType;
  // one ScanMemType for each thread
  typedef struct ScanMemType {
    // queues used in the scanning
    SumQueueType Qsuma2;
    SumQueueType Qsumb2;
    SumQueueType Qsuma;
    SumQueueType Qsumb;
    SumQueueType Qsumab;
    SumQueueType Qcount;

    QueueRealType Ia;
    QueueRealType Ja;
    QueueRealType sfm;
    QueueRealType sff;
    QueueRealType smm;

    FixedImageGradientType gradI;
    MovingImageGradientType gradJ;

    MovingImagePointType mappedMovingPoint;

  } ScanMemType;

  typedef struct ScanParaType {
    // const values during scanning
    ImageRegionType scan_region;
    SizeValueType number_of_fill_zero; // for each queue
    SizeValueType window_length; // number of voxels in the scanning window
    IndexValueType scan_region_begin_index_dim0;

    typename FixedImageType::ConstPointer I;
    typename MovingImageType::ConstPointer J;
    typename VirtualImageType::ConstPointer V;
    RadiusType r;

  } ScanParaType;

  // computation routines for normalized cross correlation

  void InitializeScanning(const ImageRegionType &scan_region,
    ScanningIteratorType &scan_it, ScanMemType &scan_mem,
    ScanParaType &scan_para ) const;

  void UpdateQueuesAtBeginingOfLine(
    const ScanningIteratorType &scan_it, ScanMemType &scan_mem,
    const ScanParaType &scan_para,
    const ThreadIdType threadID) const;

  // Increment the iterator and check to see if we're at the end of the
  // line.  If so, go to the next line.  Otherwise, add the
  // the values for the next hyperplane.
  void UpdateQueuesToNextScanWindow(
    const ScanningIteratorType &scan_it, ScanMemType &scan_mem,
    const ScanParaType &scan_para,
    const ThreadIdType threadID) const;

  void UpdateQueues(const ScanningIteratorType &scan_it,
    ScanMemType &scan_mem, const ScanParaType &scan_para,
    ThreadIdType threadID) const;

  bool ComputeInformationFromQueues(
    const ScanningIteratorType &scan_it, ScanMemType &scan_mem,
    const ScanParaType &scan_para,
    const ThreadIdType threadID) const;

 void ComputeMovingTransformDerivative(
    const ScanningIteratorType &scan_it, ScanMemType &scan_mem,
    const ScanParaType &scan_para, DerivativeType &deriv,
    MeasureType &local_cc, const ThreadIdType threadID) const;

protected:

  ANTSNeighborhoodCorrelationImageToImageObjectMetric();
  virtual ~ANTSNeighborhoodCorrelationImageToImageObjectMetric();

  virtual void PrintSelf(std::ostream & os, Indent indent) const;

private:
  //purposely not implemented
  ANTSNeighborhoodCorrelationImageToImageObjectMetric(
      const Self &);
  //purposely not implemented
  void operator=(const Self &);

  static void NeighborhoodScanningWindowGetValueAndDerivativeThreadedCallback(
      const DenseThreaderDomainType& virtualImageSubRegion,
      ThreadIdType threadID, Superclass * dataHolder);

  // Radius of the neighborhood window centered at each pixel
  RadiusType m_Radius;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkANTSNeighborhoodCorrelationImageToImageObjectMetric.hxx"
#endif

#endif
