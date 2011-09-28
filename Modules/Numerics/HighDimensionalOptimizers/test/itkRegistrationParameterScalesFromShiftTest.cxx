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
#include "itkRegistrationParameterScalesFromShift.h"
#include "itkObjectToObjectMetric.h"

#include "itkImage.h"
#include "itkAffineTransform.h"
#include "itkTranslationTransform.h"

/**
 *  \class RegistrationParameterScalesFromShiftTestMetric for test.
 *  Create a simple metric to use for testing here.
 */
template< class TFixedImage,class TMovingImage,class TVirtualImage = TFixedImage >
class ITK_EXPORT RegistrationParameterScalesFromShiftTestMetric:
  public itk::ObjectToObjectMetric
{
public:
  /** Standard class typedefs. */
  typedef RegistrationParameterScalesFromShiftTestMetric                       Self;
  typedef itk::ObjectToObjectMetric                               Superclass;
  typedef itk::SmartPointer< Self >                               Pointer;
  typedef itk::SmartPointer< const Self >                         ConstPointer;

  typedef typename Superclass::MeasureType          MeasureType;
  typedef typename Superclass::DerivativeType       DerivativeType;
  typedef typename Superclass::ParametersType       ParametersType;
  typedef typename Superclass::ParametersValueType  ParametersValueType;

  itkTypeMacro(RegistrationParameterScalesFromShiftTestMetric, ObjectToObjectMetric);

  itkNewMacro(Self);

  // Pure virtual functions that all Metrics must provide
  unsigned int GetNumberOfParameters() const { return 5; }

  MeasureType GetValue() const
    {
    return 1.0;
    }

  void GetValueAndDerivative( MeasureType & value, DerivativeType & derivative ) const
    {
    value = 1.0;
    derivative.Fill(0.0);
    }

  unsigned int GetNumberOfLocalParameters() const
  { return 0; }

  bool HasLocalSupport() const
  { return false; }

  void UpdateTransformParameters( DerivativeType &, ParametersValueType ) {}

  const ParametersType & GetParameters() const
  { return m_Parameters; }

  void Initialize(void) throw ( itk::ExceptionObject ) {}

  ParametersType  m_Parameters;

  // Image related types
  typedef TFixedImage                             FixedImageType;
  typedef TMovingImage                            MovingImageType;
  typedef TVirtualImage                           VirtualImageType;

  typedef typename FixedImageType::ConstPointer   FixedImageConstPointer;
  typedef typename MovingImageType::ConstPointer  MovingImageConstPointer;
  typedef typename VirtualImageType::Pointer      VirtualImagePointer;

  /* Set/get images */
  /** Connect the Fixed Image.  */
  itkSetConstObjectMacro(FixedImage, FixedImageType);
  /** Get the Fixed Image. */
  itkGetConstObjectMacro(FixedImage, FixedImageType);
  /** Connect the Moving Image.  */
  itkSetConstObjectMacro(MovingImage, MovingImageType);
  /** Get the Moving Image. */
  itkGetConstObjectMacro(MovingImage, MovingImageType);
  /** Set all virtual domain image */
  itkSetObjectMacro(VirtualDomainImage, VirtualImageType);
  /** Get the virtual domain image */
  itkGetObjectMacro(VirtualDomainImage, VirtualImageType);

  /* Image dimension accessors */
  itkStaticConstMacro(FixedImageDimension, itk::SizeValueType,
      ::itk::GetImageDimension<FixedImageType>::ImageDimension);
  itkStaticConstMacro(MovingImageDimension, itk::SizeValueType,
      ::itk::GetImageDimension<MovingImageType>::ImageDimension);
  itkStaticConstMacro(VirtualImageDimension, itk::SizeValueType,
      ::itk::GetImageDimension<VirtualImageType>::ImageDimension);

  /**  Type of the Transform Base classes */
  typedef ::itk::Transform<CoordinateRepresentationType,
    itkGetStaticConstMacro( MovingImageDimension ),
    itkGetStaticConstMacro( VirtualImageDimension )>  MovingTransformType;

  typedef ::itk::Transform<CoordinateRepresentationType,
    itkGetStaticConstMacro( FixedImageDimension ),
    itkGetStaticConstMacro( VirtualImageDimension )>  FixedTransformType;

  typedef typename FixedTransformType::Pointer        FixedTransformPointer;
  typedef typename MovingTransformType::Pointer       MovingTransformPointer;

  typedef typename FixedTransformType::JacobianType   FixedTransformJacobianType;
  typedef typename MovingTransformType::JacobianType  MovingTransformJacobianType;

  /** Connect the fixed transform. */
  itkSetObjectMacro(FixedTransform, FixedTransformType);
  /** Get a pointer to the fixed transform.  */
  itkGetConstObjectMacro(FixedTransform, FixedTransformType);
  /** Connect the moving transform. */
  itkSetObjectMacro(MovingTransform, MovingTransformType);
  /** Get a pointer to the moving transform.  */
  itkGetConstObjectMacro(MovingTransform, MovingTransformType);

private:

  FixedImageConstPointer  m_FixedImage;
  MovingImageConstPointer m_MovingImage;
  VirtualImagePointer     m_VirtualDomainImage;

  FixedTransformPointer   m_FixedTransform;
  MovingTransformPointer  m_MovingTransform;

  RegistrationParameterScalesFromShiftTestMetric() {}
  ~RegistrationParameterScalesFromShiftTestMetric() {}

};

/**
 */
int itkRegistrationParameterScalesFromShiftTest(int , char* [])
{

  // Image begins
  const itk::SizeValueType    ImageDimension = 2;
  typedef double         PixelType;

  // Image Types
  typedef itk::Image<PixelType,ImageDimension>           FixedImageType;
  typedef itk::Image<PixelType,ImageDimension>           MovingImageType;
  typedef itk::Image<PixelType,ImageDimension>           VirtualImageType;

  FixedImageType::Pointer  fixedImage  = FixedImageType::New();
  MovingImageType::Pointer movingImage = MovingImageType::New();
  VirtualImageType::Pointer virtualImage = fixedImage;

  MovingImageType::SizeType    size;
  size.Fill(100);

  movingImage->SetRegions( size );
  fixedImage->SetRegions( size );
  // Image done

  // Transform begins
  typedef itk::AffineTransform<double, ImageDimension>      MovingTransformType;
  MovingTransformType::Pointer movingTransform =  MovingTransformType::New();
  movingTransform->SetIdentity();

  typedef itk::TranslationTransform<double, ImageDimension> FixedTransformType;
  FixedTransformType::Pointer fixedTransform =    FixedTransformType::New();
  fixedTransform->SetIdentity();
  // Transform done

  // Metric begins
  typedef RegistrationParameterScalesFromShiftTestMetric
    <FixedImageType, MovingImageType>   MetricType;
  MetricType::Pointer metric = MetricType::New();

  metric->SetVirtualDomainImage( virtualImage );
  metric->SetFixedImage( fixedImage );
  metric->SetMovingImage( movingImage );

  metric->SetFixedTransform( fixedTransform );
  metric->SetMovingTransform( movingTransform );
  // Metric done

  // Testing RegistrationParameterScalesFromShift
  typedef itk::RegistrationParameterScalesFromShift< MetricType >
    RegistrationParameterScalesFromShiftType;
  RegistrationParameterScalesFromShiftType::Pointer shiftScaleEstimator
    = RegistrationParameterScalesFromShiftType::New();

  shiftScaleEstimator->SetMetric(metric);
  shiftScaleEstimator->SetTransformForward(true); //by default, scales for the moving transform
  shiftScaleEstimator->SetSamplingStrategy(
    RegistrationParameterScalesFromShiftType::CornerSampling);
  shiftScaleEstimator->Print( std::cout );

  RegistrationParameterScalesFromShiftType::ScalesType movingScales(
    movingTransform->GetNumberOfParameters());
  shiftScaleEstimator->EstimateScales(movingScales);
  std::cout << "Shift scales for the affine transform = " << movingScales << std::endl;

  // Check the correctness
  RegistrationParameterScalesFromShiftType::ScalesType theoreticalMovingScales(
    movingTransform->GetNumberOfParameters());
  VirtualImageType::PointType upperPoint;
  virtualImage->TransformIndexToPhysicalPoint(virtualImage->
    GetLargestPossibleRegion().GetUpperIndex(), upperPoint);

  itk::SizeValueType param = 0;
  for (itk::SizeValueType row = 0; row < ImageDimension; row++)
    {
    for (itk::SizeValueType col = 0; col < ImageDimension; col++)
      {
      theoreticalMovingScales[param++] = upperPoint[col] * upperPoint[col];
      }
    }
  for (itk::SizeValueType row = 0; row < ImageDimension; row++)
    {
    theoreticalMovingScales[param++] = 1;
    }

  bool affinePass = true;
  for (itk::SizeValueType p = 0; p < theoreticalMovingScales.GetSize(); p++)
    {
    if (vcl_abs((movingScales[p] - theoreticalMovingScales[p])
      / theoreticalMovingScales[p]) > 0.1 )
      {
      affinePass = false;
      break;
      }
    }
  bool nonUniformForAffine = false;
  for (itk::SizeValueType p = 1; p < movingScales.GetSize(); p++)
    {
    if (movingScales[p] != movingScales[0])
      {
      nonUniformForAffine = true;
      break;
      }
    }

  // Check done

  // Scales for the fixed transform
  shiftScaleEstimator->SetTransformForward(false);

  RegistrationParameterScalesFromShiftType::ScalesType fixedScales(
    fixedTransform->GetNumberOfParameters());
  shiftScaleEstimator->EstimateScales(fixedScales);
  std::cout << "Shift scales for the translation transform = "
    << fixedScales << std::endl;

  // Check the correctness
  RegistrationParameterScalesFromShiftType::ScalesType theoreticalFixedScales(
    fixedTransform->GetNumberOfParameters());
  theoreticalFixedScales.Fill(1.0);

  bool translationPass = true;
  for (itk::SizeValueType p = 0; p < theoreticalFixedScales.GetSize(); p++)
    {
    if (vcl_abs((fixedScales[p] - theoreticalFixedScales[p])
      / theoreticalFixedScales[p]) > 0.1 )
      {
      translationPass = false;
      break;
      }
    }
  bool uniformForTranslation = true;
  for (itk::SizeValueType p = 1; p < fixedScales.GetSize(); p++)
    {
    if (fixedScales[p] != fixedScales[0])
      {
      uniformForTranslation = false;
      break;
      }
    }
  // Check done

  // Testing RegistrationParameterScalesFromShift done
  std::cout << std::endl;

  if (!affinePass)
    {
    std::cout << "Failed: the shift scales for the affine transform are not correct." << std::endl;
    }
  else
    {
    std::cout << "Passed: the shift scales for the affine transform are correct." << std::endl;
    }

  if (!translationPass)
    {
    std::cout << "Failed: the shift scales for the translation transform are not correct." << std::endl;
    }
  else
    {
    std::cout << "Passed: the shift scales for the translation transform are correct." << std::endl;
    }

  if (!uniformForTranslation)
    {
    std::cout << "Error: the shift scales for a translation transform are not equal for all parameters." << std::endl;
    }
  if (!nonUniformForAffine)
    {
    std::cout << "Error: the shift scales for an affine transform are equal for all parameters." << std::endl;
    }

  if (affinePass && translationPass
    && nonUniformForAffine && uniformForTranslation)
    {
    std::cout << "Test passed" << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }
}
