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
#ifndef itkComposeDisplacementFieldsImageFilter_h
#define itkComposeDisplacementFieldsImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkVectorInterpolateImageFunction.h"

namespace itk
{

/**
 * \class ComposeDisplacementFieldsImageFilter
 *
 * \brief Compose two displacement fields.
 *
 * \author Nick Tustison
 * \author Brian Avants
 *
 * \ingroup ITKDisplacementField
 *
 */

template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT ComposeDisplacementFieldsImageFilter
  : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  typedef ComposeDisplacementFieldsImageFilter          Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                            Pointer;
  typedef SmartPointer<const Self>                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** Extract dimension from input image. */
  itkStaticConstMacro( ImageDimension, unsigned int,
    TInputImage::ImageDimension );

  typedef TInputImage                          InputFieldType;
  typedef TOutputImage                         OutputFieldType;

  /** Image typedef support. */
  typedef typename OutputFieldType::PixelType     PixelType;
  typedef typename OutputFieldType::PixelType     VectorType;
  typedef typename OutputFieldType::RegionType    RegionType;
  typedef typename OutputFieldType::IndexType     IndexType;

  typedef typename OutputFieldType::PointType     PointType;
  typedef typename OutputFieldType::SpacingType   SpacingType;
  typedef typename OutputFieldType::PointType     OriginType;
  typedef typename OutputFieldType::SizeType      SizeType;
  typedef typename OutputFieldType::DirectionType DirectionType;

  /** Other typedef */
  typedef typename VectorType::ComponentType      RealType;
  typedef VectorInterpolateImageFunction
    <InputFieldType, RealType>                    InterpolatorType;

  /** Get the interpolator. */
  itkGetModifiableObjectMacro( Interpolator, InterpolatorType );

  /** Set the deformation field */
  void SetDisplacementField( const InputFieldType *field )
    {
    itkDebugMacro( "setting displacement field to " << field );
    if ( field != this->GetInput( 0 ) )
      {
      this->SetInput( 0, field );
      this->Modified();
      if( !this->m_Interpolator.IsNull() )
        {
        this->m_Interpolator->SetInputImage( field );
        }
      }
    }

  /**
   * Get the deformation field.
   */
  const InputFieldType* GetDisplacementField() const
    {
    return this->GetInput( 0 );
    }

  /** Set the warping field */
  void SetWarpingField( const InputFieldType *field )
    {
    itkDebugMacro( "setting warping field to " << field );
    if ( field != this->GetInput( 1 ) )
      {
      this->SetInput( 1, field );
      }
    }

  /**
   * Get the warping field.
   */
  const InputFieldType* GetWarpingField() const
    {
    return this->GetInput( 1 );
    }

  /* Set the interpolator. */
  virtual void SetInterpolator( InterpolatorType* interpolator );

protected:

  /** Constructor */
  ComposeDisplacementFieldsImageFilter();

  /** Deconstructor */
  virtual ~ComposeDisplacementFieldsImageFilter() ITK_OVERRIDE;

  /** Standard print self function **/
  void PrintSelf( std::ostream& os, Indent indent ) const ITK_OVERRIDE;

  /** preprocessing function */
  void BeforeThreadedGenerateData() ITK_OVERRIDE;

  /** Multithreaded function which generates the output field. */
  void ThreadedGenerateData( const RegionType &, ThreadIdType ) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ComposeDisplacementFieldsImageFilter);

  /** The interpolator. */
  typename InterpolatorType::Pointer             m_Interpolator;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkComposeDisplacementFieldsImageFilter.hxx"
#endif

#endif
