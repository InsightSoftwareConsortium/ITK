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
#ifndef itkPolylineMaskImageFilter_h
#define itkPolylineMaskImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{
/** \class PolylineMaskImageFilter
 * \brief Implements image masking operation constrained by a polyline on a plane
 * perpendicular to certain viewing direction.
 *
 * This class is parameterized over the types of the input image, polyline, vector
 * and output image.
 *
 * \ingroup ImageToImageFilter
 * \ingroup ITKImageIntensity
 */
template< typename TInputImage, typename TPolyline, typename TVector,
          typename TOutputImage >
class ITK_TEMPLATE_EXPORT PolylineMaskImageFilter:public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef PolylineMaskImageFilter                         Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PolylineMaskImageFilter, ImageToImageFilter);

  /** Number of dimensions. */
  itkStaticConstMacro(NDimensions, unsigned int,
                      TInputImage::ImageDimension);

  itkStaticConstMacro(InputDimension, unsigned int, 3);

  /** Some convenient typedefs for input image. */
  typedef TInputImage                           InputImageType;
  typedef typename InputImageType::ConstPointer InputImagePointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename InputImageType::PixelType    InputImagePixelType;
  typedef Point< double, 3 >                    PointType;
  typedef Point< double, 2 >                    ProjPlanePointType;

  /** Standard matrix type for this class. */
  typedef Matrix< double, itkGetStaticConstMacro(InputDimension), itkGetStaticConstMacro(InputDimension) > MatrixType;

  /** typedef for the vector type. */
  typedef TVector VectorType;

  /** typedef for the polyline type. */
  typedef TPolyline PolylineType;

  /** typedef for the output image. */
  typedef TOutputImage                         OutputImageType;
  typedef typename OutputImageType::Pointer    OutputImagePointer;
  typedef typename OutputImageType::RegionType OutputImageRegionType;
  typedef typename OutputImageType::PixelType  OutputImagePixelType;

  /** Set input image. */
  void SetInput1(const InputImageType *image);

  /** Set input polyline. */
  void SetInput2(const PolylineType *polyline);

  /** Set/Get viewing direction vector. */
  itkSetMacro(ViewVector, VectorType);
  itkGetConstMacro(ViewVector, VectorType);

  /** Set/Get up direction vector. */
  itkSetMacro(UpVector, VectorType);
  itkGetConstMacro(UpVector, VectorType);

  /** Set/Get camera center point. */
  itkSetMacro(CameraCenterPoint, PointType);
  itkGetConstMacro(CameraCenterPoint, PointType);

  /** Set/Get focal distance of the camera. */
  itkSetMacro(FocalDistance, double);
  itkGetConstMacro(FocalDistance, double);

  /** Set/Get camera focal point on the projection plane. */
  itkSetMacro(FocalPoint, ProjPlanePointType);
  itkGetConstMacro(FocalPoint, ProjPlanePointType);

  /** Generate 3D rotation matrix using the viewing and up vectors. */
  void GenerateRotationMatrix();

  /** 3D rotation and perspective projection transform. */
  ProjPlanePointType TransformProjectPoint(PointType inputPoint);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( VectorHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< typename VectorType::ValueType > ) );
  // End concept checking
#endif

protected:
  PolylineMaskImageFilter();
  virtual ~PolylineMaskImageFilter() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PolylineMaskImageFilter);

  VectorType m_ViewVector;
  VectorType m_UpVector;

  MatrixType m_RotationMatrix;

  PointType m_CameraCenterPoint;

  ProjPlanePointType m_FocalPoint;

  double m_FocalDistance;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPolylineMaskImageFilter.hxx"
#endif

#endif
