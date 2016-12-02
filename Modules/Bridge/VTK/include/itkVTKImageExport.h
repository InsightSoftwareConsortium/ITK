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
#ifndef itkVTKImageExport_h
#define itkVTKImageExport_h

#include "itkVTKImageExportBase.h"
#include "itkConceptChecking.h"

namespace itk
{
/** \class VTKImageExport
 * \brief Connect the end of an ITK image pipeline to a VTK pipeline.
 *
 * VTKImageExport can be used at the end of an ITK image pipeline to
 * connect with a VTK pipeline that begins with vtkImageImport.
 * Callbacks provided by VTKImageExport are registered with
 * vtkImageImport to connect the pipeline execution together.  Once
 * connected, update requests coming through the VTK pipeline are
 * automatically propagated to the ITK pipeline.
 *
 * While VTKImageExportBase provides the pipeline functionality
 * independent of image type, instances must be created through
 * VTKImageExport.  This class provides the implementations for
 * callbacks that depend on the image type.
 *
 * Note that not all image types will work correctly.  VTK will only
 * support images of 1, 2, or 3 dimensions.  Scalar value types can be
 * one of: float, double, char, unsigned char, short, unsigned short,
 * int, unsigned int, long, unsigned long.
 *
 * VTKImageExport also supports pixel types with multiple
 * components (like RGBPixel).
 *
 * \ingroup IOFilters
 * \sa VTKImageExportBase
 * \ingroup ITKVTK
 */
template< typename TInputImage >
class ITK_TEMPLATE_EXPORT VTKImageExport:public VTKImageExportBase
{
public:
  /** Standard class typedefs. */
  typedef VTKImageExport             Self;
  typedef VTKImageExportBase         Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKImageExport, VTKImageExportBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** The type of the input image. */
  typedef TInputImage InputImageType;

#ifdef ITK_USE_CONCEPT_CHECKING
  itkConceptMacro( ImageDimensionCheck,
                   ( Concept::SameDimensionOrMinusOneOrTwo<
                     3,itkGetStaticConstMacro(InputImageDimension) > ) );
#endif
  /** Set the input image of this image exporter. */
  using Superclass::SetInput;
  void SetInput(const InputImageType *);
  InputImageType * GetInput();

protected:
  VTKImageExport();
  ~VTKImageExport() {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  typedef typename InputImageType::Pointer    InputImagePointer;
  typedef typename InputImageType::RegionType InputRegionType;
  typedef typename InputRegionType::SizeType  InputSizeType;
  typedef typename InputRegionType::IndexType InputIndexType;

  int * WholeExtentCallback() ITK_OVERRIDE;

  double * SpacingCallback() ITK_OVERRIDE;

  double * OriginCallback() ITK_OVERRIDE;

  float * FloatSpacingCallback() ITK_OVERRIDE;

  float * FloatOriginCallback() ITK_OVERRIDE;

  const char * ScalarTypeCallback() ITK_OVERRIDE;

  int NumberOfComponentsCallback() ITK_OVERRIDE;

  void PropagateUpdateExtentCallback(int *) ITK_OVERRIDE;

  int * DataExtentCallback() ITK_OVERRIDE;

  void * BufferPointerCallback() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VTKImageExport);

  std::string m_ScalarTypeName;
  int         m_WholeExtent[6];
  int         m_DataExtent[6];
  double      m_DataSpacing[3];
  double      m_DataOrigin[3];
  float       m_FloatDataSpacing[3];
  float       m_FloatDataOrigin[3];
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVTKImageExport.hxx"
#endif

#endif
