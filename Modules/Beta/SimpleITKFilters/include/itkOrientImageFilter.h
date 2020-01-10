/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkOrientImageFilter_h
#define itkOrientImageFilter_h

#include "itkPermuteAxesImageFilter.h"
#include "itkFlipImageFilter.h"
#include "itkSpatialOrientationAdapter.h"
#include <map>
#include <string>

namespace itk
{
/** \class OrientImageFilter
 * \brief Permute axes and then flip images as needed to obtain
 *  agreement in coordinateOrientation codes.
 *
 * This class satisfies a common requirement in medical imaging, which
 * is to properly orient a 3 dimensional image with respect to anatomical
 * features. Due to the wide variety of hardware used to generate 3D images
 * of human anatomy, and the even wider variety of image processing software,
 * it is often necessary to re-orient image volume data.
 *
 * OrientImageFilter depends on a set of constants that describe all possible
 * labeled according to the following scheme:
 * Directions are labeled in terms of following pairs:
 *   - Left and Right (Subject's left and right)
 *   - Anterior and Posterior (Subject's front and back)
 *   - Inferior and Superior (Subject's bottom and top, i.e. feet and head)
 *
 * The initials of these directions are used in a 3 letter code in the
 * enumerated type itk::SpatialOrientation::ValidCoordinateOrientationFlags.
 * The initials are given fastest moving index first, second fastest second,
 * third fastest third.
 * Examples:
 *  - ITK_COORDINATE_ORIENTATION_RIP
 *    -# Right to Left varies fastest (0th pixel on Subject's right)
 *    -# Inferior to Superior varies second fastest
 *    -# Posterior to Anterior varies slowest.
 *  - ITK_COORDINATE_ORIENTATION_LSA
 *    -# Left to Right varies fastest (0th pixel on Subject's left)
 *    -# Superior to Inferior varies second fastest
 *    -# Anterior to Posterior varies slower
 *
 * In order to use this filter, you need to supply an input
 * image, the current orientation of the input image (set with
 * SetGivenCoordinateOrientation) and the desired orientation.
 * (set with SetDesiredCoordinateOrientation).
 * You may explicitly set the DesiredOrientation with
 * SetDesiredCoordinateOrientation (if UseImageDirection is "off") or
 * you can use the image's direction cosines to set the
 * DesiredOrientation (if UseImageDirection is "on").
 * When reading image files that define the coordinate orientation
 * of the image, the current orientation is stored in the MetadataDictionary
 * for the itk::Image object and the Image.Direction direction cosine
 * matrix created from the file.
 *
 * As an example, if you wished to keep all images within your program in the
 * orientation corresponding to the Analyze file format's 'CORONAL' orientation
 * you could do something like the following
 *
   \code
   // DEPRECATED -- using metadata for orientation is no longer supported
   //
   #include "itkAnalyzeImageIO.h"
   #include "itkMetaDataObject.h"
   #include "itkImage.h"
   #include "itkOrientImageFilter.h"
   using ImageType = itk::Image<unsigned char,3>;
   using ImageReaderType = itk::ImageFileReader< TstImageType >;
   ImageType::Pointer ReadAnalyzeFile(const char *path)
   {
     itk::AnalyzeImageIO::Pointer io = itk::AnalyzeImageIO::New();
     ImageReaderType::Pointer fileReader = ImageReaderType::New();
     fileReader->SetImageIO(io);
     fileReader->SetFileName(path);
     fileReader->Update();
     ImageType::Pointer rval = fileReader->GetOutput();

   // DEPRECATED -- use direction cosines
   //
    itk::SpatialOrientation::ValidCoordinateOrientationFlags fileOrientation;
    itk::ExposeMetaData<itk::SpatialOrientation::ValidCoordinateOrientationFlags>
      (rval->GetMetaDataDictionary(),itk::ITK_CoordinateOrientation,fileOrientation);
     itk::OrientImageFilter<ImageType,ImageType>::Pointer orienter =
       itk::OrientImageFilter<ImageType,ImageType>::New();
     orienter->SetGivenCoordinateOrientation(fileOrientation); // deprecated

     orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP);
     orienter->SetInput(rval);
     orienter->Update();
     rval = orienter->GetOutput();
     return rval;
   }
   \endcode
 *
 * Or, using the direction cosines of the image,
   \code
   #include "itkAnalyzeImageIO.h"
   #include "itkImage.h"
   #include "itkOrientImageFilter.h"
   using ImageType = itk::Image<unsigned char,3>;
   using ImageReaderType = itk::ImageFileReader< ImageType >;
   ImageType::Pointer ReadAnalyzeFile(const char *path)
   {
     itk::AnalyzeImageIO::Pointer io = itk::AnalyzeImageIO::New();
     ImageReaderType::Pointer fileReader = ImageReaderType::New();
     fileReader->SetImageIO(io);
     fileReader->SetFileName(path);
     fileReader->Update();
     ImageType::Pointer rval = fileReader->GetOutput();

     itk::OrientImageFilter<ImageType,ImageType>::Pointer orienter =
       itk::OrientImageFilter<ImageType,ImageType>::New();
     orienter->UseImageDirectionOn();
     orienter->SetDesiredCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP);
     orienter->SetInput(rval);
     orienter->Update();
     rval = orienter->GetOutput();
     return rval;
   }
   \endcode
 * \ingroup ITKImageGrid
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT OrientImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(OrientImageFilter);

  /** Standard class type aliases. */
  using Self = OrientImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageConstPointer = typename OutputImageType::ConstPointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;
  using CoordinateOrientationCode = SpatialOrientation::ValidCoordinateOrientationFlags;

  /** Axes permuter type. */
  using PermuterType = PermuteAxesImageFilter<TInputImage>;
  using PermuteOrderArrayType = typename PermuterType::PermuteOrderArrayType;

  /** Axes flipper type. */
  using FlipperType = FlipImageFilter<TInputImage>;
  using FlipAxesArrayType = typename FlipperType::FlipAxesArrayType;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(OrientImageFilter, ImageToImageFilter);

  /** Set/Get the orientation codes to define the coordinate transform. */
  itkGetEnumMacro(GivenCoordinateOrientation, CoordinateOrientationCode);
  void
  SetGivenCoordinateOrientation(CoordinateOrientationCode newCode);

  inline void
  SetGivenCoordinateDirection(const typename TInputImage::DirectionType & GivenDirection)
  {
    SetGivenCoordinateOrientation(itk::SpatialOrientationAdapter().FromDirectionCosines(GivenDirection));
  }

  itkGetEnumMacro(DesiredCoordinateOrientation, CoordinateOrientationCode);
  void
  SetDesiredCoordinateOrientation(CoordinateOrientationCode newCode);

  inline void
  SetDesiredCoordinateDirection(const typename TOutputImage::DirectionType & DesiredDirection)
  {
    SetDesiredCoordinateOrientation(itk::SpatialOrientationAdapter().FromDirectionCosines(DesiredDirection));
  }

  /** Controls how the GivenCoordinateOrientation is determined.
   * If set to On, the direction cosines determine the coordinate
   * orientation. If set to Off, the user must use the
   * SetGivenCoordinateOrientation method to establish the
   * orientation. For compatibility with the original API, the default value
   * is Off. */
  itkBooleanMacro(UseImageDirection);
  itkGetConstMacro(UseImageDirection, bool);
  itkSetMacro(UseImageDirection, bool);

  /** Get axes permute order. */
  itkGetConstReferenceMacro(PermuteOrder, PermuteOrderArrayType);

  /** Get flip axes. */
  itkGetConstReferenceMacro(FlipAxes, FlipAxesArrayType);

  /** Convenience methods to set desired slice orientation
   *  These methods allow a limited selection of slice orientations
   *  without having to specify the SpatialOrientation.
   *
   *  SetDesiredCoordinateOrientationToAxial is equivalent to
   *  SetDesiredCoordinateOrientation (ITK_COORDINATE_ORIENTATION_RAI).
   *
   *  SetDesiredCoordinateOrientationToCoronal is equivalent to
   *  SetDesiredCoordinateOrientation (ITK_COORDINATE_ORIENTATION_RSA).
   *
   *  SetDesiredCoordinateOrientationToSagittal is equivalent to
   *  SetDesiredCoordinateOrientation (ITK_COORDINATE_ORIENTATION_ASL).
   */
  void
  SetDesiredCoordinateOrientationToAxial()
  {
    this->SetDesiredCoordinateOrientation(SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAI);
  }

  void
  SetDesiredCoordinateOrientationToCoronal()
  {
    this->SetDesiredCoordinateOrientation(SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSA);
  }

  void
  SetDesiredCoordinateOrientationToSagittal()
  {
    this->SetDesiredCoordinateOrientation(SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASL);
  }

  /** OrientImageFilter produces an image which is a different
   * dimensionality than its input image, in general. As such,
   * OrientImageFilter needs to provide an implementation for
   * GenerateOutputInformation() in order to inform the pipeline
   * execution model. The original documentation of this method is
   * below.
   * \sa ProcessObject::GenerateOutputInformaton() */
  void
  GenerateOutputInformation() override;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputConvertibleToOutput, (Concept::Convertible<InputImagePixelType, OutputImagePixelType>));
  itkConceptMacro(SameDimension, (Concept::SameDimension<Self::InputImageDimension, Self::OutputImageDimension>));
  itkConceptMacro(DimensionShouldBe3, (Concept::SameDimension<Self::InputImageDimension, 3>));
  // End concept checking
#endif

protected:
  OrientImageFilter();
  ~OrientImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** OrientImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void
  GenerateInputRequestedRegion() override;

  /** OrientImageFilter will produce the entire output. */
  void
  EnlargeOutputRequestedRegion(DataObject * itkNotUsed(output)) override;

  /*** Member functions used by GenerateData: */
  void
  DeterminePermutationsAndFlips(const SpatialOrientation::ValidCoordinateOrientationFlags fixed_orient,
                                const SpatialOrientation::ValidCoordinateOrientationFlags moving_orient);

  /** Returns true if a permute is required. Returns false otherwise. */
  bool
  NeedToPermute();

  /** Returns true if flipping is required. Returns false otherwise. */
  bool
  NeedToFlip();

  /** Single-threaded version of GenerateData. This filter delegates
   * to PermuteAxesImageFilter and FlipImageFilter. */
  void
  GenerateData() override;

private:
  std::string
  GetMajorAxisFromPatientRelativeDirectionCosine(double x, double y, double z);

  CoordinateOrientationCode m_GivenCoordinateOrientation{ SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP };
  CoordinateOrientationCode m_DesiredCoordinateOrientation{ SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP };
  bool                      m_UseImageDirection{ false };

  PermuteOrderArrayType m_PermuteOrder;
  FlipAxesArrayType     m_FlipAxes;

  std::map<std::string, CoordinateOrientationCode> m_StringToCode;
  std::map<CoordinateOrientationCode, std::string> m_CodeToString;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkOrientImageFilter.hxx"
#endif

#endif
