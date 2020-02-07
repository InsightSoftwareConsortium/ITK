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
#ifndef itkLabelGeometryImageFilter_h
#define itkLabelGeometryImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNumericTraits.h"
#include "itkArray.h"
#include "itkSimpleDataObjectDecorator.h"
#include <map>
#include <vector>
#include "vnl/algo/vnl_symmetric_eigensystem.h"
#include "vnl/vnl_det.h"
#include "itkMath.h"

namespace itk
{
/** \class LabelGeometryImageFilter
 * \brief Given a label map and an optional intensity image, compute
 * geometric features.
 *
 * This filter enables the measurement of geometric features of all objects in
 * a labeled ND volume. This labeled volume can represent, for instance, a
 * medical image segmented into different anatomical structures or a microscope
 * image segmented into individual cells. This filter is closely related to the
 * itkLabelStatisticsImageFilter, which measures statistics of image regions
 * defined by a labeled mask such as min, max, and mean intensity, intensity
 * standard deviation, and bounding boxes. This filter, however, measures the
 * geometry of the labeled regions themselves.
 *
 * It calculates features similar to the regionprops command of Matlab.  The
 * set of measurements that it enables include: volume, centroid, eigenvalues,
 * eigenvectors, axes lengths, eccentricity, elongation, orientation, bounding
 * box, oriented bounding box, and rotation matrix.  These features are based
 * solely on the labeled mask itself.  It also calculates integrated intensity
 * and weighted centroid, which are measured on an intensity image under the
 * labeled mask. These features represent the set of currently calculated
 * features, but the framework of the filter is designed so that it can be
 * easily expanded to measure a wide variety of other features.
 *
 *  \authors Dirk Padfield and James Miller.
 *
 *  This work is part of the National Alliance for Medical Image
 *  Computing (NAMIC), funded by the National Institutes of Health
 *  through the NIH Roadmap for Medical Research, Grant U54 EB005149.
 *  Information on the National Centers for Biomedical Computing
 *  can be obtained from http://commonfund.nih.gov/bioinformatics.
 *
 *  This filter was contributed in the Insight Journal paper:
 *  "A Label Geometry Image Filter for Multiple Object Measurement"
 *  by Padfield D., Miller J
 *  http://www.insight-journal.org/browse/publication/301
 *  https://hdl.handle.net/1926/1493
 *
 * \ingroup ITKReview
 *
 * \sphinx
 * \sphinxexample{Nonunit/Review/GeometricPropertiesOfRegion,Geometric Properties Of Labeled Region}
 * \endsphinx
 */
template <typename TLabelImage, typename TIntensityImage = TLabelImage>
class ITK_TEMPLATE_EXPORT LabelGeometryImageFilter : public ImageToImageFilter<TLabelImage, TIntensityImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LabelGeometryImageFilter);

  /** Standard Self type alias */
  using Self = LabelGeometryImageFilter;
  using Superclass = ImageToImageFilter<TLabelImage, TIntensityImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelGeometryImageFilter, ImageToImageFilter);

  /** Image related type alias. */
  using IntensityImageType = TIntensityImage;
  using InputImagePointer = typename TIntensityImage::Pointer;
  using RegionType = typename TIntensityImage::RegionType;
  using SizeType = typename TIntensityImage::SizeType;
  using IndexType = typename TIntensityImage::IndexType;
  using PixelType = typename TIntensityImage::PixelType;

  /** Label image related type alias. */
  using LabelImageType = TLabelImage;
  using LabelImagePointer = typename TLabelImage::Pointer;
  using LabelRegionType = typename TLabelImage::RegionType;
  using LabelSizeType = typename TLabelImage::SizeType;
  using LabelIndexType = typename TLabelImage::IndexType;
  using LabelPixelType = typename TLabelImage::PixelType;
  using LabelPointType = typename TLabelImage::PointType;

  /** Image related type alias. */
  static constexpr unsigned int ImageDimension = TLabelImage::ImageDimension;

  /** Type to use for computations. */
  using RealType = typename NumericTraits<PixelType>::RealType;

  /** Smart Pointer type to a DataObject. */
  using DataObjectPointer = typename DataObject::Pointer;

  /** Type of DataObjects used for scalar outputs */
  using RealObjectType = SimpleDataObjectDecorator<RealType>;

  /** Bounding Box-related type alias */
  using BoundingBoxType = itk::FixedArray<typename LabelIndexType::IndexValueType, Self::ImageDimension * 2>;
  using BoundingBoxFloatType = itk::FixedArray<float, Self::ImageDimension * 2>;

  // using BoundingBoxVerticesType = itk::FixedArray<
  // LabelPointType,std::pow(2.0,Self::ImageDimension)>;
  using BoundingBoxVerticesType = std::vector<LabelPointType>;

  /** Axes Length-related type alias */
  using AxesLengthType = itk::FixedArray<RealType, Self::ImageDimension>;

  /** Index array type alias */
  using IndexArrayType = itk::FixedArray<typename LabelIndexType::IndexValueType, Self::ImageDimension>;

  /** vector of labels */
  using LabelsType = std::vector<LabelPixelType>;

  /** vector of indices */
  using LabelIndicesType = std::vector<LabelIndexType>;

  /** Vector type */
  using VectorType = std::vector<double>;

  /** Matrix type */
  using MatrixType = vnl_matrix<double>;

  /** \class LabelGeometry
   * \brief Geometry stored per label
   * \ingroup ITKReview
   */
  class LabelGeometry
  {
  public:
    // default constructor
    LabelGeometry()
    {
      // initialized to the default values
      this->m_Label = 0;
      this->m_Sum = NumericTraits<RealType>::ZeroValue();

      const unsigned int imageDimension = Self::ImageDimension;

      // m_BoundingBox.resize(imageDimension*2);
      for (unsigned int i = 0; i < imageDimension * 2; i += 2)
      {
        m_BoundingBox[i] = NumericTraits<typename IndexType::IndexValueType>::max();
        m_BoundingBox[i + 1] = NumericTraits<typename IndexType::IndexValueType>::NonpositiveMin();
      }

      m_BoundingBoxVolume = 0;
      m_BoundingBoxSize.Fill(0);
      m_PixelIndices.clear();
      m_Centroid.Fill(0);
      m_WeightedCentroid.Fill(0);
      m_ZeroOrderMoment = 0;
      m_FirstOrderRawMoments.Fill(0);
      m_FirstOrderWeightedRawMoments.Fill(0);
      m_Eigenvalues.resize(ImageDimension);
      m_Eigenvalues.clear();
      m_Eigenvectors.set_size(ImageDimension, ImageDimension);
      m_Eigenvectors.fill(0);
      m_AxesLength.Fill(0);
      m_Eccentricity = 1;
      m_Elongation = 1;
      m_Orientation = 0;
      LabelPointType emptyPoint;
      emptyPoint.Fill(0);
      unsigned int numberOfVertices = 1 << ImageDimension;
      m_OrientedBoundingBoxVertices.resize(numberOfVertices, emptyPoint);
      m_OrientedBoundingBoxVolume = 0;
      m_OrientedBoundingBoxSize.Fill(0);
      m_OrientedLabelImage = LabelImageType::New();
      m_OrientedIntensityImage = IntensityImageType::New();
      m_OrientedBoundingBoxOrigin.Fill(0);
      m_RotationMatrix.set_size(ImageDimension, ImageDimension);
      m_RotationMatrix.fill(0.0);

      m_SecondOrderRawMoments.set_size(ImageDimension, ImageDimension);
      m_SecondOrderCentralMoments.set_size(ImageDimension, ImageDimension);
      for (unsigned int i = 0; i < ImageDimension; i++)
      {
        for (unsigned int j = 0; j < ImageDimension; j++)
        {
          m_SecondOrderRawMoments(i, j) = 0;
          m_SecondOrderCentralMoments(i, j) = 0;
        }
      }
    }

    LabelPixelType                          m_Label;
    RealType                                m_Sum;
    LabelPointType                          m_Centroid;
    LabelPointType                          m_WeightedCentroid;
    SizeValueType                           m_ZeroOrderMoment;
    IndexArrayType                          m_FirstOrderRawMoments;
    IndexArrayType                          m_FirstOrderWeightedRawMoments;
    SizeValueType                           m_FirstOrderRawCrossMoment;
    RealType                                m_FirstOrderCentralCrossMoment;
    MatrixType                              m_SecondOrderRawMoments;
    MatrixType                              m_SecondOrderCentralMoments;
    VectorType                              m_Eigenvalues;
    MatrixType                              m_Eigenvectors;
    FixedArray<float, Self::ImageDimension> m_AxesLength;
    RealType                                m_Eccentricity;
    RealType                                m_Elongation;
    RealType                                m_Orientation;
    BoundingBoxType                         m_BoundingBox;
    LabelSizeType                           m_BoundingBoxSize;
    RealType                                m_BoundingBoxVolume;
    LabelIndicesType                        m_PixelIndices;
    BoundingBoxVerticesType                 m_OrientedBoundingBoxVertices;
    RealType                                m_OrientedBoundingBoxVolume;
    LabelPointType                          m_OrientedBoundingBoxSize;
    typename LabelImageType::Pointer        m_OrientedLabelImage;
    typename IntensityImageType::Pointer    m_OrientedIntensityImage;
    MatrixType                              m_RotationMatrix;
    LabelPointType                          m_OrientedBoundingBoxOrigin;
  };

  /** Type of the map used to store data per label */
  // Map from the label to the class storing all of the geometry information.
  using MapType = std::map<LabelPixelType, LabelGeometry>;
  using MapIterator = typename std::map<LabelPixelType, LabelGeometry>::iterator;
  using MapConstIterator = typename std::map<LabelPixelType, LabelGeometry>::const_iterator;

  // Macros for enabling the calculation of additional features.
  itkGetMacro(CalculatePixelIndices, bool);
  itkBooleanMacro(CalculatePixelIndices);
  void
  SetCalculatePixelIndices(const bool value)
  {
    // CalculateOrientedBoundingBox, CalculateOrientedLabelImage, and
    // CalculateOrientedIntensityImage all need CalculatePixelIndices to be
    // turned
    // on if they are turned on.  So, CalculatePixelIndices cannot be
    // turned off if any of these flags are turned on.
    if (value == false)
    {
      if ((this->m_CalculateOrientedBoundingBox == true) || (this->m_CalculateOrientedLabelRegions == true) ||
          (this->m_CalculateOrientedIntensityRegions == true))
      {
        // We cannot change the value, so return.
        return;
      }
    }

    if (this->m_CalculatePixelIndices != value)
    {
      this->m_CalculatePixelIndices = value;
      this->Modified();
    }
  }

  itkGetMacro(CalculateOrientedBoundingBox, bool);
  itkBooleanMacro(CalculateOrientedBoundingBox);
  void
  SetCalculateOrientedBoundingBox(const bool value)
  {
    if (this->m_CalculateOrientedBoundingBox != value)
    {
      this->m_CalculateOrientedBoundingBox = value;
      this->Modified();
    }

    // CalculateOrientedBoundingBox needs
    // CalculatePixelIndices to be turned on.
    if (value == true)
    {
      this->SetCalculatePixelIndices(true);
    }
  }

  itkGetMacro(CalculateOrientedLabelRegions, bool);
  itkBooleanMacro(CalculateOrientedLabelRegions);
  void
  SetCalculateOrientedLabelRegions(const bool value)
  {
    if (this->m_CalculateOrientedLabelRegions != value)
    {
      this->m_CalculateOrientedLabelRegions = value;
      this->Modified();

      // CalculateOrientedLabelImage needs
      // CalculateOrientedBoundingBox to be turned on.
      if (value == true)
      {
        SetCalculateOrientedBoundingBox(true);
      }
    }
  }

  itkGetMacro(CalculateOrientedIntensityRegions, bool);
  itkBooleanMacro(CalculateOrientedIntensityRegions);
  void
  SetCalculateOrientedIntensityRegions(const bool value)
  {
    if (this->m_CalculateOrientedIntensityRegions != value)
    {
      this->m_CalculateOrientedIntensityRegions = value;
      this->Modified();

      // CalculateOrientedIntensityImage needs
      // CalculateOrientedBoundingBox to be turned on.
      if (value == true)
      {
        this->SetCalculateOrientedBoundingBox(true);
      }
    }
  }

  /** Set the intensity image */
  void
  SetIntensityInput(const TIntensityImage * input)
  {
    // Process object is not const-correct so the const casting is required.
    this->SetNthInput(1, const_cast<TIntensityImage *>(input));
  }

  /** Get the label image */
  const TIntensityImage *
  GetIntensityInput() const
  {
    return static_cast<TIntensityImage *>(const_cast<DataObject *>(this->ProcessObject::GetInput(1)));
  }

  /** Does the specified label exist? Can only be called after
   * a call to Update(). */
  bool
  HasLabel(LabelPixelType label) const
  {
    return m_LabelGeometryMapper.find(label) != m_LabelGeometryMapper.end();
  }

  /** Get the number of labels used */
  SizeValueType
  GetNumberOfObjects() const
  {
    return m_LabelGeometryMapper.size();
  }

  SizeValueType
  GetNumberOfLabels() const
  {
    return this->GetNumberOfObjects();
  }

  /** Get the labels that are in the image. */
  std::vector<LabelPixelType>
  GetLabels() const
  {
    return m_AllLabels;
  }

  /** Return the all pixel indices for a label. */
  LabelIndicesType
  GetPixelIndices(LabelPixelType label) const;

  /** Return the number of pixels for a label.  This is the same as
   * the volume and the zero order moment */
  SizeValueType
  GetVolume(LabelPixelType label) const;

  /** Return the number of pixels for all labels. */
  // std::vector< SizeValueType > GetAllCounts() const;

  /** Return the computed integrated pixel intensity for a label. */
  RealType
  GetIntegratedIntensity(LabelPixelType label) const;

  /** Return the unweighted centroid for a label. */
  LabelPointType
  GetCentroid(LabelPixelType label) const;

  /** Return the weighted centroid for a label. */
  LabelPointType
  GetWeightedCentroid(LabelPixelType label) const;

  /** Return the eigenvalues as a vector. */
  VectorType
  GetEigenvalues(LabelPixelType label) const;

  /** Return the eigenvectors as a matrix. */
  MatrixType
  GetEigenvectors(LabelPixelType label) const;

  /** Return the axes length for a label. */
  AxesLengthType
  GetAxesLength(LabelPixelType label) const;

  /** Return the minor axis length for a label.  This is a convenience
   * class that returns the shortest length from GetAxesLength. */
  RealType
  GetMinorAxisLength(LabelPixelType label) const;

  /** Return the major axis length for a label.  This is a convenience
   * class that returns the longest length from GetAxesLength. */
  RealType
  GetMajorAxisLength(LabelPixelType label) const;

  /** Return the eccentricity for a label. */
  RealType
  GetEccentricity(LabelPixelType label) const;

  /** Return the elongation for a label.  This is defined as the
   * length of the major axis divided by the length of the minor axis. */
  RealType
  GetElongation(LabelPixelType label) const;

  /** Return the orientation for a label defined in radians. */
  RealType
  GetOrientation(LabelPixelType label) const;

  /** Return the computed bounding box for a label.
   * This is organized in min/max pairs as [min(X), max(X), min(Y),
   * max(Y), min(Z), max(Z),...]  */
  BoundingBoxType
  GetBoundingBox(LabelPixelType label) const;

  /** Return the volume of the bounding box. */
  RealType
  GetBoundingBoxVolume(LabelPixelType label) const;

  /** Return the size of the bounding box. */
  LabelSizeType
  GetBoundingBoxSize(LabelPixelType label) const;

  /** Return the oriented bounding box vertices.  The order of the
   * vertices corresponds with binary counting, where min is zero and
   * max is one.  For example, in 2D, binary counting will give
   * [0,0],[0,1],[1,0],[1,1], which corresponds to
   * [minX,minY],[minX,maxY],[maxX,minY],[maxX,maxY]. Each
   * vertex is defined as an ND point.   */
  BoundingBoxVerticesType
  GetOrientedBoundingBoxVertices(LabelPixelType label) const;

  /** Return the volume of the oriented bounding box. */
  RealType
  GetOrientedBoundingBoxVolume(LabelPixelType label) const;

  /** Return the size of the oriented bounding box. */
  LabelPointType
  GetOrientedBoundingBoxSize(LabelPixelType label) const;

  /** Return the origin of the oriented bounding box. */
  LabelPointType
  GetOrientedBoundingBoxOrigin(LabelPixelType label) const;

  /** Return the rotation matrix defined by the
   * eigenvalues/eigenvectors. */
  MatrixType
  GetRotationMatrix(LabelPixelType label) const;

  /** Return the region defined by the bounding box. */
  RegionType
  GetRegion(LabelPixelType label) const;

  /** Return the label region defined by the oriented bounding box. */
  TLabelImage *
  GetOrientedLabelImage(LabelPixelType label) const;

  /** Return the intensity region defined by the oriented bounding
   * box. */
  TIntensityImage *
  GetOrientedIntensityImage(LabelPixelType label) const;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<PixelType>));
  // End concept checking
#endif

protected:
  LabelGeometryImageFilter();
  ~LabelGeometryImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

private:
  bool
  CalculateOrientedBoundingBoxVertices(vnl_symmetric_eigensystem<double> eig, LabelGeometry & m_LabelGeometry);

  bool m_CalculatePixelIndices;
  bool m_CalculateOrientedBoundingBox;
  bool m_CalculateOrientedLabelRegions;
  bool m_CalculateOrientedIntensityRegions;

  MapType       m_LabelGeometryMapper;
  LabelGeometry m_LabelGeometry;
  LabelsType    m_AllLabels;
}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLabelGeometryImageFilter.hxx"
#endif

#endif
