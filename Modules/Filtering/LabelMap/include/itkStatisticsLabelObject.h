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
#ifndef itkStatisticsLabelObject_h
#define itkStatisticsLabelObject_h

#include "itkShapeLabelObject.h"
#include "itkHistogram.h"

namespace itk
{
/**
 *\class StatisticsLabelObject
 *  \brief A Label object to store the common attributes related to the statistics of the object
 *
 * StatisticsLabelObject stores  the common attributes related to the statistics of the object
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \ingroup DataRepresentation
 * \ingroup ITKLabelMap
 */
template <typename TLabel, unsigned int VImageDimension>
class StatisticsLabelObject : public ShapeLabelObject<TLabel, VImageDimension>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(StatisticsLabelObject);

  /** Standard class type aliases */
  using Self = StatisticsLabelObject;
  using Superclass = ShapeLabelObject<TLabel, VImageDimension>;
  using LabelObjectType = typename Superclass::LabelObjectType;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using ConstWeakPointer = WeakPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(StatisticsLabelObject, LabelObject);

  using LabelMapType = LabelMap<Self>;

  static constexpr unsigned int ImageDimension = VImageDimension;

  using IndexType = typename Superclass::IndexType;

  using PointType = Point<double, Self::ImageDimension>;

  using LabelType = TLabel;

  using LineType = typename Superclass::LineType;

  using LengthType = typename Superclass::LengthType;

  using MatrixType = Matrix<double, Self::ImageDimension, Self::ImageDimension>;

  using VectorType = Vector<double, Self::ImageDimension>;

  using HistogramType = Statistics::Histogram<double>;

  using AttributeType = typename Superclass::AttributeType;
  static constexpr AttributeType MINIMUM = 200;
  static constexpr AttributeType MAXIMUM = 201;
  static constexpr AttributeType MEAN = 202;
  static constexpr AttributeType SUM = 203;
  static constexpr AttributeType STANDARD_DEVIATION = 204;
  static constexpr AttributeType VARIANCE = 205;
  static constexpr AttributeType MEDIAN = 206;
  static constexpr AttributeType MAXIMUM_INDEX = 207;
  static constexpr AttributeType MINIMUM_INDEX = 208;
  static constexpr AttributeType CENTER_OF_GRAVITY = 209;
  //  static constexpr AttributeType CENTRAL_MOMENTS = 210;
  static constexpr AttributeType WEIGHTED_PRINCIPAL_MOMENTS = 211;
  static constexpr AttributeType WEIGHTED_PRINCIPAL_AXES = 212;
  static constexpr AttributeType KURTOSIS = 213;
  static constexpr AttributeType SKEWNESS = 214;
  static constexpr AttributeType WEIGHTED_ELONGATION = 215;
  static constexpr AttributeType HISTOGRAM = 216;
  static constexpr AttributeType WEIGHTED_FLATNESS = 217;

  static AttributeType
  GetAttributeFromName(const std::string & s)
  {
    if (s == "Minimum")
    {
      return MINIMUM;
    }
    else if (s == "Maximum")
    {
      return MAXIMUM;
    }
    else if (s == "Mean")
    {
      return MEAN;
    }
    else if (s == "Sum")
    {
      return SUM;
    }
    else if (s == "StandardDeviation")
    {
      return STANDARD_DEVIATION;
    }
    else if (s == "Variance")
    {
      return VARIANCE;
    }
    else if (s == "Median")
    {
      return MEDIAN;
    }
    else if (s == "MaximumIndex")
    {
      return MAXIMUM_INDEX;
    }
    else if (s == "MinimumIndex")
    {
      return MINIMUM_INDEX;
    }
    else if (s == "CenterOfGravity")
    {
      return CENTER_OF_GRAVITY;
    }
    /*
    else if( s == "CentralMoments" )
      {
      return CENTRAL_MOMENTS;
      }
    */
    else if (s == "WeightedPrincipalMoments")
    {
      return WEIGHTED_PRINCIPAL_MOMENTS;
    }
    else if (s == "WeightedPrincipalAxes")
    {
      return WEIGHTED_PRINCIPAL_AXES;
    }
    else if (s == "Kurtosis")
    {
      return KURTOSIS;
    }
    else if (s == "Skewness")
    {
      return SKEWNESS;
    }
    else if (s == "WeightedElongation")
    {
      return WEIGHTED_ELONGATION;
    }
    else if (s == "Histogram")
    {
      return HISTOGRAM;
    }
    else if (s == "WeightedFlatness")
    {
      return WEIGHTED_FLATNESS;
    }
    // can't recognize the name
    return Superclass::GetAttributeFromName(s);
  }

  static std::string
  GetNameFromAttribute(const AttributeType & a)
  {
    switch (a)
    {
      case MINIMUM:
        return "Minimum";
        break;
      case MAXIMUM:
        return "Maximum";
        break;
      case MEAN:
        return "Mean";
        break;
      case SUM:
        return "Sum";
        break;
      case STANDARD_DEVIATION:
        return "StandardDeviation";
        break;
      case VARIANCE:
        return "Variance";
        break;
      case MEDIAN:
        return "Median";
        break;
      case MAXIMUM_INDEX:
        return "MaximumIndex";
        break;
      case MINIMUM_INDEX:
        return "MinimumIndex";
        break;
      case CENTER_OF_GRAVITY:
        return "CenterOfGravity";
        break;
      /*      case CENTRAL_MOMENTS:
              return "CentralMoments";
              break;*/
      case WEIGHTED_PRINCIPAL_MOMENTS:
        return "WeightedPrincipalMoments";
        break;
      case WEIGHTED_PRINCIPAL_AXES:
        return "WeightedPrincipalAxes";
        break;
      case KURTOSIS:
        return "Kurtosis";
        break;
      case SKEWNESS:
        return "Skewness";
        break;
      case WEIGHTED_ELONGATION:
        return "WeightedElongation";
        break;
      case HISTOGRAM:
        return "Histogram";
        break;
      case WEIGHTED_FLATNESS:
        return "WeightedFlatness";
        break;
    }
    // can't recognize the name
    return Superclass::GetNameFromAttribute(a);
  }

  using RegionType = ImageRegion<Self::ImageDimension>;

  using CentroidType = typename Superclass::CentroidType;

  template <typename TSourceLabelObject>
  void
  CopyAttributesFrom(const TSourceLabelObject * src)
  {
    Superclass::template CopyAttributesFrom<TSourceLabelObject>(src);

    m_Minimum = src->GetMinimum();
    m_Maximum = src->GetMaximum();
    m_Mean = src->GetMean();
    m_Sum = src->GetSum();
    m_StandardDeviation = src->GetStandardDeviation();
    m_Variance = src->GetVariance();
    m_Median = src->GetMedian();
    m_MaximumIndex = src->GetMaximumIndex();
    m_MinimumIndex = src->GetMinimumIndex();
    m_CenterOfGravity = src->GetCenterOfGravity();
    // m_CentralMoments = src->GetCentralMoments();
    m_WeightedPrincipalMoments = src->GetWeightedPrincipalMoments();
    m_WeightedPrincipalAxes = src->GetWeightedPrincipalAxes();
    m_Kurtosis = src->GetKurtosis();
    m_Skewness = src->GetSkewness();
    m_WeightedElongation = src->GetWeightedElongation();
    m_Histogram = src->GetHistogram();
    m_WeightedFlatness = src->GetWeightedFlatness();
  }

  template <typename TSourceLabelObject>
  void
  CopyAllFrom(const TSourceLabelObject * src)
  {
    itkAssertOrThrowMacro((src != nullptr), "Null Pointer");
    this->template CopyLinesFrom<TSourceLabelObject>(src);
    this->template CopyAttributesFrom<TSourceLabelObject>(src);
  }

  const double &
  GetMinimum() const
  {
    return m_Minimum;
  }

  void
  SetMinimum(const double & v)
  {
    m_Minimum = v;
  }

  const double &
  GetMaximum() const
  {
    return m_Maximum;
  }

  void
  SetMaximum(const double & v)
  {
    m_Maximum = v;
  }

  const double &
  GetMean() const
  {
    return m_Mean;
  }

  void
  SetMean(const double & v)
  {
    m_Mean = v;
  }

  const double &
  GetSum() const
  {
    return m_Sum;
  }

  void
  SetSum(const double & v)
  {
    m_Sum = v;
  }

  const double &
  GetStandardDeviation() const
  {
    return m_StandardDeviation;
  }

  void
  SetStandardDeviation(const double & v)
  {
    m_StandardDeviation = v;
  }

  const double &
  GetVariance() const
  {
    return m_Variance;
  }

  void
  SetVariance(const double & v)
  {
    m_Variance = v;
  }

  const double &
  GetMedian() const
  {
    return m_Median;
  }

  void
  SetMedian(const double & v)
  {
    m_Median = v;
  }

  const IndexType &
  GetMaximumIndex() const
  {
    return m_MaximumIndex;
  }

  void
  SetMaximumIndex(const IndexType & v)
  {
    m_MaximumIndex = v;
  }

  const IndexType &
  GetMinimumIndex() const
  {
    return m_MinimumIndex;
  }

  void
  SetMinimumIndex(const IndexType & v)
  {
    m_MinimumIndex = v;
  }

  const PointType &
  GetCenterOfGravity() const
  {
    return m_CenterOfGravity;
  }

  void
  SetCenterOfGravity(const PointType & v)
  {
    m_CenterOfGravity = v;
  }

  /*
  const MatrixType & GetCentralMoments() const
    {
    return m_CentralMoments;
    }

  void SetCentralMoments( const MatrixType & v )
    {
    m_CentralMoments = v;
    }*/

  const VectorType &
  GetWeightedPrincipalMoments() const
  {
    return m_WeightedPrincipalMoments;
  }

  void
  SetWeightedPrincipalMoments(const VectorType & v)
  {
    m_WeightedPrincipalMoments = v;
  }

  const MatrixType &
  GetWeightedPrincipalAxes() const
  {
    return m_WeightedPrincipalAxes;
  }

  void
  SetWeightedPrincipalAxes(const MatrixType & v)
  {
    m_WeightedPrincipalAxes = v;
  }

  const double &
  GetSkewness() const
  {
    return m_Skewness;
  }

  void
  SetSkewness(const double & v)
  {
    m_Skewness = v;
  }

  const double &
  GetKurtosis() const
  {
    return m_Kurtosis;
  }

  void
  SetKurtosis(const double & v)
  {
    m_Kurtosis = v;
  }

  const double &
  GetWeightedElongation() const
  {
    return m_WeightedElongation;
  }

  void
  SetWeightedElongation(const double & v)
  {
    m_WeightedElongation = v;
  }

  const HistogramType *
  GetHistogram() const
  {
    return m_Histogram;
  }

  void
  SetHistogram(const HistogramType * v)
  {
    m_Histogram = v;
  }

  const double &
  GetWeightedFlatness() const
  {
    return m_WeightedFlatness;
  }

  void
  SetWeightedFlatness(const double & v)
  {
    m_WeightedFlatness = v;
  }

  // some helper methods - not really required, but really useful!
  /** Affine transform for mapping to and from principal axis */
  using AffineTransformType = AffineTransform<double, Self::ImageDimension>;
  using AffineTransformPointer = typename AffineTransformType::Pointer;

  /** Get the affine transform from principal axes to physical axes
   * This method returns an affine transform which transforms from
   * the principal axes coordinate system to physical coordinates. */
  AffineTransformPointer
  GetWeightedPrincipalAxesToPhysicalAxesTransform() const
  {
    typename AffineTransformType::MatrixType matrix;
    typename AffineTransformType::OffsetType offset;
    for (unsigned int i = 0; i < ImageDimension; i++)
    {
      offset[i] = m_CenterOfGravity[i];
      for (unsigned int j = 0; j < ImageDimension; j++)
      {
        matrix[j][i] = m_WeightedPrincipalAxes[i][j]; // Note the transposition
      }
    }

    AffineTransformPointer result = AffineTransformType::New();

    result->SetMatrix(matrix);
    result->SetOffset(offset);

    return result;
  }

  /** Get the affine transform from physical axes to principal axes
   * This method returns an affine transform which transforms from
   * the physical coordinate system to the principal axes coordinate
   * system. */
  AffineTransformPointer
  GetPhysicalAxesToWeightedPrincipalAxesTransform() const
  {
    typename AffineTransformType::MatrixType matrix;
    typename AffineTransformType::OffsetType offset;
    for (unsigned int i = 0; i < ImageDimension; i++)
    {
      offset[i] = m_CenterOfGravity[i];
      for (unsigned int j = 0; j < ImageDimension; j++)
      {
        matrix[j][i] = m_WeightedPrincipalAxes[i][j]; // Note the transposition
      }
    }

    AffineTransformPointer result = AffineTransformType::New();
    result->SetMatrix(matrix);
    result->SetOffset(offset);

    AffineTransformPointer inverse = AffineTransformType::New();
    result->GetInverse(inverse);

    return inverse;
  }

protected:
  StatisticsLabelObject()
  {
    m_Minimum = 0;
    m_Maximum = 0;
    m_Mean = 0;
    m_Sum = 0;
    m_StandardDeviation = 0;
    m_Variance = 0;
    m_Median = 0;
    m_MaximumIndex.Fill(0);
    m_MinimumIndex.Fill(0);
    m_CenterOfGravity.Fill(0);
    // m_CentralMoments.Fill(0);
    m_WeightedPrincipalMoments.Fill(0);
    m_WeightedPrincipalAxes.Fill(0);
    m_Kurtosis = 0;
    m_Skewness = 0;
    m_WeightedElongation = 0;
    m_Histogram = nullptr;
    m_WeightedFlatness = 0;
  }

  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);

    os << indent << "Minimum: " << m_Minimum << std::endl;
    os << indent << "Maximum: " << m_Maximum << std::endl;
    os << indent << "Mean: " << m_Mean << std::endl;
    os << indent << "Sum: " << m_Sum << std::endl;
    os << indent << "StandardDeviation: " << m_StandardDeviation << std::endl;
    os << indent << "Variance: " << m_Variance << std::endl;
    os << indent << "Median: " << m_Median << std::endl;
    os << indent << "Skewness: " << m_Skewness << std::endl;
    os << indent << "Kurtosis: " << m_Kurtosis << std::endl;
    os << indent << "WeightedElongation: " << m_WeightedElongation << std::endl;
    os << indent << "WeightedFlatness: " << m_WeightedFlatness << std::endl;
    os << indent << "MaximumIndex: " << m_MaximumIndex << std::endl;
    os << indent << "MinimumIndex: " << m_MinimumIndex << std::endl;
    os << indent << "CenterOfGravity: " << m_CenterOfGravity << std::endl;
    // os << indent << "CentralMoments: " << std::endl << m_CentralMoments;
    os << indent << "WeightedPrincipalMoments: " << m_WeightedPrincipalMoments << std::endl;
    os << indent << "WeightedPrincipalAxes: " << std::endl << m_WeightedPrincipalAxes;
    itkPrintSelfObjectMacro(Histogram);
  }

private:
  double    m_Minimum;
  double    m_Maximum;
  double    m_Mean;
  double    m_Sum;
  double    m_StandardDeviation;
  double    m_Variance;
  double    m_Median;
  IndexType m_MaximumIndex;
  IndexType m_MinimumIndex;
  PointType m_CenterOfGravity;
  // MatrixType m_CentralMoments;
  VectorType m_WeightedPrincipalMoments;
  MatrixType m_WeightedPrincipalAxes;
  double     m_Skewness;
  double     m_Kurtosis;
  double     m_WeightedElongation;

  typename HistogramType::ConstPointer m_Histogram;

  double m_WeightedFlatness;
};
} // end namespace itk

#endif
