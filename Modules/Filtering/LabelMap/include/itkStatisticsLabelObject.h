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
#ifndef itkStatisticsLabelObject_h
#define itkStatisticsLabelObject_h

#include "itkShapeLabelObject.h"
#include "itkHistogram.h"

namespace itk
{
/** \class StatisticsLabelObject
 *  \brief A Label object to store the common attributes related to the statistics of the object
 *
 * StatisticsLabelObject stores  the common attributes related to the statistics of the object
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \ingroup DataRepresentation
 * \ingroup ITKLabelMap
 */
template< typename TLabel, unsigned int VImageDimension >
class StatisticsLabelObject:public ShapeLabelObject< TLabel, VImageDimension >
{
public:
  /** Standard class typedefs */
  typedef StatisticsLabelObject                       Self;
  typedef ShapeLabelObject< TLabel, VImageDimension > Superclass;
  typedef typename Superclass::LabelObjectType        LabelObjectType;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;
  typedef WeakPointer< const Self >                   ConstWeakPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(StatisticsLabelObject, LabelObject);

  typedef LabelMap< Self > LabelMapType;

  itkStaticConstMacro(ImageDimension, unsigned int, VImageDimension);

  typedef typename Superclass::IndexType IndexType;

  typedef Point< double, itkGetStaticConstMacro(ImageDimension) > PointType;

  typedef TLabel LabelType;

  typedef typename Superclass::LineType LineType;

  typedef typename Superclass::LengthType LengthType;

  typedef Matrix< double, itkGetStaticConstMacro(ImageDimension), itkGetStaticConstMacro(ImageDimension) > MatrixType;

  typedef Vector< double, itkGetStaticConstMacro(ImageDimension) > VectorType;

  typedef Statistics::Histogram< double > HistogramType;

  typedef typename Superclass::AttributeType AttributeType;
  itkStaticConstMacro(MINIMUM, AttributeType, 200);
  itkStaticConstMacro(MAXIMUM, AttributeType, 201);
  itkStaticConstMacro(MEAN, AttributeType, 202);
  itkStaticConstMacro(SUM, AttributeType, 203);
  itkStaticConstMacro(STANDARD_DEVIATION, AttributeType, 204);
  itkStaticConstMacro(VARIANCE, AttributeType, 205);
  itkStaticConstMacro(MEDIAN, AttributeType, 206);
  itkStaticConstMacro(MAXIMUM_INDEX, AttributeType, 207);
  itkStaticConstMacro(MINIMUM_INDEX, AttributeType, 208);
  itkStaticConstMacro(CENTER_OF_GRAVITY, AttributeType, 209);
//  itkStaticConstMacro(CENTRAL_MOMENTS, AttributeType, 210);
  itkStaticConstMacro(WEIGHTED_PRINCIPAL_MOMENTS, AttributeType, 211);
  itkStaticConstMacro(WEIGHTED_PRINCIPAL_AXES, AttributeType, 212);
  itkStaticConstMacro(KURTOSIS, AttributeType, 213);
  itkStaticConstMacro(SKEWNESS, AttributeType, 214);
  itkStaticConstMacro(WEIGHTED_ELONGATION, AttributeType, 215);
  itkStaticConstMacro(HISTOGRAM, AttributeType, 216);
  itkStaticConstMacro(WEIGHTED_FLATNESS, AttributeType, 217);

  static AttributeType GetAttributeFromName(const std::string & s)
  {
    if ( s == "Minimum" )
      {
      return MINIMUM;
      }
    else if ( s == "Maximum" )
      {
      return MAXIMUM;
      }
    else if ( s == "Mean" )
      {
      return MEAN;
      }
    else if ( s == "Sum" )
      {
      return SUM;
      }
    else if ( s == "StandardDeviation" )
      {
      return STANDARD_DEVIATION;
      }
    else if ( s == "Variance" )
      {
      return VARIANCE;
      }
    else if ( s == "Median" )
      {
      return MEDIAN;
      }
    else if ( s == "MaximumIndex" )
      {
      return MAXIMUM_INDEX;
      }
    else if ( s == "MinimumIndex" )
      {
      return MINIMUM_INDEX;
      }
    else if ( s == "CenterOfGravity" )
      {
      return CENTER_OF_GRAVITY;
      }
    /*
    else if( s == "CentralMoments" )
      {
      return CENTRAL_MOMENTS;
      }
    */
    else if ( s == "WeightedPrincipalMoments" )
      {
      return WEIGHTED_PRINCIPAL_MOMENTS;
      }
    else if ( s == "WeightedPrincipalAxes" )
      {
      return WEIGHTED_PRINCIPAL_AXES;
      }
    else if ( s == "Kurtosis" )
      {
      return KURTOSIS;
      }
    else if ( s == "Skewness" )
      {
      return SKEWNESS;
      }
    else if ( s == "WeightedElongation" )
      {
      return WEIGHTED_ELONGATION;
      }
    else if ( s == "Histogram" )
      {
      return HISTOGRAM;
      }
    else if ( s == "WeightedFlatness" )
      {
      return WEIGHTED_FLATNESS;
      }
    // can't recognize the name
    return Superclass::GetAttributeFromName(s);
  }

  static std::string GetNameFromAttribute(const AttributeType & a)
  {
    switch ( a )
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

  typedef ImageRegion< itkGetStaticConstMacro(ImageDimension) > RegionType;

  typedef typename Superclass::CentroidType CentroidType;

  template< typename TSourceLabelObject >
  void CopyAttributesFrom( const TSourceLabelObject * src )
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

  template< typename TSourceLabelObject >
  void CopyAllFrom(const TSourceLabelObject *src)
  {
    itkAssertOrThrowMacro ( ( src != ITK_NULLPTR ), "Null Pointer" );
    this->template CopyLinesFrom<TSourceLabelObject>( src );
    this->template CopyAttributesFrom<TSourceLabelObject>( src );
  }

  const double & GetMinimum() const
  {
    return m_Minimum;
  }

  void SetMinimum(const double & v)
  {
    m_Minimum = v;
  }

  const double & GetMaximum() const
  {
    return m_Maximum;
  }

  void SetMaximum(const double & v)
  {
    m_Maximum = v;
  }

  const double & GetMean() const
  {
    return m_Mean;
  }

  void SetMean(const double & v)
  {
    m_Mean = v;
  }

  const double & GetSum() const
  {
    return m_Sum;
  }

  void SetSum(const double & v)
  {
    m_Sum = v;
  }

  const double & GetStandardDeviation() const
  {
    return m_StandardDeviation;
  }

  void SetStandardDeviation(const double & v)
  {
    m_StandardDeviation = v;
  }

  const double & GetVariance() const
  {
    return m_Variance;
  }

  void SetVariance(const double & v)
  {
    m_Variance = v;
  }

  const double & GetMedian() const
  {
    return m_Median;
  }

  void SetMedian(const double & v)
  {
    m_Median = v;
  }

  const IndexType & GetMaximumIndex() const
  {
    return m_MaximumIndex;
  }

  void SetMaximumIndex(const IndexType & v)
  {
    m_MaximumIndex = v;
  }

  const IndexType & GetMinimumIndex() const
  {
    return m_MinimumIndex;
  }

  void SetMinimumIndex(const IndexType & v)
  {
    m_MinimumIndex = v;
  }

  const PointType & GetCenterOfGravity() const
  {
    return m_CenterOfGravity;
  }

  void SetCenterOfGravity(const PointType & v)
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

  const VectorType & GetWeightedPrincipalMoments() const
  {
    return m_WeightedPrincipalMoments;
  }

  void SetWeightedPrincipalMoments(const VectorType & v)
  {
    m_WeightedPrincipalMoments = v;
  }

  const MatrixType & GetWeightedPrincipalAxes() const
  {
    return m_WeightedPrincipalAxes;
  }

  void SetWeightedPrincipalAxes(const MatrixType & v)
  {
    m_WeightedPrincipalAxes = v;
  }

  const double & GetSkewness() const
  {
    return m_Skewness;
  }

  void SetSkewness(const double & v)
  {
    m_Skewness = v;
  }

  const double & GetKurtosis() const
  {
    return m_Kurtosis;
  }

  void SetKurtosis(const double & v)
  {
    m_Kurtosis = v;
  }

  const double & GetWeightedElongation() const
  {
    return m_WeightedElongation;
  }

  void SetWeightedElongation(const double & v)
  {
    m_WeightedElongation = v;
  }

  const HistogramType * GetHistogram() const
  {
    return m_Histogram;
  }

  void SetHistogram(const HistogramType *v)
  {
    m_Histogram = v;
  }

  const double & GetWeightedFlatness() const
  {
    return m_WeightedFlatness;
  }

  void SetWeightedFlatness(const double & v)
  {
    m_WeightedFlatness = v;
  }

  // some helper methods - not really required, but really useful!
  /** Affine transform for mapping to and from principal axis */
  typedef AffineTransform< double, itkGetStaticConstMacro(ImageDimension) > AffineTransformType;
  typedef typename AffineTransformType::Pointer                             AffineTransformPointer;

  /** Get the affine transform from principal axes to physical axes
   * This method returns an affine transform which transforms from
   * the principal axes coordinate system to physical coordinates. */
  AffineTransformPointer GetWeightedPrincipalAxesToPhysicalAxesTransform() const
  {
    typename AffineTransformType::MatrixType matrix;
    typename AffineTransformType::OffsetType offset;
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      offset[i]  = m_CenterOfGravity[i];
      for ( unsigned int j = 0; j < ImageDimension; j++ )
        {
        matrix[j][i] = m_WeightedPrincipalAxes[i][j];    // Note the transposition
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
  AffineTransformPointer GetPhysicalAxesToWeightedPrincipalAxesTransform(void) const
  {
    typename AffineTransformType::MatrixType matrix;
    typename AffineTransformType::OffsetType offset;
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      offset[i]    = m_CenterOfGravity[i];
      for ( unsigned int j = 0; j < ImageDimension; j++ )
        {
        matrix[j][i] = m_WeightedPrincipalAxes[i][j];    // Note the transposition
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
    m_Histogram = ITK_NULLPTR;
    m_WeightedFlatness = 0;
  }

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
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
    itkPrintSelfObjectMacro( Histogram );
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(StatisticsLabelObject);

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
