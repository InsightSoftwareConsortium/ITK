#ifndef __itkBloxBoundaryProfileItem_h
#define __itkBloxBoundaryProfileItem_h

#include "vnl/vnl_vector_fixed.h"
#include "itkBloxBoundaryPointItem.h"
#include "itkBloxItem.h"

namespace itk
{
   
template <unsigned int VImageDimension>
class BloxBoundaryProfileItem: public BloxItem
{
public:
   /** The point type used to store the position of the boundary profile */
   typedef Point<double, VImageDimension> PositionType;

   /** The type of vector used to store the gradient of the BoundaryPointItem * */
   typedef CovariantVector<double, VImageDimension> GradientType;

   /** Vector type */
   typedef vnl_vector<double> VectorType;

   /** The type of boundary point item we store pointers to */
   typedef BloxBoundaryPointItem<VImageDimension> BPItemType;
  
   /** Set the position of the first boundary point in physical space */
   void SetBoundaryPoint(BPItemType* point)
   { m_BoundaryPoint = point; }

   /** Set and get lower intensity estimates */
   void SetLowerIntensity(double lowerIntensity)
   { m_LowerIntensity = lowerIntensity; }

   double GetLowerIntensity(void)
   { return(m_LowerIntensity); }

   /** Set and get upper intensity estimates */
   void SetUpperIntensity(double upperIntensity)
   { m_UpperIntensity = upperIntensity; }

   double GetUpperIntensity(void)
   { return(m_UpperIntensity); }

   /** Set and get mean estimates */
   void SetMean(double mean)
   { m_Mean = mean; }

   double GetMean(void)
   { return(m_Mean); }

   /** Set and get boundary width estimates */
   void SetBoundaryWidth(double boundaryWidth)
   { m_BoundaryWidth = boundaryWidth; }
   
   double GetBoundaryWidth(void)
   { return(m_BoundaryWidth); }

   /** Set and get the length of profile */
   void SetProfileLength(unsigned int profileLength)
   { m_ProfileLength = profileLength; }

   unsigned int GetProfileLength(void)
   { return(m_ProfileLength); }

   /** Set and get mean normalized by profile length */
   void SetMeanNormalized(void)
   { m_MeanNormalized = m_Mean - m_ProfileLength/2; }

   double GetMeanNormalized(void)
   { return(m_MeanNormalized); }
      
   /** Set and get standard deviation */
   void SetStandardDeviation(double standardDeviation)
   { m_StandardDeviation = standardDeviation; }

   double GetStandardDeviation(void)
   { return(m_StandardDeviation); }

   /** Set and get standard deviation normalized by profile length */
   void SetStandardDeviationNormalized(void)
   { m_StandardDeviationNormalized = m_StandardDeviation / m_ProfileLength; }

   double GetStandardDeviationNormalized(double)
   { return(m_StandardDeviationNormalized); }

   /** Set and get optimal boundary location */
   void SetOptimalBoundaryLocation(VectorType spatialFunctionOriginVector, VectorType orientation)
      {
      VectorType optimalBoundaryLocation;
      optimalBoundaryLocation = m_MeanNormalized * orientation;
      optimalBoundaryLocation = spatialFunctionOriginVector + optimalBoundaryLocation;
      for(unsigned int i = 0; i < VImageDimension; i++)
         { m_OptimalBoundaryLocation[i] = optimalBoundaryLocation[i]; }
      }

   PositionType GetOptimalBoundaryLocation(void)
   { return(m_OptimalBoundaryLocation); }

   /** Set the gradient of the boundary profile * */
   void SetGradient(GradientType grad){m_Gradient = grad;};

   /** Get the gradient of the boundary profile * */
   GradientType GetGradient(){return m_Gradient;};

   BloxBoundaryProfileItem();
   ~BloxBoundaryProfileItem();

private:

   double       m_LowerIntensity;
   double       m_UpperIntensity;
   double       m_Mean;
   double       m_BoundaryWidth;
   unsigned int m_ProfileLength;
   double       m_MeanNormalized;
   double       m_StandardDeviation;
   double       m_StandardDeviationNormalized;
   
   /** The position of the estimated boundary location */
   PositionType m_OptimalBoundaryLocation;

   /** The gradient of the boundary point (non-normalized) * */
   GradientType m_Gradient;
};

} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBloxBoundaryProfileItem.txx"
#endif

#endif
