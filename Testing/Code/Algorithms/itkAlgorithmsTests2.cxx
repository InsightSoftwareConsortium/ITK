// this file defines the itkBasicFiltersTest for the test driver
// and all it expects is that you have a function called RegisterTests

#include "vnl/vnl_sample.h"
#include "itkTestMain.h" 


void RegisterTests()
{
  vnl_sample_reseed(8775070);
  REGISTER_TEST(itkAlgorithmsPrintTest  );
  REGISTER_TEST(itkAlgorithmsPrintTest2  );
  REGISTER_TEST(itkAlgorithmsPrintTest3  );
  REGISTER_TEST(itkCompareHistogramImageToImageMetricTest   );
  REGISTER_TEST(itkKullbackLieblerCompareHistogramImageToImageMetricTest );
  REGISTER_TEST(itkMIRegistrationFunctionTest );
  REGISTER_TEST(itkPointSetToPointSetRegistrationTest );
  REGISTER_TEST(itkVectorThresholdSegmentationLevelSetImageFilterTest );
  REGISTER_TEST(itkVectorFuzzyConnectednessImageFilterTest );
  REGISTER_TEST(itkVoronoiDiagram2DTest );
  REGISTER_TEST(itkVoronoiSegmentationImageFilterTest );
  REGISTER_TEST(itkVoronoiPartitioningImageFilterTest );
  REGISTER_TEST(itkWatershedImageFilterTest );
  REGISTER_TEST(itkVnlFFTTest);
#if defined(USE_FFTW)
  REGISTER_TEST(itkFFTWFFTTest);
#endif
#if defined(USE_SCSL)
  REGISTER_TEST(itkSCSLFFTTest);
#endif
}

