/**
 *  Example on the use of the CannyEdgeDetectionImageFilter
 *
 */

import InsightToolkit.*;


public class CannyEdgeDetectionImageFilter
{
  public static void main( String argv[] )
  {
    itkImageFileReaderF2_Pointer reader = itkImageFileReaderF2.itkImageFileReaderF2_New();
    itkImageFileWriterUC2_Pointer writer = itkImageFileWriterUC2.itkImageFileWriterUC2_New();

    itkCannyEdgeDetectionImageFilterF2F2_Pointer filter = itkCannyEdgeDetectionImageFilterF2F2.itkCannyEdgeDetectionImageFilterF2F2_New();

    itkRescaleIntensityImageFilterF2UC2_Pointer outputCast = itkRescaleIntensityImageFilterF2UC2.itkRescaleIntensityImageFilterF2UC2_New();

    filter.SetInput( reader.GetOutput() );
    outputCast.SetInput( filter.GetOutput() );
    writer.SetInput( outputCast.GetOutput() );

    reader.SetFileName( argv[0] );
    writer.SetFileName( argv[1] );

    short outputMinimum = 0;
    short outputMaximum = 0;

    outputCast.SetOutputMinimum( outputMinimum );
    outputCast.SetOutputMaximum( outputMaximum );

    filter.SetVariance(  Float.parseFloat( argv[2] ) );
    filter.SetThreshold( Float.parseFloat( argv[3] ) );

    writer.Update();
  }

}


