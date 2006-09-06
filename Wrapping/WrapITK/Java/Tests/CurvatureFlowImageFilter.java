/**
 *  Example on the use of the CurvatureFlowImageFilter
 *
 */

import InsightToolkit.*;


public class CurvatureFlowImageFilter
{
  public static void main( String argv[] )
  {
    itkImageFileReaderIUS2_Pointer reader = itkImageFileReaderIUS2.itkImageFileReaderIUS2_New();
    itkImageFileWriterIUS2_Pointer writer = itkImageFileWriterIUS2.itkImageFileWriterIUS2_New();

    itkCastImageFilterIUS2IF2_Pointer inputCast = itkCastImageFilterIUS2IF2.itkCastImageFilterIUS2IF2_New();
      
    itkCurvatureFlowImageFilterIF2IF2_Pointer filter = itkCurvatureFlowImageFilterIF2IF2.itkCurvatureFlowImageFilterIF2IF2_New();

    itkRescaleIntensityImageFilterIF2IUS2_Pointer outputCast = itkRescaleIntensityImageFilterIF2IUS2.itkRescaleIntensityImageFilterIF2IUS2_New();

    inputCast.SetInput( reader.GetOutput() );
    filter.SetInput( inputCast.GetOutput() );
    outputCast.SetInput( filter.GetOutput() );
    writer.SetInput( outputCast.GetOutput() );

    outputCast.SetOutputMinimum(  0  );
    outputCast.SetOutputMaximum( 255 );

    filter.SetNumberOfIterations( Integer.parseInt( argv[2] ) );
    filter.SetTimeStep(           Float.parseFloat( argv[3] ) );

    reader.SetFileName( argv[0] );
    writer.SetFileName( argv[1] );

    writer.Update();
  }

}


