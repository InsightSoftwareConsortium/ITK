/**
 *  Example on the use of the GradientAnisotropicDiffusionImageFilter
 *
 */

import InsightToolkit.*;


public class GradientAnisotropicDiffusionImageFilter
{
  public static void main( String argv[] )
  {
    itkImageFileReaderIUS2_Pointer reader = itkImageFileReaderIUS2.itkImageFileReaderIUS2_New();
    itkImageFileWriterIUS2_Pointer writer = itkImageFileWriterIUS2.itkImageFileWriterIUS2_New();

    itkCastImageFilterIUS2IF2_Pointer inputCast = itkCastImageFilterIUS2IF2.itkCastImageFilterIUS2IF2_New();
      
    itkGradientAnisotropicDiffusionImageFilterIF2IF2_Pointer filter = itkGradientAnisotropicDiffusionImageFilterIF2IF2.itkGradientAnisotropicDiffusionImageFilterIF2IF2_New();

    itkRescaleIntensityImageFilterIF2IUS2_Pointer outputCast = itkRescaleIntensityImageFilterIF2IUS2.itkRescaleIntensityImageFilterIF2IUS2_New();

    inputCast.SetInput( reader.GetOutput() );
    filter.SetInput( inputCast.GetOutput() );
    outputCast.SetInput( filter.GetOutput() );
    writer.SetInput( outputCast.GetOutput() );

    outputCast.SetOutputMinimum(  0  );
    outputCast.SetOutputMaximum( 255 );

    filter.SetNumberOfIterations(   Integer.parseInt( argv[2] ) );
    filter.SetTimeStep(             Float.parseFloat( argv[3] ) );
    filter.SetConductanceParameter( Float.parseFloat( argv[4] ) );

    reader.SetFileName( argv[0] );
    writer.SetFileName( argv[1] );

    writer.Update();
  }

}


