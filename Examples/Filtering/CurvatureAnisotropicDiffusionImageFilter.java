/**
 *  Example on the use of the CurvatureAnisotropicDiffusionImageFilter
 *
 */

import InsightToolkit.*;


public class CurvatureAnisotropicDiffusionImageFilter
{
  public static void main( String argv[] )
  {
    itkImageFileReaderUS2_Pointer reader = itkImageFileReaderUS2.itkImageFileReaderUS2_New();
    itkImageFileWriterUS2_Pointer writer = itkImageFileWriterUS2.itkImageFileWriterUS2_New();

    itkCastImageFilterUS2F2_Pointer inputCast = itkCastImageFilterUS2F2.itkCastImageFilterUS2F2_New();
      
    itkCurvatureAnisotropicDiffusionImageFilterF2F2_Pointer filter = itkCurvatureAnisotropicDiffusionImageFilterF2F2.itkCurvatureAnisotropicDiffusionImageFilterF2F2_New();

    itkRescaleIntensityImageFilterF2US2_Pointer outputCast = itkRescaleIntensityImageFilterF2US2.itkRescaleIntensityImageFilterF2US2_New();

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


