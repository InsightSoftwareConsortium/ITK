/**
 *  Example on the use of the CastImageFilter
 *
 */

import InsightToolkit.*;

public class CastImageFilter
{
  public static void main( String argv[] )
  {
    System.out.println("CastImageFilter Example");

    itkImageFileReaderIUC2_Pointer reader = itkImageFileReaderIUC2.itkImageFileReaderIUC2_New();
    itkImageFileWriterIUS2_Pointer writer = itkImageFileWriterIUS2.itkImageFileWriterIUS2_New();

    itkCastImageFilterIUC2IUS2_Pointer filter = itkCastImageFilterIUC2IUS2.itkCastImageFilterIUC2IUS2_New();

    filter.SetInput( reader.GetOutput() );
    writer.SetInput( filter.GetOutput() );

    reader.SetFileName( argv[0] );
    writer.SetFileName( argv[1] );

    writer.Update();
  }

}


