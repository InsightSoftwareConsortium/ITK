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

    itkImageFileReaderUC2_Pointer reader = itkImageFileReaderUC2.itkImageFileReaderUC2_New();
    itkImageFileWriterUS2_Pointer writer = itkImageFileWriterUS2.itkImageFileWriterUS2_New();

    itkCastImageFilterUC2US2_Pointer filter = itkCastImageFilterUC2US2.itkCastImageFilterUC2US2_New();

    filter.SetInput( reader.GetOutput() );
    writer.SetInput( filter.GetOutput() );

    reader.SetFileName( argv[0] );
    writer.SetFileName( argv[1] );

    writer.Update();
  }

}


