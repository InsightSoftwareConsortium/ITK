/**
 *  Example on the use of the MeanImageFilter
 *
 */

import InsightToolkit.*;

public class MeanImageFilter
{
  public static void main( String argv[] )
  {
    System.out.println("MeanImageFilter Example");

    itkImageFileReaderUS2_Pointer reader = itkImageFileReaderUS2.itkImageFileReaderUS2_New();
    itkImageFileWriterUS2_Pointer writer = itkImageFileWriterUS2.itkImageFileWriterUS2_New();

    itkMeanImageFilterUS2US2_Pointer filter = itkMeanImageFilterUS2US2.itkMeanImageFilterUS2US2_New();

    filter.SetInput( reader.GetOutput() );
    writer.SetInput( filter.GetOutput() );

    reader.SetFileName( argv[0] );
    writer.SetFileName( argv[1] );


    int radius = Integer.parseInt( argv[2] );

    itkSize2 sizeRadius = new itkSize2();

    sizeRadius.SetElement( 0, radius );
    sizeRadius.SetElement( 1, radius );

    filter.SetRadius( sizeRadius );

    writer.Update();
  }

}


