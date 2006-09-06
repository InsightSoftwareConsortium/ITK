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

    itkImageFileReaderIUS2_Pointer reader = itkImageFileReaderIUS2.itkImageFileReaderIUS2_New();
    itkImageFileWriterIUS2_Pointer writer = itkImageFileWriterIUS2.itkImageFileWriterIUS2_New();

    itkMeanImageFilterIUS2IUS2_Pointer filter = itkMeanImageFilterIUS2IUS2.itkMeanImageFilterIUS2IUS2_New();

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


