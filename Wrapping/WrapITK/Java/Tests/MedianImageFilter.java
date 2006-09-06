/**
 *  Example on the use of the MedianImageFilter
 *
 */

import InsightToolkit.*;

public class MedianImageFilter
{
  public static void main( String argv[] )
  {
    System.out.println("MedianImageFilter Example");

    itkImageFileReaderIUS2_Pointer reader = itkImageFileReaderIUS2.itkImageFileReaderIUS2_New();
    itkImageFileWriterIUS2_Pointer writer = itkImageFileWriterIUS2.itkImageFileWriterIUS2_New();

    itkMedianImageFilterIUS2IUS2_Pointer filter = itkMedianImageFilterIUS2IUS2.itkMedianImageFilterIUS2IUS2_New();

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


