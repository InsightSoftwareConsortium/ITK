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

    itkImageFileReaderUS2_Pointer reader = itkImageFileReaderUS2.itkImageFileReaderUS2_New();
    itkImageFileWriterUS2_Pointer writer = itkImageFileWriterUS2.itkImageFileWriterUS2_New();

    itkMedianImageFilterUS2US2_Pointer filter = itkMedianImageFilterUS2US2.itkMedianImageFilterUS2US2_New();

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


