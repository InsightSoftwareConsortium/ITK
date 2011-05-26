
def hist(input, bins=256, log=True, xrange=None, yrange=None, title="WrapITK histogram"):
  """Draw the histogram of the input image using http://gnuplot-py.sourceforge.net/ (http://gnuplot-py.sourceforge.net/).
  """
  import Gnuplot, itk, numpy, math
  img = itk.image(input)
  a = itk.PyBuffer[img].GetArrayFromImage(img)
  hist = numpy.histogram(a, bins=bins)
  x = hist[1]
  y = hist[0]
  if log:
    y = map(math.log10, y)
  if xrange == None:
    xrange = ( min(x)-1, max(x)+1 )
  g = Gnuplot.Gnuplot()
  g.title( title )
  g.xlabel('Pixel values')
  if log:
    g.ylabel('log10( freq )')
  else:
    g.ylabel('freq')
  g('set data style boxes')
  g('set style fill solid border -1')
  g.set_range( "xrange", xrange )
  if yrange != None:
    g.set_range( "yrange", yrange )
  g.plot( zip( x, y ) )
  return g
