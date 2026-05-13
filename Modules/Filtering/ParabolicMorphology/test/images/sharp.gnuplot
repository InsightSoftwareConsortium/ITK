set term postscript eps enhanced color
set xlabel "image index"
set ylabel "gray level"
plot[0:100][0:255] "../images/inputprof.txt" using 1:2 with linespoints title "Raw input",\
"../images/blurredprof.txt" using 1:2 with lines title "Blurred input",\
"../images/sharp1.txt" using 1:2 with lines title "Sharpening 1 iteration",\
"../images/sharp2.txt" using 1:2 with lines title "Sharpening 2 iterations",\
"../images/sharp3.txt" using 1:2 with lines title "Sharpening 3 iterations",\
"../images/sharp10.txt" using 1:2 with lines title "Sharpening 10 iterations",\
"../images/sharp100.txt" using 1:2 with lines title "Sharpening 100 iterations"
