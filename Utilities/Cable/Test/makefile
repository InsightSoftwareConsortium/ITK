CXX = /home/kingb2/Programs/MyGCC/inst/bin/g++
INSIGHT_INCLUDES = -I/home/kingb2/Insight/Code/Common \
		   -I/home/kingb2/Insight/Insight3DParty/vxl

vect3Test: vect3Test.C
	$(CXX) -fxml -o junk -c vect3Test.C ; rm junk

itkDirectory.xml:
	$(CXX) -fxml $(INSIGHT_INCLUDES) -o junk -c /home/kingb2/Insight/Code/Common/itkDirectory.cxx > itkDirectory.xml ; rm junk
