set src=.\src\
fl32 /Ox  %src%fft.f90 %src%pgplot.for %src%psplot.for  %src%MSFLIB.F90
del *.obj *rsp