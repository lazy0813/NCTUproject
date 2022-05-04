set src=.\src\
fl32 /Ox  %src%ASC2SAC.for %src%rwsac.for %src%greg2doy.f90 %src%pgplot.for %src%BPFILTER.FOR  %src%psplot.for  %src%MSFLIB.F90
del *.obj *rsp