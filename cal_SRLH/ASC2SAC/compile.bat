set src=.\src\
fl32 /Ox  %src%ASC2SAC.for %src%rwsac.for %src%greg2doy.f90 %src%pgplot.for %src%BPFILTER.FOR
del *.obj *rsp
copy ASC2SAC.exe ..\..\bin\
copy ASC2SAC.exe C:\bin\