set src=.\src\
fl32 /Ox  %src%stft_st_grp.f %src%pgplot.f %src%FFT.f90 %src%BPFILTER.f
del *.obj *rsp