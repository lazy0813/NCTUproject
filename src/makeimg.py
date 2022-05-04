import numpy as np
import os
import glob
import cv2
from PIL import Image 

"""
make image for Morakot earthquake
"""
path_img = 'E:/Python/Git_rep/Seismic signal CNN/makefeature/1.seismic_image_feature/1.image_feature/imgMeqv2'
path = os.path.join(path_img, '*.txt')
files = glob.glob(path)
i=0
for f1 in files:
    i=i+1
    f1base = os.path.basename(f1)
    f1base_1 = f1base.split('.')[0]
    f1base_2 = f1base.split('.')[1]
    imgname = f1base_1[0:5]+f1base_2[4:]
    data = np.loadtxt(f1,skiprows=2)
    data = np.reshape(data, (-1, 200))
    img = Image.fromarray(np.uint8(data * 255) , 'L')
    img.save('E:/Python/CP-CNNv3.0/Backupdatav3/eq/'+imgname+'.jpg')
    
"""
make image for Morakot collapse
"""
path_img = 'E:/Python/Git_rep/Seismic signal CNN/makefeature/1.seismic_image_feature/1.image_feature/imgMv2'
path = os.path.join(path_img, '*.txt')
files = glob.glob(path)
i=0
for f1 in files:
    i=i+1
    f1base = os.path.basename(f1)
    f1base_1 = f1base.split('.')[0]
    f1base_2 = f1base.split('.')[1]
    imgname = f1base_1[0:4]+f1base_2[4:]
    data = np.loadtxt(f1,skiprows=2)
    data = np.reshape(data, (-1, 200))
    img = Image.fromarray(np.uint8(data * 255) , 'L')
    img.save('E:/Python/CP-CNNv3.0/Backupdatav3/col/'+imgname+'.jpg')

"""
make image for GeoScan collapse
"""
path_img = 'E:/Python/Git_rep/Seismic signal CNN/makefeature/1.seismic_image_feature/1.image_feature/imgGSv2'
path = os.path.join(path_img, '*.txt')
files = glob.glob(path)
i=0
for f1 in files:
    i=i+1
    f1base = os.path.basename(f1)
    f1base_1 = f1base.split('.')[0]
    f1base_2 = f1base.split('.')[1]
    imgname = f1base_1[0:3]+f1base_2[4:]
    data = np.loadtxt(f1,skiprows=2)
    data = np.reshape(data, (-1, 200))
    img = Image.fromarray(np.uint8(data * 255) , 'L')
    img.save('E:/Python/CP-CNNv3.0/Backupdatav3/col/'+imgname+'.jpg')

