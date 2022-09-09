# yum packages if decide to use Amazon Linux 2.3
# but at least the aarch64 had missing libgfortran.so.3 
# so didn't implement this any further
sudo yum install openssl-devel openssl dunits2-devel \
sudo yum install gdal-devel gdal-libs gdal-python gdal  \
  arpack hdf5 libgfortran


# pff... gdal need gfortran.so.3 which was not found in amazon linux aarch64
  
