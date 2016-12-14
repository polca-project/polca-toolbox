#!/bin/bash
sudo apt-get -y install build-essential gcc g++
sudo apt-get -y install subversion
sudo apt-get -y install qt5-default

wget http://amber-v7.cs.tu-dortmund.de/lib/exe/fetch.php/tech:ogdf.v2015.05.zip
unzip tech\:ogdf.v2015.05.zip
cd OGDF
./makeMakefile.sh
make -j4
sudo make install
cd ..

wget https://sourceforge.net/projects/pyqt/files/QScintilla2/QScintilla-2.9.3/QScintilla_gpl-2.9.3.tar.gz
tar zxvf QScintilla_gpl-2.9.3.tar.gz
cd QScintilla_gpl-2.9.3/Qt4Qt5
qmake qscintilla.pro
make -j4
sudo make install
cd ../..

wget http://downloads.sourceforge.net/project/quazip/quazip/0.7.2/quazip-0.7.2.zip
unzip quazip-0.7.2.zip
cd quazip-0.7.2
qmake quazip.pro PREFIX=/usr/
make -j4
sudo make install
cd ..

svn co https://github.com/polca-project/polca-toolbox/trunk/POGADE
mkdir build
cd build
qmake ../POGADE/pogade.pro
make -j4
cd ..
cp build/pogade pogade

rm -rf OGDF
rm -rf QScintilla_gpl-2.9.3*
rm -rf quazip-0.7.2*
rm tech\:ogdf.v2015.05.zip
rm -rf build
rm -rf POGADE
