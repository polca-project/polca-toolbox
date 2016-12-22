#!/bin/bash

sudo apt-get -y install cabal-install subversion ghc build-essential openjdk-8-jdk antlr alex happy
cabal update

svn co https://github.com/polca-project/polca-toolbox/trunk/C_source2source s2s
cd s2s
make configure
make rules FILE=rules.c
make all
cd ..

mkdir -p bin

cp s2s/ast bin
cp s2s/expand bin
cp s2s/polca_annotate bin
cp s2s/polca_apply bin
cp s2s/polca_changes bin
cp s2s/polca_features bin
cp s2s/polca_init bin
cp s2s/polca_pretty bin
cp s2s/polca_rollup bin
cp s2s/polca_s2s bin
cp s2s/polca_s2s_ext bin
cp s2s/polca_trans bin
cp s2s/polca_trans_int bin
cp s2s/pragma_reader bin
cp s2s/random_oracle bin
cp s2s/rules_tester bin
cp s2s/stml2has bin

rm -rf s2s

